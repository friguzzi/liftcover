

import numpy as np
def init(algorithm="em_python", processor="cpu"):
    if algorithm=="gd_python":
        import torch
    elif algorithm=="em_python":
        if processor=="gpu":
            try:
                import cupy as cp
                cp.cuda.runtime.getDeviceCount()
                print2("GPU exists!")
            except:
                print2("GPU does not exist.")
    else:
        raise ValueError("Unknown algorithm")

def ll(pr, co, zero=0.000001):
    probs=xp.array(pr)
    counts=xp.array(co)
    nprobs=xp.maximum(1.0-probs, zero)
    return xp.sum(counts * xp.log(nprobs))

def rule_contrib(co, pr):
    probs=xp.array(pr)
    counts=xp.array(co)
    return xp.multiply.reduce((1-probs)**counts)

def update_eta(probex, eta0, par0, mi0, zero=0.000001):
    eta= xp.array(eta0)
    par= xp.array(par0)
    mi= xp.array(mi0)
    probex= max(probex, zero)
    condp = par/probex
    ocondp = 1-condp
    eta10=xp.multiply(ocondp,mi)
    eta11=xp.multiply(condp,mi)
    eta1= eta+xp.stack([eta10,eta11],1)
    return list(eta1)

def eta0(min0):
    min=xp.array(min0,dtype=xp.float64)
    return list((xp.stack([min,xp.zeros_like(min)],1)))


def lli(probs, counts, xp=np, zero=0.000001):
    nprobs=xp.maximum(1.0-probs, zero)
    return xp.sum(counts * xp.log(nprobs))


def update_etai(probex, eta, par, mi, zero=0.000001):
    probex= max(probex, zero)
    condp = par/probex
    ocondp = 1-condp
    eta10=xp.multiply(ocondp,mi)
    eta11=xp.multiply(condp,mi)
    eta1= eta+xp.stack([eta10,eta11],1)
    return eta1

def eta0i(min,xp=np):
    return xp.stack([min,xp.zeros_like(min)],1)

def expectation(par,mi,min, xp=np, zero=0.000001):
    lln=lli(par,min,xp,zero)
    eta=eta0i(min,xp)
    prod=xp.multiply.reduce((1-par)**mi,axis=1)
    #probex=xp.where(probex==0.0,zero,probex)
    probex=xp.maximum(1.0-prod, zero)
    ll=lln+xp.sum(xp.log(probex))
    condp = xp.divide(par[xp.newaxis,:].T,probex).T
    ocondp = xp.maximum(1-condp,0)
    eta10=xp.sum(xp.multiply(ocondp,mi),axis=0)
    eta11=xp.sum(xp.multiply(condp,mi),axis=0)
    eta1= eta+xp.stack([eta10,eta11],1)
    return eta1, ll

def maximization(eta, regularization="no", xp=np, zero=0.000001, gamma=10, a=0,b=10):
    if regularization=="no":
        return maximization_no(eta, xp, zero)
    elif regularization=="l1":
        return maximization_l1(eta, xp, zero, gamma)
    elif regularization=="l2":
        return maximization_l2(eta, xp, zero, gamma)
    elif regularization=="bayesian":
        return maximization_bayesian(eta, xp, a, b)
    else:
        raise ValueError("Unknown regularization type")
    
def maximization_no(eta, xp=np, zero=0.000001):
    sum=xp.sum(eta,axis=1)
    eta1=eta[:,1]
    par=xp.divide(eta1,sum,where=sum!=0.0)
    return par

def maximization_l1(eta, xp=np, zero=0.000001, gamma=10):
    eta0=eta[:,0]
    eta1=eta[:,1]
    par=4*eta1/(2*(gamma+eta0+eta1+xp.sqrt((eta0+eta1)**2+gamma**2+2*gamma*(eta0-eta1))))
    return par

def maximization_l2(eta, xp=np, zero=0.000001, gamma=10):
    sum=3*xp.sum(eta,axis=1)+gamma
    eta0=eta[:,0]
    eta1=eta[:,1]
    arccos=xp.arccos(xp.sqrt(gamma/sum)*(9*eta0/2-9*eta1+gamma)/(3*eta0+3*eta1+gamma))
    par=2*xp.sqrt(sum/gamma)*xp.cos(arccos/3-2*xp.pi/3)/3+1/3
    return par

def maximization_bayesian(eta, xp=np, a=0,b=10):
    sum=xp.sum(eta,axis=1)
    eta1=eta[:,1]
    par=xp.divide(eta1+a,sum+a+b)
    return par

def em(par, mi, min, xp=np, maxiter=100, tol=0.0001, tolr=0.00001, regularization="no", zero=0.000001, gamma=10, a=0,b=10, ver=1):
    ll=-1e20
    for i in range(maxiter):
        eta, ll1=expectation(par,mi,min,xp,zero)
        print4(ver,"Iteration ",i, " LL ",ll1.item())
        par1=maximization(eta,regularization,xp,zero,gamma,a,b)
        diff=xp.abs(ll1-ll)
        par=par1
        ll=ll1
        if diff<tol or diff<(-ll1)*tolr:
            break
    return par, ll

def random_restarts(mi0,min0,device="cpu",random_restarts_number=1, maxiter=100, tol=0.0001, tolr=0.00001, regularization="no", zero=0.000001, gamma=10, a=0,b=10, ver=1):
    if device=="gpu":
        import cupy as xp
    else:
        xp=np
    mi= xp.array(mi0)
    min=xp.array(min0)
    max_ll=-1e20
    max_par=[]
    for i in range(random_restarts_number):
        print3(ver,"Restart number ",i)
        par0= xp.random.uniform(0.0,1.0,len(min))
        par1, ll1=em(par0,mi,min,xp,maxiter,tol,tolr,regularization,zero,gamma,a,b,ver)
        print3(ver,"Random_restart: Score ",ll1.item())
        if ll1>max_ll:
            max_ll=ll1
            max_par=par1
    return max_par.tolist(), max_ll.item()


def lltorch(probs, counts, device, torch, zero=0.000001):
    one=torch.tensor(1.0,device=device)
    zero=torch.tensor(zero,device=device)
    nprobs=one-probs
    nprobs=nprobs.maximum(zero)
    return torch.sum(counts * nprobs.log())

def compute_ll(mi,min,parR,zero=0.000001):
    parR=torch.tensor(parR,requires_grad=True)
    mi= torch.tensor(mi)
    min=torch.tensor(min)
    par=torch.special.expit(parR)
    print("parR ",parR)
    print("par ",par)
    lln=lltorch(par,min)
    print("lln ",lln)
    #print("mi ",mi,"min ",min,"par ",par)
    one=torch.tensor(1.0)
    zero=torch.tensor(zero)
    prod=torch.sum(torch.log(one-par)*mi,axis=1)
    print("prod ",prod)
    probex=torch.maximum(one-torch.exp(prod), zero)
    ll=lln+torch.sum(torch.log(probex))
    print("ll ",ll)
    ll.backward()
    print("par.grad ",parR.grad)
    return ll.item()

class Model:
    def __init__(self,min,mi,device,torch,parR=False,regularization="no",gamma=10,zero=0.000001):
        self.min=min
        self.mi=mi
        self.zero=zero
        self.regularization=regularization
        self.gamma=gamma
        self.device=device
        self.torch=torch
        if parR:
            self.parR=torch.tensor(parR,dtype=torch.float64,requires_grad=Truei,device=device)
        else:
            self.parR=torch.randn_like(self.min,dtype=torch.float64,requires_grad=True,device=device)

    def forward(self,parR):
        par=self.torch.special.expit(parR)
        lln=lltorch(par,self.min,self.device,self.torch,self.zero)
        one=self.torch.tensor(1.0,device=self.device)
        zero=self.torch.tensor(self.zero,device=self.device)
        prod=self.torch.sum(self.torch.log(one-par)*self.mi,axis=1)
        probex=self.torch.maximum(one-self.torch.exp(prod), zero)
        ll=lln+self.torch.sum(self.torch.log(probex))
        ll=-ll
        if self.regularization=="l1":
            ll=ll+self.gamma*self.torch.sum(par)
        elif self.regularization=="l2":
            ll=ll+self.gamma*self.torch.sum(self.torch.square(par))
            
        return ll
    
    def parameters(self):
        return [self.parR]

def gd(min,mi,device,torch,parR=False,maxiter=1000,tol=0.0001, opt="fixed_learning_rate", 
       regularization="no", gamma=10, lr=0.01,
       lr_adam=0.001, betas=(0.9,0.999), eps=1e-8, zero=0.000001, ver=0):
    model=Model(min,mi,device,torch,parR,regularization,gamma,zero)

    if opt=="fixed_learning_rate":
        optimizer = torch.optim.SGD(model.parameters(), lr=lr)
    elif opt=="adam":
        optimizer = torch.optim.Adam(model.parameters(), lr=lr_adam, betas=betas, eps=eps)
    else:
        raise ValueError("Unknown optimizer")
    
    ll=1e20
    for i in range(maxiter):
        optimizer.zero_grad()
        ll1=model.forward(model.parR)
        print4(ver,"GD iteration ",i, " LL ",ll1.item())
        ll1.backward()
        optimizer.step()
        diff=torch.abs(ll1-ll)
        ll=ll1
        if diff.item()<tol:
            break
        
    return model.parR, -ll


def random_restarts_gd(mi,min,processor="cpu",random_restarts_number=1, 
                       opt="fixed_learning_rate", maxiter=100, tol=0.0001, regularization="no", 
                        gamma=10, lr=0.01,
                        lr_adam=0.001, betas=(0.9,0.999), eps=1e-8, zero=0.000001, ver=0):
    import torch
    if processor=="gpu":
        device = torch.device('cuda')
    elif processor=="cpu":
        device=torch.device('cpu')
    else:
        raise ValueError("Unknown processor")

    min=torch.tensor(min,device=device)
    mi=torch.tensor(mi,device=device)
    max_ll=-1e20
    max_par=[]
    
    for i in range(random_restarts_number):
        print3(ver,"GD Restart number ",i)
        par1, ll1=gd(min,mi,device,torch,maxiter=maxiter,tol=tol, opt=opt, regularization=regularization,
                     gamma=gamma, lr=lr, lr_adam=lr_adam, 
                     betas=betas, eps=eps, zero=zero, ver=ver)
        print3(ver,"GD Random_restart: Score ",ll1.item())
        if ll1>max_ll:
            max_ll=ll1
            max_par=par1
    par=torch.special.expit(max_par)
    return par.tolist(), max_ll.item()

def print1(_,*arg):
    print(*arg)

def print2(ver,*arg):
    if ver>1:
        print(*arg)

def print3(ver,*arg):
    if ver>2:
        print(*arg)

def print4(ver,*arg):
    if ver>3:
        print(*arg)

