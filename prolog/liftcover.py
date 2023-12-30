

import numpy as np

def ll(pr, co, zero=0.000001):
    probs=np.array(pr)
    counts=np.array(co)
    nprobs=np.maximum(1.0-probs, zero)
    return np.sum(counts * np.log(nprobs))

def rule_contrib(co, pr):
    probs=np.array(pr)
    counts=np.array(co)
    return np.multiply.reduce((1-probs)**counts)

def update_eta(probex, eta0, par0, mi0, zero=0.000001):
    eta= np.array(eta0)
    par= np.array(par0)
    mi= np.array(mi0)
    probex= max(probex, zero)
    condp = par/probex
    ocondp = 1-condp
    eta10=np.multiply(ocondp,mi)
    eta11=np.multiply(condp,mi)
    eta1= eta+np.stack([eta10,eta11],1)
    return list(eta1)

def eta0(min0):
    min=np.array(min0,dtype=np.float64)
    return list((np.stack([min,np.zeros_like(min)],1)))


def lli(probs, counts, zero=0.000001):
    nprobs=np.maximum(1.0-probs, zero)
    return np.sum(counts * np.log(nprobs))


def update_etai(probex, eta, par, mi, zero=0.000001):
    probex= max(probex, zero)
    condp = par/probex
    ocondp = 1-condp
    eta10=np.multiply(ocondp,mi)
    eta11=np.multiply(condp,mi)
    eta1= eta+np.stack([eta10,eta11],1)
    return eta1

def eta0i(min):
    return np.stack([min,np.zeros_like(min)],1)

def expectation(par0,mi0,min0, zero=0.000001):
    par= np.array(par0)
    mi= np.array(mi0)
    min=np.array(min0)
    lln=lli(par,min)
    eta=eta0i(min)
    prod=np.multiply.reduce((1-par)**mi,axis=1)
    #probex=np.where(probex==0.0,zero,probex)
    probex=np.maximum(1.0-prod, zero)
    ll=lln+np.sum(np.log(probex))
    condp = np.divide(par[np.newaxis,:].T,probex).T
    ocondp = np.maximum(1-condp,0)
    eta10=np.sum(np.multiply(ocondp,mi),axis=0)
    eta11=np.sum(np.multiply(condp,mi),axis=0)
    eta1= eta+np.stack([eta10,eta11],1)
    return list((eta1)), ll

def maximization(eta, regularization="no", zero=0.000001, gamma=10, a=0,b=10):
    if regularization=="no":
        return maximization_no(eta, zero)
    elif regularization=="l1":
        return maximization_l1(eta, zero, gamma)
    elif regularization=="l2":
        return maximization_l2(eta, zero, gamma)
    elif regularization=="bayesian":
        return maximization_bayesian(eta, a, b)
    else:
        raise ValueError("Unknown regularization type")
    
def maximization_no(eta, zero=0.000001):
    eta=np.array(eta)
    sum=np.sum(eta,axis=1)
    eta1=eta[:,1]
    par=np.divide(eta1,sum,where=sum!=0.0)
    return list(par)

def maximization_l1(eta, zero=0.000001, gamma=10):
    eta=np.array(eta)
    eta0=eta[:,0]
    eta1=eta[:,1]
    par=4*eta1/(2*(gamma+eta0+eta1+np.sqrt((eta0+eta1)**2+gamma**2+2*gamma*(eta0-eta1))))
    return list(par)

def maximization_l2(eta, zero=0.000001, gamma=10):
    eta=np.array(eta)
    sum=3*np.sum(eta,axis=1)+gamma
    eta0=eta[:,0]
    eta1=eta[:,1]
    arccos=np.arccos(np.sqrt(gamma/sum)*(9*eta0/2-9*eta1+gamma)/(3*eta0+3*eta1+gamma))
    par=2*np.sqrt(sum/gamma)*np.cos(arccos/3-2*np.pi/3)/3+1/3
    return list(par)

def maximization_bayesian(eta, a=0,b=10):
    eta=np.array(eta)
    sum=np.sum(eta,axis=1)
    eta1=eta[:,1]
    par=np.divide(eta1+a,sum+a+b)
    return list(par)

def em(par0, mi0, min0, maxiter=100, tol=0.0001, zero=0.000001, regularization="no", gamma=10, a=0,b=10):
    par=par0
    mi=mi0
    min=min0
    ll=0
    for i in range(maxiter):
        eta, ll1=expectation(par,mi,min,zero)
        par1=maximization(eta,regularization,zero,gamma,a,b)
        if np.abs(ll1-ll)<tol:
            break
        par=par1
        ll=ll1
    return par, ll