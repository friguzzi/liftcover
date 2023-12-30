

import numpy as xp
import torch

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


def lli(probs, counts, zero=0.000001):
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

def eta0i(min):
    return xp.stack([min,xp.zeros_like(min)],1)

def expectation(par,mi,min, zero=0.000001):
    lln=lli(par,min)
    eta=eta0i(min)
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
    sum=xp.sum(eta,axis=1)
    eta1=eta[:,1]
    par=xp.divide(eta1,sum,where=sum!=0.0)
    return par

def maximization_l1(eta, zero=0.000001, gamma=10):
    eta0=eta[:,0]
    eta1=eta[:,1]
    par=4*eta1/(2*(gamma+eta0+eta1+xp.sqrt((eta0+eta1)**2+gamma**2+2*gamma*(eta0-eta1))))
    return par

def maximization_l2(eta, zero=0.000001, gamma=10):
    sum=3*xp.sum(eta,axis=1)+gamma
    eta0=eta[:,0]
    eta1=eta[:,1]
    arccos=xp.arccos(xp.sqrt(gamma/sum)*(9*eta0/2-9*eta1+gamma)/(3*eta0+3*eta1+gamma))
    par=2*xp.sqrt(sum/gamma)*xp.cos(arccos/3-2*xp.pi/3)/3+1/3
    return par

def maximization_bayesian(eta, a=0,b=10):
    sum=xp.sum(eta,axis=1)
    eta1=eta[:,1]
    par=xp.divide(eta1+a,sum+a+b)
    return par

def em(par, mi, min, maxiter=100, tol=0.0001, tolr=0.00001, regularization="no", zero=0.000001, gamma=10, a=0,b=10):
    ll=-1e20
    for i in range(maxiter):
        eta, ll1=expectation(par,mi,min,zero)
        par1=maximization(eta,regularization,zero,gamma,a,b)
        diff=xp.abs(ll1-ll)
        par=par1
        ll=ll1
        if diff<tol or diff<(-ll1)*tolr:
            break
    return par, ll

def random_restarts(mi0,min0,random_restarts_number=1, maxiter=100, tol=0.0001, tolr=0.00001, regularization="no", zero=0.000001, gamma=10, a=0,b=10):
    mi= xp.array(mi0)
    min=xp.array(min0)
    max_ll=-1e20
    max_par=[]
    for i in range(random_restarts_number):
        par0= xp.random.uniform(0.0,1.0,len(min))
        par1, ll1=em(par0,mi,min,maxiter,tol,tolr,regularization,zero,gamma,a,b)
        if ll1>max_ll:
            max_ll=ll1
            max_par=par1
    return list(max_par), max_ll

def compute_ll(mi,min,parR,zero=0.000001):
    parR=xp.array(parR)
    mi= xp.array(mi)
    min=xp.array(min)
    par=1/(1+xp.exp(-parR))
    print("parR ",parR)
    print("par ",par)
    lln=lli(par,min)
    print("lln ",lln)
    #print("mi ",mi,"min ",min,"par ",par)
    prod=xp.multiply.reduce((1-par)**mi,axis=1)
    probex=xp.maximum(1.0-prod, zero)
    ll=lln+xp.sum(xp.log(probex))
    return ll