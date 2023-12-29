

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
    print("par ",par)
    probex=np.multiply.reduce((1-par)**mi,axis=1)
    nprobex=np.maximum(1.0-probex, zero)
    ll=lln+np.sum(np.log(nprobex))
    print("probex ",probex)
    print("ll ",ll)
    probex= np.maximum(probex, zero)
    condp = np.divide(par[np.newaxis,:].T,probex).T
    print("condp ",condp)
    ocondp = 1-condp
    print("ocondp ",ocondp)
    print("mi ",mi )
    eta10=np.multiply(ocondp,mi)
    print("eta10 ",eta10)
    eta11=np.multiply(condp,mi)
    print("eta11 ",eta11)
    eta1= eta+np.stack([eta10,eta11],1)
    return list((eta)), ll