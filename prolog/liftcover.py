

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