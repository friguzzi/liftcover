import torch

def expectation_torch(par, mi, min, device, zero=1e-6):
    """
    Torch implementation of the expectation step.
    """
    # Ensure tensors are on the correct device
    par = par.to(device)
    mi = mi.to(device)
    min = min.to(device)
    
    nprobs = torch.maximum(1.0 - par, torch.tensor(zero, device=device))
    lln = torch.sum(min * torch.log(nprobs))

    eta = torch.stack([min, torch.zeros_like(min)], dim=1)
    prod = torch.prod((1 - par) ** mi, dim=1)
    probex = torch.maximum(1.0 - prod, torch.tensor(zero, device=device))
    ll = lln + torch.sum(torch.log(probex))

    condp = (par / probex[:, None])
    ocondp = torch.maximum(1 - condp, torch.tensor(0.0, device=device))
    eta10 = torch.sum(ocondp * mi, dim=0)
    eta11 = torch.sum(condp * mi, dim=0)
    eta1 = eta + torch.stack([eta10, eta11], dim=1)

    return eta1, ll

def maximization_torch(eta, regularization="no", device="cpu", zero=1e-6, gamma=10, a=0, b=10):
    """
    Torch implementation of the maximization step with various regularizations.
    """
    eta = eta.to(device)
    if regularization == "no":
        return maximization_no_torch(eta, device, zero)
    elif regularization == "l1":
        return maximization_l1_torch(eta, device, zero, gamma)
    elif regularization == "l2":
        return maximization_l2_torch(eta, device, zero, gamma)
    elif regularization == "bayesian":
        return maximization_bayesian_torch(eta, device, a, b)
    else:
        raise ValueError("Unknown regularization type")

def maximization_no_torch(eta, device, zero=1e-6):
    sum_eta = torch.sum(eta, axis=1)
    eta1 = eta[:, 1]
    par = torch.div(eta1, sum_eta, rounding_mode='floor').to(device)
    return par

def maximization_l1_torch(eta, device, zero=1e-6, gamma=10):
    eta0 = eta[:, 0]
    eta1 = eta[:, 1]
    par = 4 * eta1 / (2 * (gamma + eta0 + eta1 + torch.sqrt((eta0 + eta1) ** 2 + gamma ** 2 + 2 * gamma * (eta0 - eta1))))
    return par.to(device)

def maximization_l2_torch(eta, device, zero=1e-6, gamma=10):
    sum_eta = 3 * torch.sum(eta, axis=1) + gamma
    eta0 = eta[:, 0]
    eta1 = eta[:, 1]
    arccos = torch.arccos(torch.sqrt(gamma / sum_eta) * (9 * eta0 / 2 - 9 * eta1 + gamma) / (3 * eta0 + 3 * eta1 + gamma))
    par = 2 * torch.sqrt(sum_eta / gamma) * torch.cos(arccos / 3 - 2 * torch.pi / 3) / 3 + 1 / 3
    return par.to(device)

def maximization_bayesian_torch(eta, device, a=0, b=10):
    sum_eta = torch.sum(eta, axis=1)
    eta1 = eta[:, 1]
    par = (eta1 + a) / (sum_eta + a + b)
    return par.to(device)

def em_torch(par, mi, min, device, maxiter=100, tol=0.0001, tolr=0.00001, regularization="no", zero=1e-6, gamma=10, a=0, b=10, ver=1):
    """
    Torch implementation of the EM algorithm.
    """
    ll = -1e20
    for i in range(maxiter):
        eta, ll1 = expectation_torch(par, mi, min, device, zero)
        if ver > 3:
            print(f"Iteration {i}, Log-Likelihood: {ll1.item()}")
        par1 = maximization_torch(eta, regularization, device, zero, gamma, a, b)
        diff = torch.abs(ll1 - ll)
        par = par1
        ll = ll1
        if diff < tol or diff < (-ll1) * tolr:
            break
    return par, ll

def random_restarts_torch(mi0, min0, device="cpu", random_restarts_number=1, maxiter=100, tol=0.0001, tolr=0.00001, regularization="no", zero=1e-6, gamma=10, a=0, b=10, ver=1):
    """
    Torch implementation with random restarts to avoid local minima.
    """
    device = torch.device(device)
    mi = torch.tensor(mi0, device=device)
    min = torch.tensor(min0, device=device)
    max_ll = -1e20
    max_par = None
    for i in range(random_restarts_number):
        if ver > 2:
            print(f"Restart number {i}")
        par0 = torch.rand(len(min), device=device)
        par1, ll1 = em_torch(par0, mi, min, device, maxiter, tol, tolr, regularization, zero, gamma, a, b, ver)
        if ver > 2:
            print(f"Random restart score: {ll1.item()}")
        if ll1 > max_ll:
            max_ll = ll1
            max_par = par1
    return max_par.tolist(), max_ll.item()



def main():
    # Improved sample data: counts of observations fitting different categories
    mi0 = [
        [8, 2, 1],  # Example counts suggesting a mixture where one category is dominant
        [3, 7, 0],
        [5, 3, 4],
        [4, 1, 5],
        [2, 8, 0],
        [1, 3, 6],
        [6, 4, 0],
        [3, 3, 4],
        [5, 2, 3],
        [9, 1, 1],
    ]
    
    # Non-uniform initial probabilities that suggest a structure in the data
    min0 = [0.5, 0.5, 0.5]  

    # Choose 'cuda' if GPU is available; otherwise, 'cpu'
    device = 'cuda' if torch.cuda.is_available() else 'cpu'

    # Run the EM algorithm with random restarts
    best_params, best_ll = random_restarts_torch(
        mi0, min0, device=device, random_restarts_number=5, maxiter=50, tol=1e-4, tolr=1e-5, regularization="no", ver=4
    )

    print("Best Parameters:", best_params)
    print("Max Log-Likelihood:", best_ll)

if __name__ == "__main__":
    main()


