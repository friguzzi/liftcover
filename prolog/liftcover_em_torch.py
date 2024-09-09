import torch
import random

def lli_torch(probs, counts, zero=1e-6):
    # Log-Likelihood calculation with safeguard against zero probabilities
    nprobs = torch.maximum(1.0 - probs, torch.tensor(zero, device=probs.device))
    return torch.sum(counts * torch.log(nprobs))

def eta0i_torch(min):
    # Initial eta with min values and zeros
    return torch.stack([min, torch.zeros_like(min)], dim=1)

def expectation_torch(par, mi, min, zero=1e-6):
    # Expectation step
    lln = lli_torch(par, min, zero)
    eta = eta0i_torch(min)

    prod = ((1 - par).unsqueeze(0).expand_as(mi).pow(mi)).prod(dim=1)
    probex = torch.maximum(1.0 - prod, torch.tensor(zero, device=prod.device))
    ll = lln + torch.sum(torch.log(probex))

    condp = (par.unsqueeze(0) / probex.unsqueeze(1)).expand_as(mi)
    ocondp = torch.maximum(1 - condp, torch.tensor(0.0, device=condp.device))

    eta[:, 0].add_((ocondp * mi).sum(dim=0))  # In-place addition
    eta[:, 1].add_((condp * mi).sum(dim=0))   # In-place addition

    return eta, ll

def maximization_no_torch(eta, zero=1e-6):
    # Maximization step without regularization
    sum_vals = eta.sum(dim=1)
    eta1 = eta[:, 1]
    par = eta1 / torch.where(sum_vals != 0.0, sum_vals, torch.tensor(zero, device=eta.device))
    return par

def maximization_l1_torch(eta, zero=1e-6, gamma=10):
    # Maximization step with l1 regularization
    eta0 = eta[:, 0]
    eta1 = eta[:, 1]
    denominator = 2 * (gamma + eta0 + eta1 + ((eta0 + eta1) ** 2 + gamma ** 2 + 2 * gamma * (eta0 - eta1)).sqrt_())
    par = 4 * eta1 / denominator
    return par

def maximization_l2_torch(eta, zero=1e-6, gamma=10):
    # Maximization step with l2 regularization
    sum_vals = 3 * eta.sum(dim=1) + gamma
    eta0 = eta[:, 0]
    eta1 = eta[:, 1]
    arccos_input = (gamma / sum_vals).sqrt_() * (9 * eta0 / 2 - 9 * eta1 + gamma) / (3 * eta0 + 3 * eta1 + gamma)
    arccos = arccos_input.acos()
    par = 2 * (sum_vals / gamma).sqrt_() * (arccos / 3 - 2 * torch.pi / 3).cos_() / 3 + 1 / 3
    return par

def maximization_bayesian_torch(eta, a=0, b=10):
    # Maximization step with ayesian regularization
    sum_vals = eta.sum(dim=1)
    eta1 = eta[:, 1]
    par = (eta1 + a) / (sum_vals + a + b)
    return par

def maximization_torch(eta, regularization="no", zero=1e-6, gamma=10, a=0, b=10):
    # Switch between different regularization methods
    if regularization == "no":
        return maximization_no_torch(eta, zero)
    elif regularization == "l1":
        return maximization_l1_torch(eta, zero, gamma)
    elif regularization == "l2":
        return maximization_l2_torch(eta, zero, gamma)
    elif regularization == "bayesian":
        return maximization_bayesian_torch(eta, a, b)
    else:
        raise ValueError("Unknown regularization type")

def em_torch(par, mi, min, maxiter=100, tol=1e-4, tolr=1e-5, regularization="no", zero=1e-6, gamma=10, a=0, b=10, ver=1):
    # EM algorithm  with torch
    ll = -1e20
    for i in range(maxiter):
        eta, ll1 = expectation_torch(par, mi, min, zero)
        print(f"Iteration {i}, Log-Likelihood: {ll1.item()}")
        
        par1 = maximization_torch(eta, regularization, zero, gamma, a, b)
        diff = torch.abs(ll1 - ll)
        par.copy_(par1)  # In-place update
        ll = ll1
        if diff < tol or diff < (-ll1) * tolr:
            break
    return par, ll

def random_restarts_torch(mi0, min0, device="cpu", random_restarts_number=1, maxiter=100, tol=1e-4, tolr=1e-5, regularization="no", zero=1e-6, gamma=10, a=0, b=10, ver=1):
    # Random restarts EM algorithm with torch
    xp_device = torch.device(device)
    mi = torch.tensor(mi0, dtype=torch.float32, device=xp_device)
    min = torch.tensor(min0, dtype=torch.float32, device=xp_device)
    
    max_ll = -1e20
    max_par = torch.empty(0, device=xp_device)
    
    for i in range(random_restarts_number):
        print(f"Restart number {i}")
        par0 = torch.rand(len(min), device=xp_device)
        par1, ll1 = em_torch(par0, mi, min, maxiter, tol, tolr, regularization, zero, gamma, a, b, ver)
        
        print(f"Random restart score: {ll1.item()}")
        if ll1 > max_ll:
            max_ll = ll1
            max_par = par1.clone()
    
    return max_par.tolist(), max_ll.item()

def main():
    mi0 = [
        [8, 2, 1],  
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
    
    min0 = [3, 4, 6]
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    regularization = random.choice(["no", "l1", "l2", "bayesian"])
    print("Regularization:", regularization)
    best_params, best_ll = random_restarts_torch(
        mi0, min0, device=device, random_restarts_number=5, maxiter=50, tol=1e-4, tolr=1e-5, regularization=regularization, ver=4
    )

    print("Best Parameters:", best_params)
    print("Max Log-Likelihood:", best_ll)

if __name__ == "__main__":
    main()
