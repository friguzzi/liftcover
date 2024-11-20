import torch
import random

def lli_torch(probs, counts, zero=1e-6):
    # Log-Likelihood calculation with safeguard against zero probabilities
    nprobs = torch.clamp(1.0 - probs, min=zero)
    return torch.sum(counts * torch.log(nprobs))

def eta0i_torch(min):
    # Initial eta with min values and zeros
    return torch.stack([min, torch.zeros_like(min)], dim=1)

def expectation_torch(par, mi, min, zero=1e-6):
    # Expectation step
    lln = lli_torch(par, min, zero)
    eta = eta0i_torch(min)

    # par_broadcasted = par.unsqueeze(0).expand_as(mi)  
    prod = (1 - par).pow(mi).prod(dim=1) 

    probex = torch.clamp(1.0 - prod, min=zero)
    ll = lln + torch.sum(torch.log(probex))

    condp = (par / probex.unsqueeze(1))  
    ocondp = torch.clamp(1 - condp, min=0.0)

    # Use in-place operations
    eta[:, 0].add_((ocondp * mi).sum(dim=0))  # In-place addition
    eta[:, 1].add_((condp * mi).sum(dim=0))   # In-place addition

    return eta, ll

def expectation_torch_in_chunks(par, mi, min_vals, chunk_size=100, zero=1e-6):
    lln = lli_torch(par, min_vals, zero)
    eta = eta0i_torch(min_vals)

    num_chunks = (mi.size(0) + chunk_size - 1) // chunk_size  # Calculate number of chunks

    for i in range(num_chunks):
        # Select the current chunk
        start = i * chunk_size
        end = min(start + chunk_size, mi.size(0))  # Use Python's built-in min function

        mi_chunk = mi[start:end]

        # Apply element-wise operations for the chunk
        prod_chunk = (1 - par).pow(mi_chunk).prod(dim=1)
        probex_chunk = torch.clamp(1.0 - prod_chunk, min=zero)

        # Update the log-likelihood for the chunk
        lln.add_(torch.sum(torch.log(probex_chunk)))

        # Update eta for the chunk
        condp_chunk = par / probex_chunk.unsqueeze(1)
        ocondp_chunk = torch.clamp(1 - condp_chunk, min=0.0)

        eta[:, 0].add_((ocondp_chunk * mi_chunk).sum(dim=0))
        eta[:, 1].add_((condp_chunk * mi_chunk).sum(dim=0))

    return eta, lln




def maximization_no_torch(eta, zero=1e-6):
    # Maximization step without regularization
    sum_vals = eta.sum(dim=1)
    eta1 = eta[:, 1]
    # Use in-place division with torch.where to handle zero safely
    par = torch.empty_like(eta1).copy_(eta1)
    par.div_(torch.where(sum_vals != 0.0, sum_vals, torch.tensor(zero, device=eta.device)))
    return par

def maximization_l1_torch(eta, zero=1e-6, gamma=10):
    # Maximization step with l1 regularization
    eta0 = eta[:, 0]
    eta1 = eta[:, 1]
    denominator = 2 * (gamma + eta0 + eta1 + (eta0 + eta1).pow_(2).add_(gamma ** 2).add_(2 * gamma * (eta0 - eta1)).sqrt_())
    par = 4 * eta1 / denominator
    return par

def maximization_l2_torch(eta, zero=1e-6, gamma=10):
    # Maximization step with l2 regularization
    sum_vals = 3 * eta.sum(dim=1).add_(gamma)
    eta0 = eta[:, 0]
    eta1 = eta[:, 1]
    arccos_input = ((gamma / sum_vals).sqrt_() * (9 * eta0 / 2 - 9 * eta1 + gamma) / (3 * eta0 + 3 * eta1 + gamma)).clamp_(-1.0, 1.0)
    arccos = arccos_input.acos_()
    par = 2 * (sum_vals / gamma).sqrt_() * (arccos / 3 - 2 * torch.pi / 3).cos_() / 3 + 1 / 3
    return par

def maximization_bayesian_torch(eta, a=0, b=10):
    # Maximization step with Bayesian regularization
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
    # EM algorithm with torch
    ll = -1e20
    for i in range(maxiter):
        eta, ll1 = expectation_torch_in_chunks(par, mi, min)
        print4(ver,f"Iteration {i}, Log-Likelihood: {ll1.item()}")
        
        par1 = maximization_torch(eta, regularization, zero, gamma, a, b)
        diff = torch.abs(ll1 - ll)
        par.copy_(par1)  # In-place update
        ll = ll1
        if diff < tol or diff < (-ll1) * tolr:
            break
    return par, ll

def fixed_initial_par(mi0, min0, device="cpu", parameter=0.90, maxiter=100, tol=1e-4, tolr=1e-5, regularization="no", zero=1e-6, gamma=10, a=0, b=10, ver=1):
    if device=="gpu":
        device="cuda"
    # Random restarts EM algorithm with torch
    xp_device = torch.device(device)
    mi = torch.tensor(mi0, dtype=torch.float32, device=xp_device)
    min = torch.tensor(min0, dtype=torch.float32, device=xp_device)
    par0 = torch.empty((len(min),), device=xp_device)
    par0=par0.new_full((len(min),),parameter)
    par, ll = em_torch(par0, mi, min, maxiter, tol, tolr, regularization, zero, gamma, a, b, ver)
    return par.tolist(), ll.item()

def random_restarts_torch(mi0, min0, device="cpu", random_restarts_number=1, maxiter=100, tol=1e-4, tolr=1e-5, regularization="no", zero=1e-6, gamma=10, a=0, b=10, ver=1):
    if device=="gpu":
        device="cuda"
    # Random restarts EM algorithm with torch
    xp_device = torch.device(device)
    mi = torch.tensor(mi0, dtype=torch.float32, device=xp_device)
    min = torch.tensor(min0, dtype=torch.float32, device=xp_device)
    
    max_ll = -1e20
    max_par = torch.empty(0, device=xp_device)
    
    for i in range(random_restarts_number):
        print4(ver,f"Restart number {i}")
        par0 = torch.rand(len(min), device=xp_device)
        par1, ll1 = em_torch(par0, mi, min, maxiter, tol, tolr, regularization, zero, gamma, a, b, ver)
        
        print4(ver,f"Random restart score: {ll1.item()}")
        if ll1 > max_ll:
            max_ll = ll1
            max_par = par1.detach().clone()  # Use detach() to avoid gradient tracking issues
    
    return max_par.tolist(), max_ll.item()

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
def main():
    '''mi0 = [
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
    '''
    mi0=[
        [1,1,1]
    ]
#    min0 = [3, 4, 6]
    min0 = [0, 0, 0]
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    regularization = "l1" #random.choice(["no", "l1", "l2", "bayesian"])
    print("Regularization:", regularization)
    best_params, best_ll = fixed_initial_par(mi0, min0, device=device, regularization=regularization)
    #random_restarts_torch(
    #    mi0, min0, device=device, random_restarts_number=5, maxiter=50, tol=1e-4, tolr=1e-5, regularization=regularization, ver=4
    #)

    print("Best Parameters:", best_params)
    print("Max Log-Likelihood:", best_ll)

if __name__ == "__main__":
    main()
