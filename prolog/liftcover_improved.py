import torch
import torch.nn as nn
from torch.optim import Adam, SGD

def init_pytorch(processor="cpu"):
    # Set device based on the processor
    if processor == "gpu":
        if torch.cuda.is_available():
            device = torch.device("cuda")
        elif torch.backends.mps.is_available():  # For macOS MPS backend
            device = torch.device("mps")
        else:
            raise RuntimeError("GPU requested, but no GPU is available.")
    elif processor == "cpu":
        device = torch.device("cpu")
    else:
        raise ValueError("Unknown processor")
    
    return device

class EMGDModel(nn.Module):
    def __init__(self, min_counts, mi_counts, regularization="no", gamma=10, zero=1e-6):
        super(EMGDModel, self).__init__()
        self.min_counts = min_counts
        self.mi_counts = mi_counts
        self.zero = zero
        self.regularization = regularization
        self.gamma = gamma
        self.par = nn.Parameter(torch.rand(min_counts.shape[1], dtype=torch.float32))
        
    def forward(self):
        par = torch.sigmoid(self.par)  # Sigmoid to keep parameters in [0, 1]
        log_likelihood = self.compute_log_likelihood(par)
        
        if self.regularization == "l1":
            log_likelihood -= self.gamma * torch.sum(par)
        elif self.regularization == "l2":
            log_likelihood -= self.gamma * torch.sum(par ** 2)
        
        return -log_likelihood  # Minimize negative log-likelihood

    def compute_log_likelihood(self, par):
        one = torch.tensor(1.0, device=par.device)
        zero = torch.tensor(self.zero, device=par.device)
        
        # Compute the product term
        prod = torch.sum(torch.log(one - par) * self.mi_counts, dim=1)
        prob_ex = torch.maximum(one - torch.exp(prod), zero)
        
        # Log likelihood of observed data
        log_likelihood = self.compute_lli(par) + torch.sum(torch.log(prob_ex))
        return log_likelihood

    def compute_lli(self, par):
        nprobs = torch.maximum(torch.tensor(1.0, device=par.device) - par, torch.tensor(self.zero, device=par.device))
        return torch.sum(self.min_counts * torch.log(nprobs))

def em_pytorch(min_counts, mi_counts, processor="cpu", maxiter=100, tol=1e-4, lr=1e-3, 
               regularization="no", gamma=10, zero=1e-6, ver=1, use_multiprocessing=False):
    device = init_pytorch(processor)
    min_counts = torch.tensor(min_counts, dtype=torch.float32, device=device)
    mi_counts = torch.tensor(mi_counts, dtype=torch.float32, device=device)
    
    model = EMGDModel(min_counts, mi_counts, regularization, gamma, zero).to(device)

    # Use DataParallel for multi-GPU, if available
    if use_multiprocessing and torch.cuda.device_count() > 1:
        model = nn.DataParallel(model)

    optimizer = Adam(model.parameters(), lr=lr)
    prev_loss = float('inf')

    for epoch in range(maxiter):
        optimizer.zero_grad()
        loss = model()
        loss.backward()
        optimizer.step()

        diff = torch.abs(prev_loss - loss.item())
        if ver > 1:
            print(f"EM Epoch {epoch + 1}/{maxiter}, Loss: {loss.item()}")

        if diff < tol:
            break

        prev_loss = loss.item()

    return torch.sigmoid(model.par).detach().cpu().numpy(), -prev_loss

def gd_pytorch(min_counts, mi_counts, processor="cpu", maxiter=1000, tol=1e-4, lr=1e-2, 
               opt="fixed_learning_rate", regularization="no", gamma=10, zero=1e-6, ver=1):
    device = init_pytorch(processor)
    min_counts = torch.tensor(min_counts, dtype=torch.float32, device=device)
    mi_counts = torch.tensor(mi_counts, dtype=torch.float32, device=device)

    model = EMGDModel(min_counts, mi_counts, regularization, gamma, zero).to(device)

    # Use DataParallel for multi-GPU, if available
    if torch.cuda.device_count() > 1:
        model = nn.DataParallel(model)

    # Select optimizer based on provided option
    if opt == "fixed_learning_rate":
        optimizer = SGD(model.parameters(), lr=lr)
    elif opt == "adam":
        optimizer = Adam(model.parameters(), lr=lr)
    else:
        raise ValueError("Unknown optimizer")

    prev_loss = float('inf')

    for epoch in range(maxiter):
        optimizer.zero_grad()
        loss = model()
        loss.backward()
        optimizer.step()

        diff = torch.abs(prev_loss - loss.item())
        if ver > 1:
            print(f"GD Epoch {epoch + 1}/{maxiter}, Loss: {loss.item()}")

        if diff < tol:
            break

        prev_loss = loss.item()

    return torch.sigmoid(model.par).detach().cpu().numpy(), -prev_loss

def random_restarts_em_gd(min_counts, mi_counts, processor="cpu", random_restarts_number=1, 
                          maxiter=100, tol=0.0001, method="em", opt="fixed_learning_rate", 
                          regularization="no", gamma=10, lr=0.01, zero=0.000001, ver=1):
    best_params = None
    best_ll = -float('inf')
    
    for i in range(random_restarts_number):
        if ver > 1:
            print(f"Random Restart {i+1}/{random_restarts_number} for {method.upper()}")

        if method == "em":
            params, ll = em_pytorch(min_counts, mi_counts, processor, maxiter, tol, lr, 
                                    regularization, gamma, zero, ver)
        elif method == "gd":
            params, ll = gd_pytorch(min_counts, mi_counts, processor, maxiter, tol, lr, 
                                    opt, regularization, gamma, zero, ver)
        else:
            raise ValueError("Unknown method")

        if ll > best_ll:
            best_ll = ll
            best_params = params

        if ver > 1:
            print(f"Current Best LL: {best_ll}")

    return best_params, best_ll

# Example usage
if __name__ == "__main__":
    min_counts = [[10, 20], [15, 5]]  # Example data
    mi_counts = [[5, 10], [3, 7]]
    
    # Run EM with random restarts
    em_params, em_ll = random_restarts_em_gd(min_counts, mi_counts, processor="gpu", 
                                             random_restarts_number=3, maxiter=100, tol=1e-4, 
                                             method="em", ver=2)
    print("EM Estimated Parameters:", em_params)
    print("EM Log Likelihood:", em_ll)

    # Run GD with random restarts
    gd_params, gd_ll = random_restarts_em_gd(min_counts, mi_counts, processor="gpu", 
                                             random_restarts_number=3, maxiter=100, tol=1e-4, 
                                             method="gd", opt="adam", lr=0.01, ver=2)
    print("GD Estimated Parameters:", gd_params)
    print("GD Log Likelihood:", gd_ll)
