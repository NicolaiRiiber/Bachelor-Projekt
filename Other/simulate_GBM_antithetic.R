#' Simulate GBM antithetic method
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_GBM_AV <- function(x0, mu, sigma, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- x0
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((mu-0.5*sigma^2)*dt + sigma * sqrt(dt) * z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] * exp((mu-0.5*sigma^2)*dt + sigma * sqrt(dt) * z2)
      AV_paths[i, k] <- (simulated_paths[i, k] + simulated_paths2[i, k])/2
    }
  }
  
  return(AV_paths)
  
}


r = log(1 + 0.03)
dt = 1/mSteps
S0 = 100
Simulated_path <-matrix(nrow=10, ncol= 10)
Simulated_path2 <-matrix(nrow=10, ncol= 10)
AV_path <- matrix(nrow = 10, ncol = 10)
Simulated_path[,1] <- Simulated_path2[,1] <-AV_path[,1] <- S0 


for(j in 1:10){                                       
    for(i in 2:10){
        X1 <- rnorm(1,0,1)
        X2 <- -X1
        Simulated_path[j,i] <- Simulated_path[j,i-1] * exp((r-(0.25^2)/2)*dt+0.25*sqrt(dt)*X1)
        Simulated_path2[j,i] <- Simulated_path2[j,i-1] * exp((r-(0.25^2)/2)*dt+0.25*sqrt(dt)*X2)
        AV_path[j,i] <- (Simulated_path[j, i] + Simulated_path2[j, i]) / 2
    }
}

Payoff = 0
for(j in 1:10){                                      
    Q = 0                                            
    for(i in i:2){                                  
        ifelse(i==1, S1 <- S0, S1 <- Simulated_path[j,(i-1)*5])  
        S2 <- Simulated_path[j,i*5]
        Q <- Q + min(0.08,max(0, (S2/S1)-1))
    }
    Payoff = Payoff + exp(-r*2)*max(Q,0.16)
}

MCPrice = Payoff / 10
MCPrice

Payoff2 = 0
for(j in 1:10){                                      
    Q2 = 0                                            
    for(i in 1:2){                                  
        ifelse(i==1, S1 <- S0, S1 <- Simulated_path2[j,(i-1)*5])  
        S2 <- Simulated_path2[j,i*5]
        Q2 <- Q2 + min(0.08,max(0, (S2/S1)-1))
    }
    Payoff2 = Payoff2 + exp(-r*2)*max(Q,0.16)
}

Price2 = Payoff2 / 10
PriceAV = (Price + Price2) / 2
PriceAV