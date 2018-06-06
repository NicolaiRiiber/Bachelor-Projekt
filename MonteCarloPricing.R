MonteCarloPrice <- function(mSteps, NSteps, nSim, GF, LF, LC, Volatility, InterestRate){
    
  CompTime = proc.time() # Starting timer.
  r = log(1 + InterestRate)
  dt = 1/mSteps
  S0 = 100
  Payoff = 0
  S = 0
  TheStockPrice<-matrix(nrow=nSim, ncol= mSteps*N_Obs)
  
  
  P_t <- function(S){                                     # The stock price is assumed to follow a normal distribution.
    S*exp((r-(Volatility^2)/2)*dt+Volatility*sqrt(dt)*rnorm(1,0,1))
  }
  
  for(j in 1:nSim){                                       # For all simulated paths.
    for(i in 1:(mSteps*NSteps)){                          # For all time steps.
      ifelse(i==1, S <- S0, S <- TheStockPrice[j,i-1])    # The stock price is updated for each time step on the simulated paths.
      TheStockPrice[j,i] <- P_t(S)                        # The stock price is updated by generating a path from the normal distribution.
    }
  }
  
  for(j in 1:nSim){                                       # For alle simulated paths.
    Q = 0                                                 # Initial payoff is set to 0.
    for(i in 1:NSteps){                                   # For all observation dates (i.e. the dates at which payoff is to be calculated).
      ifelse(i==1, S1 <- S0, S1 <- TheStockPrice[j,(i-1)*mSteps])  
      S2 <- TheStockPrice[j,i*mSteps]
      Q <- Q + min(LC,max(LF, (S2/S1)-1))
    }
    Payoff = Payoff + exp(-r*NSteps)*max(Q,GF)
  }
  
  MCPrice = (Payoff/nSim)
  
  if (as.numeric(proc.time() - CompTime)[3] >= 60){
    CompTimeEnd = round(seconds_to_period(as.numeric(proc.time() - CompTime)[3]), 0)         # Calculates the elapsed computation time if more than (or equal to) 60 seconds.
    print(paste("The elapsed computation time was:", CompTimeEnd))}                          # Prints the elapsed computation time.
  else{ CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")      # Calculates the elapsed computation time if less than 60 seconds..
    print(paste("The elapsed computation time was:", CompTimeEnd))}                          
  
  print(paste("The contract value is:", round(MCPrice, 5)))
  print(paste("The relative error [%] compared to Wilmott is:", round(((0.1731 - MCPrice) / 0.1731) * 100,5)))
}

MonteCarloPrice(100, 5, 100, 0.16, 0, 0.08, 0.25, 0.03)