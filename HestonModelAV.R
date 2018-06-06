HestonModelAV <- function(mSteps, NSteps, nSim, GF, LF, LC, V0, InterestRate, Rho, k, theta, sigma){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + InterestRate)
    dt = 1/mSteps
    S0 = 100
    V0 = V0
    Payoff = 0
    Payoff2 = 0
    SimulatedPath<- SimulatedPath1 <- matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath2 <- SimulatedPath22 <- matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath[,1] <- SimulatedPath1[,1] <- S0
    SimulatedPath2[,1] <- SimulatedPath22[,1] <- V0
    
    for(j in 1:nSim){                                         # For all simulated paths.
        for(i in 2:(mSteps*NSteps)){                          # For all time steps.
            z_t <- rnorm(1,0,1)
            z_v <- rnorm(1,0,1)
            z_t2 <- -z_t
            z_v2 <- -z_v
            z_s <- (Rho) * z_v + sqrt(1-(Rho)^2) * z_t
            z_s2 <- (Rho) * z_v2 + sqrt(1-(Rho)^2) * z_t2
            #SimulatedPath2[j ,i] <- SimulatedPath2[j, i-1] + k*dt*(theta-max(SimulatedPath2[j,i-1],0))+sigma*sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_v
            #SimulatedPath22[j ,i] <- SimulatedPath22[j, i-1] + k*dt*(theta-max(SimulatedPath22[j,i-1],0))+sigma*sqrt(max(SimulatedPath22[j,i-1],0)*dt)*z_v2
            SimulatedPath2[j, i] <- (sqrt(max(SimulatedPath2[j, i-1],0))+1/2*sigma*sqrt(dt)*z_v)^2+k*(theta-max(SimulatedPath2[j,i-1],0))*dt-1/4*sigma^2*dt
            SimulatedPath22[j, i] <- (sqrt(max(SimulatedPath22[j, i-1],0))+1/2*sigma*sqrt(dt)*z_v2)^2+k*(theta-max(SimulatedPath22[j,i-1],0))*dt-1/4*sigma^2*dt
            SimulatedPath[j, i] <- SimulatedPath[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_s+1/2*max(SimulatedPath2[j,i-1],0)*dt*((z_s)^2-1))
            SimulatedPath1[j, i] <- SimulatedPath1[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath22[j,i-1],0)*dt)*z_s2+1/2*max(SimulatedPath22[j,i-1],0)*dt*((z_s2)^2-1))
            #SimulatedPath[j, i] <- SimulatedPath[j, i-1]*exp((r-0.5*max(SimulatedPath2[j,i-1],0))*dt+sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_s)
            #SimulatedPath1[j,i] <- SimulatedPath1[j,i-1]*exp((r-0.5*max(SimulatedPath22[j,i-1],0))*dt+sqrt(max(SimulatedPath22[j,i-1],0)*dt)*z_s2)
       }
    }
    
    x <- c(SimulatedPath2[1,100], SimulatedPath2[1,200], SimulatedPath2[1,300], SimulatedPath2[1,400], SimulatedPath2[1,500])
    y <- c(SimulatedPath[1,100], SimulatedPath[1,200], SimulatedPath[1,300], SimulatedPath[1,400], SimulatedPath2[1,500])
    plot(x,y)
    
    for(j in 1:nSim){                                         # For alle simulated paths.
        Q = 0                                                 # Initial payoff is set to 0.
        Q1= 0
        for(i in 1:NSteps){                                   # For all observation dates (i.e. the dates at which payoff is to be calculated).
            ifelse(i==1, S1 <- S0, S1 <- SimulatedPath[j,(i-1)*mSteps])   # The value of the underlying stock on the observation dates is stored.
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LC,max(LF, (S2/S1)-1))                           # The payoff from the simulated path is calculated.
            ifelse(i==1, S3 <- S0, S3 <- SimulatedPath1[j,(i-1)*mSteps])  # The value of the underlying stock (Antithetic path) on the obs dates is stored.
            S4 <- SimulatedPath1[j,i*mSteps]
            Q1 <- Q1 + min(LC,max(LF, (S4/S3)-1))                         # The payoff from the simulated antithetic path is calculated.
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GF)                        # The total contract payoff is updated.
        Payoff2 = Payoff2 + exp(-r*NSteps)*max(Q1,GF)                     # The total contract payoff is updated. (Antithetic path)
    }
    
    MCPrice = (Payoff/nSim)      # Monte Carlo estimate.
    MCPrice2 = (Payoff2/nSim)    # Monte Carlo estimate (Atithetic path).
    AV = (MCPrice + MCPrice2)/2  # The final price is found as the average of the two MC estimates. 
    
    if (as.numeric(proc.time() - CompTime)[3] >= 60){
        CompTimeEnd = round(seconds_to_period(as.numeric(proc.time() - CompTime)[3]), 0)       # Calculates the elapsed computation time if more than (or equal to) 60 seconds.
        print(paste("The elapsed computation time was:", CompTimeEnd))}                        # Prints the elapsed computation time.
    else{ CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")      # Calculates the elapsed computation time if less than 60 seconds..
    print(paste("The elapsed computation time was:", CompTimeEnd))}                          
    
    print(paste("The contract value is:", round(AV, 5)))
    #print(paste("The relative error [%] compared to Wilmott is:", round(((0.1731 - AV) / 0.1731) * 100,5)))
}

HestonModelAV(300, 5, 7500, 0.16, 0, 0.08, 0.20, 0.03, -0.30, 2.0, 0.20, 0.20)
HestonModelAV(100, 5, 1000, 0.16, 0, 0.08, 0.20, 0.03, -0.30, 2.0, 0.20, 0.20)
HestonModel(200, 5, 10000, 0.16, 0, 0.08, 0.01, 0.03, -0.70, 0.5, 0.01, 0)

CliquetPriceTheta <- c(0.15582, 0.17636, 0.17327)
Theta <- c(0.001, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

CliquetPriceSigma <- c()
Sigma <- c(0.01, 0.03, 0.05, 0.07, 0.09, 0.11, 0.13, 0.15, 0.)

k <- c(0.01, 0.5, 1, 1.5, 2, 2.5, 3.0)
Price <- c(0.17161, 0.17015, 0.16921, 0.1687, 0.16882, 0.16811, 0.16835)
qplot(k, Price) + geom_line()

sigma <- c(0.01, 0.05, 0.1, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)
Price2 <- c()
qplot(sigma, Price2) + geom_line()

rho <- seq(-1,1,0.1)
Price3 <- c(0.17252, 0.17114, 0.17145, 0.17068, 0.17043, 0.16975, 0.1697, 0.16865, 0.16852, 0.16716, 0.16735, 0.16677, 0.16648, 0.1661, 0.16502, 0.1646)
qplot(rho, Price3) + geom_line()