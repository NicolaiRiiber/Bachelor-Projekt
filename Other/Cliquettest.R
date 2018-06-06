Volatility <- seq(0.01, 0.65, length = 50)
IntRate = 0.03
mSteps = 100
QSteps = mSteps
NSteps = 5
GlobalFloor = 0.16
LocalCap = 0.08

CliquetPrice = vector(length = length(IntRate)) # Empty vector that will be filled with option prices corresponding to each volatility.
  LocalFloor = 0
  r = log(1 + IntRate)
  dt = 1/mSteps # Discretizing the time steps. 
  
  for (k in 1:length(Volatility)){            # Calculating price for each volatility.
    u = exp(Volatility[k]*sqrt(dt))         # Cox, Ross & Rubinstein parameters.
    d = (1/u)
    p = (exp(r*dt)-d)/(u-d)                 # Risk-neutral probability.
    
    Q_upperBound = 0.5                      # Q upper bound corresponding to approx. 5 * Local Cap.
    Q_lowerBound = LocalFloor               # Q lower bound corresponding to local floor (i.e. 0).
    Q = seq(Q_lowerBound, Q_upperBound, length = QSteps) # Q is discretized with the number of QSteps.
    
    xi = vector(length = mSteps)
    ValueObs = vector(length = QSteps)
    Values = matrix(data = 0, nrow = mSteps, ncol = mSteps) # Empty matrix that will be filled with the values in all states.
    
    for (i in 1:mSteps){
      xi[i] = (u**(i-1))*(d**(mSteps-i))}                       
    
    for (i in 1:QSteps){
      ValueObs[i] = max(GlobalFloor, Q[i])}
    
    for (n in NSteps:1){                    # For all reset periods.
      for (j in 1:QSteps){                  # For all Q-steps (i.e. all possible payoffs).
        for (t in mSteps:1){               # For all m-steps (i.e. all sub-periods).
          for (i in 1:t){                  # For all knots.
            if (t == mSteps){
              Q_up <- xi[i]*u
              Q_down <- xi[i]*d
              Values[i,t]=exp(-r*dt)*(p*approxfun(Q,ValueObs,method="linear",rule=2)(Q[j]+min(LocalCap,max(LocalFloor, Q_up-1)))+(1-p)*approxfun(Q,ValueObs,method="linear",rule=2)(Q[j]+min(LocalCap,max(LocalFloor,Q_down-1))))
            }
            else{
              Values[i,t] = exp(-r*dt)*(p*Values[i+1, t+1]+(1-p)*Values[i,t+1])
            }
          }
        }
        ValueObs[j] <- Values[1,1] 
      }
    }   
    CliquetPrice[k] <- approxfun(Q, ValueObs, method = "linear", rule = 2)(0)     # Cliquet Option price is calculated at Q = 0. 
  }
  


p3 <- qplot(Volatility, CliquetPrice, xlab = "Volatility", ylab = "Cliquet Option Pris", geom = "line") +
  geom_line(size = 0.8, colour = "skyblue3") + geom_point(colour = "skyblue3")

Price2 <- c(0.1834364,0.1802939, 0.1772449,0.1742854, 0.1714112, 0.1686184, 0.1659032, 0.1606921, 0.1557511, 0.1510563, 0.1465857, 0.1423196, 0.1382401)
Rate <- c(0.01, 0.015, 0.020, 0.025, 0.030, 0.035, 0.040, 0.050, 0.060, 0.070, 0.080, 0.090, 0.10)
p2 <-qplot(Rate, Price2,  xlab = "Rente", ylab = "Cliquet Option Pris") + geom_line(colour = "skyblue3") + geom_point(colour = "skyblue3")

grid.arrange(p3, p2, ncol = 2)
