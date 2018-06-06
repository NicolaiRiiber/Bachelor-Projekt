library(ggplot2)
library("ggthemes")
library("scales")
require(gridExtra)

Cliquet <- function(mSteps, QSteps, NSteps, GF, LC, Vol, IntRate){

  CompTime0 <- proc.time() # Initializing computing time.
  GlobalFloor = GF
  LocalFloor = 0
  LocalCap = LC
  Volatility = Vol
  r = log(1 + IntRate)
  m = mSteps
  N_Obs = NSteps
  dt = 1/m
  u = exp(Volatility*sqrt(dt))
  d = (1/u)
  p = (exp(r*dt)-d)/(u-d)
  Q_upperBound = 0.5
  Q_lowerBound = LocalFloor
  Q_number = QSteps
  Q = seq(Q_lowerBound, Q_upperBound, length = Q_number)

  xi <- vector(length = m)
  ValueObs <- vector(length = Q_number)
  ValueObs_New <- vector(length = Q_number)

  Values <- matrix(data = 0, nrow = m, ncol = m)

  for (i in 1:m){
    xi[i] = (u^(i-1))*(d^(m-i))
  }                       

  for (i in 1:Q_number){
    ValueObs[i] = max(GlobalFloor, Q[i])
  }

  for (n in N_Obs:1){
    for (j in 1:Q_number){
     for (t in m:1){
        for (i in 1:t){
          if (t==m){
            q_up <- xi[i]*u
            q_down <- xi[i]*d
            Values[i,t]=exp(-r*dt)*(p*approxfun(Q,ValueObs,method="linear",rule=2)(Q[j]+min(LocalCap,max(LocalFloor,q_up-1)))+(1-p)*approxfun(Q,ValueObs,method="linear",rule=2)(Q[j]+min(LocalCap,max(LocalFloor,q_down-1))))
          }
          else{
            Values[i,t]<-exp(-r*dt)*(p*Values[i+1,t+1]+(1-p)*Values[i,t+1])
          }
        }
      }
      ValueObs_New[j] <- Values[1,1] 
    }
    ValueObs = ValueObs_New
  }
  CompTime1 <- paste(round(as.numeric(proc.time() - CompTime0)[3], 2), "seconds")
  ValueCliquet <- approxfun(Q,ValueObs,method="linear",rule=2)(0)
  print(paste("The Cliquet Option price is:", round(ValueCliquet, 4)))
  print(paste("The elapsed computing time was:", CompTime1))
}

Cliquet(400, 100, 5, 0.16, 0.08, 0.25, 0.03)

Price1 <- c(0.1421951, 0.1537603, 0.1661932, 0.173582, 0.1742761, 0.1742881, 0.1743245, 0.1743642, 0.1722555, 0.1709568, 0.1700595, 0.1689058, 0.1665403, 0.1644005, 0.1624422, 0.1606357, 0.1589606, 0.1574019)
Vol <- c(0.010, 0.025, 0.050, 0.10, 0.12, 0.15, 17, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70)
p1 <- qplot(Vol, Price1, xlim = c(0,0.75), ylim = c(0.14,0.175), xlab = "Volatilitet", ylab = "Cliquet Option Pris", geom = "line") #+ geom_line(colour = "skyblue3") + geom_point(colour = "skyblue3") + theme_solarized(light = FALSE)

Price2 <- c(0.1834364,0.1802939, 0.1772449,0.1742854, 0.1714112, 0.1686184, 0.1659032, 0.1606921, 0.1557511, 0.1510563, 0.1465857, 0.1423196, 0.1382401)
Rate <- c(0.01, 0.015, 0.020, 0.025, 0.030, 0.035, 0.040, 0.050, 0.060, 0.070, 0.080, 0.090, 0.10)
p2 <-qplot(Rate, Price2,  xlab = "Rente", ylab = "Cliquet Option Pris") + geom_line(colour = "skyblue3") + geom_point(colour = "skyblue3")

Price0 <- c(0.17226, 0.17242, 0.17315, 0.1733, 0.17307, 0.17292, 0.17297, 0.17293, 0.17287, 0.17273, 0.17300, 0.17300, 0.17297, 0.17306, 0.17309)
Price4 <- c(0.16880, 0.17363, 0.17431, 0.17163, 0.17278, 0.17316, 0.17354, 0.17315, 0.17298)
time <- c(14, 25, 27, 42, 51, 77, 110, 160, 326, 387, 595, 802, 1560, 1668, 3548)
time4 <- c(1, 2, 6, 12, 16, 62, 113, 173, 585)
RelativeError <- abs(1-0.1731/Price0)*100

p5 <- qplot(time, Price0, ylim = c(0.1722, 0.17339), xlab = "Beregningstid [s]", ylab = "Cliquet Option Pris", geom = "line", color = "Binomial") + 
  #geom_segment(aes(x = time, y = Price0, xend = time, yend = 0.1731), colour = "lightgrey") + 
  geom_hline(colour = "black", yintercept = 0.1731) + 
  #geom_point(size = 2.5, colour = "skyblue3", shape = 16) +  
  theme_grey()
qplot()

p4 <- qplot(time4, Price4, xlab = "Beregningstid [s]", ylab = "Cliquet Option Pris", geom = "line", color = "Monte Carlo") +   geom_hline(color = "black", yintercept = 0.1731) +
    theme_grey()


grid.arrange(p5, p4, ncol = 2)


