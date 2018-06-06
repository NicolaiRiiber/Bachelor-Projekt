GlobalFloor=0.16
LocalFloor = 0
LocalCap=0.08
Volatility = 0.28

r = log(1+0.03)
m=100
N_Obs=5
dt = 1/m
u=exp(Volatility*sqrt(dt))
d=(1/u)
p=(exp(r*dt)-d)/(u-d)
S0=100
B=10 #Antal stier
Gns = 0
S=0
Time<-seq(0,N_Obs,by=dt)
TheStockPrice<-matrix(nrow=B, ncol= m*N_Obs)

P_t<-function(S){S*exp((r-((Volatility)^2)/2)*dt+Volatility*sqrt(dt)*rnorm(1,0,1
))}

for(j in 1:B){
  for(i in 1:(m*N_Obs)){
    ifelse(i==1,S<-S0,S<-TheStockPrice[j,i-1])
    TheStockPrice[j,i]<-P_t(S)
  }
}

Data1 <- data.frame(
  Tid = Time[1:500],
  Stocks = c(TheStockPrice[1,], TheStockPrice[2,], TheStockPrice[3,], TheStockPrice[4,],
             TheStockPrice[5,], TheStockPrice[6,], TheStockPrice[7,], TheStockPrice[8,], TheStockPrice[9,], TheStockPrice[10,]),
  Simulation = factor(c(rep("1", length(TheStockPrice[1,])), rep("2", length(TheStockPrice[1,])), rep("3", length(TheStockPrice[1,])), rep("4", length(TheStockPrice[1,]))
  ,rep("5", length(TheStockPrice[1,])), rep("6", length(TheStockPrice[1,])), rep("7", length(TheStockPrice[1,])), rep("8", length(TheStockPrice[1,]))
  ,rep("9", length(TheStockPrice[1,])), rep("10", length(TheStockPrice[1,]))
  ))
)

qplot(Tid, Stocks, data = Data1, colour = Simulation, xlab = "Time [years]", ylab = "Stock Price", geom = "line")  + 
  geom_line(size = 1.0) +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.95),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

qplot(Tid, TheStockPrice[2,], geom = "line", xlab = "Tid", ylab = "Underliggende aktiv", xlim = c(0, 3.5), ylim = c(50, 155)) + 
  geom_line(size = 0.7, colour = "skyblue3") +
  geom_segment(aes(x = 1, y = 50, xend = 1, yend = TheStockPrice[2,101]), linetype = "dashed") + geom_point(aes(x=1, y=TheStockPrice[2,101]), colour="black") +
  geom_segment(aes(x = 2, y = 50, xend = 2, yend = TheStockPrice[2,201]), linetype = "dashed") + geom_point(aes(x=2, y=TheStockPrice[2,201]), colour="black") +
  geom_segment(aes(x = 3, y = 50, xend = 3, yend = TheStockPrice[2,301]), linetype = "dashed") + geom_point(aes(x=3, y=TheStockPrice[2,301]), colour="black") +
  geom_point(aes(x=0, y=100)) #+ theme_solarized(light = FALSE)


Data <- data.frame(
    Q0 = Q,
    Vol = c(One, Ten, Twenty, Thirty, Forty), 
    Volatilitet = factor(c(rep("1 % ", 100), rep("10 %", 100), rep("20 %", 100), rep("30 %", 100), rep("40 %", 100)))
)

qplot(Q0, Vol, data = Data, colour = Volatilitet, xlab = "Q", ylab = "Cliquet Option Pris", geom = "line")  + 
    geom_line(size = 1.2) +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank()
    )

Price0 <- c(0.17226, 0.17242, 0.17315, 0.1733, 0.17307, 0.17292, 0.17297, 0.17293, 0.17287, 0.17273, 0.17300, 0.17300, 0.17297, 0.17306, 0.17309)
Price4 <- c(0.16880, 0.17363, 0.17431, 0.17163, 0.17278, 0.17316, 0.17354, 0.17315, 0.17298)
time <- c(14, 25, 27, 42, 51, 77, 110, 160, 326, 387, 595, 802, 1560, 1668, 3548)
time4 <- c(1, 2, 6, 12, 16, 62, 113, 173, 585)
RelativeError0 <- abs(1-Price0/0.1731)*100
RelativeError4 <- abs(1-Price4/0.1731)*100
RelativeErrorAV <- abs(1-AVPrice/0.1731)*100

AVPrice <- c(0.17327, 0.17302, 0.17298, 0.1729, 0.17324, 0.17306, 0.17297, 0.17319, 0.1732, 0.17314)
AVtime <- c(1, 3, 6, 16, 33, 60, 118, 182, 611, 1005)

pbin1 <- qplot(Time, Price, data = DataPricingBin, colour = Metode, xlab = "Beregningstid [s]", ylab = "Cliquet Option Pris", geom = "line", ylim = c(0.1715, 0.1735)) + 
    geom_hline(colour = "black", yintercept = 0.1731) + 
    theme_grey() +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.60, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    )

qplot(Time, Price, data = DataPricingBin, colour = Metode, xlab = "Beregningstid [s]", ylab = "Cliquet Option Pris", geom = "line") + 
    geom_hline(colour = "black", yintercept = 0.1731) + 
    theme_grey() +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.70, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    )

pMC1 <- qplot(Time, Price, data = DataPricingMC, colour = Metode, xlab = "Beregningstid [s]", ylab = "Cliquet Option Pris", geom = "line") +   geom_hline(color = "black", yintercept = 0.1731) +
    theme_grey() +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.60, 0.40),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    )

DataPricingBin <- data.frame(
    Time = time,
    Price = Price0,
    Metode = factor(c(rep("Binomial", length(time)))),
    Error = RelativeError0
)

DataPricingMC <- data.frame(
    Time = time4,
    Price = Price4,
    Metode = factor(c(rep("Monte Carlo", length(time4)))),
    Error = RelativeError4
)

DataPricingAV <- data.frame(
    Time = AVtime,
    Price = AVPrice,
    Metode = factor(c(rep("AV Monte Carlo", 10))),
    Error = RelativeErrorAV
)

pAV1 <- qplot(Time, Price, data = DataPricingAV, colour = Metode, xlab = "Beregningstid [s]", ylab = "Cliquet Option Pris", geom = "line", ylim = c(0.1715, 0.174)) +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.60, 0.40),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    ) +
    geom_hline(color = "black", yintercept = 0.1731)

pAV2 <- qplot(Time, Error, data = DataPricingAV, colour = Metode, xlab = "Beregningstid [s]", ylab = "Relativ Afvigelse [%]", ylim = c(0, 0.50), geom = "line") +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.60, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    )

grid.arrange(pAV1, pAV2, ncol = 2)

pbin2 <- qplot(Time, Error, data = DataPricingBin, colour = Metode, xlab = "Beregningstid [s]", ylab = "Relativ Afvigelse [%]", geom = "line", ylim = c(0, 0.75)) +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.60, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    )
pMC2 <- qplot(Time, Error, data = DataPricingMC, colour = Metode, xlab = "Beregningstid [s]", ylab = "Relativ Afvigelse [%]", geom = "line") +
    geom_line() +
    theme(
        legend.justification = c(0, 1),
        legend.position = c(0.60, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank()
    )

grid.arrange(pbin1, pbin2, pMC1, pMC2, pAV1, pAV2, nrow = 3)

