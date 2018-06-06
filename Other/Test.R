GlobalFloor = 0.16
LocalFloor = 0
LocalCap = 0.08
Volatility = 0.40
r = log(1.03)
m = 100
N_Obs = 5
dt = 1/m
u = exp(Volatility*sqrt(dt))
d = (1/u)
p = (exp(r*dt)-d)/(u-d)

Q_upperBound = 5 * LocalCap
Q_lowerBound = LocalFloor
Q_number = 100

Q = seq(Q_lowerBound, Q_upperBound, length = Q_number)

xi <- vector(length = m)
ValueObs <- vector(length = Q_number)
ValueObs_New <- vector(length = Q_number)

Values <- matrix(nrow = m, ncol = m)

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

ValueCliquet <- approxfun(Q,ValueObs,method="linear",rule=2)(0)
ValueCliquet


qplot(Q,Twenty, xlim = c(0, 0.4) , ylim = c(0.15, 0.35)) + geom_point(colour = "#B79F00")
p2 <- qplot(Q,Thirty, xlim = c(0, 0.4) , ylim = c(0.15, 0.35)) + geom_point(colour = "#F8766D")

qplot(xlab = "Q", ylab = "Cliquet Option Price") + 
  geom_point(aes(x = Q, y = Twenty), colour = "#00BFC4") + 
  geom_point(aes(x = Q, y = Thirty), colour = "#F8766D") +
  geom_point(aes(x = Q, y = Ten), colour = "#F564E3") + 
  geom_point(aes(x = Q, y = One), colour = "#00BA38") + 
  labs(color = "Volatility")

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



#00BA38 green
#619CFF blue
#F564E3 purple
#B79F00 yellow


grid.arrange(p1, p2, ncol = 2)

install.packages("xlsx")
library(xlsx)

write.xlsx(Data, file = "Data.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

Data1 <- data.frame(
    Q1 = Q[1:100],
    Q10 = Q[101:200],
    Q20 = Q[201:300],
    Q30 = Q[301:400],
    Q40 = Q[401, 500],
    One = One,
    Ten = Ten,
    Twenty = Twenty,
    Thirty = Thirty,
    Forty = Forty
)

write.xlsx(Data1, file = "Data1.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
