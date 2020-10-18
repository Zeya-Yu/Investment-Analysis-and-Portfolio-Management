library(quantmod)
getSymbols("TIER",from = "2019-03-18" , to = "2019-04-08")  # Mar 25
date = index(TIER)
price_TIER <- as.vector(TIER$TIER.Close)
getSymbols("SPY",from = "2019-03-18" , to = "2019-04-08")
price_SPY <- as.vector(SPY$SPY.Close)
CR_TIER <- price_TIER/price_TIER[1]
CR_SPY <- price_SPY/price_SPY[1]
CAR <- (CR_TIER-CR_SPY)[-1]

plot(CAR~seq(-5,8),xlab="transaction day" ,  ylab = "Cumulative abnormal return",
      type='l',main = "TIER", las = 2)
grid()
abline(v = 0,lty = 4, col = 2)








getSymbols("LOV",from = "2019-03-16" , to = "2019-04-09")  # Mar 25
date = index(LOV)
price_LOV <- as.vector(LOV$LOV.Close)
price_LOV <- na.omit(price_LOV)
getSymbols("SPY",from = "2019-03-16" , to = "2019-04-08")
price_SPY <- as.vector(SPY$SPY.Close)
CR_LOV <- price_LOV/price_LOV[1]
CR_SPY <- price_SPY/price_SPY[1]
CAR <- (CR_LOV-CR_SPY)[-1]

plot(CAR~seq(-5,8),xlab="transaction day" ,  ylab = "Cumulative abnormal return",
     type='l',main = "LOV", las = 2)
grid()
abline(v = 0,lty = 4, col = 2)

############################
getSymbols("TIER",from = "2019-03-18" , to = "2019-04-08")  # Mar 25
date = index(TIER)
price_TIER <- as.vector(TIER$TIER.Close)
getSymbols("SPY",from = "2019-03-18" , to = "2019-04-08")
price_SPY <- as.vector(SPY$SPY.Close)
CR_TIER <- price_TIER/price_TIER[1]
CR_SPY <- price_SPY/price_SPY[1]
CAR <- (CR_TIER-CR_SPY)[-1]

plot(CAR~seq(-5,8),xlab="transaction day" ,  ylab = "Cumulative abnormal return",
     type='l',main = "TIER", las = 2)
grid()
abline(v = 0,lty = 4, col = 2)


getSymbols("GG",from = "2019-03-18" , to = "2019-04-06")  # Mar 25
date = index(GG)
price_GG <- as.vector(GG$GG.Close)
price_GG <- na.omit(price_GG)
getSymbols("SPY",from = "2019-03-18" , to = "2019-04-06")
price_SPY <- as.vector(SPY$SPY.Close)
CR_GG <- price_GG/price_GG[1]
CR_SPY <- price_SPY/price_SPY[1]
CAR <- (CR_GG-CR_SPY)[-1]

plot(CAR~seq(-5,8),xlab="transaction day" ,  ylab = "Cumulative abnormal return",
     type='l',main = "GG", las = 2)
grid()
abline(v = 0,lty = 4, col = 2)


getSymbols("ACRGF",from = "2019-04-10" , to = "2019-04-24")  # Mar 25
date = index(ACRGF)
price_ACRGF <- as.vector(ACRGF$ACRGF.Close)
price_ACRGF <- na.omit(price_ACRGF)
getSymbols("SPY",from = "2019-04-10" , to = "2019-04-24")
price_SPY <- as.vector(SPY$SPY.Close)
CR_ACRGF <- price_ACRGF/price_ACRGF[1]
CR_SPY <- price_SPY/price_SPY[1]
CAR <- (CR_ACRGF-CR_SPY)[-1]

plot(CAR~seq(-5,2),xlab="transaction day" ,  ylab = "Cumulative abnormal return",
     type='l',main = "ACRGF", las = 2)
grid()
abline(v = 0,lty = 4, col = 2)


getSymbols("HRS",from = "2019-03-28" , to = "2019-04-18")  # Mar 25
date = index(HRS)
price_HRS <- as.vector(HRS$HRS.Close)
price_HRS <- na.omit(price_HRS)
getSymbols("SPY",from = "2019-03-28" , to = "2019-04-18")
price_SPY <- as.vector(SPY$SPY.Close)
CR_HRS <- price_HRS/price_HRS[1]
CR_SPY <- price_SPY/price_SPY[1]
CAR <- (CR_HRS-CR_SPY)[-1]

plot(CAR~seq(-5,8),xlab="transaction day" ,  ylab = "Cumulative abnormal return",
     type='l',main = "HRS", las = 2)
grid()
abline(v = 0,lty = 4, col = 2)

####  GG(03-25)  ACRGF(04-17)   HRS(04-04)





