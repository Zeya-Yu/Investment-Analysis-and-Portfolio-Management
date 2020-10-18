library(quantmod)

#1
lst <- c("TIER",  "GG", "ACRGF", "HRS")
atime <- c("2019-03-25", "2019-03-25", "2019-04-17", "2019-04-04")
start <- "2019-01-01"

getSymbols(lst[1], from = start)
n = which(index(eval(as.name(lst[1]))) == atime[1])
price <- na.omit(as.vector(eval(as.name(lst[1]))[, 6]))
L = length(price)
ne = min(20, L - n)
price = price[(n - 20):(n + ne)]

getSymbols("SPY", from = start)
price_SPY <- na.omit(as.vector(SPY$SPY.Adjusted))
n1 = which(index(SPY) == atime[1])
price_SPY <- price_SPY[(n1 - 20):(n1 + ne)]
CR <- price / price[1]
CR_SPY <- price_SPY / price_SPY[1]
CAR <- (CR - CR_SPY)
CAR <- append(CAR, rep(NA, 20 - ne))

plot(CAR ~ seq(-20, 20), xlab = "transaction day", ylab = "Cumulative abnormal return",
     type = 'l', las = 1, col=1, ylim=c(-0.1,0.2))

for (i in 2:length(lst)) {
  getSymbols(lst[i], from = start)
  n = which(index(eval(as.name(lst[i]))) == atime[i])
  price <- na.omit(as.vector(eval(as.name(lst[i]))[,6]))
  L = length(price)
  ne = min(20, L - n)
  price = price[(n - 20):(n + ne)]
  
  getSymbols("SPY", from = start)
  price_SPY <- na.omit(as.vector(SPY$SPY.Adjusted))
  n1 = which(index(SPY) == atime[i])
  price_SPY <- price_SPY[(n1 - 20):(n1 + ne)]
  CR <- price / price[1]
  CR_SPY <- price_SPY / price_SPY[1]
  CAR <- (CR - CR_SPY)
  CAR <- append(CAR,rep(NA,20-ne))
  
  lines(CAR ~ seq(-20, 20), col=i)
}

grid()
abline(h = 0)
abline(v = 0, lty = 4)
legend("topleft", title = "Company", legend = lst, lty = rep(1, length(lst)), col = 1:length(lst), cex = 0.5)

#2
lst <- c("TIER")
atime <- c("2019-03-25")
start <- "2019-01-01"

getSymbols(lst[1], from = start)
n = which(index(eval(as.name(lst[1]))) == atime[1])
price <- na.omit(as.vector(eval(as.name(lst[1]))[, 6]))
L = length(price)
ne = min(20, L - n)
price = price[(n - 20):(n + ne)]

getSymbols("SPY", from = start)
price_SPY <- na.omit(as.vector(SPY$SPY.Adjusted))
n1 = which(index(SPY) == atime[1])
price_SPY <- price_SPY[(n1 - 20):(n1 + ne)]
CR <- price / price[1]
CR_SPY <- price_SPY / price_SPY[1]
CAR <- (CR - CR_SPY)
CAR <- append(CAR, rep(NA, 20 - ne))

plot(CAR ~ seq(-20, 20), xlab = "transaction day", ylab = "Cumulative abnormal return",
     type = 'l', las = 1, col = 1, ylim = c(-0.05, 0.2))

grid()
abline(h = 0)
abline(v = 0, lty = 4, col=2)
legend("topleft", title = "Company", legend = lst, lty = rep(1, length(lst)), col = 1:length(lst), cex = 0.5)

#3
lst <- c("LOV")
atime <- c("2019-03-25")
start <- "2019-01-01"

getSymbols(lst[1], from = start)
n = which(index(eval(as.name(lst[1]))) == atime[1])
price <- na.omit(as.vector(eval(as.name(lst[1]))[, 6]))
L = length(price)
ne = min(20, L - n)
price = price[(n - 20):(n + ne)]

getSymbols("SPY", from = start)
price_SPY <- na.omit(as.vector(SPY$SPY.Adjusted))
n1 = which(index(SPY) == atime[1])
price_SPY <- price_SPY[(n1 - 20):(n1 + ne)]
CR <- price / price[1]
CR_SPY <- price_SPY / price_SPY[1]
CAR <- (CR - CR_SPY)
CAR <- append(CAR, rep(NA, 20 - ne))

plot(CAR ~ seq(-20, 20), xlab = "transaction day", ylab = "Cumulative abnormal return",
     type = 'l', las = 1, col = 1, ylim = c(-0.05, 0.6))

grid()
abline(h = 0)
abline(v = 0, lty = 4, col=2)
legend("topleft", title = "Company", legend = lst, lty = rep(1, length(lst)), col = 1:length(lst), cex = 0.5)