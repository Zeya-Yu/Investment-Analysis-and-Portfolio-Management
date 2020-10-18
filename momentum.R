library(dplyr)
library(quantmod)
library(stringi)
library(stringr)
library(tseries)
options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.warning4.0" = FALSE)

# setwd("~/Documents/for one drive/OneDrive - 
#       University of Southern California/fbe555/project/r code 2")

#industry = function(ticker) {
#url = paste("https://in.finance.yahoo.com/q/in?s=", ticker, sep = '')
#mydata = as.data.frame(readLines(url, warn = FALSE))
#names(mydata) = "text"
#ind = str_match(as.character(mydata$text[5]), '(?<=Industry</span>)(.*?)(?=</span>)')[2]
#ind = str_match(ind, '[^>]+$')
#ind = str_replace_all(ind, '&amp;', '&')
#return(ind)
#}

#nas <- read.csv("nasdaq.csv",header = T)
#ny <- read.csv("nyse.csv" , header = T)
#nas <- nas %>% select(Symbol,industry)
#ny <- ny %>% select(Symbol, industry)
#ny[] <- sapply(ny, as.character)
#nas[] <- sapply(nas, as.character)
#for (i in 1:nrow(ny)) {
#ny[i, 2] = industry(ny[i, 1])
#}
#for (i in 1:nrow(nas)) {
#nas[i, 2] = industry(nas[i, 1])
#}
#write.csv(ny, "ny.csv")
#write.csv(nas, "nas.csv")

fin <- NULL
ny <- read.csv("ny.csv") %>% select(-X)
ny[] <- sapply(ny, as.character)
ny[is.na(ny$industry), 2] <- "NA"
nyf <- unique(ny$industry)
nas <- read.csv("nas.csv") %>% select(-X)
nas[] <- sapply(nas, as.character)
nas[is.na(nas$industry), 2] <- "NA"
nasf <- unique(nas$industry)

spy <- getSymbols("SPY", from = Sys.Date() - 300, to = Sys.Date())
spy <- as.vector((eval(as.name("SPY"))[, 6]))
spy <- spy[(length(spy) - 120):length(spy)]
spyr <- spy[-1] / spy[1] - 1

ind_price <- function(ind, mkt) {
  lst <- as.vector(subset(mkt, mkt$industry == ind)[, 1]) %>% sapply(stri_trim)
  suppressWarnings(rt <- try(getSymbols(lst[1], from = Sys.Date() - 300, to = Sys.Date()), silent = T))
  if ("try-error" %in% class(rt)) {
    stock_price <- NULL
  } else {
    stock_price <- as.vector((eval(as.name(lst[1]))[, 6]))
    n <- length(stock_price)
    if (n <= 121) {
      stock_price <- NULL
    } else {
      if (n >= 200) {
        flag <- ((mean(stock_price[(n - 49):n], na.rm = TRUE) > mean(stock_price[(n - 199):n], na.rm = TRUE))
                 && (stock_price[n] >= mean(stock_price[(n - 199):n], na.rm = TRUE)))
        if (flag) {
          stock_price <- as.matrix(stock_price)
          colnames(stock_price)[1] <- lst[1]
        } else {
          stock_price <- NULL
        }
      } else {
        flag <- ((mean(stock_price[(n - 49):n], na.rm = TRUE) > mean(stock_price, na.rm = TRUE))
                 && (stock_price[n] >= mean(stock_price, na.rm = TRUE)))
        if (flag) {
          stock_price <- as.matrix(stock_price)
          colnames(stock_price)[1] <- lst[1]
        } else {
          stock_price <- NULL
        }
      }
    }
  }
  if (length(lst) >= 2) {
    for (j in 2:length(lst)) {
      suppressWarnings(rt <- try(getSymbols(lst[j], from = Sys.Date() - 300, to = Sys.Date()), silent = T))
      if ("try-error" %in% class(rt)) {
        next
      }
      temp <- as.vector(eval(as.name(lst[j]))[, 6])
      n <- length(temp)
      if (n <= 121) {
        next
      } else {
        if (n >= 200) {
          flag <- ((mean(temp[(n - 49):n], na.rm = TRUE) > mean(temp[(n - 199):n], na.rm = TRUE))
                   && (temp[n] >= mean(temp[(n - 199):n], na.rm = TRUE)))
          if (!flag) {
            next
          }
        } else {
          flag <- ((mean(temp[(n - 49):n], na.rm = TRUE) > mean(temp, na.rm = TRUE))
                   && (temp[n] >= mean(temp, na.rm = TRUE)))
          if (!flag) {
            next
          }
        }
        if (is.null(stock_price)) {
          stock_price <- as.matrix(temp)
          colnames(stock_price)[ncol(stock_price)] <- lst[j]
        } else {
          test <- length(temp) - nrow(stock_price)
          if (test < 0) {
            temp <- append(temp, rep(NA, abs(test)), after = 0)
          }
          if (test > 0) {
            stock_price <- rbind(matrix(NA, test, ncol(stock_price)), stock_price)
          }
          stock_price <- cbind(stock_price, temp)
          colnames(stock_price)[ncol(stock_price)] <- lst[j]
        }
      }
    }
  }
  rownames(stock_price) = NULL
  return(stock_price)
}

sm <- function(mkt,ind_sym){
  for (i in 1:length(ind_sym)) {
    lst <- as.vector(subset(mkt, mkt$industry == ind_sym[i])[, 1]) %>% sapply(stri_trim)
    stock_price <- ind_price(ind_sym[i], mkt)
    if (is.null(stock_price)) {
      next
    }
    if (ncol(stock_price) > 1) {
      ntd <- nrow(stock_price)
      n6 <- ntd - 120
      n9 <- ntd - 180
      tot6_avg <- rowMeans(t(t(stock_price[(n6 + 1):ntd,]) / stock_price[n6,]) - 1)
      if (ntd>=180) {
        priceat9 <- vector(length = ncol(stock_price))
        for (k in 1:ncol(stock_price)) {
          priceat9[k] <- stock_price[n9:ntd,k][!is.na(stock_price[n9:ntd, k])][1]
          stock_price[n9:ntd,k][!is.na(stock_price[n9:ntd, k])][1] <- NA
        }
        tot9_avg <- rowMeans(t(t(stock_price[(n9 + 1):ntd,]) / priceat9) - 1, na.rm = TRUE)
      }
      for (j in 1:ncol(stock_price)) {
        p6 <- stock_price[n6:ntd, j]
        tot6 <- p6[-1] / p6[1] - 1
        suppressWarnings(t_spy <- try(t.test(na.trim(tot6 - spyr), alternative = "greater"), silent = T))
        if (("try-error" %in% class(t_spy)) || (t_spy$p.value > 0.025)) {
          next
        }
        suppressWarnings(t_6 <- try(t.test(na.trim(tot6 - tot6_avg), alternative = "greater"), silent = T))
        if ("try-error" %in% class(t_6)) {
          next
        }
        if (length(na.trim(stock_price[, j])) < 180) {
          if (t_6$p.value <= 0.025) {
            fin <- c(fin, colnames(stock_price)[j])
          }
        } else {
          p9 <- stock_price[n9:ntd, j]
          tot9 <- p9[-1] / p9[1] - 1
          suppressWarnings(t_9 <- try(t.test(na.trim(tot9 - tot9_avg), alternative = "greater"), silent = T))
          if ("try-error" %in% class(t_9)) {
            next
          }
          if ((t_6$p.value <= 0.025) && (t_9$p.value > 0.1)) {
            fin <- c(fin, colnames(stock_price)[j])
          }
        }
      }
    }
  }
}



sm(ny,nyf)
sm(nas,nasf)



write.csv(fin, "fin.csv")