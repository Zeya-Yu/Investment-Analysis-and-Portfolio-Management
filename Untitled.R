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
    tot <- NULL
  } else {
    tot <- as.vector((eval(as.name(lst[1]))[, 6]))
    n <- length(tot)
    if (n <= 121) {
      tot <- NULL
    } else {
      if (n >= 200) {
        flag <- ((mean(tot[(n - 49):n], na.rm = TRUE) > mean(tot[(n - 199):n], na.rm = TRUE))
                 && (tot[n] >= mean(tot[(n - 199):n], na.rm = TRUE)))
        if (flag) {
          tot <- as.matrix(tot)
          colnames(tot)[1] <- lst[1]
        } else {
          tot <- NULL
        }
      } else {
        flag <- ((mean(tot[(n - 49):n], na.rm = TRUE) > mean(tot, na.rm = TRUE))
                 && (tot[n] >= mean(tot, na.rm = TRUE)))
        if (flag) {
          tot <- as.matrix(tot)
          colnames(tot)[1] <- lst[1]
        } else {
          tot <- NULL
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
        if (is.null(tot)) {
          tot <- as.matrix(temp)
          colnames(tot)[ncol(tot)] <- lst[j]
        } else {
          test <- length(temp) - nrow(tot)
          if (test < 0) {
            temp <- append(temp, rep(NA, abs(test)), after = 0)
          }
          if (test > 0) {
            tot <- rbind(matrix(NA, test, ncol(tot)), tot)
          }
          tot <- cbind(tot, temp)
          colnames(tot)[ncol(tot)] <- lst[j]
        }
      }
    }
  }
  rownames(tot) = NULL
  return(tot)
}


sm <- function(data,symbol){
  for (i in 1:length(symbol)) {
    lst <- as.vector(subset(data, data$industry == symbol[i])[, 1]) %>% sapply(stri_trim)
    ind <- ind_price(symbol[i], data)
    if (is.null(ind)) {
      next
    }
    if (ncol(ind) > 1) {
      ntd <- nrow(ind)
      n6 <- ntd - 120
      n9 <- ntd - 180
      m1 <- rowMeans(t(t(ind[(n6 + 1):ntd,]) / ind[n6,]) - 1)
      v <- vector(length = ncol(ind))
      for (k in 1:ncol(ind)) {
        v[k] <- ind[!is.na(ind[, k]), k][1]
        ind[!is.na(ind[, k]), k][1] <- NA
      }
      m2 <- rowMeans(t(t(ind[(n9 + 1):ntd,]) / v) - 1, na.rm = TRUE)
      for (j in 1:ncol(ind)) {
        d1 <- ind[n6:ntd, j]
        tot1 <- d1[-1] / d1[1] - 1
        suppressWarnings(test <- try(t.test(na.trim(tot1 - spyr), alternative = "greater"), silent = T))
        if (("try-error" %in% class(test)) || (test$p.value > 0.025)) {
          next
        }
        suppressWarnings(rst <- try(t.test(na.trim(tot1 - m1), alternative = "greater"), silent = T))
        if ("try-error" %in% class(rst)) {
          next
        }
        if (length(na.trim(ind[, j])) < 180) {
          if (rst$p.value <= 0.025) {
            fin <- c(fin, colnames(ind)[j])
          }
        } else {
          d2 <- ind[n9:ntd, j]
          tot2 <- d2[-1] / d2[1] - 1
          suppressWarnings(rst2 <- try(t.test(na.trim(tot2 - m2), alternative = "greater"), silent = T))
          if ("try-error" %in% class(rst2)) {
            next
          }
          if ((rst$p.value <= 0.025) && (rst2$p.value > 0.1)) {
            fin <- c(fin, colnames(ind)[j])
          }
        }
      }
    }
  }
}

sm(ny,nyf)
sm(nas,nasf)








write.csv(fin, "fin.csv")