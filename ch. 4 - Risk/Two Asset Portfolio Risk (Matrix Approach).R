# Two Asset Portfolio Risk (Matrix Approach)

library(xts)
library(quantmod)

# Import AMZN and IBM Data from Yahoo Finance and Calculate Total Returns

setwd("C:/dev/finance/r/yahoo data")

assignGlobal <- function(name, val) {
  assign(name, val, envir=.GlobalEnv)
}

import <- function(ticker.sym) {
  # data.AMZN
  data.variable.name <- paste("data.", ticker.sym, sep="")
  # "AMZN.csv"
  data.filename <- paste(ticker.sym, ".csv", sep="")
  # data.AMZN <- read.csv("AMZN.csv",header=TRUE)
  assignGlobal(data.variable.name, read.csv(data.filename, header=TRUE))
  # get date column
  date <- eval(parse(text=paste(data.variable.name, "[,1]")))
  # excel formated date
  date <- as.Date(date, format="%d-%m-%y")
  # replace date column with correctly formated version
  withoutDateColumn <- eval(parse(text=paste(data.variable.name, "[,-1]")))
  data <- cbind(date, withoutDateColumn)
  
  #assignGlobal(data.variable.name, 
  #             data.variable.name[order(date),])
  assignGlobal(data.variable.name, xts(data[,2:7], 
                                       order.by=date))
  eval(parse(text=paste(
    'names(', data.variable.name, 
    ') <<- paste(c("Open", "High", "Low", "Close", "Volume", "Adjusted"))',
    sep = "")))
}

import("AMZN")
import("IBM")

AMZN.return <- Delt(data.AMZN$Adjusted)
IBM.return  <- Delt(data.IBM$Adjusted)

# Combine the Two Return Series
returns <- cbind(AMZN.return, IBM.return)
names(returns) <- c("AMZN.return", "IBM.return")

# remove 1st & last row since na
returns <- returns[-1, ]
returns <- returns[(-nrow(returns)), ]

# Create Vector of Weights
wgt.2asset <- matrix(c(0.25, 0.75), 1)

# Create Transposed Vector of Weights
tWgt.2asset <- t(wgt.2asset)

# Construct Variance-Covariance Matrix
matrix.return <- as.matrix(returns)

options(scipen=100)
# annualize the variances and covariance
vcov.2asset <- cov(matrix.return) * 252

# Calculate Portfolio Risk
matrix.var2asset <- as.numeric( wgt.2asset %*% vcov.2asset %*% tWgt.2asset )
matrix.sd2asset  <- sqrt(matrix.var2asset)






