# Portfolio Risk - Multiple Assets

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

securitiesList <- c("AMZN", "IBM", "TSLA", "MSFT")

lapply(securitiesList, import)

# multi to hold only the adjusted closing prices
multi <- merge(data.AMZN$Adjusted,
               data.IBM$Adjusted,
               data.TSLA$Adjusted,
               data.MSFT$Adjusted)
names(multi) <- securitiesList

# Calculate Returns for Each Security
matrix.price <- matrix(multi, nrow(multi))
# apply Delt to columns (2 for columns, 1 for rows)
matrix.return <- apply(matrix.price, 2, Delt)

# Clean up Returns Data
lastIndex <- nrow(matrix.return)
matrix.return <- matrix.return[c(-1, -(lastIndex-1), -lastIndex), ]
colnames(matrix.return) <- securitiesList

# Calculate Annualized Variance-Covariance Matrix
options(scipen="100")
vcov <- cov(matrix.return)

# annualize the VCOV matrix
vcov.annual <- vcov * 252

# Row vector of portfolio Weights
# sum(wgt) == 1
wgt <- c(.2, .2, .3, .3)
matrix.wgt <- matrix(wgt, 1)

# Column Vector of Weights
tMatrix.wgt <- t(matrix.wgt)

# Portfolio Variance
portfolio.var <- as.numeric( matrix.wgt %*% vcov.annual %*% tMatrix.wgt )

# Portfolio Standard Deviation
portfolio.std <- sqrt(portfolio.var)

cat("Portfio Securities:", securitiesList, "\n")
cat("Portfolio std:", portfolio.std)
