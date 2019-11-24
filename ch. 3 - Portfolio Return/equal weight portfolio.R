library(quantmod)
library(xts)

setwd('C:/dev/finance/r/yahoo data')

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
import("TSLA")

portfolio <- merge(data.AMZN$Close, data.AMZN$Adjusted,
                   data.IBM$Close, data.IBM$Adjusted,
                   data.TSLA$Close, data.TSLA$Adjusted)
names(portfolio) <- c("AMZN.Close", "AMZN.Adjusted",
                          "IBM.Close", "IBM.Adjusted",
                          "TSLA.Close", "TSLA.Adjusted")

portfolio <- na.omit(portfolio)

# Calculate Returns of Each Security
portfolio$AMZN.ret <- Delt(portfolio$AMZN.Adjusted)
portfolio$IBM.ret <- Delt(portfolio$IBM.Adjusted)
portfolio$TSLA.ret <- Delt(portfolio$TSLA.Adjusted)

# Convert to data.frame Object and Subset Data
portfolio <- cbind(data.frame(index(portfolio)),
                   data.frame(portfolio))
names(portfolio)[1] <- paste("date")

# subset
portfolio <- subset(portfolio,
                    portfolio$date >= "2012-12-31"
                      & portfolio$date <= "2013-12-31")

ewPortfolio <- portfolio[, c(1, (ncol(portfolio)-2):ncol(portfolio))]
names(ewPortfolio) <- c("date", "AMZN", "IBM", "TSLA")

# Converting Net Returns to Gross Returns
ewPortfolio$AMZN <- 1 + ewPortfolio$AMZN
ewPortfolio$IBM <- 1 + ewPortfolio$IBM
ewPortfolio$TSLA <- 1 + ewPortfolio$TSLA

getQuarterValDataFrame <- function(beginDate, endDate, previousCumAmount) {
  # number of securities in the portfolio called in for quarterly calculations
  num.sec <- 3
  
  # Calculate EW Portfolio Values for 1Q 2013
  # investment was made at the closing price on the last trading day of 2012
  ew.quarter <- subset(ewPortfolio,
                       ewPortfolio$date >= as.Date(beginDate) 
                       & ewPortfolio$date <= as.Date(endDate))
  
  #  cumprod to calculate the cumulative gross returns
  ew.quarter$AMZN <- cumprod(ew.quarter$AMZN)
  ew.quarter$IBM  <- cumprod(ew.quarter$IBM)
  ew.quarter$TSLA <- cumprod(ew.quarter$TSLA)
  
  # index values
  ew.quarter$AMZN.idx <- (previousCumAmount / num.sec) * ew.quarter$AMZN
  ew.quarter$IBM.idx  <- (previousCumAmount / num.sec) * ew.quarter$IBM
  ew.quarter$TSLA.idx <- (previousCumAmount / num.sec) * ew.quarter$TSLA
  
  # value of portfolio is equal to portfolio components
  quarter.val <- data.frame(rowSums(ew.quarter[, (ncol(ew.quarter)-2):ncol(ew.quarter)]))
  # names(quarter.val) <- paste("portfolio.val")
  # quarter.val$date <- ew.quarter$date
  return(quarter.val)
}

calculateEwPortfolio <- function(quarterValDataFrame) {
  # aggregate portfolio value at end of quarter
  return(
    quarterValDataFrame[nrow(quarterValDataFrame), 1]
    )
}

q1.val <- getQuarterValDataFrame("2013-01-01", "2013-03-21", 1)
q1.inv <- calculateEwPortfolio(q1.val)

q2.val <- getQuarterValDataFrame("2013-04-01", "2013-06-30", q1.inv)
q2.inv <- calculateEwPortfolio(q2.val)

q3.val <- getQuarterValDataFrame("2013-07-01", "2013-09-30", q2.inv)
q3.inv <- calculateEwPortfolio(q3.val)

q4.val <- getQuarterValDataFrame("2013-10-01", "2013-12-31", q3.inv)
q4.inv <- calculateEwPortfolio(q4.val)

# Combine Quarterly EW Portfolio Values into One Data Object
ew.portfolio.val <- rbind(q1.val, q2.val, q3.val, q4.val)

plot(x=index(ew.portfolio.val),
     y=ew.portfolio.val[,1],
     xlab="Date",
     ylab="Value of $1 Invested",
     type="l",
     col="purple",
     lty=1,
     lwd=2,
     main=paste("Value of $1 Invested with", "\n",
                "Quarterly Equal Weight Rebalancing", sep=""))

abline(h=1, lty=2)




