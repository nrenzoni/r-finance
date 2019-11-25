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

# vw portfolio
vwPortfolio <- portfolio[,c(1,2,4,6,8:10)]
rownames(vwPortfolio) <- seq(1:nrow(vwPortfolio))

# overwrite the net returns with gross returns
vwPortfolio$AMZN.ret <- 1 + vwPortfolio$AMZN.ret
vwPortfolio$IBM.ret <- 1 + vwPortfolio$IBM.ret
vwPortfolio$TSLA.ret <- 1 + vwPortfolio$TSLA.ret

# Construct Series of Calendar Days
date <- seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by=1)
date <- data.frame(date)

price.qtr <- vwPortfolio[,c(1:4)]
price.qtr <- na.locf(merge(x=date, y=price.qtr, by="date", all.x=TRUE))

# Keep Only Prices at the End of Each Calendar Quarter
price.qtr <- subset(price.qtr,
                    price.qtr$date == as.Date("2012-12-31")
                      | price.qtr$date == as.Date("2013-03-31")
                      | price.qtr$date == as.Date("2013-06-30")
                      | price.qtr$date == as.Date("2013-09-30"))

# Obtain Shares Outstanding Data from SEC Filings
price.qtr$AMZN.shout <- c(454000000,455000000,457000000,458000000)
price.qtr$IBM.shout <- c(1117367676,1108794396,1095425823,1085854383)
price.qtr$TSLA.shout <- c(1115233000,1084766000,1065046000,1013059000)

# Calculate Market Capitalization of Each Security
weights <- price.qtr
weights$AMZN.mcap <- weights$AMZN.Close * weights$AMZN.shout
weights$IBM.mcap  <- weights$IBM.Close * weights$IBM.shout
weights$TSLA.mcap <- weights$TSLA.Close * weights$TSLA.shout

# Calculate Quarter-end Aggregate Market Capitalization
weights$tot.mcap <- rowSums(weights[ ,(ncol(weights)-2):ncol(weights)])

# Calculate Quarter-end Weights of Each Security in the Portfolio
weights$AMZN.wgt <- weights$AMZN.mcap / weights$tot.mcap
weights$IBM.wgt <- weights$IBM.mcap / weights$tot.mcap
weights$TSLA.wgt <- weights$TSLA.mcap / weights$tot.mcap

weight <- weights[, c(1,(ncol(weights)-2):ncol(weights))]

# weights are applicable to the start of the next quarter
weight$date <- weight$date + 1

# Calculating Quarterly VW Portfolio Values 

# create series of dates from December 31, 2012 to December 31, 2013 
# so that we can ???nd the applicable weights at the beginning of each quarter
date <- seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by=1)
date <- data.frame(date)

vwRet <- na.locf(merge(date, weight, by="date", all.x=TRUE))

# extract beginning of quarter weights
q1.vw.wgt <- subset(vwRet, vwRet$date == as.Date("2013-01-01"))
q2.vw.wgt <- subset(vwRet, vwRet$date == as.Date("2013-04-01"))
q3.vw.wgt <- subset(vwRet, vwRet$date == as.Date("2013-07-01"))
q4.vw.wgt <- subset(vwRet, vwRet$date == as.Date("2013-10-01"))


plotQuarter <- function(quarter.vw.wgt, quarterNum) {
  pieColors <- c("red", "green", "blue")
  pie.values <- as.numeric(quarter.vw.wgt[, -1])
  pct <- round(pie.values * 100)
  pie.labels <- paste(c("Amazon", "IBM", "Tesla"), pct)
  pie.labels <- paste(pie.labels, "%", sep="")
  pie(pie.values,
      labels=pie.labels,
      col=pieColors,
      main=paste("Q", quarterNum, " Value Weighting", sep=""))
}

# Create Pie Charts of the Weights
par(mfrow=c(2,2))

plotQuarter(q1.vw.wgt, "1")
plotQuarter(q2.vw.wgt, "2")
plotQuarter(q3.vw.wgt, "3")
plotQuarter(q4.vw.wgt, "4")


calculateVwQuarterlyPortfolio <- function(beginDate, endDate, weightsDataFrame) {
  quarter.val <- subset(vwPortfolio[, c(1,5:7)],
                        vwPortfolio$date >= as.Date(beginDate)
                        & vwPortfolio$date <= as.Date(endDate))
  names(quarter.val) <- paste(c("date", "AMZN", "IBM", "TSLA"))
  
  # calculate cumulative gross return for each security
  quarter.val$AMZN <- cumprod(quarter.val$AMZN)
  quarter.val$IBM <- cumprod(quarter.val$IBM)
  quarter.val$TSLA <- cumprod(quarter.val$TSLA)
  
  # apply the quarter-end weights
  # applying market capitalization weights for each security 
  # based on price & shares outstanding data available last day previous quarter
  quarter.val$AMZN.idx <- weightsDataFrame$AMZN.wgt * quarter.val$AMZN
  quarter.val$IBM.idx  <- weightsDataFrame$IBM.wgt  * quarter.val$IBM
  quarter.val$TSLA.idx <- weightsDataFrame$TSLA.wgt * quarter.val$TSLA
  
  totalValue <- data.frame(rowSums(quarter.val[, 5:7]))
  names(totalValue)<-paste("portfolio.value")
  totalValue$date <- quarter.val$date
  
  return(totalValue)
}

getLastDay <- function(portfolioDataFrame) {
  lastDayTotalValue <- portfolioDataFrame[nrow(portfolioDataFrame), 1]
  return(lastDayTotalValue)
}

q1.vw.val <- calculateVwQuarterlyPortfolio("2013-01-01", "2013-03-31", q1.vw.wgt)
q1.vw.inv <- getLastDay(q1.vw.val)

q2.vw.val <- calculateVwQuarterlyPortfolio("2013-04-01", "2013-06-30", q2.vw.wgt)
q2.vw.inv <- getLastDay(q2.vw.val)

q3.vw.val <- calculateVwQuarterlyPortfolio("2013-07-01", "2013-09-30", q3.vw.wgt)
q3.vw.inv <- getLastDay(q3.vw.val)

q4.vw.val <- calculateVwQuarterlyPortfolio("2013-10-01", "2013-12-31", q4.vw.wgt)
q4.vw.inv <- getLastDay(q4.vw.val)

# Combining Quarterly VW Portfolio Values into One Data Object
vw.portfolio.value <- rbind(q1.vw.val,
                            q2.vw.val,
                            q3.vw.val,
                            q4.vw.val)












