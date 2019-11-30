library(xts)
library(quantmod)

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

securitiesList <- c("AMZN")

lapply(securitiesList, import)

#################################
# Parkinson risk measure
#################################

# only Keep High and Low Price
parkinson <- cbind(data.AMZN$High, data.AMZN$Low)
parkinson <- parkinson[-1, ]

# Calculate Terms in Parkinson Formula
parkinson$log.hi.low <- log(parkinson$High / parkinson$Low)
parkinson$log.square <- (parkinson$log.hi.low) ** 2

# Sum of the values Under the log.square column
parkinson.sum <- sum(parkinson$log.square)
  
# Daily Parkinson Volatility Measure
parkinson.volatility <- sqrt( 1 / (4 * nrow(parkinson) * log(2)) * parkinson.sum )

# Annualized Parkinson Volatility
annual.parkinson.volatility <- parkinson.volatility * sqrt(252)

cat("Annualized Parkinson Volatility: ", 
    annual.parkinson.volatility * 100, 
    "%", "\n",
    sep="")

#################################
# Garman-Klass
#################################

garman.klass <- cbind(data.AMZN$Open,
                      data.AMZN$High,
                      data.AMZN$Low,
                      data.AMZN$Close)
garman.klass <- garman.klass[-1, ]

# First Term
garman.klass.one <- ( 1 / (2 * nrow(garman.klass))) * parkinson.sum

# Second Term
garman.klass.two <- ((2 * log(2) - 1) / nrow(garman.klass)) *
  sum(log(garman.klass$Close / garman.klass$Open) ** 2)

# Daily Garman-Klass Volatility
garman.klass.volatility <- sqrt(garman.klass.one - garman.klass.two)

# Annualized Volatility
annual.garman.klass.volatility <- garman.klass.volatility * sqrt(252)

cat("Annualized Garman-Klass Volatility: ", 
    annual.garman.klass.volatility * 100, 
    "%", "\n",
    sep="")

#################################
# Rogers, Satchell, and Yoon Volatility
#################################

# prior risk measures assume mean return is zero.
rsy.volatility <- cbind(data.AMZN$Open,
                        data.AMZN$High,
                        data.AMZN$Low,
                        data.AMZN$Close)
rsy.volatility <- rsy.volatility[-1, ]

# Product of First Two Log Terms
rsy.one <- log(rsy.volatility$High / rsy.volatility$Close)
rsy.two <- log(rsy.volatility$High / rsy.volatility$Open)
rsy.one.two <- rsy.one * rsy.two

# Product of Last Two Log Terms
rsy.three <- log(rsy.volatility$Low / rsy.volatility$Close)
rsy.four <- log(rsy.volatility$Low / rsy.volatility$Open)
rsy.three.four <- rsy.three * rsy.four

# RSY Volatility Measure
rsy.volatility <- sqrt((1 / nrow(rsy.volatility)) * 
                         sum(rsy.one.two + rsy.three.four))

# Annualized RSY Volatility Measure
annual.rsy.volatility <- rsy.volatility * sqrt(252)

cat("Annualized Rogers, Satchell, and Yoon Volatility: ", 
    annual.rsy.volatility * 100, 
    "%", "\n",
    sep="")

#################################
# Yang and Zhang
#################################

#  handles both opening jumps and drift
yz.volatility <- cbind(data.AMZN$Open,
                       data.AMZN$High,
                       data.AMZN$Low,
                       data.AMZN$Close)
yz.volatility$lag.close <- Lag(yz.volatility$Close, k=1)
yz.volatility <- yz.volatility[-1, ]

# First Term in Yang-Zhang Equation
yz.one.mean <- mean(log(yz.volatility$Open / yz.volatility$lag.close))
yz.one <- 1 / (nrow(yz.volatility) - 1) * sum((
  log(yz.volatility$Open / yz.volatility$lag.close) - yz.one.mean) ** 2)

# Second Term in the Yang-Zhang Equation
yz.two.mean <- mean(log(yz.volatility$Close / yz.volatility$Open))
yz.two <- 1 / (nrow(yz.volatility) - 1) * sum((
  log(yz.volatility$Close / yz.volatility$Open) - yz.two.mean) ** 2)

# Calculation of k
# Yang and Zhang suggest that value of ?? should be 1.34
alpha <- 1.34
k <- (alpha - 1) / (alpha + (nrow(yz.volatility) + 1) / 
  (nrow(yz.volatility) - 1))

# Annualized Yang-Zhang Volatility
annual.yz.volatility <- 
  sqrt(yz.one + k * yz.two + (1-k) * rsy.volatility ^ 2) *
  sqrt(252)

cat("Annualized Yang-Zhang Volatility: ", 
    annual.yz.volatility * 100, 
    "%", "\n",
    sep="")


#################################
# Comparing the Risk Measures
#################################

AMZN.returns <- data.AMZN$Adjusted
AMZN.returns$return <- Delt(AMZN.returns$Adjusted)
AMZN.returns <- AMZN.returns$return
AMZN.returns <- AMZN.returns[-1]

# Log Returns
closeToClose.return <- AMZN.returns
closeToClose.return$logReturn <- log(1 + closeToClose.return$return)

# Standard Deviation of the Returns
closeToClose.volatility <- sd(closeToClose.return$logReturn)

# Annualized Close-to-Close Volatility
annual.closeToClose.volatility <- closeToClose.volatility * sqrt(252)

cat("Annualized Close-to-Close Volatility: ", 
    annual.closeToClose.volatility * 100, 
    "%", "\n",
    sep="")


# Table of the Different Volatility Measures
volatility.measures <- rbind(annual.closeToClose.volatility,
                             annual.parkinson.volatility,
                             annual.garman.klass.volatility,
                             annual.rsy.volatility,
                             annual.yz.volatility)

rownames(volatility.measures) <- c("Close-To-Close",
                                   "Parkinson",
                                   "Garman-Klass",
                                   "Rogers et al",
                                   "Yang-Zhang")

colnames(volatility.measures) <- c("Volatility")









