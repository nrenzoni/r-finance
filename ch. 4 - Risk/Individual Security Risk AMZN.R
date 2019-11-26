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

import("AMZN")

# Calculate Returns
AMZN.return <- data.AMZN$Adjusted
AMZN.return$return <- Delt(AMZN.return$Adjusted)
AMZN.return <- AMZN.return[-1, 2]

# Calculate Full Period (2011-2013) Variance and Standard Deviation
AMZN.var.full <- var(AMZN.return)
AMZN.sd.full <- sd(AMZN.return)

# Calculate Variance and Standard Deviation for 2011
AMZN.2011 <- subset(AMZN.return,
                    index(AMZN.return) >= "2011-01-01"
                      & index(AMZN.return) <= "2011-12-31")

AMZN.var.2011 <- var(AMZN.2011)
AMZN.sd.2011 <- sd(AMZN.2011)

# Calculate Variance and Standard Deviation for 2012 and 2013
AMZN.2012 <- subset(AMZN.return,
                    index(AMZN.return) >= "2012-01-01"
                      & index(AMZN.return) <= "2012-12-31")

AMZN.var.2012 <- var(AMZN.2012)
AMZN.sd.2012 <- sd(AMZN.2012)

AMZN.2013 <- subset(AMZN.return,
                    index(AMZN.return) >= "2013-01-01" 
                      & index(AMZN.return) <= "2013-12-31")

AMZN.var.2013 <- var(AMZN.2013)
AMZN.sd.2013 <- sd(AMZN.2013)

# Calculate Average Return for the Full Period and Each of the Subperiods
mean.return.full <- mean(AMZN.return)
mean.return.2011 <- mean(AMZN.2011)
mean.return.2012 <- mean(AMZN.2012)
mean.return.2013 <- mean(AMZN.2013)

# Combine All Data
AMZN.risk <- rbind(
  cbind(AMZN.var.full, AMZN.var.2011, AMZN.var.2012, AMZN.var.2013),
  cbind(AMZN.sd.full, AMZN.sd.2011, AMZN.sd.2012, AMZN.sd.2013),
  cbind(mean.return.full, mean.return.2011, mean.return.2012, mean.return.2013)
)

rownames(AMZN.risk) <- c("Variance", "Std Dev", "Mean")
colnames(AMZN.risk) <- c("2011-2013", "2011", "2012", "2013")

# print risk table
options(digits=3)
print("AMZN risk")
print(AMZN.risk)
options(digits=7)




