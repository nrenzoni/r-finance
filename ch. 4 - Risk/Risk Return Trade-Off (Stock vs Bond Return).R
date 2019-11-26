setwd("C:/dev/finance/r/kenneth french data")

FF.raw <- read.fwf(file="F-F_Research_Data_Factors.txt",
                   width=c(6,8,8,8,8), skip=4)

# Clean up Data
FF.raw <- FF.raw[-1120:-nrow(FF.raw), ]
names(FF.raw)<-paste(c("text.date","RmxRf","SMB","HML","Rf"))
FF.raw <- FF.raw[, c(-1, -3, -4)]

FF.raw$RmxRf <- as.numeric(as.character(FF.raw$RmxRf)) / 100
FF.raw$Rf    <- as.numeric(as.character(FF.raw$Rf)) / 100

# date
FF.raw$date <- seq(as.Date("1926-07-01"), by="months", length.out=nrow(FF.raw))
FF.raw$date <- as.yearmon(FF.raw$date, "%Y-%m-%d")

# add the risk-free rate back
FF.raw$Rm <-FF.raw$RmxRf + FF.raw$Rf

# Subset Data from December 1963 to December 2013
FF <- subset(FF.raw,
             FF.raw$date >= "1963-12-01"
              & FF.raw$date <= "2013-12-31")

# Calculate Gross Returns for the Market and Risk-free Rate
FF$Gross.rm <- 1 + FF$Rm
FF$Gross.rm[1] <- 1

FF$Gross.rf <- 1 + FF$Rf
FF$Gross.rf[1] <- 1

# Calculate Cumulative Returns for the Market and Risk-free Rate
FF$cum.rm <- cumprod(FF$Gross.rm)
FF$cum.rf <- cumprod(FF$Gross.rf)

# Plot the Data
y.range <- range(FF$cum.rm, FF$cum.rf)
title1 <- "Stock vs. Bond Returns"
title2 <- "1964 to 2013"

plot(x=FF$date,
     y=FF$cum.rm,
     type="l",
     xlab="date",
     ylab="Value of $1 Investment",
     ylim=y.range,
     main=paste(title1, "\n", title2, sep=""))

lines(x=FF$date, y=FF$cum.rf, lty=2)

legend("topleft",
       c("Stocks (2013 Ending Value: $124.89)",
         "Bonds (2013 Ending Value: $12.10)"),
       lty=c(1,2))



# Plot Stock and Bond Returns
y.range <- range(FF$Rm, FF$Rf)
title1 <- "Volatility of Stock vs. Bond Returns"
title2 <- "1964 to 2013"

plot(x=FF$date,
     FF$Rm,
     type="l",
     xlab="Date",
     ylab="Returns (%)",
     ylim=y.range,
     col="blue",
     main=paste(title1, "\n", title2, sep=""))

lines(x=FF$date, y=FF$Rf, col="red")

abline(h=0)

legend("topleft",
       c("Stocks", "Bonds"),
       lty=c(1,2),
       col=c("blue", "red"))






