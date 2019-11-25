setwd("ch. 3 - Portfolio Return")

source("./equal weight portfolio.R")
source("./value weight portfiolio.R")

# Normalized EW and VW Portfolio Price Chart
port.val<-merge(vw.portfolio.val, ew.portfolio.val, by="date")
names(port.val) <- paste(c("date","VW.cum","EW.cum"))

# Plot the Data
par(mfrow=c(1,1))

y.range <- range(port.val[ ,2:3])

plot(port.val$EW.cum,
     type="l",
     xlab="Date",
     ylab="Value of Investment",
     ylim=y.range,
     lty=1,
     main="Value of $1 Investment in Equal-Weighted and
     Value-Weighted Portfolios of AMZN, YHOO, and IBM
     December 31, 2012 - December 31, 2013")

lines(port.val$VW.cum, lty=2)

abline(h=1, lty=1)

legend("topleft",
       c("Equal-Weighted Portfolio","Value-Weighted Portfolio"),
       lty=c(1,2))
