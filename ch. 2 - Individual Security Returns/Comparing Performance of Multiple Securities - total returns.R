library(quantmod)
library(xts)

setwd('C:/finance_data/')

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
import("GSPC")

# Combine Data
multi <- merge(data.AMZN$Adjusted, 
               data.GSPC$Adjusted, 
               data.IBM$Adjusted)

# Converting Data into a data.frameObject
multi.df <- cbind(data.frame(index(multi)),
                  data.frame(multi))

names(multi.df) <- paste(c("date","AMZN","GSPC","IBM"))

# Constructing Normalized Values for Each Security
multi.df$AMZN.idx <- multi.df$AMZN / multi.df$AMZN[1]
multi.df$GSPC.idx <- multi.df$GSPC / multi.df$GSPC[1]
multi.df$IBM.idx <- multi.df$IBM / multi.df$IBM[1]


# Plotting the Index Values of Each Security
y.range <- range(multi.df$AMZN.idx, 
                 multi.df$GSPC.idx, 
                 multi.df$IBM.idx, 
                 na.rm=TRUE)

# set matrix size for graph
par(mfrow=c(1,1))

graph.col.GSPC <- "black"
graph.col.AMZN <- "red"
graph.col.IBM <- "blue"

plot(x=multi.df$date,
     xlab="Date",
     y=multi.df$GSPC.idx,
     ylim=y.range,
     ylab="Value of $1 Investment",
     type="l",
     col=graph.col.GSPC,
     lty=1,
     lwd=2,
     main=paste("Value of $1 Invested in AMZN, IBM,", "\n",
                "And the S&P 500 Index, Based on Total Returns", "\n",
                "December 31, 2010 - December 31, 2013", sep=""))

lines(x=multi.df$date,
      y=multi.df$AMZN.idx,
      col=graph.col.AMZN,
      lty=1,
      lwd=2)

lines(x=multi.df$date,
      y=multi.df$IBM.idx,
      col=graph.col.IBM,
      lty=1,
      lwd=2)

abline(h=1, lty=2, col="gray")

legend("topleft",
       c("AMZN","IBM","S&P 500 Index"),
       col=c(graph.col.GSPC, graph.col.AMZN, graph.col.IBM),
       lty=c(1,1,1),
       lwd=c(2,2,2))





