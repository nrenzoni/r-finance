# Value at risk - Gaussian

setwd("C:/dev/finance/r/generated data")

# analysis of portfolio with equal weights (EW)
# originally invested $1 million

portfolio.return <- read.csv("Hypothetical Portfolio (Daily).csv")
portfolio.cum.return <- portfolio.return$EW.cum[nrow(portfolio.return)] * ( 10 ^ 6 )
portfolio.return <- portfolio.return$ew.return[-1]

# Calculate Mean and Standard Deviation of Historical Daily Portfolio Returns
portfolio.mean <- mean(portfolio.return)
portfolio.riskStd <- sd(portfolio.return)

# VaR(α) = −(μ − σ ∗ Zα) ∗ I
# α is the signiﬁcance level of the VaR
# μ is the portfolio average return
# σ is standard deviation of the portfolio returns
# Zα is the z-score based on the VaR signiﬁcance level
# I is the current portfolio value

var01.gaussian <- 
  -(portfolio.mean + portfolio.riskStd * qnorm(0.01)) * portfolio.cum.return

var05.gaussian <- 
  -(portfolio.mean + portfolio.riskStd * qnorm(0.05)) * portfolio.cum.return


  
  
  



