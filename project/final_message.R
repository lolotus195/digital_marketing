rm(list = ls())

GetMessageLabel <- function(df) {
  cnames <- sprintf("V%d", 1:9)
  for (cname in cnames) {
    df[,cname] <- as.character(df[,cname])
  }
  apply(df[,cnames], 1, function(x) { 
    sprintf("(V1, ..., V9) = (%s)", paste(x, collapse=", "))
  })
}


dat <- read.csv('./experiment_results2.csv')
dat$click_rate <- dat$Clicks / dat$N

dat$binom.variance <- dat$N * dat$click_rate * (1 - dat$click_rate)
dat$binom.se <- sqrt(dat$binom.variance)
dat$click_rate_hi <- (dat$Clicks + qnorm(0.975)*dat$binom.se) / dat$N
dat$click_rate_lo <- (dat$Clicks - qnorm(0.975)*dat$binom.se) / dat$N

GetMessageLabel(dat[1,])

sprintf('Predicted click rate: %.5f', 100*dat$click_rate[1])

sprintf('95 percent confidence interval: (%.5f, %.5f)', 
        100*dat$click_rate_lo[1], 100*dat$click_rate_hi[1])
