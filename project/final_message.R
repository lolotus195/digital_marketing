rm(list = ls())

require(dplyr)
source("model.R")

GetMessageLabel <- function(df) {
  cnames <- sprintf("V%d", 1:9)
  for (cname in cnames) {
    df[,cname] <- as.character(df[,cname])
  }
  apply(df[,cnames], 1, function(x) { 
    sprintf("(V1, ..., V9) = (%s)", paste(x, collapse=", "))
  })
}

# Final message ----

dat <- read.csv('results/experiment2.csv')
dat$click_rate <- dat$Clicks / dat$N

dat$binom.variance <- dat$N * dat$click_rate * (1 - dat$click_rate)
dat$binom.se <- sqrt(dat$binom.variance)
dat$click_rate_hi <- (dat$Clicks + qnorm(0.975)*dat$binom.se) / dat$N
dat$click_rate_lo <- (dat$Clicks - qnorm(0.975)*dat$binom.se) / dat$N

GetMessageLabel(dat[1,])

sprintf('Predicted click rate: %.5f', 100*dat$click_rate[1])

sprintf('95 percent confidence interval: (%.5f, %.5f)', 
        100*dat$click_rate_lo[1], 100*dat$click_rate_hi[1])

# Expected profit ----

dat1 <- RelevelData(read.csv('results/experiment1.csv'), exp.levels)
dat2 <- RelevelData(read.csv('results/experiment2.csv'), exp.levels)
dat.both <- rbind(dat1 %>% mutate(series="#1"), 
                  dat2 %>% mutate(series="#2"))

experiment.cost <- nrow(dat.both) * 200
experiment.revenue <- sum(dat.both$Clicks * 0.10)

final.revenue <- dat$click_rate[1] * (5e6 - sum(dat.both$N)) * 0.10
final.revenue_hi <- dat$click_rate_hi[1] * (5e6 - sum(dat.both$N)) * 0.10
final.revenue_lo <- dat$click_rate_lo[1] * (5e6 - sum(dat.both$N)) * 0.10

final.profit <- final.revenue + experiment.revenue - experiment.cost
final.profit_hi <- final.revenue_hi + experiment.revenue - experiment.cost
final.profit_lo <- final.revenue_lo + experiment.revenue - experiment.cost

sprintf('Predicted profit: $%.2f', final.profit)

sprintf('95 percent confidence interval: ($%.2f, $%.2f)', 
        final.profit_lo, final.profit_hi)

# Some stuff ----

reg6 <- glm(Clicks/N ~ V6, weights = N, data = dat.both, family = 'binomial')
