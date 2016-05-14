# Setup ----------------------------------------------------------------------

rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

require(ggplot2)

# I hate the name dz1, change to zipdat instead
load('ZipDat.Rd')
zipdat <- dz1
rm(dz1)

# -----------------------------------------------------------------------------
# Q1: If the costs of servicing each customer are $10 (rather than zero) what 
# is the optimal uniform price that Ziprecruiter should charge?

# Explore: plot sub rate over price
sub.rate <- data.frame(subs = table(zipdat$SUB, zipdat$prc)[2,],
                       total = colSums(table(zipdat$SUB, zipdat$prc)),
                       prices = sort(unique(zipdat$prc)))
sub.rate$rate <- sub.rate$subs / sub.rate$total
sub.rate$binom.se <- sqrt(sub.rate$total * sub.rate$rate * (1 - sub.rate$rate))
sub.rate$rate.hi <- (sub.rate$subs + qnorm(0.975)*sub.rate$binom.se) / sub.rate$total
sub.rate$rate.lo <- (sub.rate$subs - qnorm(0.975)*sub.rate$binom.se) / sub.rate$total

# Let's run a toy regression
reg <- glm(rate ~ prices, weights = total, data = sub.rate, family = 'binomial')
# reg2 <- glm(SUB ~ prc, data = zipdat, family = 'binomial') # SAME
sub.rate$pred <- predict(reg, type = 'response')
sub.rate$pred.se <- predict(reg, type = 'response', se.fit = T)$se.fit

arrow.se <- arrow(angle = 90, length = unit(0.05, 'inches'), ends = 'both')
g <- ggplot(sub.rate) + geom_point(aes(x = prices, y = rate), size = 3) + 
  geom_segment(aes(x = prices, xend = prices, 
                   y = sub.rate$rate.lo, yend = sub.rate$rate.hi),
               arrow = arrow.se) + 
  geom_line(aes(x = prices, y = pred)) + 
  geom_ribbon(aes(x = prices, ymin = pred - qnorm(0.975)*pred.se,
                  ymax = pred + qnorm(0.975)*pred.se), alpha = 0.2)
plot(g)

# Simulate demand and revenue under various cost assumptions
sim <- data.frame(prices = 0:500,
                  rate = predict(reg, newdata = data.frame(prices = 0:500), 
                                 type = 'response'))
sim$profit0 <- sim$rate * sim$prices
sim$profit10 <- sim$rate * (sim$prices - 10)

pdat <- melt(sim[,c('prices', 'profit0', 'profit10')], id.vars = 'prices')
g <- ggplot(pdat) + geom_line(aes(x = prices, y = value, color = variable))
plot(g)

# Optimal prices: 294; 303
# These are probably not statistically distinguishable! 
# But I am too lazy to check just now.
sim$prices[which.max(sim$profit0)]
sim$prices[which.max(sim$profit10)]

# -----------------------------------------------------------------------------
# Q2: Ziprecruiter is deciding between segmenting their customer base either by 
# state or by job category and then charging a flat fee for each group within 
# the segment (i.e. for each state or category). In other words they would 
# charge different uniform prices for each state or job category. Use the data 
# and the code to justify which approach you think would get them higher 
# expected revenues.

# BY STATE
reg.state <- reg <- glm(SUB ~ prc*job_state, 
                        data = zipdat, family = 'binomial')
revenue.by.state <- sapply(unique(zipdat$job_state), function(s) {
  
  alpha <- coef(reg.state)['(Intercept)']
  beta_price <- coef(reg.state)['prc']
  beta_state <- coef(reg.state)[paste('job_state', s, sep = '')]
  beta_price_state <- coef(reg.state)[paste('prc:job_state', s, sep = '')]
  
  prices <- 1:500
  
  rate <- alpha + 
    beta_state + 
    beta_price * prices + 
    beta_price_state * prices
  rate <- 1/(1+exp(-rate))
  
  num.customers <- sum(zipdat$job_state == s) * rate
  revenue <- num.customers * prices
  return(max(revenue))
})
revenue.by.state <- unlist(revenue.by.state)

# BY JOB CATEGORY
reg.jcat <- reg <- glm(SUB ~ prc*job_category_classified_by_ai, 
                        data = zipdat, family = 'binomial')
revenue.by.jcat <- sapply(unique(zipdat$job_category_classified_by_ai), function(jc) {
  
  alpha <- coef(reg.jcat)['(Intercept)']
  beta_price <- coef(reg.jcat)['prc']
  beta_jcat <- coef(reg.jcat)[paste('job_category_classified_by_ai', jc, sep = '')]
  beta_price_jcat <- coef(reg.jcat)[paste('prc:job_category_classified_by_ai', jc, sep = '')]
  
  prices <- 1:500
  
  rate <- alpha + 
    beta_jcat + 
    beta_price * prices + 
    beta_price_jcat * prices
  rate <- 1/(1+exp(-rate))
  
  num.customers <- sum(zipdat$job_category_classified_by_ai == jc) * rate
  revenue <- num.customers * prices
  return(max(revenue))
})
revenue.by.jcat <- unlist(revenue.by.jcat)

# Compare
sum(revenue.by.state, na.rm=T)
sum(revenue.by.jcat, na.rm=T)

# Better to charge optimal state-by-state price
sum(revenue.by.state, na.rm=T) > sum(revenue.by.jcat, na.rm=T)

# -----------------------------------------------------------------------------
# Q3: Does your answer to Q2 change if you assume marginal costs per customer 
# are $10 and the decision was made based on profits rather than revenues?

# BY STATE
profit.by.state <- sapply(unique(zipdat$job_state), function(s) {
  
  alpha <- coef(reg.state)['(Intercept)']
  beta_price <- coef(reg.state)['prc']
  beta_state <- coef(reg.state)[paste('job_state', s, sep = '')]
  beta_price_state <- coef(reg.state)[paste('prc:job_state', s, sep = '')]
  
  prices <- 1:500
  
  rate <- alpha + 
    beta_state + 
    beta_price * prices + 
    beta_price_state * prices
  rate <- 1/(1+exp(-rate))
  
  num.customers <- sum(zipdat$job_state == s) * rate
  profit <- num.customers * (prices - 10)
  return(max(profit))
})
profit.by.state <- unlist(profit.by.state)

# BY JOB CATEGORY
profit.by.jcat <- sapply(unique(zipdat$job_category_classified_by_ai), function(jc) {
  
  alpha <- coef(reg.jcat)['(Intercept)']
  beta_price <- coef(reg.jcat)['prc']
  beta_jcat <- coef(reg.jcat)[paste('job_category_classified_by_ai', jc, sep = '')]
  beta_price_jcat <- coef(reg.jcat)[paste('prc:job_category_classified_by_ai', jc, sep = '')]
  
  prices <- 1:500
  
  rate <- alpha + 
    beta_jcat + 
    beta_price * prices + 
    beta_price_jcat * prices
  rate <- 1/(1+exp(-rate))
  
  num.customers <- sum(zipdat$job_category_classified_by_ai == jc) * rate
  profit <- num.customers * (prices - 10)
  return(max(profit))
})
profit.by.jcat <- unlist(profit.by.jcat)

# Compare
sum(profit.by.state, na.rm=T)
sum(profit.by.jcat, na.rm=T)

# Better to charge optimal state-by-state price even with MC = 10
sum(profit.by.state, na.rm=T) > sum(profit.by.jcat, na.rm=T)
