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

max.price <- 1e4

GetRevenue <- function(int.var, int.reg) {
  rev <- sapply(unique(zipdat[,int.var]), function(s) {

    alpha <- coef(int.reg)['(Intercept)']
    beta_int <- coef(int.reg)[paste(int.var, s, sep = '')]
    beta_price <- coef(int.reg)['prc']
    beta_price_int <- coef(int.reg)[paste('prc:', int.var, s, sep = '')]
    
    prices <- 1:max.price
    
    base.model <- T # assume baseline model by default
    if(is.na(beta_price_int) | is.na(beta_int)) { 
      # no data, just use baseline model
      rate <- coef(reg)['(Intercept)'] + prices * coef(reg)['prices']
    } else {
      # Want the standard error of beta_price + beta_price_int
      se.price <- summary(int.reg)$coef['prc','Std. Error']
      se.price_int <- summary(int.reg)$coef[paste('prc:', int.var, s, sep = ''),'Std. Error']
      se.both <- sqrt(se.price^2 + se.price_int^2)
      
      # Let's be 50% confident that elasticity is actually negative
      is.negative <- (beta_price + beta_price_int + qnorm(0.75)*se.both) < 0
      
      if (is.negative) {
        # Elasticity is probably negative
        rate <- alpha + beta_int + prices * (beta_price + beta_price_int)
        base.model <- F
      } else { 
        # Can't be sure of negative elasticity, use baseline model
        rate <- coef(reg)['(Intercept)'] + prices * coef(reg)['prices']
      }
    }
    rate <- 1/(1+exp(-rate))
    
    num.customers <- sum(zipdat[,int.var] == s) * rate
    revenue <- num.customers * prices
    return(list(max(revenue), 
                prices[which.max(revenue)], 
                num.customers[which.max(revenue)],
                base.model))
  })
  return(rev)
}

# BY STATE
reg.state <- glm(SUB ~ prc*job_state, 
                 data = zipdat, family = 'binomial')
by.state <- GetRevenue('job_state', reg.state)
revenue.by.state <- data.frame(state = unique(zipdat$job_state),
                               revenue = unlist(by.state[1,]),
                               price = unlist(by.state[2,]),
                               N = unlist(by.state[3,]),
                               base.model = unlist(by.state[4,]))

# sort by revenue for plotting purposes
revenue.by.state <- revenue.by.state[order(revenue.by.state$revenue, decreasing = T),]
revenue.by.state$xmax <- cumsum(revenue.by.state$N)
revenue.by.state$xmin <- cumsum(revenue.by.state$N) - revenue.by.state$N

g1 <- ggplot(revenue.by.state) + 
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=price, fill=base.model), 
            color = '#000000') +
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)') + 
  theme_bw()
plot(g1)

# BY JOB CATEGORY
reg.jcat <- glm(SUB ~ prc*job_category_classified_by_ai, 
                data = zipdat, family = 'binomial')
by.jcat <- GetRevenue('job_category_classified_by_ai', reg.jcat)
revenue.by.jcat <- data.frame(state = unique(zipdat$job_category_classified_by_ai),
                               revenue = unlist(by.jcat[1,]),
                               price = unlist(by.jcat[2,]),
                               N = unlist(by.jcat[3,]),
                               base.model = unlist(by.jcat[4,]))

# sort by revenue for plotting purposes
revenue.by.jcat <- revenue.by.jcat[order(revenue.by.jcat$revenue, decreasing = T),]
revenue.by.jcat$xmax <- cumsum(revenue.by.jcat$N)
revenue.by.jcat$xmin <- cumsum(revenue.by.jcat$N) - revenue.by.jcat$N

g2 <- ggplot(revenue.by.jcat) + 
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=price, fill=base.model), 
            color = '#000000') +
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)') + 
  theme_bw()
plot(g2)

# Compare
sum(revenue.by.state$revenue)
sum(revenue.by.jcat$revenue)

# With my crazy assumptions it is better to price on job category
sum(revenue.by.jcat$revenue) > sum(revenue.by.state$revenue)

# -----------------------------------------------------------------------------
# Q3: Does your answer to Q2 change if you assume marginal costs per customer 
# are $10 and the decision was made based on profits rather than revenues?

# BY STATE
profit.by.state <- sapply(unique(zipdat$job_state), function(s) {
  
  alpha <- coef(reg.state)['(Intercept)']
  beta_price <- coef(reg.state)['prc']
  beta_state <- coef(reg.state)[paste('job_state', s, sep = '')]
  beta_price_state <- coef(reg.state)[paste('prc:job_state', s, sep = '')]
  
  prices <- 1:max.price
  
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
  
  prices <- 1:max.price
  
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
