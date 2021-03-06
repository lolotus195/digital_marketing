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
# reg2 <- glm(SUB ~ prc, data = zipdat, family = binomial)
sub.rate$pred <- predict(reg, type = 'response')
sub.rate$pred.se <- predict(reg, type = 'response', se.fit = T)$se.fit

# arrow.se <- arrow(angle = 90, length = unit(0.05, 'inches'), ends = 'both')
# g <- ggplot(sub.rate) + geom_point(aes(x = prices, y = rate), size = 3) + 
#   geom_segment(aes(x = prices, xend = prices, 
#                    y = sub.rate$rate.lo, yend = sub.rate$rate.hi),
#                arrow = arrow.se) + 
#   geom_line(aes(x = prices, y = pred)) + 
#   geom_ribbon(aes(x = prices, ymin = pred - qnorm(0.975)*pred.se,
#                   ymax = pred + qnorm(0.975)*pred.se), alpha = 0.2)
# plot(g)

# Simulate demand and revenue under various cost assumptions
sim <- data.frame(prices = 0:500,
                  rate = predict(reg, newdata = data.frame(prices = 0:500), 
                                 type = 'response'))
sim$profit0 <- sim$rate * sim$prices
sim$profit10 <- sim$rate * (sim$prices - 10)

# pdat <- melt(sim[,c('prices', 'profit0', 'profit10')], id.vars = 'prices')
# g <- ggplot(pdat) + geom_line(aes(x = prices, y = value, color = variable))
# plot(g)

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

GetRevenue <- function(int.var, int.reg, cost.per.customer) {
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
    profit <- num.customers * (prices - cost.per.customer)
    return(list(max(profit), 
                prices[which.max(profit)], 
                num.customers[which.max(profit)],
                base.model))
  })
  return(rev)
}

# BY STATE
reg.state <- glm(SUB ~ prc*job_state, 
                 data = zipdat, family = 'binomial')
by.state <- GetRevenue('job_state', reg.state, 0)
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
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)')
plot(g1)

# BY JOB CATEGORY
reg.jcat <- glm(SUB ~ prc*job_category_classified_by_ai, 
                data = zipdat, family = 'binomial')
by.jcat <- GetRevenue('job_category_classified_by_ai', reg.jcat, 0)
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
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)')
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
by.state <- GetRevenue('job_state', reg.state, 10)
profit.by.state <- data.frame(state = unique(zipdat$job_state),
                              revenue = unlist(by.state[1,]),
                              price = unlist(by.state[2,]),
                              N = unlist(by.state[3,]),
                              base.model = unlist(by.state[4,]))

# sort by revenue for plotting purposes
profit.by.state <- profit.by.state[order(profit.by.state$revenue, decreasing = T),]
profit.by.state$xmax <- cumsum(profit.by.state$N)
profit.by.state$xmin <- cumsum(profit.by.state$N) - profit.by.state$N

g3 <- ggplot(profit.by.state) + 
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=price, fill=base.model), 
            color = '#000000') +
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)')
plot(g3)

# BY JOB CATEGORY
by.jcat <- GetRevenue('job_category_classified_by_ai', reg.jcat, 10)
profit.by.jcat <- data.frame(state = unique(zipdat$job_category_classified_by_ai),
                             revenue = unlist(by.jcat[1,]),
                             price = unlist(by.jcat[2,]),
                             N = unlist(by.jcat[3,]),
                             base.model = unlist(by.jcat[4,]))

# sort by revenue for plotting purposes
profit.by.jcat <- profit.by.jcat[order(profit.by.jcat$revenue, decreasing = T),]
profit.by.jcat$xmax <- cumsum(profit.by.jcat$N)
profit.by.jcat$xmin <- cumsum(profit.by.jcat$N) - profit.by.jcat$N

g4 <- ggplot(profit.by.jcat) + 
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=price, fill=base.model), 
            color = '#000000') +
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)')
plot(g4)

# Compare
sum(profit.by.state$revenue)
sum(profit.by.jcat$revenue)

# With my crazy assumptions it is better to price on job category
sum(profit.by.jcat$revenue) > sum(profit.by.state$revenue)

# Try facet_wrap ----

# MakeLabels <- function(df, topN) {
#   df <- df[order(df$revenue, decreasing = T),]
#   c(as.character(df$state[1:topN]), rep('', nrow(df)-topN))
# }
# revenue.by.state$label <- MakeLabels(revenue.by.state, 5)
# revenue.by.jcat$label <- MakeLabels(revenue.by.jcat, 5)
# profit.by.state$label <- MakeLabels(profit.by.state, 5)
# profit.by.jcat$label <- MakeLabels(profit.by.jcat, 5)

fdat <- rbind(revenue.by.state, revenue.by.jcat, profit.by.state, profit.by.jcat)
fdat$mcost <- c(rep(0, nrow(revenue.by.state) + nrow(revenue.by.jcat)),
                rep(10, nrow(profit.by.state) + nrow(profit.by.jcat)))
fdat$type <- c(rep('Segment by State', nrow(revenue.by.state)), 
               rep('Segment by Job Category', nrow(revenue.by.jcat)), 
               rep('Segment by State', nrow(revenue.by.state)), 
               rep('Segment by Job Category', nrow(revenue.by.jcat)))
# fdat$label.x <- 0.5*(fdat$xmax+fdat$xmin)

g5 <- ggplot(fdat[fdat$mcost==0,]) + 
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=price, fill=base.model), 
            color = '#000000') +
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)') + 
  scale_fill_discrete('Model Type:', labels = c('Segment-Specific', 'General')) +
  theme(legend.position = 'bottom') +
  facet_wrap(~ type)
plot(g5)

g6 <- ggplot(fdat[fdat$mcost==10,]) + 
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=price, fill=base.model), 
            color = '#000000') +
  labs(x = '# of Paying Customers', y = 'Optimal Price ($)') + 
  scale_fill_discrete('Model Type:', labels = c('Segment-Specific', 'General')) +
  theme(legend.position = 'bottom') +
  facet_wrap(~ type)
plot(g6)

ggsave('./writeup/seg_mc_0.pdf', g5)
ggsave('./writeup/seg_mc_10.pdf', g6)

# Make some summary tables ----

# Revenue
sum.mc0 <- cbind(revenue.by.state[1:10, c('state', 'revenue', 'price')],
                 revenue.by.jcat[1:10, c('state', 'revenue', 'price')])
sum.mc0[,2] <- prettyNum(sum.mc0[,2], big.mark = ',', format = 'g', digits = 2)
sum.mc0[,5] <- prettyNum(sum.mc0[,5], big.mark = ',', format = 'g', digits = 2)
sum.mc0[,4] <- gsub('&', '+', sum.mc0[,4])
ExportTable(table = sum.mc0, file = 'summary_mc_0', 
            caption = 'Top 10 Segments for Cost = \\$0',
            colnames = c('State', 'Revenue', 'Price', 
                         'Job Category', 'Revenue', 'Price'), 
            include.rownames = F, align = 'lllr|llr')

# Profit
sum.mc10 <- cbind(profit.by.state[1:10, c('state', 'revenue', 'price')],
                  profit.by.jcat[1:10, c('state', 'revenue', 'price')])
sum.mc10[,2] <- prettyNum(sum.mc10[,2], big.mark = ',', format = 'g', digits = 2)
sum.mc10[,5] <- prettyNum(sum.mc10[,5], big.mark = ',', format = 'g', digits = 2)
sum.mc10[,4] <- gsub('&', '+', sum.mc10[,4])
ExportTable(table = sum.mc10, file = 'summary_mc_10', 
            caption = 'Top 10 Segments for Cost = \\$0',
            colnames = c('State', 'Revenue', 'Price', 
                         'Job Category', 'Revenue', 'Price'), 
            include.rownames = F, align = 'lllr|llr')


# Explore fits ----

GetR2 <- function(res) {
  r2 <- 1 - res$deviance / res$null.deviance
  
  n <- length(res$y)
  p <- length(res$coefficients)-1
  adj.r2 <- r2 - (1-r2)*(p/(n-p-1))
  
  return(list(r2, adj.r2))
}

fit1 <- glm(SUB ~ prc, data = zipdat, family = 'binomial')
fit2 <- glm(SUB ~ log(prc), data = zipdat, family = 'binomial')

GetR2(fit1)
GetR2(fit2)

compare <- data.frame(prc = 0:500,
                      rate1 = predict(fit1, newdata = data.frame(prc = 0:500), 
                                      type = 'response'),
                      rate2 = predict(fit2, newdata = data.frame(prc = 0:500), 
                                      type = 'response'))

pdat <- melt(compare, id.vars = 'prc')

g <- ggplot(pdat) + geom_line(aes(x = prc, y = value, color = variable))
plot(g)
