# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Group Homework #1

# A new DMP claims that hey have a better set of variables
# to match your customers on. 
# In particular, they have an ecom_index which they claim 
# Offers an inrementally better match for you.
# Unfortunately the cost of matching customers via them is 
# significantly higher!

# They have given you a sample target dataset 
# and a matched version of your customer dataset
# These are in cust2.rdat and target2.rdat
# Hint: Careful the ecom_index is in the 12th column

# Use this new data to answer the following questions.
# 1. Would you go with the new DMP? Justify your answer.
# 2. Is there any circomstance when you would ignore the 
#    matching and go after the entire target file audience? Justify.

# Houskeeping -----------------------------------------------------------------

rm(list=ls())

set.seed(0xDedBeef)

require(ggplot2)
require(reshape2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# Set up data -----------------------------------------------------------------

load(file='cust.rdat')
load(file='cust2.rdat')
load(file='target.rdat')
load(file='target2.rdat')

# Cost of matching a customer
cost <- 0.5
cost2 <- 3.25

# K-means clustering ----------------------------------------------------------

ProfitKMeans <- function(dat.cust, dat.targ, cost, cutoff, plot=F) {
  # Create seed data set
  # Should not have machine ID or spend
  val.cust <- dat.cust[dat.cust$spend > cutoff,
                       !names(dat.cust) %in% c('machine_id', 'spend')]
  cust.mm <- model.matrix(~.+0, data=val.cust) # +0 keeps first factor, no intercept
  
  # Make sure target data matches seed data
  targ.mm <- model.matrix(~.+0, data=dat.targ)
  targ.mm <- targ.mm[, intersect(colnames(cust.mm), colnames(targ.mm))]
  
  max.centers <- 20
  profit.kmeans <- matrix(NA, max.centers, max.centers)
  
  for (num.centers in 1:max.centers) {
    # K-means on target data
    km.targ <- kmeans(targ.mm, centers=num.centers)
    
    # "K-means" on customer data (only one cluster)
    km.cust <- kmeans(cust.mm, centers=1)
    
    # distance between seed and target clusters
    kdist <- sapply(1:num.centers, function(c) {
      dist(rbind(km.targ$centers[c,], km.cust$centers))
    })
    
    match.order <- order(kdist)
    
    profit.kmeans[1:num.centers, num.centers] <- sapply(1:num.centers, function(c) {
      matches <- which(km.targ$cluster %in% match.order[1:c])
      sum(dat.targ$spend[matches] - cost*length(matches))
    })
  }
  
  if(plot) {
    pdat <- melt(profit.kmeans, varnames=c('k','n.matches'), value.name='profit')
    pdat <- pdat[!is.na(pdat$profit),]
    
    idx.max <- which.max(pdat$profit)
    g <- ggplot(pdat) + geom_tile(aes(x=n.matches, y=k, fill=profit)) + 
      scale_fill_gradient(low=gg_color_hue(3)[1], high=gg_color_hue(3)[2]) +
      scale_y_continuous(trans='reverse') + 
      annotate('rect', xmin=pdat$n.matches[idx.max]-0.5, xmax=pdat$n.matches[idx.max]+0.5, 
               ymin=pdat$k[idx.max]-0.5, ymax=pdat$k[idx.max]+0.5,
               color='white', alpha=0.5) + 
      annotate('text', x=pdat$n.matches[idx.max]-3.5, y=pdat$k[idx.max]+0.75,
               label=sprintf('Max profit: %.f\nNum clusters: %.f\nCluster matches: %.f',
                             max(pdat$profit), pdat$n.matches[idx.max], pdat$k[idx.max]),
               hjust=0, vjust=1) + 
      labs(x='k (# of clusters)', y='# of clusters matched') +
      theme_minimal()
    plot(g)
  }
  
  return(max(profit.kmeans, na.rm=T))
}

# Deciles of non-zero spend amount (same for both data sets)
cutoff <- quantile(cust$spend[cust$spend>0], seq(0.1, 0.95, 0.05))
# cutoff <- quantile(cust$spend[cust$spend>0], seq(0.1, 0.8, 0.2))

# k-means clustering using the decile cutoffs to define a "valuable" customer
profit <- sapply(cutoff, function(c) {
  cat(c,',',sep='')
  list(ProfitKMeans(cust, target, cost, c, plot=F),
       ProfitKMeans(cust2, target2, cost2, c, plot=F))
})
profit1 <- unlist(profit[1,])
profit2 <- unlist(profit[2,])

# Plot the "best" result
ProfitKMeans(cust, target, cost, cutoff[which.max(profit1)], plot=T)
ProfitKMeans(cust2, target2, cost2, cutoff[which.max(profit2)], plot=T)

# Conclusion: 
# 1) no matter what cutoff you use to define value, or how many  clusters you 
#    use, or how many clusters you match on, this is an unprofitable strategy
# 2) the 2nd index is not valuable enough to justify its cost; the first data
#    set is still the more valuable one