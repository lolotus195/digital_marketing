# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Group Homework #1
rm(list=ls())

# A new DMP claims that hey have a better set of variables
# to match your customers on. 
# In particular, they have an ecom_index which they claim 
# Offers an inrementally better match for you.
# Unfortunately the cost of matching customers via them is 
# significantly higher!

cost = 3.25

# They have given you a sample target dataset 
# and a matched version of your customer dataset
# These are in cust2.rdat and target2.rdat
# Hint: Careful the ecom_index is in the 12th column

# Use this new data to answer the following questions.
# 1. Would you go with the new DMP? Justify your answer.
# 2. Is there any circumstance when you would ignore the 
#    matching and go after the entire target file audience? Justify.

## Part 1 ##
## Load data
load(file="cust2.rdat")
load(file="target2.rdat")
library(FNN)

## What's in them?
head(cust2)
head(target2)

## Profit from blanketing everyone
profit_all = sum(target2[,"spend"])-cost*length(target2[,"spend"])
profit_all # Loss of $57,399.67!

## Identify the variables that matter
reg = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index+ecom_index, data=cust2)
summary(reg)

summary(cust2$spend)
quantile(cust2$spend, c(.95, .975, .98, .99)) 
#setting the cutoff for spend at $10 gets us aroun the top 2.5% of customers
head(cust2)
dim(cust2)
cs=cust2[cust2$spend>10,c(2:10,12)]

## Create seed and target
seed = with(cs,cbind(household_size==6, household_income==6, ecom_index))
targ = with(target2,cbind(household_size==6, household_income==6, ecom_index))

# Storage
profit=rep(0,20)
# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
}

# Plot
plot(profit,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
k.star = which.max(profit); k.star
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit.star = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
profit.star # Profit of $4,413.61

# Explore matches
# How many did we really match?
table(target2[matches,"spend"]>0)

# What percentage did we capture?
perc.captured = sum(target2[matches,"spend"])/sum(target2[,"spend"])
perc.captured # 75.4% captured

## Compare versus original
# Load Data
load(file="cust.rdat")
load(file="target.rdat")

# What's in them?
head(cust)
head(target)

cost_original = .5
cs_original=cust[cust$spend>10,2:10]

## Redo Analysis ##
reg_original = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index, data=cust)
summary(reg_original)

summary(cust$spend)
quantile(cust$spend, c(.95, .975, .98, .99)) 
#setting the cutoff for spend at $10 gets us aroun the top 2.5% of customers
cs_original=cust[cust$spend>10,c(2:10)]

## Create seed and target
seed_original = with(cs_original,cbind(household_size==6, household_income==6, retail_index))
targ_original = with(target,cbind(household_size==6, household_income==6, retail_index))

# Storage
profit_original=rep(0,20)
# Loop
for(k in 1:20){
  mk = get.knnx(targ_original,seed_original,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit_original[k] = sum(target[matches,"spend"])-cost_original*length(target[matches,"spend"])
}

# Plot
plot(profit_original,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
k.star.o = which.max(profit_original); k.star.o
mk.star.o = get.knnx(targ_original,seed_original,k=k.star.o,algorithm='brute')
matches=unique(as.vector(mk.star.o$nn.index))
profit.star.o = sum(target[matches,"spend"])-cost_original*length(target[matches,"spend"])
profit.star.o # Profit of $3,559.14

# How do the two compare?
plot(profit,type='b',pch=19,ylim=c(-500,5000), col="blue")
lines(profit_original,type='b', col="green")
abline(h=0,lty=2)
legend("bottomright", c("profit_new", "profit_original"), lty=c(1,1), col=c("blue","green"))

#  Part 2
## Profit from blanketing everyone
## Cost Grid
cost_grid <- seq(0, 5, .1)
profit_grid <- rep(0,50)

# Loop
for(c in 1:50){
  cost_c <- cost_grid[c] 
  profit_grid[c] = sum(target[,"spend"])-cost_c*length(target[,"spend"])
  }

# Plot
plot(profit_grid,type='b',pch=19)
abline(h=2,lty=2)

profit_grid[4] # profit of 674.03
cost_grid[4] # cost of 0.3

profit_all_check = sum(target[,"spend"])-cost_original*length(target[,"spend"])
profit_all_check # Loss of $3,263.17

