# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Lookalike Models with k-NN

rm(list=ls())

# Load Data
load(file="cust.rdat")
load(file="target.rdat")

# What's in them?
names(cust)

# Obviously we can blanket all customers
cost = .5
profit_all = sum(target[,"spend"])-cost*length(target[,"spend"])
profit_all

# Building the Lookalike Audience
# Let's start with k-Means

# First we need to create the "Seed"
# Valuable Customers
# What is our cutoff? $10
cs=cust[cust$spend>10,2:10]
CV = mean(cust[cust$spend>10,"spend"]); CV
EV = mean(cust[,"spend"]); EV

# In this case we have to  use model matrix 
# because variables are categorical
seed=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index-1, data=cs)

# Note -1 is becuase we dont need an intercept

# Transform the target dataset to match cust 

ts = target[,2:10]
targ=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index-1, data=ts)

# Now make sure all variables (names) match
# If not, retain only those that do
targ=targ[,intersect(colnames(seed),colnames(targ))]

# Now lets run kmeans on the target
km = kmeans(targ,centers = 13)
# find the average profile of seed
km.seed = kmeans(seed,centers=1)
# Then lets find closest cluster to that profile
ds=NULL
for(cc in 1:13) 
{ds = c(ds,dist(rbind(km.seed$centers,km$centers[cc,])))}
ds.min = which.min(ds); ds.min                  
# This gives us a segment to target
matches = which(km$cluster==ds.min)
# How did we do?
profit.km = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
profit.km # bleh!

# How about Nearest Neighbors?
# We will need a package to do kNN
# Let's use FNN

# install.packages("FNN")
# Use package
library(FNN)

# Lets say we want the 10 nearest
# neighbors for every obs in seed
# we can get this by
m10 = get.knnx(targ,seed,k=10,algorithm='brute')

# Need to be careful of duoplicates
matches=unique(as.vector(m10$nn.index))

# profit
profit10 = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
profit10

# Lets repeat this for different k
# Storage
profit=rep(0,20)

# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit[k] = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
}

plot(profit,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
# Best k
k.star = which.max(profit); k.star
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit.star = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])

profit.star

# Explore matches
# How many did we really match?
table(target[matches,"spend"]>0)

# Waht percentage did we capture?
perc.captured = sum(target[matches,"spend"])/sum(target[,"spend"])
perc.captured

# Can we do better?
# Which variables really matters?
# This is a very naive approach
# Let's regress spend on variables
reg = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index, data=cust)
summary(reg)


# Redo knn with 
seed = with(cs,cbind(retail_index,household_income==6,household_size==6))
targ = with(target,cbind(retail_index,household_income==6,household_size==6))
# Storage
profit2=rep(0,20)
# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit2[k] = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
}

# Plot
plot(profit2,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
k.star = which.max(profit2); k.star
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit2.star = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])

# Explore matches
# How many did we really match?
table(target[matches,"spend"]>0)

# What percentage did we capture?
perc.captured = sum(target[matches,"spend"])/sum(target[,"spend"])
perc.captured

# How do the two compare
plot(profit2,type='b',pch=19,ylim=c(-500,4000))
lines(profit,type='b')
abline(h=0,lty=2)

# Why the difference?

