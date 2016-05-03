# Digital and Algorithmic Marketing
# Session 5, April 27 2016
# Instructor: Sanjog Misra
# Topic: Optimal Design and Content Optimization
 
# Create a Design Matrix
# install.packages("AlgDesign") 
library(AlgDesign)

# Full Factorial Design
mat = gen.factorial(levels=c(4,4,2,2,2,4,2),varNames=paste("V",1:7,sep=""),factors="all")

head(mat)
tail(mat)

# Let's Generate data from this design
# Convert to a model matrix
mm1 = model.matrix(~.,data=mat) 
head(mm1)

# Some coefficients 
cf.mm  = c(-3,-.5,.2,.3,.5,.1,-.2,.6,-1,.05,-.1,-.1,.15,.25)

# Simulating Consumer behavior
# Utility
u = mm1%*%cf.mm
# probability of clicking
prob = 1/(1+exp(-u))
# Number of Customers 
N=1000
# Generate Choices for each "message"
set.seed(12345)
y = rep(0,length(prob))
for(j in 1:length(prob))
{
	y[j] = rbinom(1,N,prob[j])
}

# responses
hist(y)

# How many coefs are there?
ncol(mm1)
# Can we find set of messages to test?
set.seed(61113)
ds1 = optFederov( ~ V1+V2+V3+V4+V5+V6+V7,data = mat, nTrials = 16,criterion="I")$design

# Pretend we ran the experiment
# Find those messages in our data
y.ds1 = y[as.numeric(rownames(ds1))]
# Now build a model with that subset
mm.ds1 = model.matrix(~.,ds1)

sm.ds1 = summary(glm(cbind(y.ds1,N-y.ds1)~mm.ds1-1,family='binomial'))
sm.ds1

# Does this even matter?
# What if we just took 16 random rows
ds0 = mat[sample(1:1024,16),]
y.ds0 = y[as.numeric(rownames(ds0))]
mm.ds0 = model.matrix(~.,ds0)
sm.ds0 = summary(glm(cbind(y.ds0,N-y.ds0)~mm.ds1-1,family='binomial'))

#Compare
cbind(coef(sm.ds1)[,1:2],coef(sm.ds0)[,1:2],cf.mm)

# Plot 
plot(coef(sm.ds1)[,1],cf.mm,pch=19,col='orange')
points(coef(sm.ds0)[,1],cf.mm,pch=19,col='magenta')

# Lets repeat with N larger!




