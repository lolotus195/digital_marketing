####
# Use the Ziprecruiter case data and code to answer the following questions:

# Q1: If the costs of servicing each customer are $10 (rather than zero) what is the optimal uniform price
#     that Ziprecruiter should charge?
# 
# Q2: Ziprecruiter is deciding between segmenting their customer base either by state or by job category 
#     and then charging a flat fee for each group within the segment (i.e. for each state or category). 
#     In other words they would charge different uniform prices for each state or job category. 
#     Use the data and the code to justify which approach you think would get them higher expected revenues.
# 
# Q3: Does your answer to Q2 change if you assume marginal costs per customer are $10 and the decision was 
#     made based on profits rather than revenues?


####
# Setup -------------------------------
####

library(Hmisc)

# load data
load(file="ZipDat.Rd")

####
# Question 1 -------------------------------
####

tabS = table(dz1$prc,dz1$SUB)
tabS = tabS[,2]/rowSums(tabS)

# Barplots for Revenues ~ Price Point
bp2 = barplot((as.numeric(names(tabS))-10)*tabS,ylim=c(0,55),ylab="Revenues/Customer",xlab="Price Point")
hts = (as.numeric(names(tabS)) - 10)*tabS
se2 = as.numeric(names(se))*se # Delta Method Used here
errbar(bp2[,1],hts,hts+1.96*se2,hts-1.96*se2,add=T)

# Simple Regression
reg1 = lm(SUB~prc,data=dz1)
reg2 = lm(SUB~prc+I(prc^2),data=dz1)

# Elasticity
tabS = table(dz1$prc,dz1$SUB)
probs = tabS[,2]/rowSums(tabS)
prcs = as.numeric(names(probs))
elas = (reg2$coef[2]+reg2$coef[3]*prcs)*prcs/probs
elas1 = (reg1$coef[2])*prcs/probs

#Make a new profitability vairable
dz1$profit = dz1$prc - 10
reg3 = lm(SUB~profit,data=dz1)
elas3 = (reg3$coef[2])*prcs/probs

# Suggests inflexion around $249-$299 
g2 = glm(SUB~(factor(job_state)+factor(job_category_classified_by_ai))*(profit),data=dz1,family='binomial')
# Why are there warnings?
summary(predict(g2,type="response"))

# Construct Prediction Function
predict.rev1 = function(prft,jstate="CA",jcat="Legal")
{
  #Supress Warnings
  options(warn=-1)
  # "New Data"
  nd = data.frame(job_state=jstate,job_category_classified_by_ai=jcat,profit=prft)
  # Predict
  phat = predict(g2,nd,type='response')  
  # Reset Warnings
  options(warn=0)
  # Expected Revenues
  erev = as.numeric(phat*prft)
  return(erev)
}


#Optimize
# optimize(f = predict.rev, interval = c(0,1000),jstate = "TX",jcat = jcats[1], maximum = TRUE)
max_profit = optimize(f = predict.rev1, interval = c(0,1000), maximum = TRUE)

####
# Question 2 -------------------------------
####

####
# Question 3 -------------------------------
####


