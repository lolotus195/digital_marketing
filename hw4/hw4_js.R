# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Homework #4 (Group)

# Hello World

# The persado_experiment.xlsx file contains a real experiment run by Persado
# Use the data to answer the following questions.

# Q1: Assuming that each all relevant variables were tested how many possible 
#		message combinations are there? 

# Create a Design Matrix
# install.packages("AlgDesign") 
library(AlgDesign)

# Full Factorial Design
mat = gen.factorial(levels=c(4,4,2,2,2,4,2),varNames=paste("V",1:7,sep=""),factors="all")
# ?gen.factorial
# mat = gen.factorial generates a design matrix

head(mat)
tail(mat)

## There are 1024 possible message combinations: 7 factors with levels (4,4,2,2,2,4,2)

# Q2: Estimate two logit models based on the data (assuming a simple linear 
#		specification) - one for opens and another for clicks. Discuss your results based
#		on the nature of the variables (see variable names and descriptions tab).

setwd("C:/Users/Jonathan/Documents/Classes/Digital & Algorithmic Marketing/digital_marketing")
persado <- read.csv("persado_experiment_clean.csv",header = TRUE)
head(persado)
names(persado)

#how to create a matrix in R
> B = matrix( 
  +   c(2, 4, 3, 1, 5, 7), 
  +   nrow=3, 
  +   ncol=2) 

persado_matrix = matrix #to be continued

open_rate <- persado$unique_opened/persado$unique_sent
click_rate <- persado$uniqe_clicks/persado$unique_sent

open_Prob = glm(open_rate ~ persado$intro + persado$headline + persado$main_text + persado$button + persado$action + persado$purpose + persado$symbol, family = "binomial")#the function for logistic regression
summary(open_Prob)

click_Prob = glm(click_rate ~ persado$intro + persado$headline + persado$main_text + persado$button + persado$action + persado$purpose + persado$symbol, family = "binomial")#the function for logistic regression
summary(open_Prob)

# Q3: Use the estimated models to compute predicted probabilities for all possible message
# 		combinations (separate predictions for opens and clicks). 
#		Which messages have the highest fitted response probabilities? 
#		Are the two messages similar or different? Discuss.

# Q4: Please use the historical data provided in the project section of chalk to provide 
#		me your experimental design. See HistData.R for details.	  

 