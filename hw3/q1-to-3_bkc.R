# Digital and Algorithmic Marketing
# Homework #3 (Group)
# Instructor: Sanjog Misra
# Topic: Matching Models

# I have cleand and simplified the dating data we saw in class.
# The dataset called df has variables
# SenderLooks (1-11): A categorical variable reflecting 
#       the percentile group rated by UC undergrads. 
#       1 is low and 11 is high
# ReceiverLooks (same type as SenderLooks)
# SenderGender (1=Female, 2 = Male)
# ReciverGender (as above)
# y (0/1 :  reflecting if a message was sent)

# Load the data
getwd()
setwd("C:/Users/Chingono/Documents/Algo Marketing/HW3")
load("HW4df.rdat")
summary(df)
library(ggplot2)
library(matchingR)

# Some Simple plots
# Distribution of Women Senders
barplot(table(df[df$SenderGender==1,]$SenderLooks))
# Distribution of Men Senders
barplot(table(df[df$SenderGender==2,]$SenderLooks))



# Logit Models
# For Men and Women I run separate Logit models to capture the 
# impact looks of the receiver have on the probability of sending a message
lres.m = glm(y~ReceiverLooks,data=df[df$SenderGender==2,],family='binomial')
lres.f = glm(y~ReceiverLooks,data=df[df$SenderGender==1,],family='binomial')

y_hat_m = predict(lres.m, newdata=df[df$SenderGender==2,], type="response")
y_hat_f = predict(lres.f, newdata=df[df$SenderGender==1,], type="response")

boxplot(y_hat_m~y, data=df[df$SenderGender==2,])
boxplot(y_hat_f~y, data=df[df$SenderGender==1,])

# Peek at results...
summary(lres.m)
summary(lres.f)
# One can use these results to predict the probability 
# that a man will send a message to a woman with 
# looks = Xfemale
pred.prob.male=function(Xfemale){
  predict(lres.m,newdata=data.frame(ReceiverLooks=factor(Xfemale,levels=1:11)),type='response')
}
# And similarly for women
pred.prob.female=function(Xmale){
  predict(lres.f,newdata=data.frame(ReceiverLooks=factor(Xmale,levels=1:11)),type='response')
}

# For example
pred.prob.male(5)
pred.prob.female(5)

# We can plot the probabilities for Male Senders
plot(x=1:11,y=pred.prob.male(1:11),type='b',pch=19,col="steelblue",xlab="Receiver Looks",ylab="Predicted Probability")
# and over Women Senders
lines(x=1:11,y=pred.prob.female(1:11),type='b',pch=19,col='darkred')


# using these functions I can create a match score
# that simply multiplies the two predictions and takes a square root
pred.match = function(Xmale,Xfemale)
{
  as.numeric(sqrt(pred.prob.female(Xmale)*pred.prob.male(Xfemale)))
}

# So a Man and Woman of with looks =1,1
# have a match score of
pred.match(1,1)
# 0.04904689 
# While Man and Woman of with looks= 11,11
# have a score of...
pred.match(11,11)
#  0.1234561

# Use the data and the code above to answer the following questions

# Q1: What can you say about the preferences of sender men and women 
#     related to the looks of the receiver? Are there differences across the genders?

### 1.  Men are more visually stimulated. The probability of sending a message is
###     more sensitive to receiver looks when the sender is male.

summary(df[df$SenderGender==1,]) # Prior probability of sending message (women)
                                 # y = 0.08061

summary(df[df$SenderGender==2,]) # Prior probability of sending message (men)
                                 # y = 0.09651 

### 2.  Men have a higher prior probability of sending a message (9.65%)
###     than women (8.06%)        

# Numbers of men and women
summary(df$SenderGender==1) # 68,194 women
summary(df$SenderGender==2) # 179,710 men

68194/(68194+179710) # 27.5% women
1- 0.2750823         # 72.5% men

# Q2: Does the utility/preference function change depending on the 
#     looks of the Sender? Are there differences in how these changes for men and women?

#     Here is some code to help:
#     This code estimates preferences for Sender Males with Looks rated less than 6
      lres.m5 = glm(y~ReceiverLooks,data=df[df$SenderGender==2 & as.numeric(df$SenderLooks)<6,],family='binomial')
#     We can then create a prediction function
      pred.prob.m5=function(Xfemale){
      predict(lres.m5,newdata=data.frame(ReceiverLooks=factor(Xfemale,levels=1:11)),type='response')
}
#     And compare to the full sample
#     All Sender=Male 
      plot(x=1:11,y=pred.prob.male(1:11),type='b',pch=19,col="steelblue",xlab="Receiver Looks",ylab="Predicted Probability")
#     for Looks < 6 
      lines(x=1:11,y=pred.prob.m5(1:11),type='b',pch=19,col='blue',lty=2)
#
      lres.f5 = glm(y~ReceiverLooks,data=df[df$SenderGender==1 & as.numeric(df$SenderLooks)<6,],family='binomial')
      #     We can then create a prediction function
      pred.prob.f5=function(Xmale){
        predict(lres.f5,newdata=data.frame(ReceiverLooks=factor(Xmale,levels=1:11)),type='response')
      }
      #     And compare to the full sample
      #     All Sender=Female
      plot(x=1:11,y=pred.prob.female(1:11),type='b',pch=19,col="purple",xlab="Receiver Looks",ylab="Predicted Probability")
      #     for Looks < 6 
      lines(x=1:11,y=pred.prob.f5(1:11),type='b',pch=19,col='red',lty=2)
      
#      You can use these ideas to explore various levels of sender looks.
#     Of course you can do this for women senders as well...

###  1. Men of below-average attractiveness are less likely to contact
###     women who are very attractive (receiver looks of 10 or 11)
###  2. Women of below-average attractiveness are more likely to contact
###     men of low attractiveness. They are less likely to contact
###     attractive men      
      

# Q3: Using the pred.match function explore the match scores between 
#     men and women at various looks percentiles. Comment on and 
#     explain your findings. For example, you may want to explore 
#     why pred.match(2,10) differs from pred.match(10,2).    
# pred.match(Xmale,Xfemale)
pred.match(2,10) # 0.1003579
pred.match(10,2) # 0.0653594

men_looks <- as.numeric(1:11)
women_looks <- as.numeric(1:11)

pred_matrix <- matrix(NA,nrow=11,ncol=11)


## Match prediction matrix 
for(i in 1:11){
  for(j in 1:11){
  pred_matrix[i,j] = pred.match(men_looks[i], women_looks[j])
  }
  }
pred_matrix

## Match prediction matrix for women
for(i in 1:11){
  for(j in 1:11){
    men_pred[i,j] = pred.match(men_looks[i], women_looks[j])
  }
}
men_pred



          
# Q4: (Optional: Warning -- Difficult and Time Consuming!) 
#     How would you change the match function to account 
#     for the findings in Q2. How do the results compare to
#     the original pred.match function results.


