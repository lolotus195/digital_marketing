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
load("hw3.rdat")
summary(df)

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
# Peek at results...
summary(lres.m)

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
  
# Q2: Does the utility/preference function change depending on the 
#     looks of the Sender? Are there differences in how these changes for men and women?
#
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
#
#     You can use these ideas to explore various levels of sender looks.
#     Of course you can do this for women senders as well...

# Q3: Using the pred.match function explore the match scores between 
#     men and women at various looks percentiles. Comment on and 
#     explain your findings. For example, you may want to explore 
#     why pred.match(2,10) differs from pred.match(10,2).    
    
# Q4: (Optional: Warning Difficult and Time Consuming!) 
#     How would you change the match function to account 
#     for the findings in Q2. How do the results comapre to
#     the original pred.match function results.


