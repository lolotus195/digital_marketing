# Digital and Algorithmic Marketing
# Homework #2
# Instructor: Sanjog Misra
# Topic: Recommendation Systems

# if needed: install.packages("recommenderlab)

library(recommenderlab)

# A Real Dataset
data(MovieLense)  # Not sure why there is an e at the end
MovieLense

# Use the Movielens data to answer the following questions
# Please upload a pdf file to chalk. Code is not needed.


# Hints: To evalaute timing you can use the system.time command.
#       For example, system.time(r <- Recommender(data, method = "RANDOM") )
#       will tell you how gold raining a RANDOM CF algorithm takes.
#       To assess predictive accuracy you can use the function calcPredictionAccuracy
#       See ?calcPredictionAccuracy for examples 
#       or the recommenderlab vignette: 
#       https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf  

#       Finally, to examine overlap you may use the following function
rec.overlap = function(recA,recB,i)
{
  length(intersect(recA@items[[i]],recB@items[[i]]))
}

# Example
# Split the data
# Hint: You may want to look at the Reccomderlab 
#       vignette for better ways of doing this.
train <- MovieLense[1:300]
test <- MovieLense[301:350]

# Training the recommender
r1 <- Recommender(train, method = "RANDOM") 
r2 <- Recommender(train, method = "POPULAR") 

# Predict for the test data
rec1 = predict(r1,test,n=150)
rec2 = predict(r2,test,n=150)

# How many reccomendations overlap for user 12 in the test sample
# You can do this for all the users in test
rec.overlap(rec1,rec2,i=12)


