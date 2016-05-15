# Will Clark
# University of Chicago Booth School of Business
# HW2, Digital and Algorithmic Marketing
# April 2016

# Housekeeping ----------------------------------------------------------------

rm(list=ls())

set.seed(0xDedBeef)

source('../utils/source_me.R', chdir = T)
CreateDefaultPlotOpts()

require(recommenderlab)
require(ggplot2)
require(pracma)
require(stringr)

# Built-in data from recommenderlab
# 943 users (each row is a user)
# 1664 movies (each column is a movie)
data(MovieLense)
num.users <- nrow(MovieLense)
num.movies <- ncol(MovieLense)

# Q1: Split the data into training and holdout samples. -----------------------
#     Using the training data, implement a user based (UBCF) and 
#     item based (IBCF) collaborative filtering model and compare 
#     their performance. In particular, compare the time it takes to 
#     fit the model as well as their predictive accuracy. 
#     Explain the differences you find.

algs <- list('user.based'=list(name='UBCF', param=list(nn=50)))
             # 'item.based'=list(name='IBCF', param=list(k=50)))

scheme <- evaluationScheme(MovieLense, 
                           method='cross-validation', 
                           k=5, 
                           given=5,
                           goodRating=4)

num.recs <- c(1,3,5,10,15,20,50,100,150)
# out <- capture.output(results <- evaluate(scheme, algs, type = 'topNList',
                                          # n=num.recs))
sink('runtime.txt')
results <- evaluate(scheme, algs, type = 'topNList',
                    n=num.recs)
sink()

res.user <- data.frame(avg(results[[1]])) # average across all cross-validation runs
# res.item <- data.frame(avg(results[[2]])) # average across all cross-validation runs

# Combine results from IBCF, UBCF into one data frame
# res <- rbind(res.user, res.item)
# res$N <- rep(num.recs, 2)
# res$alg <- c(rep('user.based', length(num.recs)), rep('item.based', length(num.recs)))

res <- res.user
res$N <- num.recs
res$alg <- rep('user.based', length(num.recs))

# Plot "ROC" type curve
g <- ggplot(res) + geom_line(aes(x=FPR, y=TPR, color=alg)) + 
  geom_point(aes(x=FPR, y=TPR, color=alg)) + 
  geom_text(aes(x=FPR, y=TPR, label=N), hjust=1.25, vjust=0) +
  labs(x='False Positive Rate (FPR)', y='True Positive Rate (TPR)') + 
  scale_color_discrete('Algorithm', labels=c('Item-based CF', 'User-based CF'))
plot(g)

# "Area under the curve"
auc.user <- trapz(x=avg(results[[1]])[,'FPR'], y=avg(results[[1]])[,'TPR'])
auc.item <- trapz(x=avg(results[[1]])[,'FPR'], y=avg(results[[1]])[,'FPR'])

# Collect runtime statistics
# GetEvalRuntime <- function(out) {
#   times <- c()
#   alg.time <- 0
#   alg.names <- c()
#   for (s in out) {
#     if(grepl('\t',s)) {
#       x <- str_extract_all(print(str_extract(s, '(?<=\\[).+?(?=\\])')),
#                            '\\(?[0-9,.]+\\)?')[[1]]
#       alg.time <- alg.time + sum(as.numeric(x))
#     } else { # line indicates a new algorithm being timed
#       if (alg.time != 0)
#         times <- c(times, alg.time)
#       alg.time <- 0
#       alg.name <- str_split(s, ' ')[[1]][1]
#       alg.names <- c(alg.names, alg.name)
#     }
#   }
#   times <- c(times, alg.time)
#   names(times) <- alg.names
#   return(times)
# }
# alg.runtimes <- GetEvalRuntime(out)

# Q2: Make recommendations for the holdout sample based on the results. -------
#     Examine the overlap between the recommendations from UBCF and IBCF.
#     Specifically, examine the overlap in the top100 recs for test group 
#     users. Is this overlap expected? Explain what you think is going on.


# Q3: Imagine that you could append movie characteristics (Budget, ------------
#     Studio, Actors, Director etc.) to this data. How would you use this 
#     to construct a recommendation system? Outline any specifics you can 
#     about this new system.
