rm(list=ls())

set.seed(0xDedBeef)

source('../utils/source_me.R', chdir = T)
CreateDefaultPlotOpts()

require(recommenderlab)
require(ggplot2)
require(grid)
require(gridExtra)
require(stringr)

# Built-in data from recommenderlab
# 943 users (each row is a user)
# 1664 movies (each column is a movie)
data(MovieLense)
num.users <- nrow(MovieLense)
num.movies <- ncol(MovieLense)
movie.names <- colnames(MovieLense)

BY_HAND_ON <- 0
COMPUTE_OVERLAP <- 1

# Automatic -------------------------------------------------------------------

given <- 10
goodRating <- 4
split.share <- 0.8
k <- 5

algs <- list('user.based'=list(name='UBCF'),
             'item.based'=list(name='IBCF'))

scheme <- evaluationScheme(MovieLense, method = 'split', 
                           train = split.share, k = k, 
                           given = given, goodRating = goodRating)

num.recs <- c(1,3,5,10,15,20,50,100,150)

sink('output.txt')
results <- evaluate(scheme, algs, type = 'topNList',
                    n=num.recs)
sink()

res.user <- data.frame(avg(results[[1]])) # average across all cross-validation runs
res.item <- data.frame(avg(results[[2]])) # average across all cross-validation runs

FormatRes <- function(res, alg.name) {
  res$N <- rownames(res)
  res$alg.name <- rep(alg.name, nrow(res))
  
  res$TPR <- res[, 'TP'] / (res[, 'TP'] + res[, 'FN'])
  res$FPR <- res[, 'FP'] / (res[, 'FP'] + res[, 'TN'])
  
  return(res)
}
res.user <- FormatRes(res.user, 'user.based')
res.item <- FormatRes(res.item, 'item.based')

res.all <- rbind(res.item, res.user)
g1 <- ggplot(res.all) + geom_line(aes(x=FPR, y=TPR, color=alg.name)) + 
  geom_point(aes(x=FPR, y=TPR, color=alg.name)) + 
  # geom_text(aes(x=FPR, y=TPR, label=N), hjust=1.25, vjust=0) +
  labs(x='False Positive Rate (FPR)', y='True Positive Rate (TPR)') + 
  scale_color_discrete('', labels=c('Item-based CF', 'User-based CF')) +
  theme(legend.position="bottom")

g2 <- ggplot(res.all) + geom_line(aes(x=recall, y=precision, color=alg.name)) + 
  geom_point(aes(x=recall, y=precision, color=alg.name)) + 
  # geom_text(aes(x=recall, y=precision, label=N), hjust=1.25, vjust=1.25) +
  labs(x='Recall', y='Precision') + 
  scale_color_discrete('Algorithm', labels=c('Item-based CF', 'User-based CF'))

# extract legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(g1)
g3 <- grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                               g2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2, heights=c(10, 1))
ggsave('./writeup/accuracy.pdf', g3)

# Extract time to fit
x <- scan('output.txt', what="", sep='\n')
num.algs <- 2
times.all <- sapply(1:num.algs, function(i) {
  idx <- (i*k+i-k):(i*(k+1))
  alg.times <- x[idx]
  alg.name <- strsplit(alg.times[1], ' ')[[1]][1]
  alg.times <- alg.times[2:length(alg.times)]
  times.fold <- mean(sapply(1:length(alg.times), function(j) {
    as.numeric(str_extract_all(str_extract(alg.times[j], '(?<=\\[).+?(?=\\])'),
                               '\\(?[0-9,.]+\\)?')[[1]][1])
  }))
  return(times.fold)
})
names(times.all) <- c('user-based', 'item-based')

# Explore data
users.per.movie <- colCounts(MovieLense)
movies.per.user <- rowCounts(MovieLense)

mean(movies.per.user) # avg user rates 105 movies
mean(users.per.movie) # avg movie seen by 60 users

median(movies.per.user) # median user rates 64 movies
median(users.per.movie) # median movie seen by 27 users

# By hand -------------------------------------------------------------------

if(BY_HAND_ON) {
  acc <- sapply(1:k, function(k) {
    
    cat(k, ',', sep='')
    
    # Split data into 80% train and 20% test
    idx.train <- sample(1:num.users, round(num.users * split.share))
    idx.test <- setdiff(1:num.users, idx.train)
    
    # Train the recommender
    rec <- Recommender(MovieLense[idx.train,], method = 'UBCF')
    
    # Evaluate recommender on the test set:
    # Pick (given) movies that I have seen that are "good"
    # Predict 50 other movies that I wold like
    # Count the true positives and false positives
    
    fold.acc <- sapply(idx.test, function(i) {
      
      tester <- as(MovieLense[i], 'matrix')
      idx.seen <- which(tester >= goodRating)
      
      if(length(idx.seen) < given)
        idx.given <- idx.seen
      else
        idx.given <- sample(idx.seen, given)
      
      tester[,!(1:num.movies %in% idx.given)] <- NA
      tester <- as(tester, 'realRatingMatrix')
      
      pred <- predict(rec, tester, type = 'topNList', n = 50)
      
      recd <- as(pred, 'list')[[1]] # what did I recommend to you?
      not.recd <- setdiff(movie.names, recd)
      
      seen <- movie.names[idx.seen] # what did you actually watch?
      not.seen <- movie.names[-idx.seen]
      
      # To make the confusion matrix less...confusing...
      a <- length(intersect(not.seen, not.recd))
      b <- length(intersect(recd, not.seen))
      c <- length(intersect(not.seen, recd))
      d <- length(intersect(seen, recd))
      tpr <- d / (c + d)
      fpr <- 1 - (a / (a + b))
      
      return(list(tpr, fpr))
      
    })
    tpr <- mean(do.call('rbind', fold.acc[1,]), na.rm = T)
    fpr <- mean(do.call('rbind', fold.acc[2,]), na.rm = T)
    
    return(list(tpr, fpr))
  })
  cat('done.\n')
  
  tpr.hand <- mean(do.call('rbind', acc[1,]), na.rm = T)
  fpr.hand <- mean(do.call('rbind', acc[2,]), na.rm = T)
  
  tpr.auto <- res.user['50','TPR']
  fpr.auto <- res.user['50','FPR']
  
  # makes me think I am doing this correctly after all
  tpr.semi <- res.user['50', 'TP'] / (res.user['50', 'TP'] + res.user['50', 'FN'])
  fpr.semi <- res.user['50', 'FP'] / (res.user['50', 'FP'] + res.user['50', 'TN'])
}

# --------------------------------------------------------------------------- #
# Question 2 ---------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

popular100 <- names(tail(sort(users.per.movie), 100))

if (COMPUTE_OVERLAP) {
  overlap <- sapply(1:k, function(i) {
    cat(i, ',', sep='')
    
    idx.train <- sample(1:num.users, round(num.users * split.share))
    idx.test <- setdiff(1:num.users, idx.train)
    
    # Train the recommenders
    rec.user <- Recommender(MovieLense[idx.train,], method = 'UBCF')
    rec.item <- Recommender(MovieLense[idx.train,], method = 'IBCF')
    
    # Make predictions
    pred.user <- predict(rec.user, MovieLense[idx.test,], 
                         type = 'topNList', n = 100,
                         given = given, goodRating = goodRating)
    pred.item <- predict(rec.item, MovieLense[idx.test,], 
                         type = 'topNList', n = 100,
                         given = given, goodRating = goodRating)
    
    pred.user <- as(pred.user, 'list')
    pred.item <- as(pred.item, 'list')
    
    # Compare overlap
    overlap <- sapply(1:length(pred.user), function(j) {
      length(intersect(pred.user[[j]], pred.item[[j]]))
    })

    overlap.user <- sapply(1:length(pred.user), function(j) {
      length(intersect(pred.user[[j]], popular100))
    })
    
    overlap.item <- sapply(1:length(pred.item), function(j) {
      length(intersect(pred.item[[j]], popular100))
    })
    
    overlap.pop <- sapply(1:length(pred.user), function(j) {
      length(intersect(popular100, intersect(pred.user[[j]], pred.item[[j]])))
    })
    
    return(list(mean(overlap), mean(overlap.pop), 
                mean(overlap.user), mean(overlap.item)))
  })
  cat('done.\n')
  
  avg.overlap <- mean(do.call('rbind',overlap[1,]))
  avg.overlap.pop <- mean(do.call('rbind',overlap[2,]))
  avg.overlap.user <- mean(do.call('rbind',overlap[3,]))
  avg.overlap.item <- mean(do.call('rbind',overlap[4,]))
  
}
