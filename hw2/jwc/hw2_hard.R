rm(list=ls())

set.seed(0xDedBeef)

source('../utils/source_me.R', chdir = T)
CreateDefaultPlotOpts()

require(recommenderlab)
require(caret)

# Built-in data from recommenderlab
# 943 users (each row is a user)
# 1664 movies (each column is a movie)
data(MovieLense)
num.users <- nrow(MovieLense)
num.movies <- ncol(MovieLense)
movie.names <- colnames(MovieLense)

# K-fold cross validation -----------------------------------------------------

num.folds <- 5
fold.idx <- createFolds(1:num.users, k = num.folds)
rec.accuracy <- sapply(1:num.folds, function(num.fold) {
  cat(num.fold, ',', sep='')

  dat.train <- MovieLense[!(1:num.users %in% fold.idx[[num.fold]]), ]
  
  rec.user <- Recommender(dat.train, method = 'UBCF') 
  
  EvaluateFold <- function(dat, rec, num.fold, fold.idx, given, topN, good.rating) {
    fold.accuracy <- sapply(fold.idx[[num.fold]], function(user) {
      # my test user
      tester <- as(dat[user,], 'matrix')
      
      # index of all movies tester has seen and liked
      idx.seen <- which(tester >= good.rating)
      
      # index of random subset of movies tester has seen
      # must take min of given and # of seen movies
      idx.given <- idx.seen[sample(1:length(idx.seen), min(given, length(idx.seen)))]
      
      # only random subset should be rated
      tester[,!(1:num.movies %in% idx.given)] <- NA
      
      # convert tester back to realRatingsMatrix
      tester <- as(tester, 'realRatingMatrix')
      
      # now make recommendations
      pred <- predict(rec, tester, type = 'topNList', n = topN)
      
      # compare to seen movies
      recd <- as(pred, 'list')[[1]] # what did I recommend to you?
      seen <- movie.names[idx.seen] # what did you actually watch?
      
      acc <- calcPredictionAccuracy(pred, dat[user,], 
                                    goodRating = good.rating, 
                                    given = given)
      return(acc)
    })
    avg.fold.accuracy <- rowMeans(fold.accuracy)
    return(avg.fold.accuracy)  
  }
  
  fold.accuracy <- EvaluateFold(dat = MovieLense, 
                                rec = rec.user, 
                                num.fold = 1, 
                                fold.idx = fold.idx, 
                                given = 5, 
                                topN = 50, 
                                good.rating = 4)
  
  return(fold.accuracy)
})
cat('done.\n')

rec.accuracy <- rowMeans(rec.accuracy)