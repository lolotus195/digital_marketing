####
# Level Info --------------------------------------------------------------
####
all.levels <- list(V1=1:6,
                   V2=1:6,
                   V3=1:3,
                   V4=1:3,
                   V5=1:5,
                   V6=1:4,
                   V7=1:2,
                   V8=1:5,
                   V9=1:6)
exp.levels <- list(V1=c(4, 5, 6),
                   V2=c(1, 3, 6),
                   V3=1,
                   V4=c(1, 2, 3),
                   V5=c(1, 5),
                   V6=c(1, 3),
                   V7=c(1, 2),
                   V8=c(2, 3, 4, 5),
                   V9=c(2, 4, 6))

RelevelData <- function(df, level.list) {
  for (colname in names(level.list)) {
    df[,colname] <- factor(df[,colname], levels=level.list[[colname]])
  }
  return(df)
}

FinalModel <- function(data) {
  glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + V4 + V5 + V6 + V7 + I(V8 == 4) + 
        V9 + V1:V2, 
      data, family="binomial")
}
