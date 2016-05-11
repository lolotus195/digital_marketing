rm(list = ls())

dat <- read.csv('experiment_results.csv')

base.formula <- formula(~ I(V1 == 4) + I(V2 == 6) + I(V4 == 1) + 
                          I(V4 == 3) + I(V5 == 5) + I(V6 == 3) + I(V7 == 1) + I(V7 == 2) + 
                          I(V8 == 2) + I(V8 == 3) + I(V8 == 4) + I(V8 == 5) + I(V9 == 2) + 
                          I(V9 == 4) + I(V9 == 6) + I(V1 == 5):I(V2 == 3) + I(V2 == 3):I(V1 == 6))

form <- update(base.formula, cbind(Clicks, N - Clicks) ~ .)
reg <- glm(form, dat, family = 'binomial')
