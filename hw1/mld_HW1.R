# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Group Homework #1


####
# Setup and Load Data----
####
rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

load("target.rdat")
load("cust.rdat")
load("target2.rdat")
load("cust2.rdat")


####
# Determine Significant Coefs ----
####
require(gamlr)

GetSignificantCoefs <- function(data, criteria) {
  # This function just runs a gamma lasso and then extracts the 
  # variable-names from the model matrix that are significant.
  mm <- model.matrix(spend ~ . - machine_id + 0, data=data)
  mdl <- gamlr(mm, data$spend, family='gaussian')
  
  g <- PlotICs(mdl, criteria, plot.smooth = F)

  IC = switch(criteria, 
              BIC=BIC(mdl),
              AIC=AIC(mdl),
              AICc=AICc(mdl))
  cfs <- coef(mdl, select=which.min(IC))
  # Drop the intercept and find out which coefs are significantly different
  # from zero. Because we used gamlr, any non-zero coefficient will be
  # significant.  Two sets of [-1s] to ensure that we're selecting the
  # correct coefficient name.
  coefs <- names(drop(cfs)[-1][which(cfs[-1] > 0)])
  return(list(icPlot=g, coefs=coefs))
}

PrintSignifCoefs <- function(name, signif) {
  print(sprintf("'%s' has %d significant coefs: %s", 
                name, length(signif[["coefs"]]), 
                paste(signif[["coefs"]], collapse = ", ")))
}

# Original Customer/Target List
cust.signif <- GetSignificantCoefs(cust, "BIC")
GGPlotSave(cust.signif$icPlot, "cust_signif")
PrintSignifCoefs("cust", cust.signif)

cust.signif.AIC <- GetSignificantCoefs(cust, "AIC")
GGPlotSave(cust.signif.AIC$icPlot, "cust_signif")
PrintSignifCoefs("cust-AIC", cust.signif.AIC)

# New Customer/Target List
cust2.signif <- GetSignificantCoefs(cust2, "BIC")
GGPlotSave(cust2.signif$icPlot, "cust2_signif")
PrintSignifCoefs("cust2-BIC", cust2.signif)

cust2.signif.AIC <- GetSignificantCoefs(cust2, "AIC")
GGPlotSave(cust2.signif.AIC$icPlot, "cust2_signif_AIC")
PrintSignifCoefs("cust2-AIC", cust2.signif.AIC)


####
# Export Significant Coefficient ----
####
CombineCoefs <- function(lst) {
  n <- 0
  for (i in 1:length(lst)) {
    n <- max(n, length(lst[[i]]$coefs))
  }
  m <- matrix(nrow=n, ncol=length(lst))
  for (i in 1:length(lst)) {
    coefs <- lst[[i]]$coefs
    m[1:length(coefs),i] <- coefs
  }
  df <- as.data.frame(m)
  colnames(df) <- names(lst)
  return(df)
}
all.coefs <- CombineCoefs(list(
  "No ecom\\_index (BIC)" = cust.signif, 
  "With ecom\\_index (Manual)" = list(coefs=c(
    "hoh_oldest_age2", "hoh_oldest_age7", "ecom_index")),
  "With ecom\\_index (BIC)" = cust2.signif,
  "With ecom\\_index (AIC)" = cust2.signif.AIC))
all.coefs <- apply(all.coefs, 2, function(x) gsub("_", "\\\\_", x))
ExportTable(all.coefs, "series_coefs", "Significant Coefficients by Series",
            NA.string="",
            align.cols=TableAlignMultilineCenteredCM(c(0.5, rep(3.5, 4))),
            include.rownames = F)


####
# KNN using significant coefs ----
####
CalcProfits <- function(cust.data, target.data, 
                        unit_cost, signif.coefs,
                        spend_thresh=10, max_nn=25, verb=TRUE) {
  # First get the indices of the real customers.
  cst.real <- cust.data[cust.data$spend > spend_thresh,]
  
  # Then create model matrices for both the customers and the target data
  cst.mm <- model.matrix(~ . + 0, cst.real)[,signif.coefs]
  tgt.mm <- model.matrix(~ . + 0, target.data)[,signif.coefs]

  require(plyr)
  require(FNN)
  results <- ldply(1:max_nn, function(k) {
    if (verb==TRUE) cat(sprintf("%d,", k))
    
    # Get the k nearest target customers that are neighbors with current ones.
    query <- get.knnx(tgt.mm, cst.mm, k=k, algorithm="brute")
    
    # Uniquify the results.
    nn <- unique(as.vector(query$nn.index))
    
    # Calculate revenue and cost.
    revenue <- sum(target.data[nn, "spend"])
    cost <- unit_cost * length(nn)
    
    # Calculate other metrics.
    # (# we thought we'd get)/(# we'd get if we targeted all)
    frac.customers.captured =
      sum(target.data[nn, "spend"] > 0) / # num of customers that we thought would spend
      sum(target.data$spend > 0)          # num of customers that actually spent
    
    frac.revenue.captured =
      sum(target.data[nn, "spend"]) / sum(target.data$spend)
    
    # (# of customers we actually got) / (# we thought we'd get)
    frac.matches = mean(target.data[nn, "spend"] > 0)
    
    return(data.frame(k, revenue, unique.neighbors=length(nn), 
                      cost, profit=revenue-cost,
                      frac.customers.captured,
                      frac.revenue.captured,
                      frac.matches))
  })
  if (verb == TRUE) cat("done.\n")
  
  return(results)  
}

# A new DMP claims that hey have a better set of variables
# to match your customers on. 
# In particular, they have an ecom_index which they claim 
# Offers an inrementally better match for you.
# Unfortunately the cost of matching customers via them is 
# significantly higher!
cost_orig <- 0.5
cost_new <- 3.25

res.orig.gamlr <- CalcProfits(cust, target, cost_orig, cust.signif[["coefs"]])

# @k=3, revenue captured should be 0.5588503 (from lecture).
res.orig.class <- CalcProfits(cust, target, cost_orig, c("retail_index", 
                                                         "household_income6", 
                                                         "household_size6"))

res.ecom.gamlr <- CalcProfits(cust2, target2, cost_new, cust2.signif[["coefs"]])


####
# Customer Matching Threshold Search ----
####
# Cutoff Search function, applies successive cutoff values to 
# calc.profit.cb cutoff values.
CutoffSearch <- function(cust.data, target.data, 
                         unit_cost, signif.coefs, try.spend, try.label) {
  res <- ldply(1:length(try.spend), function(idx) {
    x <- try.spend[idx]
    lab <- try.label[idx]
    
    cat(sprintf("%3.2f,", x))
    cbind(thresh=x, label=lab,
          CalcProfits(cust.data, target.data, 
                      unit_cost, signif.coefs, x, verb=F))
  })
  cat("done\n")
  return(res)
}

# Use the quantiles to define the threshold search space.
try.spend <- quantile(cust2$spend[cust2$spend>0], 
                      probs=seq(0.025, 0.975, 0.025))

# Search over Will's explanatory variables.
res.manual.search <- CutoffSearch(cust2, target2, cost_new,
                                  c("hoh_oldest_age2", "hoh_oldest_age7", 
                                    "ecom_index"),
                                  unname(try.spend), names(try.spend))

res.orig.search <- CutoffSearch(cust, target, cost_orig,
                                cust.signif.AIC[["coefs"]],
                                unname(try.spend), names(try.spend))

# res.class.search <- CutoffSearch(cust, target, cost_orig,
#                                  c("retail_index", "household_income6",
#                                    "household_size6"),
#                                  unname(try.spend), names(try.spend))

# Search over gamlr's explanatory variables.
res.gamlr.aic.search <- CutoffSearch(cust2, target2, cost_new, 
                                     cust2.signif.AIC[["coefs"]],
                                     unname(try.spend), names(try.spend))

res.gamlr.bic.search <- CutoffSearch(cust2, target2, cost_new, 
                                     cust2.signif[["coefs"]], 
                                     unname(try.spend), names(try.spend))


####
# Export Best Choice Table ----
####
res.search <- rbind(
  # cbind(series="No Ecom", res.class.search),
  cbind(series="Orig. (AIC)", res.orig.search),
  cbind(series="Manual", res.manual.search),
  cbind(series="AIC", res.gamlr.aic.search),
  cbind(series="BIC", res.gamlr.bic.search))

require(dplyr)
res.search %>%
  group_by(series) %>%
  filter(profit == max(profit)) %>%
  select(series, thresh, k, profit, 
         frac.customers.captured,
         frac.revenue.captured, frac.matches) %>%
  mutate(frac.customers.captured = frac.customers.captured * 100,
         frac.revenue.captured = frac.revenue.captured * 100,
         frac.matches = frac.matches * 100) -> res.search.best

ExportTable(res.search.best, "series_max",
            "Best Matching Threshold by Series",
            colnames = c("Series", "Threshold [\\$]",
                         "Best k-NNs", "Profit [\\$]",
                         "\\% Cust. Captured", "\\% Revenue Captured",
                         "\\% Cust. Matched"),
            include.rownames = F,
            align.cols = TableAlignMultilineCenteredCM(c(0, 2.5, 2.0, 1.5, rep(2, 4))))


####
# Plot Series' Results ----
####
res.search %>%
  group_by(series, thresh) %>%
  filter(profit==max(profit)) -> res.search.plot

g <- ggplot(res.search.plot, aes(x=thresh, y=profit, color=series)) + 
  geom_line() + geom_point() + 
  scale_color_discrete("Series") +
  theme_bw() + labs(x="Spend Matching Threshold [$]", y="Max Profit")
GGPlotSave(g, "threshold_max_profit")

res.search.plot.melt <- melt(res.search.plot, 
                             id.vars = c("series", "thresh"), 
                             measure.vars=c("profit", "k"))
measure_to_label_map <- c(
  "profit" = "Profit [$] vs. Matching Threshold",
  "k" = "Best k for kNN vs. Matching Threshold"
)
g <- ggplot(res.search.plot.melt, aes(x=thresh, y=value, color=series)) +
  geom_line() + geom_point() +
  scale_color_discrete("Series") +
  facet_wrap("variable", scales = "free_y", ncol = 1,
             labeller = labeller("variable"=measure_to_label_map)) +
  theme_bw() + labs(x="Spend Matching Threshold [$]", y="")
GGPlotSave(g, "threshold_max_profit2")


####
# Save best result for plotting ----
####
res.search %>% filter(profit==max(profit)) -> res.best.info
if (nrow(res.best.info) != 1) stop("non-unique or missing best profit")

res.search %>% filter(series==res.best.info$series,
                      thresh==res.best.info$thresh) -> res.ecom.search

####
# Plot Profits ----
####
res.combined <- rbind(
  cbind(res.orig.gamlr, series="lasso_no_ecom"),
  cbind(res.orig.class, series="class_no_ecom"),
  cbind(res.ecom.gamlr, series="lasso_ecom"),
  cbind(res.ecom.search[-(1:3)], series="search_ecom"))

series_to_legend_map <- c(
  "lasso_no_ecom" = "No ecom_index\n(BIC, spend>=$10)",
  "class_no_ecom" = "No ecom_index\n(In-Class, spend>=$10)",
  "lasso_ecom" = "With ecom_index\n(BIC, spend>=$10)",
  "search_ecom" = sprintf("With ecom_index\n(%s, spend>=$%3.2f)",
                          res.ecom.search$series[1], 
                          res.ecom.search$thresh[1])
)

# Extract the measures we care about for the facet-wrap.
res.melt <- melt(res.combined, id.vars = c("k", "series"), 
                 measure.vars = c("profit",
                                  "frac.customers.captured",
                                  "frac.revenue.captured",
                                  "frac.matches"))
measure_to_label_map = c(
  "profit"="Profit [$]",
  "frac.customers.captured"="Frac. of Customers Captured",
  "frac.revenue.captured"="Frac. of Revenue Captured",
  "frac.matches"="Frac. of Customers Matched") # I don't like this title.

g <- ggplot(res.melt, aes(k, value, color=series)) + 
  geom_line() + geom_point() + 
  scale_color_discrete("Series", labels=series_to_legend_map) +
  facet_wrap("variable", scales="free_y", ncol=2, 
             labeller = labeller("variable"=measure_to_label_map)) +
  theme_bw() + labs(x="# of Nearest Neighbors Included", y="") +
  theme(legend.position = "bottom")
GGPlotSave(g, "profits")


# They have given you a sample target dataset 
# and a matched version of your customer dataset
# These are in cust2.rdat and target2.rdat
# Hint: Careful the ecom_index is in the 12th column

# Use this new data to answer the following questions.
# 1. Would you go with the new DMP? Justify your answer.
# 2. Is there any circomstance when you would ignore the 
#    matching and go after the entire target file audience? Justify.

