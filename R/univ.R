##' Description
##'
##' Perform Univariate Linear Regression on a dataset
##' @title ULM with Significance
##' @param data is dataset
##' @param out is response variable
##' @param alpha is alpha level for significance test
##' @return dataframe of predictors with p-values and
##' significance, smallest p-value (most significant), and
##' scatterplot of response variable and most significant
##' predictor in univariate linear regression model
##' @author Emily King
##' @export
##' @example univariate(data = mtcars, out = "mpg", alpha = 0.05)
##'
univariate <- function(data, out, alpha){
  allvars <- names(data)
  predictors <- setdiff(allvars, out)
  pvals <- numeric(length(predictors))
  names(pvals) <- predictors
  for(a in predictors){
    alm <- lm(data[[out]]~data[[a]])
    asumm <- summary(alm)
    pvals[a] <- asumm$coefficients["data[[a]]", "Pr(>|t|)"]
  }
  col_pvalue0 <- signif(pvals,3)
  col_pvalue <- paste0(col_pvalue0,ifelse(pvals<alpha, "*",""))
  final <- data.frame(Predictor = predictors, P_Values = col_pvalue)
  print(final)
  min <-min(names(pvals))
  minp <- paste0("Predictor with Smallest P-Value: ", min)
  print(minp)
  plot(x = data[[min]], y = data[[out]], main = "Outcome by Most Significant Predictor")
}
