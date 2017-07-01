#' Summarizing the Micro-Macro Multilevel Linear Model Fitting Results
#'
#' After fitting a micro-macro multilevel model, this function produces a user-friendly summary table of the results.
#'
#' @param model.output the output of \code{\link{micromacro.lm}} which contains model results and model specifications.
#' @return table a summary table.
#' @keywords multilevel modeling, micro-macro, using individual-level predictors to predict group-level outcome variables
#' @details
#'
#' To date, most multilevel methodologies can only unbiasedly model macro-micro multilevel situations, wherein group-level predictors (e.g., city temperature)
#' are used to predict an individual-level outcome variable (e.g., citizen personality). In contrast, this R package enables researchers to model micro-macro situations,
#' wherein individual-level (micro) predictors (and other group-level predictors) are used to predict a group-level (macro) outcome variable in an unbiased way.
#'
#' To conduct micro-macro multilevel modeling with the current package, one must first compute the adjusted group means with the function \code{\link{adjusted.predictors}}.
#' This is because in micro-macro multilevel modeling, it is statistically biased to directly regress the group-level outcome variable on the unadjusted group means of individual-level predictors (Croon & van Veldhoven, 2007).
#' Instead, one should use the best linear unbiased predictors (BLUP) of the group means (i.e., the adjusted group means), which is conveniently computed by \code{\link{adjusted.predictors}}.
#'
#' Once produced by \code{\link{adjusted.predictors}}, the adjusted group means can be used as one of the inputs of the \code{\link{micromacro.lm}} function, which reports estimation results and inferential statistics of the micro-macro multilevel model of interest.
#'
#' If group size is the same across all groups (i.e., unequal.groups = FALSE), then OLS standard errors are reported and used to determine the inferential statistics in this micro-macro model.
#' If group size is different across groups (i.e., unequal.groups = TRUE), however, then the heteroscedasticity-consistent standard errors are reported and used determine the inferential statistics in this micro-macro model (White, 1980).
#'
#' \code{\link{micromacro.summary}} produces a detailed summary on the model fitting and specifications, given the outputs of \code{\link{micromacro.lm}}.
#'
#' @seealso \code{\link{micromacro.lm}} for fitting the micro-macro multilevel linear model of interest.
#' @author Jackson G. Lu, Elizabeth Page-Gould, Nancy R. Xu (maintainer, nancyranxu@gmail.com).
#' @references
#' Akinola, M., Page-Gould, E., Mehta, P. H., & Lu, J. G. (2016). Collective hormonal profiles predict group performance. Proceedings of the National Academy of Sciences, 113 (35), 9774-9779.
#'
#' Croon, M. A., & van Veldhoven, M. J. (2007). Predicting group-level outcome variables from variables measured at the individual level: a latent variable multilevel model. Psychological methods, 12(1), 45-57.
#' @example R/Examples/micromacrolmExample2.r
#' @importFrom stats cov lm pf pt quantile sd terms.formula var
#' @export



micromacro.summary = function(model.output) {
  cat(noquote("Call:"))
  cat("\n")
  cat(noquote(sprintf("micromacro.lm( %s %s %s, ...)", gettext(model.output$model.formula)[2], gettext(model.output$model.formula)[1], gettext(model.output$model.formula)[3])))
  cat("\n")
  cat(noquote("    "))
  cat("\n")
  # summarize the residuals
  cat(noquote("Residuals:"))
  cat("\n")
  residual.summary = cbind(min(model.output$residuals), quantile(model.output$residuals, 0.25), quantile(model.output$residuals, 0.5), quantile(model.output$residuals, 0.75), max(model.output$residuals))
  dimnames(residual.summary)[[1]] = ""
  dimnames(residual.summary)[[2]] = c("Min","1Q","Median","3Q","Max")
  print(noquote(residual.summary))
  cat("\n")
  cat(noquote("    "))
  cat("\n")
  # summarize the coefficients
  cat(noquote("Coefficients:"))
  cat("\n")
  print(noquote(model.output$statistics))
  cat("\n")
  cat(noquote("---"))
  cat("\n")
  cat(noquote(sprintf("Residual standard error: %s on %s degrees of freedom", round(sd(model.output$residuals), digits = 5), length(model.output$residuals)-dim(model.output$statistics)[1])))
  cat("\n")
  cat(noquote(sprintf("Multiple R-squared: %s, Adjusted R-squared: %s", round(model.output$rsquared, digits = 10), round(model.output$rsquared.adjusted, digits = 10))))
  cat("\n")
  cat(noquote(sprintf("F-statistic: %s on %s and %s DF, p-value: %s", round(model.output$fstatistic[1], digits = 5), model.output$fstatistic[2], model.output$fstatistic[3], round(pf(model.output$fstatistic[1], model.output$fstatistic[2], model.output$fstatistic[3], lower.tail=FALSE), digits = 5))))
}
