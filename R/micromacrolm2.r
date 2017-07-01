#' Fitting Micro-Macro Multilevel Linear Models
#'
#' After computing the adjusted group means of individual-level predictors by \code{\link{adjusted.predictors}}, use \code{\link{micromacro.lm}} for estimation results and inferential statistics.
#' @param model a linear regression model formula, e.g., as.formula(y ~ x1 + x2 ... + xm).
#' @param adjusted.predictors a G-by-m data frame, where column variables are group-level predictors and the adjusted group means of individual-level predictors were computed by the \code{\link{adjusted.predictors}} function. G denotes the number of groups and m denotes the number of predictors in the model.
#' @param y an array or a G-by-1 numeric matrix that corresponds to the group-level outcome variable in the model.
#' @param unequal.groups an optional boolean variable automatically reported by the \code{\link{adjusted.predictors}} function. TRUE = group size is different across groups; FALSE = group size is the same across groups. Default is FALSE (same group size).
#' @return statistics a summary reports standard inferential statistics on linear regression,
#' e.g., "Estimate", coefficient estimates; "Uncorrected S.E."/"S.E.", OLS standard errors;
#' "Corrected S.E.", heteroskedasticity-consistent standard errors; "df", degree of freedom; "t", Student t statistics;
#' "Pr(>|t|)", two-sided p-value; "r", effect size.
#' @return rsquared r squared.
#' @return rsquared.adjusted adjusted r squared.
#' @return residuals residuals from the model.
#' @return fitted.values fitted values from the model.
#' @return fstatistic F statistics of the model.
#' @return model.formula model formula.
#' @keywords multi-level modeling, micro-macro, prediciting group-level variables from individual-level variables, different group sizes
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
#' @seealso \code{\link{adjusted.predictors}} for calculating the adjusted group means of the individual-level predictors, and \code{\link{micromacro.summary}} for a friendly output summary table.
#' @author Jackson G. Lu, Elizabeth Page-Gould, & Nancy R. Xu (maintainer, nancyranxu@gmail.com).
#' @references
#' Akinola, M., Page-Gould, E., Mehta, P. H., & Lu, J. G. (2016). Collective hormonal profiles predict group performance. Proceedings of the National Academy of Sciences, 113 (35), 9774-9779.
#'
#' Croon, M. A., & van Veldhoven, M. J. (2007). Predicting group-level outcome variables from variables measured at the individual level: A latent variable multilevel model. Psychological Methods, 12(1), 45-57.
#'
#' White, H. (1980). A heteroskedasticity-consistent covariance estimator and a direct test of heteroskedasticity. Econometrica, 48, 817-838.
#' @example R/Examples/micromacrolmExample2.r
#' @importFrom stats cov lm pf pt quantile sd terms.formula var
#' @export


micromacro.lm = function(model, adjusted.predictors, y, unequal.groups=NULL) {
  data = cbind(adjusted.predictors,y)
  uncorrected.output = lm( model, data)
  model.terms = attr(terms.formula(model), "term.labels")
  model.variables = model.terms[!grepl(":",model.terms)]
  interaction.terms = model.terms[grepl(":",model.terms)]
  if (length(interaction.terms)>0) {
    interactions = matrix( NA, nrow=dim(data)[1], ncol=length(interaction.terms) )
    for ( i in 1:length(interaction.terms) ) {
      interactions[,i] = apply( data[strsplit(interaction.terms,":")[[i]]], 1, prod )
    }
    u = cbind(rep(1,dim(data)[1]),as.matrix(cbind(data[model.variables], interactions)))
  } else {u = cbind(rep(1,dim(data)[1]),as.matrix(cbind(data[model.variables])))}
  e = uncorrected.output$residuals
  p = solve(t(u) %*% u)
  h = diag(u %*% p %*% t(u))
  d = e^2/(1-h)
  v = p %*% t(u) %*% diag(d) %*% u %*% p
  se = sqrt(diag(v))
  estim = summary(uncorrected.output)$coefficients[,1:2]
  t.stats = estim[,1]/se
  p.values = pt(-abs(t.stats),rep(uncorrected.output$df.residual,length(model.terms)+1))*2 # two-sided
  effect.sizes = sqrt( t.stats**2/(t.stats**2+uncorrected.output$df.residual) )
  estim1 = cbind(estim,se,rep(uncorrected.output$df.residual,length(model.terms)+1),t.stats,p.values,effect.sizes)
  dimnames(estim1)[[2]] = c("Estimate","Uncorrected S.E.","Corrected S.E.","df","t","Pr(>|t|)","r")
  # print( estim )
  if (is.null(unequal.groups)){ # default (null) is same group size
    t.stats2 = estim[,1]/estim[,2]
    p.values2 = pt(-abs(t.stats2),rep(uncorrected.output$df.residual,length(model.terms)+1))*2
    effect.sizes2 = sqrt( t.stats2**2/(t.stats2**2+uncorrected.output$df.residual) )
    estim2 = cbind(estim,rep(uncorrected.output$df.residual,length(model.terms)+1),t.stats2,p.values2,effect.sizes2)
    dimnames(estim2)[[2]] = c("Estimate","S.E.","df","t","Pr(>|t|)","r")
    report = list("statistics" = estim2, "rsquared" = summary(uncorrected.output)$r.squared, "rsquared.adjusted" = summary(uncorrected.output)$adj.r.squared, "residuals" = uncorrected.output$residuals, "fitted.values" = uncorrected.output$fitted.values, "fstatistic" = summary(uncorrected.output)$fstatistic, "model.formula" = model)
  } else if (isTRUE(unequal.groups)) { # true when group sizes are unequal
    report = list("statistics" = estim1, "rsquared" = summary(uncorrected.output)$r.squared, "rsquared.adjusted" = summary(uncorrected.output)$adj.r.squared, "residuals" = uncorrected.output$residuals, "fitted.values" = uncorrected.output$fitted.values, "fstatistic" = summary(uncorrected.output)$fstatistic, "model.formula" = model)
  } else { # true when group sizes are the same
    t.stats2 = estim[,1]/estim[,2]
    p.values2 = pt(-abs(t.stats2),rep(uncorrected.output$df.residual,length(model.terms)+1))*2
    effect.sizes2 = sqrt( t.stats2**2/(t.stats2**2+uncorrected.output$df.residual) )
    estim2 = cbind(estim,rep(uncorrected.output$df.residual,length(model.terms)+1),t.stats2,p.values2,effect.sizes2)
    dimnames(estim2)[[2]] = c("Estimate","S.E.","df","t","Pr(>|t|)","r")
    report = list("statistics" = estim2, "rsquared" = summary(uncorrected.output)$r.squared, "rsquared.adjusted" = summary(uncorrected.output)$adj.r.squared, "residuals" = uncorrected.output$residuals, "fitted.values" = uncorrected.output$fitted.values, "fstatistic" = summary(uncorrected.output)$fstatistic, "model.formula" = model)
  }
  return(report)
}
