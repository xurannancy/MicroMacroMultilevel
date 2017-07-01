#' Calculating the Adjusted Group Means of Individual-Level Variables in a Micro-Macro Multilevel Situation
#'
#' As the prerequisite step of fitting a micro-macro multilevel model, this function calculates the adjusted group means of individual-level predictors in an unbiased way.
#'
#' @param x.data an N-by-p data frame of individual-level predictors, where N denotes the total number of individuals and p denotes the number of individual-level predictors. Must contain no NAs.
#' @param z.data a G-by-q data frame of group-level predictors, where G denotes the total number of groups and q denotes the number of group-level predictors. Must contain no NAs.
#' @param x.gid an array or an N-by-1 numeric matrix of each individual's group ID. The order corresponds to the individuals in x.data. Duplicates expected.
#' @param z.gid an array or a G-by-1 numeric matrix of Group ID. The order corresponds to the groups in z.data. All group IDs should be unique (i.e., no duplicates allowed).
#' @return adjusted.group.means a G-by-(p+q+1) numeric matrix that contains p adjusted group means of the individual-level variables from x.data, q group-level predictors from z.data, and unique group IDs.
#' @return unequal.groups a boolean variable. TRUE = group size is different across groups; FALSE = group size is the same across groups.
#' @return group.size a G-by-2 data frame that displays unique group IDs and the corresponding group sizes.
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
#' Importantly, \code{\link{adjusted.predictors}} also reports whether group size is the same across all groups, which is a critical dummy input of the \code{\link{micromacro.lm}} function.
#'
#' @seealso \code{\link{micromacro.lm}} for fitting the micro-macro multilevel linear model of interest.
#' @author Jackson G. Lu, Elizabeth Page-Gould, Nancy R. Xu (maintainer, nancyranxu@gmail.com).
#' @references
#' Akinola, M., Page-Gould, E., Mehta, P. H., & Lu, J. G. (2016). Collective hormonal profiles predict group performance. Proceedings of the National Academy of Sciences, 113 (35), 9774-9779.
#'
#' Croon, M. A., & van Veldhoven, M. J. (2007). Predicting group-level outcome variables from variables measured at the individual level: A latent variable multilevel model. Psychological Methods, 12(1), 45-57.
#' @example R/Examples/adjustedpredictorsExample2.r
#' @importFrom stats cov lm pf pt quantile sd terms.formula var
#' @export

adjusted.predictors = function( x.data, z.data , x.gid, z.gid) {
  x.data = as.data.frame(x.data)
  z.data = as.data.frame(z.data)
  # sort individual-level data based on group ID
  x = x.data[order(x.gid),]
  # sort group-level data
  z = z.data[order(z.gid),]
  # number of individuals per group
  ng = as.numeric(unlist(tapply(rep(1,dim(x)[1]),x.gid,sum)))
  #1a: Parameters pertaining to distribution of Z
  mu_z = apply( z, 2, mean ) # 1-by-q
  sigma_zz = var( z ) # q-by-q
  #1b: matrix of observed means
  mu_x = apply( x, 2, mean ) # 1-by-p
  mu_xg = apply( x, 2, function(x) {tapply(x, x.gid, mean)} ) # G by p, mean by group, the unadjusted mean
  gid = x.gid
  x.data2 = cbind(x, gid)
  gid = z.gid
  mu_xg2 = cbind(mu_xg, gid)
  x.data.means = merge( x.data2, mu_xg2, by="gid" ) # merge group means with individual level data
  #1c: Covariance matrix of Z and X
  sigma_xiz = cov( mu_xg, z ) # p by q, between-group covariance
  sigma_zxi = t( sigma_xiz ) # q by p, transpose
  #1d: Between- and within-group covariance matrices
  SSA = sum(ng)*apply(mu_xg,1,function(x){x-mu_x})%*%t(apply(mu_xg,1,function(x){x-mu_x})) # p by p
  MSA = SSA/(dim(x)[1]-dim(z)[1]) # p by p
  SSE = matrix( 0, nrow=dim(x)[2], ncol=dim(x)[2]) # p by p
  for (k in 1:(dim(x.data.means)[1])) {
    increment = (dim(x.data.means)[2]-1)/2 # should be the same as the unique individual-level variables
    SSE_i = as.numeric( unlist( x.data.means[k,2:(dim(x.data.means)[2]-increment)] - x.data.means[k,(dim(x.data.means)[2]-increment+1):dim(x.data.means)[2]] ) )%*%t(as.numeric( unlist( x.data.means[k,2:(dim(x.data.means)[2]-increment)] - x.data.means[k,(dim(x.data.means)[2]-increment+1):dim(x.data.means)[2]] ) ))
    SSE = SSE + SSE_i
  }
  MSE = SSE/( dim(z)[1] - 1 )
  sigma_vv = MSE # p by p
  sigma_xixi = ((dim(x)[1]*(dim(z)[1] - 1))/(dim(x)[1]**2-sum(ng**2)))*(MSA-MSE) # p by p
  #2: Adjusted group means
  adjusted.means = matrix( NA, nrow=dim(z)[1], ncol=dim(x)[2] ) # G by p
  for (g in 1:dim(z)[1]) {
    if (dim(z)[2]>0) {
      W_g1 = solve(sigma_xixi + sigma_vv/as.numeric(ng)[g] + sigma_xiz%*%solve(sigma_zz)%*%sigma_zxi)%*%(sigma_xixi + sigma_xiz%*%solve(sigma_zz)%*%sigma_zxi)
      W_g2 = solve(sigma_zz)%*%sigma_zxi%*%(diag(dim(x)[2])-W_g1)
      if (dim(as.matrix(z[g,]-mu_z,nrow=1))[1] == 1){
        addx = as.matrix(z[g,]-mu_z,nrow=1)
      } else {
        addx = t(as.matrix(z[g,]-mu_z,nrow=1))
      }
      adjusted.means[g,] = t(mu_x)%*%(diag(dim(x)[2])-W_g1)+t(mu_xg[g,])%*%W_g1+addx%*%W_g2
    } else {
      W_g1 = solve(sigma_xixi + sigma_vv/as.numeric(ng)[g])%*%(sigma_xixi)
      adjusted.means[g,] = t(mu_x)%*%(diag(dim(x)[2])-W_g1)+t(mu_xg[g,])%*%W_g1
    }
  }
  adjusted.group.means = as.data.frame( cbind( adjusted.means, z, z.gid) )
  names(adjusted.group.means) = c( paste( "BLUP.", names(x), sep="" ), names(z), "gid")

  # assign boolean variable to unequal.groups which returns TRUE when groups sizes are different, FALSE otherwise
  unequal.groups = isTRUE(min(ng) != max(ng))

  group.size = as.data.frame(cbind(z.gid, ng))
  report =  list("unequal.groups" = unequal.groups, "adjusted.group.means" = adjusted.group.means, "group.size" = group.size)
  return(report)
}




