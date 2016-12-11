#Packages: MASS, arm, bridge, gam, glmboost, glmnet, h20, mgcv, ordinalNet, randomGLM

#' @rdname score_expression
#' @method score_expression bayesglm
#' @export
score_expression.bayesglm <-
function(mod, link=NULL)
{
  #These are also GLM objects; the fit is regularized but the
  #prediction step is the same
  
  NextMethod()
}

#' @rdname score_expression
#' @method score_expression cv.glmnet
#' @export
score_expression.cv.glmnet <-
function(mod, link=NULL)
{
  #FIXME
}