# Standardized coefficient extraction
# 
# Extract model coefficients in a standardized format (as a named numeric vector).
#
# @section Warning:
# The Binomial models in glmboost return coefficients which are 1/2 the coefficients
# fit by a call to glm(..., family=binomial(...)), because the response variable is
# internally recoded to -1 and +1. sqlscore multiplies the returned coefficients by 2
# to put them back on the same scale as glm, and adds the glmboost offset to the
# intercept before multiplying.
# 
# @param object An object for which the extraction of model coefficients is meaningful.
# 
# @return Model coefficients as a named numeric vector.
extract_coef <-
function(object)
{
  UseMethod("extract_coef")
}

#' @export
extract_coef.default <-
function(object)
{
  stats::coef(object)
}

#' @export
extract_coef.glmboost <-
function(object)
{
  families <- c("Negative Binomial Likelihood -- probit Link",
                "Negative Binomial Likelihood", "Poisson Likelihood", 
                "Squared Error (Regression)", "Negative Gamma Likelihood")
  stopifnot(object$family@name %in% families)
  
  # suppress coef.glmboost's message so we can print our own
  cf <- captureConditions(stats::coef(object, off2int=TRUE))$value
  
  # mboost internally recodes binomial DVs to -1 and +1, so the coefficients
  # are half those returned by glm. If we have a binomial model, let's fix this
  # and return twice the fitted coefficients, with a message
  if(object$family@name %in% families[1:2])
  {
    message("\nNOTE: Coefficients from a glmboost Binomial model are 1/2 the ",
            "coefficients\nfrom a model fit by ", "glm(... , family = 'binomial').\n", 
            "sqlscore scales these coefficients by 2 to put them on the same scale as glm.\n")
    sc <- 2
  }
  else
  {
    sc <- 1 
  }
  
  sc * cf
}

#' @export
extract_coef.cv.glmnet <-
function(object)
{
  cf <- stats::coef(object)
  
  val <- as.vector(cf)
  names(val) <- rownames(cf)
  
  #Return only the coefficients that weren't regularized to 0
  val[which(val != 0)]
}
