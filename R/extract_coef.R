# Standardized coefficient extraction
# 
# Extract model coefficients in a standardized format (as a named numeric vector).
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
  #mboost's family objects are very hard to work with, so let's
  #just handle linear regression
  stopifnot(object$family@name == "Squared Error (Regression)")
  
  stats::coef(object, off2int=TRUE)
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
