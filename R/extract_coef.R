#' Standardized coefficient extraction
#' 
#' Extract model coefficients in a standardized format (as a named numeric vector).
#' 
#' @param object An object for which the extraction of model coefficients is meaningful.
#' 
#' @return Model coefficients as a named numeric vector.
#' 
#' @rdname extract_coef
#' @export extract_coef
extract_coef <-
function(object)
{
  UseMethod("extract_coef")
}

#' @rdname extract_coef
#' @method extract_coef default
#' @export
extract_coef.default <-
function(object)
{
  stats::coef(object)
}

#' @rdname extract_coef
#' @method extract_coef glmboost
#' @export
extract_coef.glmboost <-
function(object)
{
  #mboost's family objects are very hard to work with, so let's
  #just handle linear regression
  stopifnot(object$family@name == "Squared Error (Regression)")
  
  stats::coef(object, off2int=TRUE)
}
