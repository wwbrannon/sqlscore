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
