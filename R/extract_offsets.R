#' Standardized offset extraction
#' 
#' Extract model offsets (if present) in a standardized format. The return value is a numeric vector
#' with formula terms as names and values all 1.
#' 
#' @param object An object for which the extraction of model offsets is meaningful.
#' 
#' @return Model offsets as a numeric vector with all values 1 and formula terms as names.
#' 
#' @rdname extract_offsets
#' @export extract_offsets
extract_offsets <-
function(object)
{
  UseMethod("extract_offsets")
}

#' @rdname extract_offsets
#' @method extract_offsets default
#' @export
extract_offsets.default <-
function(object)
{
  if("formula" %in% ls(object))
  {
    pos <- attr(stats::terms(object$formula), "offset")
    
    if(!is.null(pos))
    {
      ret <- rep(1, length(pos))
      names(ret) <- all.vars(object$formula)[pos]
      
      return(ret)
    } else
    {
      return(c())
    }
  } else
  {
    return(c())
  }
}

#' @rdname extract_offsets
#' @method extract_offsets glmboost
#' @export
extract_offsets.glmboost <-
function(object)
{
  #mboost doesn't support this
  c()
}
