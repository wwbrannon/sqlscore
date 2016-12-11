# Standardized factor extraction
# 
# Extract factor variables expanded by a model object's constructor
#
# Extract the names of factor variables passed to a model's constructor, and return
# them as a character vector. This is useful for linpred() in identifying expanded
# dummies so that they can be turned into CASE expressions.
# 
# @param object An object for which the extraction of input factors is meaningful.
# 
# @return Names of model input factors as a character vector.
extract_factors <-
function(object)
{
  UseMethod("extract_factors")
}

#' @export
extract_factors.default <-
function(object)
{
  nm <- attr(stats::terms(object), "dataClasses")
  factors <- nm[which(nm %in% c("factor", "character"))]
  
  names(factors)
}

#' @export
extract_factors.glmboost <-
function(object)
{
  cl <- as.list(object$call)
  if("formula" %in% names(cl))
  {
    av <- all.vars(cl$formula)
    av <- av[2:length(av)] #discard the DV
    
    tp <- sapply(object$model.frame(), is.factor)
    tp <- names(tp[which(tp)])
    
    # it's poorly documented whether model.frame() includes
    # only vars used in the fit, so let's be safe
    return(intersect(tp, av))
  } else
  {
    # the fit was with x and y arrays, can't recover factors
    return(c())
  }
}

#' @export
extract_factors.cv.glmnet <-
function(object)
{
  # cv.glmnet's x argument has to be a pre-expanded matrix;
  # there's no way to recover any dummy expansion that was done,
  # so using this package with glmnet just requires that expansion
  # be done in the DB (and that the source table have columns for the
  # dummies)
  c()
}
