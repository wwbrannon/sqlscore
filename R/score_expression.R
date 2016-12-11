#' Unevaluated prediction expressions for models
#' 
#' Generate an unevaluated call corresponding to the predict step of the passed
#' model. The call represents the link function of the linear predictor in terms
#' of elementary functions on the underlying column names, and is suitable for
#' direct translation into SQL.
#' 
#' @param mod A model object providing a coef() method.
#' @param link The name of a custom link function to apply to the linear predictor.
#' 
#' @return An unevaluated R call object representing the link-inverse function of the linear predictor.
#' 
#' @rdname score_expression
#' @export score_expression
score_expression <-
function(mod, link=NULL)
{
  #If we've been requested to use a particular link function by name,
  #generate a call to it and return that. This lets you use a
  #custom sql function or DB features (e.g., probit) that a) don't
  #have portable sql names, and that b) we can't express in closed form
  #in terms of elementary functions.
  if(!is.null(link))
  {
    lp <- linpred(mod)
    return(as.call(list(as.symbol(link), lp)))
  }
  
  #Otherwise, let's figure out what the link should be. If it
  #should be something we can't generate in closed form, stop
  #and suggest using a sql function and the link argument.
  UseMethod("score_expression")
}

#' @return None
#' 
#' @rdname score_expression
#' @method score_expression default
#' @export
score_expression.default <-
function(mod, link=NULL)
{
  stop(paste0("Don't know how to handle object of class ", class(mod),
              ". Consider using the link argument to score_expression ",
              "to set the link function by hand."))
}
