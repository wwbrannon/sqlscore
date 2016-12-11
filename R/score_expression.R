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
#' 
#' @export
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

#' @rdname score_expression
#' @method score_expression glm
#' @export
score_expression.glm <-
function(mod, link=NULL)
{
  lp <- linpred(mod)
  lnk <- mod$family$link
  
  # The comments give L(eta), the inverse of the link function,
  # in clearer notation.
  if(lnk == "probit")
  {
    # L(eta) does not exist in closed form
    stop(paste0("Link function does not exist in closed form. Consider ",
                "using the link argument to score_expression to use a ",
                "custom sql function."))
  } else if(lnk == "cauchit")
  {
    # L(eta) does not exist in closed form
    stop(paste0("Link function does not exist in closed form. Consider ",
                "using the link argument to score_expression to use a ",
                "custom sql function."))
  } else if(lnk == "identity")
  {
    # L(eta) = eta
    return(lp)
  } else if(lnk == "log")
  {
    # L(eta) = exp(eta)
    return(as.call(list(as.symbol("exp"), lp)))
  } else if(lnk == "sqrt")
  {
    # L(eta) = eta^2
    return(as.call(list(as.symbol("^"), lp, 2)))
  } else if(lnk == "1/mu^2")
  {
    # L(eta) = 1/sqrt(eta)
    e1 <- as.call(list(as.symbol("sqrt"), lp))
    return(as.call(list(as.symbol("/"), 1, e1)))
  } else if(lnk == "inverse")
  {
    # L(eta) = 1/eta
    return(as.call(list(as.symbol("/"), 1, lp)))
  } else if(lnk == "logit")
  {
    # L(eta) = 1/(1+exp(-eta))
    e1 <- as.call(list(as.symbol("*"), -1, lp))
    e2 <- as.call(list(as.symbol("exp"), e1))
    e3 <- as.call(list(as.symbol("+"), 1, e2))
    return(as.call(list(as.symbol("/"), 1, e3)))
  } else if(lnk == "cloglog")
  {
    # L(eta) = 1 - exp(-exp(eta))
    e1 <- as.call(list(as.symbol("exp"), lp))
    e2 <- as.call(list(as.symbol("-"), e1))
    e3 <- as.call(list(as.symbol("exp"), e2))
    return(as.call(list(as.symbol("-"), 1, e3)))
  } else
  {
    stop(paste0("Unrecognized link function. Hint: try the link argument ",
                "to score_expression to specify the name of a custom or ",
                "DB-specific SQL function."))
  }
}

#' @rdname score_expression
#' @method score_expression lm
#' @export
score_expression.lm <-
function(mod, link=NULL)
{
  #the only possible link function for this object is the identity link
  return(linpred(mod))
}

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
#' @method score_expression glmboost
#' @export
score_expression.glmboost <-
function(mod, link=NULL)
{
  #mboost's family objects are very hard to work with, so let's
  #just handle linear regression
  stopifnot(mod$family@name == "Squared Error (Regression)")
  
  return(linpred(mod))
}

#' @rdname score_expression
#' @method score_expression cv.glmnet
#' @export
score_expression.cv.glmnet <-
function(mod, link=NULL)
{
  lp <- linpred(mod)
  cls <- setdiff(class(mod$glmnet.fit), c("glmnet"))
  
  if(cls == "elnet") # family = gaussian
  {
    # L(eta) = eta
    return(lp)
  } else if(cls == "fishnet") # family = poisson
  {
    # L(eta) = exp(eta)
    return(as.call(list(as.symbol("exp"), lp)))
  } else if(cls == "lognet") # family = binomial
  {
    # L(eta) = 1/(1+exp(-eta))
    e1 <- as.call(list(as.symbol("*"), -1, lp))
    e2 <- as.call(list(as.symbol("exp"), e1))
    e3 <- as.call(list(as.symbol("+"), 1, e2))
    return(as.call(list(as.symbol("/"), 1, e3)))
  } else
  {
    stop("Unsupported model family for cv.glmnet")
  }
}
