#' Unevaluated prediction expressions for models
#' 
#' Generate an unevaluated call corresponding to the predict step of the passed
#' model. The call represents the response function of the linear predictor in terms
#' of elementary functions on the underlying column names, and is suitable for
#' direct translation into SQL.
#' 
#' @param mod A supported model object.
#' @param response The name of a custom response function to apply to the linear predictor.
#' 
#' @return An unevaluated R call object representing the response function of the linear predictor.
#' 
#' @rdname score_expression
#' 
#' @examples
#' # A Gaussian GLM including factors
#' mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris)
#' score_expression(mod)
#' 
#' # A binomial GLM - linear predictor is unaffected
#' mod <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris, family=binomial("logit"))
#' score_expression(mod)
#' 
#' #With a hand-specified response function
#' score_expression(mod, response="probit")
#' 
#' #With formula operators
#' x <- matrix(rnorm(100*20),100,20)
#' colnames(x) <- sapply(1:20, function(x) paste0("X", as.character(x)))
#' x <- as.data.frame(x)
#' mod <- glm(X2 ~ X3 + X5 + X15*X8, data=x)
#' score_expression(mod)
#' 
#' @export
score_expression <-
function(mod, response=NULL)
{
  #If we've been requested to use a particular response function by name,
  #generate a call to it and return that. This lets you use a
  #custom sql function or DB features (e.g., probit) that a) don't
  #have portable sql names, and that b) we can't express in closed form
  #in terms of elementary functions.
  if(!is.null(response))
  {
    lp <- linpred(mod)
    return(as.call(list(as.symbol(response), lp)))
  }
  
  #Otherwise, let's figure out what the response should be. If it
  #should be something we can't generate in closed form, stop
  #and suggest using a sql function and the response argument.
  UseMethod("score_expression")
}

#' @return None
#' 
#' @rdname score_expression
#' @method score_expression default
#' @export
score_expression.default <-
function(mod, response=NULL)
{
  stop(paste0("Don't know how to handle object of class ", class(mod),
              ". Consider using the response argument to score_expression ",
              "to set the response function by hand."))
}

#' @rdname score_expression
#' @method score_expression glm
#' @export
score_expression.glm <-
function(mod, response=NULL)
{
  lp <- linpred(mod)
  lnk <- mod$family$link
  
  # The comments give L(eta), the response / inverse of the link function,
  # in clearer notation.
  if(lnk == "probit")
  {
    # L(eta) does not exist in closed form
    stop(paste0("Response function does not exist in closed form. Consider ",
                "using the response argument to score_expression to use a ",
                "custom sql function."))
  } else if(lnk == "cauchit")
  {
    # L(eta) does not exist in closed form
    stop(paste0("Response function does not exist in closed form. Consider ",
                "using the response argument to score_expression to use a ",
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
    stop(paste0("Unrecognized response function. Hint: try the response argument ",
                "to score_expression to specify the name of a custom or ",
                "DB-specific SQL function."))
  }
}

#' @rdname score_expression
#' @method score_expression lm
#' @export
score_expression.lm <-
function(mod, response=NULL)
{
  #the only possible response function for this object is the identity function
  return(linpred(mod))
}

#' @rdname score_expression
#' @method score_expression bayesglm
#' @export
score_expression.bayesglm <-
function(mod, response=NULL)
{
  #These are also GLM objects; the fit is regularized but the
  #prediction step is the same
  
  NextMethod()
}

#' @rdname score_expression
#' @method score_expression glmboost
#' @export
score_expression.glmboost <-
function(mod, response=NULL)
{
  #mboost's family objects are very hard to work with, so let's
  #just handle linear regression, where the response is the identity
  stopifnot(mod$family@name == "Squared Error (Regression)")
  
  return(linpred(mod))
}

#' @rdname score_expression
#' @method score_expression cv.glmnet
#' @export
score_expression.cv.glmnet <-
function(mod, response=NULL)
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
