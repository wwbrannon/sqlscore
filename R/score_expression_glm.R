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
    e1 <- as.call(list(as.symbol("-"), lp))
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

#' @export
score_expression.lm <-
function(mod, link=NULL)
{
  #the only possible link function for this object is the identity link
  return(linpred(mod))
}
