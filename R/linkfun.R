link_cloglog <-
function(x)
{
    e1 <- as.call(list(as.symbol("exp"), x))
    e2 <- as.call(list(as.symbol("-"), e1))
    e3 <- as.call(list(as.symbol("exp"), e2))
    e4 <- as.call(list(as.symbol("-"), 1, e3))

    e4
}

link_logit <-
function(x)
{
    e1 <- as.call(list(as.symbol("("), x))
    e2 <- as.call(list(as.symbol("*"), -1, e1))
    e3 <- as.call(list(as.symbol("exp"), e2))
    e4 <- as.call(list(as.symbol("+"), 1, e3))
    e5 <- as.call(list(as.symbol("("), e4))
    e6 <- as.call(list(as.symbol("/"), 1, e5))

    e6
}

# this is an unfortunate name
link_inverse <-
function(x)
{
    return(as.call(list(as.symbol("/"), 1, x)))
}

link_sqrteta <-
function(x)
{
    e1 <- as.call(list(as.symbol("sqrt"), x))
    e2 <- as.call(list(as.symbol("/"), 1, e1))

    e2
}

link_sqrt <-
function(x)
{
    return(as.call(list(as.symbol("^"), x, 2)))
}

link_identity <-
function(x)
{
    x
}

link_log <-
function(x)
{
    return(as.call(list(as.symbol("exp"), x)))
}

link_cauchit <-
function(x)
{
    # L(eta) = tan(pi * (eta - 1/2))
    e0 <- as.call(list(as.symbol("acos"), -1)) # = pi

    e1 <- as.call(list(as.symbol("("), x))
    e2 <- as.call(list(as.symbol("-"), e1, 1/2))
    e3 <- as.call(list(as.symbol("("), e2))
    e4 <- as.call(list(as.symbol("*"), e0, e3))
    e5 <- as.call(list(as.symbol("tan"), e4))

    e5
}

#' @export
linkfun <-
function(obj)
{
    UseMethod("linkfun")
}

#' @method linkfun default
#' @export
linkfun.default <-
function(obj)
{
    stop(paste0("Unrecognized object type for linkfun. Hint: try the response ",
                "argument to score_expression to specify the name of a custom or ",
                "DB-specific SQL function."))
}

#' @method linkfun glm
#' @export
linkfun.glm <-
function(obj)
{
    lnk <- obj$family$link

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
        fn <- link_cauchit
    } else if(lnk == "identity")
    {
        # L(eta) = eta
        fn <- link_identity
    } else if(lnk == "log")
    {
        # L(eta) = exp(eta)
        fn <- link_log
    } else if(lnk == "sqrt")
    {
        # L(eta) = eta^2
        fn <- link_sqrt
    } else if(lnk == "1/mu^2")
    {
        # L(eta) = 1/sqrt(eta)
        fn <-link_sqrteta
    } else if(lnk == "inverse")
    {
        # L(eta) = 1/eta
        fn <- link_inverse
    } else if(lnk == "logit")
    {
        # L(eta) = 1/(1+exp(-eta))
        fn <- link_logit
    } else if(lnk == "cloglog")
    {
        # L(eta) = 1 - exp(-exp(eta))
        fn <- link_cloglog
    } else
    {
        NextMethod() # raise default error
    }

    return(structure(fn, link=lnk))
}

#' @method linkfun lm
#' @export
linkfun.lm <-
function(obj)
{
    return(structure(link_identity, link="identity"))
}

#' @method linkfun bayesglm
#' @export
linkfun.bayesglm <-
function(obj)
{
    #These are also GLM objects; the fit is regularized but the
    #prediction step is the same

    NextMethod()
}

#' @method linkfun glmboost
#' @export
linkfun.glmboost <-
function(obj)
{
    #mboost's family objects are hard to work with, so this is fragile: if mboost
    #changes its names by even one character, things break
    if(obj$family@name == "Squared Error (Regression)")
    {
        # L(eta) = eta
        lnk <- "identity"
        fn <- link_identity
    } else if(obj$family@name == "Binomial Distribution (similar to glm)") # => logit
    {
        # L(eta) = 1/(1+exp(-eta))
        lnk <- "logit"
        fn <- link_logit
    } else if(obj$family@name == "Negative Binomial Likelihood -- probit Link")
    {
        # L(eta) does not exist in closed form
        stop(paste0("Response function does not exist in closed form. Consider ",
                    "using the response argument to score_expression to use a ",
                    "custom sql function."))
    } else if(obj$family@name == "Poisson Likelihood")
    {
        # L(eta) = exp(eta)
        lnk <- 'log'
        fn <- link_log
    } else if(obj$family@name == "Negative Gamma Likelihood")
    {
        # L(eta) = exp(eta)
        lnk <- 'log'
        fn <- link_log
    } else
    {
        stop("Unsupported link family ", sQuote(obj$family@name), " for glmboost")
    }

    return(structure(fn, link=lnk))
}

#' @method linkfun cv.glmnet
#' @export
linkfun.cv.glmnet <-
function(obj)
{
    cls <- setdiff(class(obj$glmnet.fit), c("glmnet"))

    if(cls == "elnet") # family = gaussian
    {
        # L(eta) = eta
        lnk <- "identity"
        fn <- link_identity
    } else if(cls == "fishnet") # family = poisson
    {
        # L(eta) = exp(eta)
        lnk <- "log"
        fn <- link_log
    } else if(cls == "lognet") # family = binomial
    {
        # L(eta) = 1/(1+exp(-eta))
        lnk <- "logit"
        fn <- link_logit
    } else
    {
        stop("Unsupported model family for cv.glmnet")
    }

    return(structure(fn, link=lnk))
}
