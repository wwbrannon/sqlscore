score_expression.glm <-
function(mod, link=NULL)
{
  lp <- linpred(mod)
  fam <- mod$family$family
  lnk <- mod$family$link
  
  if(fam == "gaussian")
  {
    if(lnk == "identity")
    {
      
    } else if(lnk == "inverse")
    {
      
    } else if(lnk == "log")
    {
      
    } else
    {
      stop(paste0("Unrecognized link function. Hint: try the link argument ",
                  "to score_expression to specify the name of a custom or ",
                  "DB-specific SQL function."))
    }
  } else if(fam == "binomial")
  {
    if(lnk == "logit")
    {
      
    } else if(lnk == "probit")
    {
      
    } else if(lnk == "cauchit")
    {
      
    } else if(lnk == "log")
    {
      
    } else if(lnk == "cloglog")
    {
      
    } else
    {
      stop(paste0("Unrecognized link function. Hint: try the link argument ",
                  "to score_expression to specify the name of a custom or ",
                  "DB-specific SQL function."))
    }
  } else if(fam == "Gamma")
  {
    if(lnk == "identity")
    {
      
    } else if(lnk == "inverse")
    {
      
    } else if(lnk == "log")
    {
      
    } else
    {
      stop(paste0("Unrecognized link function. Hint: try the link argument ",
                  "to score_expression to specify the name of a custom or ",
                  "DB-specific SQL function."))
    }
  } else if(fam == "poisson")
  {
    if(lnk == "identity")
    {
      
    } else if(lnk == "sqrt")
    {
      
    } else if(lnk == "log")
    {
      
    } else
    {
      stop(paste0("Unrecognized link function. Hint: try the link argument ",
                  "to score_expression to specify the name of a custom or ",
                  "DB-specific SQL function."))
    }
  } else if(fam == "inverse.gaussian")
  {
    if(lnk == "identity")
    {
      
    } else if(lnk == "inverse")
    {
      
    } else if(lnk == "log")
    {
      
    } else if(lnk == "1/mu^2")
    {
      
    } else
    {
      stop(paste0("Unrecognized link function. Hint: try the link argument ",
                  "to score_expression to specify the name of a custom or ",
                  "DB-specific SQL function."))
    }
  }
  
  exp
}
