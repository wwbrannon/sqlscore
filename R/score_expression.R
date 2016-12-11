#TODO
#Handle link function if any (s3 classes for diff model types?)
#Other packages: glmnet, bayesglm, bridge, glmboost

linear_predictor <-
function(mod)
{
  cf <- coef(mod)
  
  #Handle the intercept term
  pos <- which(names(cf) == "(Intercept)")
  if(length(pos) > 0)
  {
    names(cf)[pos] <- "1"
  }
  
  #Handle offsets
  if("formula" %in% ls(mod))
  {
    pos <- attr(terms(mod$formula), "offset")
    if(!is.null(pos))
    {
      for(offset in pos)
      {
        args <- list(cf, 1)
        names(args) <- c("", all.vars(mod$formula)[offset])
        cf <- do.call(c, args)
      }
    }
  }
  
  #Translate term names into R expressions and thence to the corresponding
  #sql. When dealing with coef, fortunately, we only have to consider
  #the : and I() formula operators.
  exps <- vector("list", length(cf))
  for(i in seq_along(cf))
  {
    nm <- names(cf)[i]
    
    if(substr(nm, 1, 2) == "I(" &&
       substr(nm, nchar(nm), nchar(nm)) == ")")
    {
      exp <- parse(text=substr(nm, 3, nchar(nm) - 1))[[1]]
    } else
    {
      exp <- parse(text=gsub(":", "*", nm))[[1]]
    }
    
    exps[[i]] <- as.call(list(as.symbol("*"), exp, cf[i]))
  }
  
  cmb <- function(x, y) as.call(list(as.symbol("+"), x, y))
  Reduce(cmb, exps)
}
