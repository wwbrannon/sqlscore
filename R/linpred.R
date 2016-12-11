#' Unevaluated prediction expressions for models
#' 
#' Generate an unevaluated call corresponding to the predict step of the passed
#' model. The call represents the linear predictor in terms of elementary functions
#' on the underlying column names. Before translation into SQL, it should have a link
#' function applied by score_expression (which may be a no-op in the case of the
#' identity link).
#' 
#' @param mod A model object providing a coef() method.
#' @return An unevaluated R call object representing the linear predictor.
#' 
#' @rdname score_expression
#' @export linpred
linpred <-
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
    
    exps[[i]] <- as.call(list(as.symbol("*"), exp, unname(cf[i])))
  }
  
  cmb <- function(x, y) as.call(list(as.symbol("+"), x, y))
  Reduce(cmb, exps)
}
