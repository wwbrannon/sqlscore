# Reverse dummy expansion for factors
# 
# Given the name of a model coefficient and a vector of factors appearing
# in the model, determine whether the coefficient corresponds to a dummy
# for a level of one of the factors. If so, return an unevaluated call to
# ifelse() that is 1 when that factor variable has that value, and 0
# otherwise. If not, return the name as a symbol.
# 
# @param name The name to convert.
# @param factors The list of factor names present in the model.
# 
# @return A list of call or symbol objects.
undummy <-
function(name, factors)
{
  for(fac in factors)
  {
    mt <- regexpr(fac, name)
    len <- attr(mt, "match.length")

    if(mt == 1 && len == nchar(fac))
    {
      lev <- substr(name, len + 1, nchar(name))
      
      e1 <- as.call(list(as.symbol("=="), as.symbol(fac), lev))
      return(as.call(list(as.symbol("ifelse"), e1, 1, 0)))
    }
  }
  
  # Not a dummy for any of these factors
  as.symbol(name)
}

#' Unevaluated prediction expressions for models
#' 
#' Generate an unevaluated call corresponding to the predict step of the passed
#' model. The call represents the linear predictor in terms of elementary functions
#' on the underlying column names. Before translation into SQL, it should have a response
#' function applied by score_expression (which may be a no-op in the case of the
#' identity response).
#' 
#' @param mod A supported model object.
#' @return An unevaluated R call object representing the linear predictor.
#' 
#' @examples 
#' # A Gaussian GLM including factors
#' mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris)
#' linpred(mod)
#' 
#' # A binomial GLM - linear predictor is unaffected
#' mod <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris, family=binomial("logit"))
#' linpred(mod)
#' 
#' #With formula operators
#' x <- matrix(rnorm(100*20),100,20)
#' colnames(x) <- sapply(1:20, function(x) paste0("X", as.character(x)))
#' x <- as.data.frame(x)
#' mod <- glm(X2 ~ X3 + X5 + X15*X8, data=x)
#' linpred(mod)
#' 
#' @export
linpred <-
function(mod)
{
  cf <- extract_coef(mod)
  fc <- extract_factors(mod)
  
  #Handle the intercept term
  pos <- which(names(cf) == "(Intercept)")
  if(length(pos) > 0)
  {
    names(cf)[pos] <- "1"
  }
  
  cf <- c(cf, extract_offsets(mod))
  
  #Translate term names into R expressions and thence to the corresponding
  #sql. When dealing with coef, fortunately, we only have to consider
  #the : and I() formula operators and factor expansions.
  exps <- vector("list", length(cf))
  for(i in seq_along(cf))
  {
    nm <- names(cf)[i]
    
    if(substr(nm, 1, 2) == "I(" &&
       substr(nm, nchar(nm), nchar(nm)) == ")")
    {
      exp <- parse(text=substr(nm, 3, nchar(nm) - 1))[[1]]
    } else if(regexpr(":", nm) != -1)
    {
      parts <- as.list(strsplit(nm, ":")[[1]])
      parts <- lapply(parts, function(x) undummy(x, fc))
      
      cmb <- function(x, y) as.call(list(as.symbol("*"), x, y))
      exp <- Reduce(cmb, parts)
    } else
    {
      exp <- undummy(nm, fc)
    }
    
    exps[[i]] <- as.call(list(as.symbol("*"), exp, unname(cf[i])))
  }
  
  cmb <- function(x, y) as.call(list(as.symbol("+"), x, y))
  Reduce(cmb, exps)
}
