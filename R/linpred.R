# Reverse dummy expansion for factors
# 
# Given the name of a model coefficient and a vector of factors appearing
# in the model, determine whether the coefficient corresponds to a dummy
# for a level of one of the factors. If so, return an unevaluated call to
# ifelse() that is 1 when that factor variable has that value, and 0
# otherwise. If not, return the name as a symbol. (Because it works on
# dummy variables, this function can only handle models with treatment
# contrasts for their factors.)
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
#' @section Warning:
#' The Binomial models in glmboost return coefficients which are 1/2 the coefficients
#' fit by a call to glm(..., family=binomial(...)), because the response variable is
#' internally recoded to -1 and +1. sqlscore multiplies the returned coefficients by 2
#' to put them back on the same scale as glm, and adds the glmboost offset to the
#' intercept before multiplying.
# 
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
  if(length(names(cf)) != length(unique(names(cf))))
  {
    stop(paste0("Duplicate coefficient names detected; please rename variables ",
                "so that continuous variables' names do not conflict with the ",
                "names of factor level dummies"))
  }
  
  fc <- extract_factors(mod)
  
  cf <- c(cf, extract_offsets(mod))
  
  #Translate term names into R expressions and thence to the corresponding
  #sql. When dealing with coef, fortunately, we only have to consider
  #the : and I() formula operators and factor expansions.
  exps <- vector("list", length(cf))
  for(j in seq_along(cf))
  {
    nm <- names(cf)[j]
    parts <- as.list(strsplit(nm, ":")[[1]])
    
    for(i in seq_along(parts))
    {
      pt <- parts[[i]]
      
      if(pt == "(Intercept)")
      {
        parts[[i]] <- 1.0
      } else if(substr(pt, 1, 2) == "I(" &&
                substr(pt, nchar(pt), nchar(pt)) == ")")
      {
        parts[[i]] <- parse(text=substr(pt, 3, nchar(pt) - 1))[[1]]
      } else #it's just a variable name
      {
        parts[[i]] <- undummy(pt, fc)
      }
    }
    
    cmb <- function(x, y) as.call(list(as.symbol("*"), x, y))
    exp <- Reduce(cmb, parts)

    exps[[j]] <- as.call(list(as.symbol("*"), exp, unname(cf[j])))
  }
  
  cmb <- function(x, y) as.call(list(as.symbol("+"), x, y))
  Reduce(cmb, exps)
}
