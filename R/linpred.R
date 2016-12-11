#' Unevaluated prediction expressions for models
#' 
#' Generate an unevaluated call corresponding to the predict step of the passed
#' model. The call represents the linear predictor in terms of elementary functions
#' on the underlying column names. Before translation into SQL, it should have a response
#' function applied by score_expression (which may be a no-op in the case of the
#' identity response).
#' 
#' @param mod A model object providing a coef() method.
#' @return An unevaluated R call object representing the linear predictor.
#' 
#' @examples 
#' mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris)
#' linpred(mod)
#' 
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
  
  #Handle the intercept term
  pos <- which(names(cf) == "(Intercept)")
  if(length(pos) > 0)
  {
    names(cf)[pos] <- "1"
  }
  
  cf <- c(cf, extract_offsets(mod))
  
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
