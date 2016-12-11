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

