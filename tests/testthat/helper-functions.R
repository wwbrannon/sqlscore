rec_round <-
function(x, digits=8)
{
  if(is.numeric(x))
  {
    return(round(x, digits=digits))
  } else if(is.call(x))
  {
    lt <- as.list(x)
    return(as.call(lapply(lt, rec_round)))
  } else if(is.expression(x))
  {
    lt <- as.list(x)
    return(as.expression(lapply(lt, rec_round)))
  } else
  {
    return(x)
  }
}

