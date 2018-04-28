## Helper functions for package tests

# Recursively round an object's numeric components
#
# Take an object and return it with numeric components rounded to a some
# configurable number of significant figures. If the object is a language
# object, return an identically structured object with rounding applied.
# The main use case is to allow comparison of language objects with all.equal,
# whose default method tests for equality of deparsed strings, to use a
# numerical tolerance.
#
# @param x The object to recursively round.
# @param digits The number of digits numeric components of x should be rounded to.
#
# @return The passed object x with numeric components rounded.
rec_round <-
function(x, digits=8)
{
  if(is.numeric(x))
  {
    return(round(x, digits=digits))
  } else if(is.call(x))
  {
    lt <- as.list(x)

    # FIXME doesn't pass digits arg down
    return(as.call(lapply(lt, function(y) rec_round(y, digits=digits))))
  } else if(is.expression(x))
  {
    lt <- as.list(x)

    # FIXME doesn't pass digits arg down
    return(as.expression(lapply(lt, function(y) rec_round(y, digits=digits))))
  } else
  {
    return(x)
  }
}
