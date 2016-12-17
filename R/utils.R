# Evaluate an expression, capturing warnings and messages
#
# Evaluate the passed expression, capturing warnings and messages
# it may generate. Return warnings and messages a component of a
# list rather than allowing them to be signaled. Inspired by a post
# from Luke Tierney to the R-help mailing list.
#
# @param expr The expression to evaluate.
#
# @return A list of three elements: value, the value of the expression;
# warnings, any collected warnings; messages, any collected messages.
captureConditions <- function(expr) {
  wn <- NULL
  ms <- NULL
  
  wHandler <- function(w) {
    wn <<- c(wn, list(w))
    invokeRestart("muffleWarning")
  }
  
  mHandler <- function(m) {
    ms <<- c(ms, list(m))
    invokeRestart("muffleMessage")
  }
  
  val <- withCallingHandlers(expr, warning=wHandler, message=mHandler)
  list(value=val, warnings=wn, messages=ms)
}
