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

# Default database identifier escaping logic
#
# Escape the passed string for use as a database identifier. The logic used
# isn't specific to any particular database and likely won't work on some,
# but generally follows the SQL standard. It's not nearly enough for security
# against SQL injection attacks, but should be good enough to avoid generating
# invalid SQL from R model objects with funny names. d(b)plyr's ident() function
# doesn't quote aggressively enough for our purposes.
#
# @param s The string to escape
# @param backslash If TRUE, use backslash as escape for '"'; otherwise double '"'.
#
# @return The passed string escaped for use as a DB identifier
sql_escape_ident <-
function(s, backslash=FALSE)
{
    `%p%` <- paste0 # for convenience

    if(is.null(s))
        return(NULL) # makes default argument handling easier

    if(length(s) > 1 || !is.character(s))
        stop("Can only escape a length-1 character vector")

    if(!validUTF8(s))
        stop("Can only escape UTF-8 encoded strings")

    # 0 bytes are all kinds of bad news
    parts <- strsplit(s, '')[[1]]
    checks <- vapply(parts, function(x) utf8ToInt(x) == 0, logical(1))
    if(sum(checks) > 0)
        stop("NUL byte detected in UTF-8 string")

    if(!backslash)
        esc <- gsub('"', '""', s)
    else
        esc <- gsub('"', '\\\\"', s)

    return('"' %p% esc %p% '"')
}
