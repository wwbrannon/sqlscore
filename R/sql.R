fqtn <-
function(table, catalog=NULL, schema=NULL)
{
  #Fully qualify and escape a table name given in parts.
  #This kind of list manipulation in R is inefficient,
  #but for three short character vectors, it doesn't matter.
  
  dp <- list()
  if(!is.null(catalog) && catalog != "")
  {
    dp[[length(dp) + 1]] <- dplyr::ident(catalog)
    dp[[length(dp) + 1]] <- "."
  }
  
  if(!is.null(schema) && schema != "")
  {
    dp[[length(dp) + 1]] <- dplyr::ident(schema)
    dp[[length(dp) + 1]] <- "."
  }
  
  dp[[length(dp) + 1]] <- dplyr::ident(table)
  do.call(dplyr::build_sql, dp)
  
}

#' @export
select_statement <-
function(mod, src_table, src_schema=NULL, src_catalog=NULL, pk=c("id"),
         link=NULL)
{
  #Fully qualify and escape the src table
  src <- fqtn(src_table, src_catalog, src_schema)
  
  #Put the statement together
  parts <- list()
  parts[[length(parts) + 1]] <- "SELECT "
  
  for(i in seq_along(pk))
  {
    parts[[length(parts) + 1]] <- dplyr::ident(pk[i])
    parts[[length(parts) + 1]] <- ", "
  }
  
  se <- score_expression(mod, link=link)
  parts[[length(parts) + 1]] <- do.call(dplyr::translate_sql, list(se))
  
  parts[[length(parts) + 1]] <- " FROM "
  parts[[length(parts) + 1]] <- src
  
  #We're leaving off the terminating semicolon to let people more easily
  #tack on concluding incantations for the select (string munging is great)
  do.call(dplyr::build_sql, parts)
}

# Ideally, we'd use some kind of object-relational mapper to build
# this statement rather than just munging text, but the ones available
# for R are underdeveloped. dplyr comes closest but can't quite do this.
#' @export
create_statement <-
function(mod, dest_table, src_table,
         dest_schema=NULL, dest_catalog=NULL, src_schema=NULL,
         src_catalog=NULL, drop=FALSE, temporary=FALSE,
         pk=c("id"), link=NULL)
{
  #Fully qualify and escape the dest table
  dest <- fqtn(dest_table, dest_catalog, dest_schema)
  
  #Put the statement together
  parts <- list()
  
  if(!is.null(drop) && drop)
    parts <- c(parts, "DROP TABLE IF EXISTS ", dest, ";\n")

  parts <- c(parts, "CREATE ",
             ifelse(temporary, dplyr::sql("TEMPORARY "), ""),
             "TABLE ", dest, " AS ")
  
  ss <- select_statement(mod, src_table=src_table, src_schema=src_schema,
                         src_catalog=src_catalog, pk=pk, link=link)
  parts <- c(parts, ss)
  
  #We're leaving off the terminating semicolon to let people more easily
  #tack on concluding incantations for the select (string munging is great)
  do.call(dplyr::build_sql, parts)
}
