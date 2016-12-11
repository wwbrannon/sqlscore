#' Fully qualify and escape a table name given in parts.
#' 
#' Given the three canonical parts of a database table name (catalog, schema
#' and table), escape all three and join them together to produce the fully
#' qualified table name. Only the table name is required.
#' 
#' @param table The unqualified table name.
#' @param catalog The catalog name.
#' @param schema The schema name.
#' @param con An optional DBI connection to control the details of SQL generation
#' 
#' @return A dplyr SQL object representing the fully qualified and escaped table name.
fqtn <-
function(table, catalog=NULL, schema=NULL, con=NULL)
{
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
  
  if(!is.null(con))
    dp$con <- con
  
  do.call(dplyr::build_sql, dp)
  
}

#' Generate a SELECT statement from a model
#' 
#' Generate a SELECT statement to score the passed model on a preexisting
#' database table. The statement will generate predictions entirely in the
#' database, with no need to fetch data into R. Models need not be GLMs, but
#' their prediction steps must consist of applying a (link-inverse) function to
#' a linear predictor.
#' 
#' @param mod A model object providing a coef() method.
#' @param src_table The unqualified DB name of the source table.
#' @param src_schema The DB schema of the source table.
#' @param src_catalog The DB catalog of the source table.
#' @param pk A vector of primary key column names.
#' @param link The name of a custom link function to apply to the linear predictor.
#' @param con An optional DBI connection to control the details of SQL generation.
#' 
#' @return A dplyr SQL object representing the SELECT statement.
#' 
#' @export select_statement
select_statement <-
function(mod, src_table, src_schema=NULL, src_catalog=NULL, pk=c("id"),
         link=NULL, con=NULL)
{
  #Fully qualify and escape the src table
  src <- fqtn(src_table, src_catalog, src_schema, con=con)
  
  #Put the statement together
  parts <- list()
  parts[[length(parts) + 1]] <- "SELECT "
  
  for(i in seq_along(pk))
  {
    parts[[length(parts) + 1]] <- dplyr::ident(pk[i])
    parts[[length(parts) + 1]] <- ", "
  }
  
  se <- list(score_expression(mod, link=link), con=con)
  parts[[length(parts) + 1]] <- do.call(dplyr::translate_sql, se)
  
  parts[[length(parts) + 1]] <- " FROM "
  parts[[length(parts) + 1]] <- src
  
  #We're leaving off the terminating semicolon to let people more easily
  #tack on concluding incantations for the select (string munging is great)
  parts$con <- con
  do.call(dplyr::build_sql, parts)
}

#' Generate a CREATE TABLE statement from a model
#' 
#' Generate a CREATE TABLE statement to score the passed model on a preexisting
#' database table. The statement will generate predictions entirely in the
#' database, with no need to fetch data into R. Models need not be GLMs, but
#' their prediction steps must consist of applying a (link-inverse) function to
#' a linear predictor.
#' 
#' @param mod A model object providing a coef() method.
#' @param src_table The unqualified DB name of the source table.
#' @param src_schema The DB schema of the source table.
#' @param src_catalog The DB catalog of the source table.
#' @param dest_table The unqualified DB name of the destination table.
#' @param dest_schema The DB schema of the destination table.
#' @param dest_catalog The DB catalog of the destination table.
#' @param drop Whether to generate a DROP TABLE IF EXISTS before the CREATE TABLE.
#' @param temporary Whether the destination table should be a temporary table.
#' @param pk A vector of primary key column names.
#' @param link The name of a custom link function to apply to the linear predictor.
#' @param con An optional DBI connection to control the details of SQL generation.
#' 
#' @return A dplyr SQL object representing the SELECT statement.
#' 
#' @export create_statement
create_statement <-
function(mod, dest_table, src_table,
         dest_schema=NULL, dest_catalog=NULL, src_schema=NULL,
         src_catalog=NULL, drop=FALSE, temporary=FALSE,
         pk=c("id"), link=NULL, con=NULL)
{
  # Ideally, we'd use some kind of object-relational mapper to build
  # this statement rather than just munging text, but the ones available
  # for R are underdeveloped. dplyr comes closest but can't quite do this.
  
  #Fully qualify and escape the dest table
  dest <- fqtn(dest_table, dest_catalog, dest_schema, con=con)
  
  #Put the statement together
  parts <- list()
  
  if(!is.null(drop) && drop)
    parts <- c(parts, "DROP TABLE IF EXISTS ", dest, ";\n")

  parts <- c(parts, "CREATE ",
             ifelse(temporary, "TEMPORARY ", ""),
             "TABLE ", dest, " AS ")
  
  ss <- select_statement(mod, src_table=src_table, src_schema=src_schema,
                         src_catalog=src_catalog, pk=pk, link=link, con=con)
  parts <- c(parts, ss)
  
  #We're leaving off the terminating semicolon to let people more easily
  #tack on concluding incantations for the select (string munging is great)
  parts$con <- con
  do.call(dplyr::build_sql, parts)
}
