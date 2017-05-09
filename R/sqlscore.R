#' sqlscore: Utilities to score GLMs and related models in SQL.
#'
#' The sqlscore package provides utilities for generating sql queries
#' (particularly CREATE TABLE statements) from R model objects. The most important
#' use case is generating SQL to score a GLM or related model represented as an R
#' object, in which case the package handles parsing formula operators and
#' including the model's response function. The models scored need not be generalized
#' linear models, strictly speaking, but their prediction steps must consist of applying
#' a response function to a linear predictor. The package handles escaping and dealing
#' with formula operators, and provides a way to use a custom response function if desired.
#'
#' @section Function overview:
#' The SQL-generating functions create_statement and select_statement do what their
#' names suggest and generate CREATE TABLE and SELECT statements for model scoring.
#' Helper functions include linpred(), which generates an R call object representing
#' the linear predictor, and score_expression, an S3 generic that handles wrapping
#' the linear predictor in the response function.
#'
#' @section Supported models:
#' Specific packages and models that are known to work include: glm and lm from
#' package:stats, cv.glmnet from package:glmnet, glmboost from package:mboost,
#' and bayesglm from package:arm.
#'
#' Default S3 methods are for objects structured like those of class "glm", so
#' models not listed here may work if they resemble those objects, but are not
#' guaranteed to.
#'
#' @docType package
#' @name sqlscore
NULL

# Fully qualify and escape a table name given in parts.
#
# Given the three canonical parts of a database table name (catalog, schema
# and table), escape all three and join them together to produce the fully
# qualified table name. Only the table name is required.
#
# @param table The unqualified table name.
# @param catalog The catalog name.
# @param schema The schema name.
# @param con An optional DBI connection to control the details of SQL generation
#
# @return A dplyr/dbplyr SQL object representing the fully qualified and escaped
#         table name.
fqtn <-
function(table, catalog=NULL, schema=NULL, con=NULL)
{
  ident <- get_ident()
  sql <- get_sql()
  build_sql <- get_build_sql()

  if(is.null(table) || table == "")
    stop("Bad table name")

  if(!is.null(schema) && schema == "")
    stop("Bad schema name")

  if(!is.null(catalog) && catalog == "")
    stop("Bad catalog name")

  if(!is.null(catalog) && is.null(schema))
    stop("Cannot give a catalog name without a schema")

  #This kind of list manipulation in R is inefficient,
  #but for three short character vectors, it doesn't matter.
  dp <- list()
  if(!is.null(catalog))
  {
    dp[[length(dp) + 1]] <- c(ident(catalog))
    dp[[length(dp) + 1]] <- "."
  }

  if(!is.null(schema))
  {
    dp[[length(dp) + 1]] <- c(ident(schema))
    dp[[length(dp) + 1]] <- "."
  }

  dp[[length(dp) + 1]] <- c(ident(table))

  if(!is.null(con))
    dp$con <- con

  do.call(build_sql, dp)

}

#' Generate a SELECT statement from a model
#'
#' Generate a SELECT statement to score the passed model on a preexisting
#' database table. The statement will generate predictions entirely in the
#' database, with no need to fetch data into R. Models need not be GLMs, but
#' their prediction steps must consist of applying a response function to
#' a linear predictor.
#'
#' @section Supported packages:
#' Specific packages and models that are known to work include: glm and lm from
#' package:stats, cv.glmnet from package:glmnet, glmboost from package:mboost,
#' and bayesglm from package:arm.
#'
#' Default S3 methods are for objects structured like those of class "glm", so
#' models not listed here may work if they resemble those objects, but are not
#' guaranteed to.
#'
#' @section Warning:
#' Note that if the model object transformed its training data before fitting (e.g.,
#' centering and scaling predictors), the generated SQL statement will not include
#' those transformations. A future release may include that functionality, but
#' centering and scaling in particular are difficult to do efficiently and portably
#' in SQL.
#'
#' @param mod A supported model object.
#' @param src_table The unqualified DB name of the source table.
#' @param src_schema The DB schema of the source table.
#' @param src_catalog The DB catalog of the source table.
#' @param pk A vector of primary key column names.
#' @param response The name of a custom response function to apply to the linear predictor.
#' @param con An optional DBI connection to control the details of SQL generation.
#'
#' @return A dplyr/dbplyr SQL object representing the SELECT statement.
#'
#' @examples
#' # Basic select statements
#' mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris)
#' select_statement(mod, src_table="tbl_name")
#' select_statement(mod, src_table="tbl_name", src_schema="schema_name",
#'                  src_catalog="catalog_name")
#' select_statement(mod, src_table="tbl_name", src_schema="schema_name",
#'                  src_catalog="catalog_name", pk=c("lab", "specimen_id"))
#'
#' #With a custom response function
#' select_statement(mod, src_table="tbl_name", src_schema="schema_name",
#'                  response="probit")
#'
#' # With a model-derived non-identity response function
#' mod <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris, family=binomial("logit"))
#' select_statement(mod, src_table="tbl_name")
#'
#' #With formula operators
#' x <- matrix(rnorm(100*20),100,20)
#' colnames(x) <- sapply(1:20, function(x) paste0("X", as.character(x)))
#' x <- as.data.frame(x)
#' mod <- glm(X2 ~ X3 + X5 + X15*X8, data=x)
#' select_statement(mod, src_table="tbl_name")
#' select_statement(mod, src_table="tbl_name", response="cauchit")
#'
#' @export select_statement
select_statement <-
function(mod, src_table, src_schema=NULL, src_catalog=NULL, pk=c("id"),
         response=NULL, con=NULL)
{
  ident <- get_ident()
  build_sql <- get_build_sql()
  translate_sql <- get_translate_sql()

  #Fully qualify and escape the src table
  src <- fqtn(src_table, src_catalog, src_schema, con=con)

  #Put the statement together
  parts <- list()
  parts[[length(parts) + 1]] <- "SELECT "

  for(i in seq_along(pk))
  {
    parts[[length(parts) + 1]] <- ident(pk[i])
    parts[[length(parts) + 1]] <- ", "
  }

  se <- list(score_expression(mod, response=response), con=con)
  parts[[length(parts) + 1]] <- do.call(translate_sql, se)

  parts[[length(parts) + 1]] <- " FROM "
  parts[[length(parts) + 1]] <- src

  #We're leaving off the terminating semicolon to let people more easily
  #tack on concluding incantations for the select (string munging is great)
  parts$con <- con
  do.call(build_sql, parts)
}

#' Generate a CREATE TABLE statement from a model
#'
#' Generate a CREATE TABLE statement to score the passed model on a preexisting
#' database table. The statement will generate predictions entirely in the
#' database, with no need to fetch data into R. Models need not be GLMs, but
#' their prediction steps must consist of applying a response function to
#' a linear predictor.
#'
#' @section Supported packages:
#' Specific packages and models that are known to work include: glm and lm from
#' package:stats, cv.glmnet from package:glmnet, glmboost from package:mboost,
#' and bayesglm from package:arm.
#'
#' Default S3 methods are for objects structured like those of class "glm", so
#' models not listed here may work if they resemble those objects, but are not
#' guaranteed to.
#'
#' @section Warning:
#' Note that if the model object transformed its training data before fitting (e.g.,
#' centering and scaling predictors), the generated SQL statement will not include
#' those transformations. A future release may include that functionality, but
#' centering and scaling in particular are difficult to do efficiently and portably
#' in SQL.
#'
#' @param mod A supported model object.
#' @param src_table The unqualified DB name of the source table.
#' @param src_schema The DB schema of the source table.
#' @param src_catalog The DB catalog of the source table.
#' @param dest_table The unqualified DB name of the destination table.
#' @param dest_schema The DB schema of the destination table.
#' @param dest_catalog The DB catalog of the destination table.
#' @param drop Whether to generate a DROP TABLE IF EXISTS before the CREATE TABLE.
#' @param temporary Whether the destination table should be a temporary table.
#' @param pk A vector of primary key column names.
#' @param response The name of a custom response function to apply to the linear predictor.
#' @param con An optional DBI connection to control the details of SQL generation.
#'
#' @return A dplyr/dbplyr SQL object representing the SELECT statement.
#'
#' @examples
#' # Basic create statements
#' mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris)
#' create_statement(mod, src_table="tbl_name", dest_table="target_tbl")
#' create_statement(mod, src_table="tbl_name", src_schema="schema_name",
#'                  src_catalog="catalog_name", dest_table="target_tbl")
#' create_statement(mod, src_table="tbl_name", src_schema="schema_name",
#'                  src_catalog="catalog_name", dest_table="target_tbl",
#'                  dest_schema="target_schema", dest_catalog="target_catalog",
#'                  pk=c("lab", "specimen_id"))
#'
#' #With a custom response function
#' create_statement(mod, src_table="tbl_name", src_schema="schema_name",
#'                  dest_table="target_tbl", response="probit")
#'
#' # With a model-derived non-identity response function
#' mod <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'            data=datasets::iris, family=binomial("logit"))
#' create_statement(mod, src_table="tbl_name", dest_table="target_tbl")
#'
#' #With formula operators
#' x <- matrix(rnorm(100*20),100,20)
#' colnames(x) <- sapply(1:20, function(x) paste0("X", as.character(x)))
#' x <- as.data.frame(x)
#' mod <- glm(X2 ~ X3 + X5 + X15*X8, data=x)
#' create_statement(mod, src_table="tbl_name", dest_table="target_tbl")
#' create_statement(mod, src_table="tbl_name", dest_table="target_tbl",
#'                  response="cauchit")
#'
#' @export create_statement
create_statement <-
function(mod, dest_table, src_table,
         dest_schema=NULL, dest_catalog=NULL, src_schema=NULL,
         src_catalog=NULL, drop=FALSE, temporary=FALSE,
         pk=c("id"), response=NULL, con=NULL)
{
  # Ideally, we'd use some kind of object-relational mapper to build
  # this statement rather than just munging text, but the ones available
  # for R are underdeveloped. d(b)plyr comes closest but can't quite do this.

  build_sql <- get_build_sql()

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
                         src_catalog=src_catalog, pk=pk, response=response, con=con)
  parts <- c(parts, ss)

  #We're leaving off the terminating semicolon to let people more easily
  #tack on concluding incantations for the select (string munging is great)
  parts$con <- con
  do.call(build_sql, parts)
}
