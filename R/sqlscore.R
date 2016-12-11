#' sqlscore: Utilities to score GLMs and related models in SQL.
#'
#' The sqlscore package provides utilities for generating sql queries
#' (particularly CREATE TABLE statements) from R model objects. The most important
#' use case is generating SQL to score a model trained via R's formula interface,
#' which may use formula operators that need to be translated into SQL expressions.
#' The models scored need not be generalized linear models, strictly speaking, but
#' their prediction steps must consist of applying a (link-inverse) function to a
#' linear predictor. The package handles escaping and dealing with formula operators,
#' and provides a way to use a custom link-inverse function if desired.
#' 
#' @section Function overview:
#' The SQL-generating functions create_statement and select_statement do what their
#' names suggest and generate CREATE TABLE and SELECT statements for model scoring.
#' Helper functions include linpred(), which generates an R call object representing
#' the linear predictor, and score_expression, an S3 generic that handles wrapping
#' the linear predictor in the link-inverse function.
#'
#' @docType package
#' @name sqlscore
NULL