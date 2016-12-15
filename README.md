
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- [![Build Status](http://img.shields.io/travis/wwbrannon/sqlscore.svg?style=flat)](https://travis-ci.org/wwbrannon/sqlscore) -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sqlscore)](http://cran.r-project.org/web/packages/sqlscore) [![Downloads](http://cranlogs.r-pkg.org/badges/sqlscore)](http://cran.r-project.org/package=sqlscore) [![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://badges.mit-license.org)

sqlscore
========

Utilities for scoring GLMs and related models in SQL. Use the create\_statement and select\_statement functions to generate scoring queries from model objects. The most important use case is for very large scoring datasets, especially those which can't fit into memory or would require too much network or storage I/O if scored the usual way in R.

The SQL-generating functions in sqlscore handle various formula operators, and also take care of wrapping the model's linear predictor in the appropriate link function. If needed, you can also specify a custom link function.

Usage:
------

The SQL-generating functions create\_statement and select\_statement do what their names suggest and generate CREATE TABLE and SELECT statements for model scoring.

If, for example, you have a database table of iris measurements, you can model the sepal length and generate predictions as follows:

    mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
               data=datasets::iris)

    create_statement(mod, src_table="iris", dest_table="iris_scores")  

    #> <SQL> CREATE TABLE "iris_scores" AS SELECT id, 1.0 * 2.17126629215507 +
    "Sepal.Width" * 0.495888938388551 + "Petal.Length" * 0.829243912234806 +
    "Petal.Width" * -0.315155173326474 + CASE WHEN ("Species" = 'versicolor') THEN
    (1.0) ELSE (0.0) END * -0.723561957780729 + CASE WHEN ("Species" = 'virginica')
    THEN (1.0) ELSE (0.0) END * -1.02349781449083 FROM "iris"

To get a SELECT statement that's not wrapped in a CREATE TABLE (so that, e.g., you can add your own database-specific pieces of SQL), use select\_statement:

    mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
               data=datasets::iris)

    select_statement(mod, src_table="iris")

    #> <SQL> SELECT id, 1.0 * 2.17126629215507 + "Sepal.Width" * 0.495888938388551
    + "Petal.Length" * 0.829243912234806 + "Petal.Width" * -0.315155173326474 +
    CASE WHEN ("Species" = 'versicolor') THEN (1.0) ELSE (0.0) END *
    -0.723561957780729 + CASE WHEN ("Species" = 'virginica') THEN (1.0) ELSE (0.0)
    END * -1.02349781449083 FROM "iris"

Helper functions include linpred(), which generates an R call object representing the linear predictor, and score\_expression, an S3 generic that handles wrapping the linear predictor in the response function.

Supported model types:
----------------------

Specific packages and models that are known to work include: glm and lm from package:stats, cv.glmnet from package:glmnet, glmboost from package:mboost, and bayesglm from package:arm.

Default S3 methods are for objects structured like those of class "glm", so models not listed here may work if they resemble those objects, but are not guaranteed to.

Installation:
-------------

Install the released version from CRAN:

    install.packages('sqlscore')

Install the dev version from github:

    install.packages("devtools")
    devtools::install_github("wwbrannon/sqlscore")
