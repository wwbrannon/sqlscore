context("Table name escaping")

test_that("Basic table names are escaped", {
  expect_equal(sqlscore:::fqtn("tbl"), dplyr::sql('"tbl"'))
  expect_equal(sqlscore:::fqtn("foo", schema="bar"), dplyr::sql('"bar"."foo"'))
  expect_equal(sqlscore:::fqtn("foo", schema="bar", catalog="baz"), dplyr::sql('"baz"."bar"."foo"'))
})

test_that("More complicated names are escaped", {
  expect_equal(sqlscore:::fqtn("foo bar"), dplyr::sql('"foo bar"'))
  expect_equal(sqlscore:::fqtn("foo bar", schema="baz"), dplyr::sql('"baz"."foo bar"'))
  expect_equal(sqlscore:::fqtn("foo bar", schema="baz", catalog="quux quux"),
               dplyr::sql('"quux quux"."baz"."foo bar"'))
  expect_equal(sqlscore:::fqtn("foo", schema="baz", catalog="quux quux"),
               dplyr::sql('"quux quux"."baz"."foo"'))
})

test_that("Invalid calls throw errors", {
  expect_error(sqlscore:::fqtn(NULL))
  expect_error(sqlscore:::fqtn(0))
  expect_error(sqlscore:::fqtn(""))
  expect_error(sqlscore:::fqtn(catalog="bar")) #need table
  expect_error(sqlscore:::fqtn("foo", catalog="bar")) #need schema
  expect_error(sqlscore:::fqtn(schema="foo", catalog="bar")) #need table
})

