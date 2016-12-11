context("Table name escaping")

test_that("Basic table names are escaped", {
  expect_equal(fqtn("tbl"), dplyr::sql('"tbl"'))
  expect_equal(fqtn("foo", schema="bar"), dplyr::sql('"bar"."foo"'))
  expect_equal(fqtn("foo", schema="bar", catalog="baz"), dplyr::sql('"baz"."bar"."foo"'))
})

test_that("More complicated names are escaped", {
  expect_equal(fqtn("foo bar"), dplyr::sql('"foo bar"'))
  expect_equal(fqtn("foo bar", schema="baz"), dplyr::sql('"baz"."foo bar"'))
  expect_equal(fqtn("foo bar", schema="baz", catalog="quux quux"),
               dplyr::sql('"quux quux"."baz"."foo bar"'))
  expect_equal(fqtn("foo", schema="baz", catalog="quux quux"),
               dplyr::sql('"quux quux"."baz"."foo"'))
})

test_that("Invalid calls throw errors", {
  expect_error(fqtn(NULL))
  expect_error(fqtn(0))
  expect_error(fqtn(""))
  expect_error(fqtn(catalog="bar")) #need table
  expect_error(fqtn("foo", catalog="bar")) #need schema
  expect_error(fqtn(schema="foo", catalog="bar")) #need table
})

