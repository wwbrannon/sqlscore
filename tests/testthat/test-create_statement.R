context("Create statement generation")

mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
           data=datasets::iris)

src_tbl <- "foo bar"
src_schema <- "baz"
src_catalog <- "quux"
src <- fqtn(src_tbl, schema=src_schema, catalog=src_catalog)

dest_tbl <- "foo1 bar1"
dest_schema <- "baz1"
dest_catalog <- "quux1"
dest <- fqtn(dest_tbl, schema=dest_schema, catalog=dest_catalog)

pk <- c("customerid", "state")

ss <- select_statement(mod, src_table=src_tbl, src_schema=src_schema,
                       src_catalog=src_catalog, pk=pk)
cs <- create_statement(mod, src_table=src_tbl, src_schema=src_schema,
                       src_catalog=src_catalog, dest_table=dest_tbl,
                       dest_schema=dest_schema, dest_catalog=dest_catalog,
                       pk=pk)

test_that("the select statement is included", {
  expect_match(cs, ss, fixed=TRUE)
})

test_that("table names are included", {
  expect_match(cs, src, fixed=TRUE)
  expect_match(cs, dest, fixed=TRUE)
})

test_that("DROP TABLE is generated", {
  cs <- create_statement(mod, src_table=src_tbl, src_schema=src_schema,
                         src_catalog=src_catalog, dest_table=dest_tbl,
                         dest_schema=dest_schema, dest_catalog=dest_catalog,
                         pk=pk, drop=TRUE)

  expect_match(cs, "DROP TABLE IF EXISTS", fixed=TRUE)
})

test_that("TEMPORARY is generated", {
  cs <- create_statement(mod, src_table=src_tbl, src_schema=src_schema,
                         src_catalog=src_catalog, dest_table=dest_tbl,
                         dest_schema=dest_schema, dest_catalog=dest_catalog,
                         pk=pk, temporary=TRUE)

  expect_match(cs, "TEMPORARY", fixed=TRUE)
})

test_that("pk columns are included", {
  cs <- create_statement(mod, src_table=src_tbl, src_schema=src_schema,
                         src_catalog=src_catalog, dest_table=dest_tbl,
                         dest_schema=dest_schema, dest_catalog=dest_catalog,
                         pk=pk, temporary=TRUE)

  for(p in pk)
  {
    expect_match(cs, p, fixed=TRUE)
  }
})
