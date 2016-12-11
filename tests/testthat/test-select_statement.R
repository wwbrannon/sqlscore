context("Select statement generation")

mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
           data=datasets::iris)

src_tbl <- "foo bar"
src_schema <- "baz"
src_catalog <- "quux"
src <- fqtn(src_tbl, schema=src_schema, catalog=src_catalog)

pk <- c("customerid", "state")

ss <- select_statement(mod, src_table=src_tbl, src_schema=src_schema,
                       src_catalog=src_catalog, pk=pk)

test_that("src table name is included", {
  expect_match(ss, src, fixed=TRUE)
})

test_that("pk columns are included", {
  for(p in pk)
  {
    expect_match(ss, p, fixed=TRUE)
  }
})
