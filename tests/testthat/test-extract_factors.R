context("Factor extraction")

# Always available because it's in package:stats
test_that("glm coefficients are extracted correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris)
  expect_equal(extract_factors(mod1), c("Species"))

  mod2 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width,
              data=datasets::iris, family=binomial("logit"))
  expect_equal(extract_factors(mod2), character(0))

  mod3 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris, contrasts=list(Species="contr.poly"))
  expect_error(extract_factors(mod3))

})

# Always available because it's in package:stats
test_that("lm coefficients are extracted correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  expect_equal(extract_factors(mod1), c("Species"))

  mod2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
             data=datasets::iris)
  expect_equal(extract_factors(mod2), character(0))
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("bayesglm coefficients are extracted correctly", {
    mod1 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                          data=datasets::iris)
    expect_equal(extract_factors(mod1), c("Species"))

    mod2 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                          data=datasets::iris)
    expect_equal(extract_factors(mod2), character(0))
  })
}

# Only run if installed
if("mboost" %in% installed.packages())
{
  test_that("glmboost coefficients are extracted correctly", {
    mod1 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                             data=datasets::iris)
    expect_equal(extract_factors(mod1), c("Species"))

    mod2 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                             data=datasets::iris)
    expect_equal(extract_factors(mod2), character(0))
  })
}

# glmnet only takes pre-expanded matrices, so no factor expansion, so no tests to do
