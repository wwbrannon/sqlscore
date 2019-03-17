context("Offsets extraction")

# Always available because it's in package:stats
test_that("glm offsets are extracted correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + offset(Petal.Width) + Species,
              data=datasets::iris)
  res <- c(Petal.Width=1)
  expect_equal(sort(extract_offsets(mod1)), sort(res))

  mod2 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + offset(Petal.Length) + offset(Petal.Width) + Species,
              data=datasets::iris, family=binomial("logit"))
  res <- c(Petal.Length=1, Petal.Width=1)
  expect_equal(sort(extract_offsets(mod2)), sort(res))
})

# Always available because it's in package:stats
test_that("lm offsets are extracted correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + offset(Petal.Length) + offset(Petal.Width) + Species,
             data=datasets::iris)
  res <- c(Petal.Length=1, Petal.Width=1)
  expect_equal(sort(extract_offsets(mod1)), sort(res))
})

# Only run if installed
if(require("arm"))
{
  test_that("bayesglm offsets are extracted correctly", {
    mod1 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + offset(Petal.Width) + Species,
                          data=datasets::iris)
    res <- c(Petal.Width=1)
    expect_equal(sort(extract_offsets(mod1)), sort(res))

    mod2 <- arm::bayesglm(Sepal.Length > 5.0 ~ Sepal.Width + offset(Petal.Length) + offset(Petal.Width) + Species,
                data=datasets::iris, family=binomial("logit"))
    res <- c(Petal.Length=1, Petal.Width=1)
    expect_equal(sort(extract_offsets(mod2)), sort(res))
  })
}

# mboost and glmnet don't support glm()-style offsets, so no tests
