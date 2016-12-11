context("Coefficient extraction")

# Always available because it's in package:stats
test_that("glm coefficients are extracted correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris)
  
  mod2 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris, family=binomial("logit"))
  
})

# Always available because it's in package:stats
test_that("lm coefficients are extracted correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  
  
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("bayesglm coefficients are extracted correctly", {
    
  })
}

# Only run if installed
if("mboost" %in% installed.packages())
{
  test_that("glmboost coefficients are extracted correctly", {
  
  })
}

# Only run if installed
if("glmnet" %in% installed.packages())
{
  test_that("cv.glmnet coefficients are extracted correctly", {
  
  })
}
