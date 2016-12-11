context("Coefficient extraction")

# Set up objects shared across all tests
mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
            data=datasets::iris)

mod2 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
            data=datasets::iris, family=binomial("logit"))

x <- matrix(rnorm(100*20),100,20)
colnames(x) <- sapply(1:20, function(x) paste0("X", as.character(x)))
x <- as.data.frame(x)
mod3 <- glm(X2 ~ X3 + X5 + X15*X8, data=x)

# Always available because it's in package:stats
test_that("glm coefficients are extracted correctly", {
  
})

# Always available because it's in package:stats
test_that("lm coefficients are extracted correctly", {
  
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
