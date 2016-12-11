context("Linear predictors")

# Always available because it's in package:stats
test_that("glm offsets are extracted correctly", {
  
})

# Always available because it's in package:stats
test_that("lm offsets are extracted correctly", {
  
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("bayesglm objects are handled correctly", {
  
  })
}

# Only run if installed
if("mboost" %in% installed.packages())
{
  test_that("glmboost objects are handled correctly", {
  
  })
}

# Only run if installed
if("glmnet" %in% installed.packages())
{
  test_that("cv.glmnet objects are handled correctly", {
  
  })
}
