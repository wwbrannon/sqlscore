context("Create statement generation")

# Always available because it's in package:stats
test_that("Linear models are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Binomial GLMs are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Poisson GLMs are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Gamma GLMs are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Inverse-Gaussian GLMs are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Quasi GLMs are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Quasi-binomial GLMs are handled correctly", {
  
})

# Always available because it's in package:stats
test_that("Quasi-Poisson GLMs are handled correctly", {
  
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("Bayesglm objects are handled correctly", {
    
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
