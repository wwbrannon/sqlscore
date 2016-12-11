context("Coefficient extraction")

# Always available because it's in package:stats
test_that("glm coefficients are extracted correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris)
  res <- c(`(Intercept)`=2.17126629215507, Sepal.Width=0.495888938388551,
           Petal.Length=0.829243912234806, Petal.Width=-0.315155173326474,
           Speciesversicolor=-0.723561957780729, Speciesvirginica=-1.02349781449083)
  expect_equal(extract_coef(mod1), res) #to within numerical tolerance
  
  mod2 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris, family=binomial("logit"))
  res <- c(`(Intercept)`=-40.9486616902392, Sepal.Width=9.45599426269695,
           Petal.Length=5.37350821985281, Petal.Width=0.643343276782682,
           Speciesversicolor=0.0945379643229317, Speciesvirginica=-7.59422356504794)
  expect_equal(extract_coef(mod2), res) #to within numerical tolerance
})

# Always available because it's in package:stats
test_that("lm coefficients are extracted correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  res <- c(`(Intercept)`=2.17126629215507, Sepal.Width=0.495888938388551,
           Petal.Length=0.829243912234806, Petal.Width=-0.315155173326474,
           Speciesversicolor=-0.723561957780729, Speciesvirginica=-1.02349781449083)
  expect_equal(extract_coef(mod1), res) #to within numerical tolerance
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
