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
  expect_equal(extract_coef(mod2), res)
})

# Always available because it's in package:stats
test_that("lm coefficients are extracted correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  res <- c(`(Intercept)`=2.17126629215507, Sepal.Width=0.495888938388551,
           Petal.Length=0.829243912234806, Petal.Width=-0.315155173326474,
           Speciesversicolor=-0.723561957780729, Speciesvirginica=-1.02349781449083)
  expect_equal(extract_coef(mod1), res)
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("bayesglm coefficients are extracted correctly", {
    mod1 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                          data=datasets::iris)
    res <- c(`(Intercept)`=2.16746489171907, Sepal.Width=0.498241952725385,
             Petal.Length=0.826542543418053, Petal.Width=-0.31800128132871,
             Speciesversicolor=-0.710871735643408, Speciesvirginica=-1.00574899620441)
    expect_equal(extract_coef(mod1), res)
  })
}

# Only run if installed
if("mboost" %in% installed.packages())
{
  test_that("glmboost coefficients are extracted correctly", {
    mod1 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                             data=datasets::iris)
    res <- c(`(Intercept)`=2.47773332391217, Sepal.Width=0.536390255877286,
             Petal.Length=0.460907829277574, Speciesvirginica=-0.01924627)
    expect_equal(extract_coef(mod1), res)
  })
}

# Only run if installed
if("glmnet" %in% installed.packages())
{
  test_that("cv.glmnet coefficients are extracted correctly", {
    mod1 <- glmnet::cv.glmnet(as.matrix(datasets::iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]),
                              datasets::iris$Sepal.Length, nfolds=nrow(datasets::iris), grouped=FALSE)
    res <- c(`(Intercept)`=2.37744529, Sepal.Width=0.55616708,
             Petal.Length=0.49946578, Petal.Width=-0.09296209)
    expect_equal(extract_coef(mod1), res)
  })
}
