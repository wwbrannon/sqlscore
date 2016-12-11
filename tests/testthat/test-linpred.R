context("Linear predictors")

# Always available because it's in package:stats
test_that("glm objects are handled correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris)
  res <- expression(1 * 2.17126629215507 + Sepal.Width * 0.495888938388551 + Petal.Length * 0.829243912234806 +
                    Petal.Width * -0.315155173326474 + ifelse(Species =="versicolor", 1, 0) * -0.723561957780729 +
                    ifelse(Species == "virginica", 1, 0) * -1.02349781449083)[[1]]
  expect_equal(linpred(mod1), res)
  
  mod2 <- glm(Sepal.Length > 5.1 ~ Sepal.Width + Petal.Length + Petal.Width*Species,
              data=datasets::iris, family=binomial("logit"))
  res <- expression(1 * -28.1705053449217 + Sepal.Width * 6.14207788587817 + Petal.Length * 4.57943291393518 +
                    Petal.Width * -5.13144057928123 + ifelse(Species == "versicolor", 1, 0) * -9.12661855429427 +
                    ifelse(Species == "virginica", 1, 0) * -7.19944090181685 +
                    Petal.Width * ifelse(Species == "versicolor", 1, 0) * 11.7958718975238 +
                    Petal.Width * ifelse(Species == "virginica", 1, 0) * 4.99663234765107)[[1]]
  expect_equal(linpred(mod2), res)
  
  dat <- datasets::iris
  dat$Speciesvirginica <- runif(nrow(dat))
  mod3 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Width + Species + Speciesvirginica,
              data=dat, family=binomial("logit"))
  expect_error(linpred(mod3))
  
  mod4 <- glm(Sepal.Length > 5.5 ~ Sepal.Width + Petal.Length +
              I(Petal.Length^2) + Petal.Width*Species, data=datasets::iris,
              family=binomial("logit"))
  res <- expression(1 * -18.7181471807127 + Sepal.Width * 5.40597368654669 +
                    Petal.Length * -5.39778594822984 + Petal.Length^2 * 1.35117233348432 +
                    Petal.Width * 3.18808443447557 +
                    ifelse(Species == "versicolor", 1, 0) * 15.0754186236367 +
                    ifelse(Species == "virginica", 1, 0) * 1.0875945058869 +
                    Petal.Width * ifelse(Species == "versicolor", 1, 0) * -11.0712541155635 +
                    Petal.Width * ifelse(Species == "virginica", 1, 0) * -2.8775803835078)[[1]]
  expect_equal(linpred(mod4), res)
  
  mod5 <- glm(Sepal.Length > 5.4 ~ Sepal.Width + Petal.Length + I(Petal.Length^2)*Petal.Width + Species,
              data=datasets::iris, family=binomial("logit"))
  res <- expression(1 * -12.5343421022915 + Sepal.Width * 4.89428432396798 + Petal.Length * -6.83191369596457 +
                    Petal.Length^2 * 1.78558460676668 + Petal.Width * -5.48929854358815 +
                    ifelse(Species == "versicolor", 1, 0) * 9.26781644415418 +
                    ifelse(Species == "virginica", 1, 0) * 7.08069629143905 +
                    Petal.Length^2 * Petal.Width * -0.0812770979209563)[[1]]
  expect_equal(linpred(mod5), res)
})

# Always available because it's in package:stats
test_that("lm objects are handled correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  res <- expression(1 * 2.17126629215507 + Sepal.Width * 0.495888938388551 +
                    Petal.Length * 0.829243912234806 + Petal.Width * -0.315155173326474 +
                    ifelse(Species == "versicolor", 1, 0) * -0.723561957780729 +
                    ifelse(Species == "virginica", 1, 0) * -1.02349781449083)[[1]]
  expect_equal(linpred(mod1), res)
  
  mod2 <- glm(Sepal.Length > 5.1 ~ Sepal.Width + Petal.Length + Petal.Width*Species,
              data=datasets::iris)
  res <- expression(1 * -1.25282405639425 + Sepal.Width * 0.418955303990332 +
                    Petal.Length * 0.0846743947913316 + Petal.Width * -0.110360531989962 +
                    ifelse(Species == "versicolor", 1, 0) * 0.613665953192789 +
                    ifelse(Species == "virginica", 1, 0) * 0.985024678481778 +
                    Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.138972137824933 +
                    Petal.Width * ifelse(Species == "virginica", 1, 0) * -0.120777639794957)[[1]]
  expect_equal(linpred(mod2), res)
  
  dat <- datasets::iris
  dat$Speciesvirginica <- runif(nrow(dat))
  mod3 <- lm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Width + Species + Speciesvirginica, data=dat)
  expect_error(linpred(mod3))
  
  mod4 <- lm(Sepal.Length > 5.5 ~ Sepal.Width + Petal.Length +
              I(Petal.Length^2) + Petal.Width*Species, data=datasets::iris)
  res <- expression(1 * -1.43943112575401 + Sepal.Width * 0.249854275927215 +
                    Petal.Length * 0.519248409391791 + Petal.Length^2 * -0.0385741824495878 +
                    Petal.Width * -0.132602655638309 +
                    ifelse(Species == "versicolor", 1, 0) * -0.302845895786481 +
                    ifelse(Species == "virginica", 1, 0) * 0.257598536771361 +
                    Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.378876215002793 +
                    Petal.Width * ifelse(Species == "virginica", 1, 0) * 0.00251914065652157)[[1]]
  expect_equal(linpred(mod4), res)
  
  mod5 <- lm(Sepal.Length > 5.4 ~ Sepal.Width + Petal.Length + I(Petal.Length^2)*Petal.Width + Species,
             data=datasets::iris)
  res <- expression(1 * -1.10520884181372 + Sepal.Width * 0.251100958359728 +
                    Petal.Length * 0.187579915463766 + Petal.Length^2 * 0.015417547111206 +
                    Petal.Width * 0.17723467890755 +
                    ifelse(Species == "versicolor", 1, 0) * 0.281192253227174 +
                    ifelse(Species == "virginica", 1, 0) * 0.24503151729728 +
                    Petal.Length^2 * Petal.Width * -0.0123767801813735)[[1]]
  expect_equal(linpred(mod5), res)
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("bayesglm objects are handled correctly", {
    mod1 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                          data=datasets::iris)
    res <- expression(1 * 2.16746489171907 + Sepal.Width * 0.498241952725385 +
                      Petal.Length * 0.826542543418053 + Petal.Width * -0.31800128132871 +
                      ifelse(Species == "versicolor", 1, 0) * -0.710871735643408 +
                      ifelse(Species == "virginica", 1, 0) * -1.00574899620441)[[1]]
    expect_equal(linpred(mod1), res)
    
    mod2 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width*Species,
                          data=datasets::iris)
    res <- expression(1 * 1.99545967215538 + Sepal.Width * 0.506995212999199 +
                      Petal.Length * 0.867042200976733 + Petal.Width * 0.0188280540574158 +
                      ifelse(Species == "versicolor", 1, 0) * -0.194703506935769 +
                      ifelse(Species == "virginica", 1, 0) * -1.24974513059032 +
                      Petal.Width * ifelse(Species == "versicolor", 1, 0) * -0.744858892123401 +
                      Petal.Width * ifelse(Species == "virginica", 1, 0) * -0.25531224497399)[[1]]
    expect_equal(linpred(mod2), res)
    
    dat <- datasets::iris
    dat$Speciesvirginica <- runif(nrow(dat))
    mod3 <- arm::bayesglm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Width + Species + Speciesvirginica,
                          data=dat)
    expect_error(linpred(mod3))
    
    mod4 <- arm::bayesglm(Sepal.Length > 5.5 ~ Sepal.Width + Petal.Length +
                          I(Petal.Length^2) + Petal.Width*Species, data=datasets::iris)
    res <- expression(1 * -1.4346899233495 + Sepal.Width * 0.249586998117276 +
                      Petal.Length * 0.512013559420262 + Petal.Length^2 * -0.0377965691034411 +
                      Petal.Width * -0.112062767080678 +
                      ifelse(Species == "versicolor", 1, 0) * -0.281829200750801 +
                      ifelse(Species == "virginica", 1, 0) * 0.264591594904849 +
                      Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.352029568620347 +
                      Petal.Width * ifelse(Species == "virginica", 1, 0) * -0.015597021874813)[[1]]
    expect_equal(linpred(mod4), res)
    
    mod5 <- arm::bayesglm(Sepal.Length > 5.4 ~ Sepal.Width + Petal.Length +
                          I(Petal.Length^2)*Petal.Width + Species, data=datasets::iris)
    res <- expression(1 * -1.10543449173636 + Sepal.Width * 0.249139673873688 +
                      Petal.Length * 0.192019035159504 + Petal.Length^2 * 0.0152752748174516 +
                      Petal.Width * 0.181409173124436 +
                      Petal.Length^2 * Petal.Width * -0.0124807335510125 +
                      ifelse(Species == "versicolor", 1, 0) * 0.267492573720143 +
                      ifelse(Species == "virginica", 1, 0) * 0.228922506843958)[[1]]
    expect_equal(linpred(mod5), res)
  })
}

# Only run if installed
if("mboost" %in% installed.packages())
{
  test_that("glmboost objects are handled correctly", {
    mod1 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                             data=datasets::iris)
    res <- expression(1 * 2.47773332391217 + Sepal.Width * 0.536390255877286 +
                      Petal.Length * 0.460907829277574 +
                      ifelse(Species == "virginica", 1, 0) * -0.0192462659183535)[[1]]
    expect_equal(linpred(mod1), res)
    
    mod2 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + I(Petal.Length^2) + Petal.Length +
                             Petal.Width*Species, data=datasets::iris)
    res <- expression(1 * 3.43698205637091 + Sepal.Width * 0.42137619505081 +
                      Petal.Length^2 * 0.0616046072500591 +
                      ifelse(Species == "versicolor", 1, 0) * 0.0510858703485424 +
                      Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.0912283527275175)[[1]]
    expect_equal(linpred(mod2), res)
    
    dat <- datasets::iris
    dat$Speciesversicolor <- runif(nrow(dat))
    mod3 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Width + Species + Speciesversicolor,
                             data=dat)
    expect_error(linpred(mod3))
    
    mod4 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + I(Petal.Length^2) +
                             Petal.Width*Species, data=datasets::iris)
    res <- expression(1 * 3.43698205637091 + Sepal.Width * 0.42137619505081 +
                      Petal.Length^2 * 0.0616046072500591 +
                      ifelse(Species == "versicolor", 1, 0) * 0.0510858703485424 +
                      Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.0912283527275175)[[1]]
    expect_equal(linpred(mod4), res)
    
    mod5 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length +
                            I(Petal.Length^2)*Petal.Width + Species, data=datasets::iris)
    res <- expression(1 * 3.41937220521108 + Sepal.Width * 0.426321405614481 +
                      Petal.Length^2 * 0.0617291562521652 +
                      ifelse(Species == "versicolor", 1, 0) * 0.173093269190147)[[1]]
    expect_equal(linpred(mod5), res)
  })
}

# Only run if installed
if("glmnet" %in% installed.packages())
{
  test_that("cv.glmnet objects are handled correctly", {
    mod1 <- glmnet::cv.glmnet(as.matrix(datasets::iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]),
                              datasets::iris$Sepal.Length, nfolds=nrow(datasets::iris), grouped=FALSE)
    res <- expression(1 * 2.37744529257366 + Sepal.Width * 0.556167083965938 +
                      Petal.Length * 0.499465780735234 + Petal.Width * -0.0929620855657578)[[1]]
    expect_equal(linpred(mod1), res)
  })
}
