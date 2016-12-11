context("Score expressions")

# The method for bayesglm is too simple to need testing; the method for lm
# is too, honestly, but appears below anyway.

test_that("Custom links are handled correctly", {
  mod1 <- glm(Sepal.Length > 5.7 ~ Sepal.Width + Petal.Length + Petal.Width*Species,
              data=datasets::iris, family=binomial("probit"))
  res <- expression(probit(1 * -9.18013117208555 + Sepal.Width * 1.26395879330326 + 
                             Petal.Length * 2.57755373383459 + Petal.Width * -5.48066677666731 + 
                             ifelse(Species == "versicolor", 1, 0) * -3.15485475773281 + 
                             ifelse(Species == "virginica", 1, 0) * -3.57030608656997 + 
                             Petal.Width * ifelse(Species == "versicolor", 1, 0) * 4.02643831156467 + 
                             Petal.Width * ifelse(Species == "virginica", 1, 0) * 4.18477992288368))[[1]]
  expect_equal(rec_round(score_expression(mod1, response="probit")), rec_round(res))
})

test_that("Probit links are handled correctly", {
  mod1 <- glm(Sepal.Length > 5.7 ~ Sepal.Width + Petal.Length + Petal.Width*Species,
              data=datasets::iris, family=binomial("probit"))
  expect_error(score_expression(mod1))
})

test_that("Cauchit links are handled correctly", {
  mod1 <- glm(Sepal.Length > 5.1 ~ Sepal.Width + Petal.Length + Petal.Width*Species,
              data=datasets::iris, family=binomial("cauchit"))
  expect_error(score_expression(mod1))
})

test_that("Identity links are handled correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris)
  res <- expression(1 * 2.17126629215507 + Sepal.Width * 0.495888938388551 + Petal.Length * 0.829243912234806 +
                      Petal.Width * -0.315155173326474 + ifelse(Species =="versicolor", 1, 0) * -0.723561957780729 +
                      ifelse(Species == "virginica", 1, 0) * -1.02349781449083)[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
  
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  res <- expression(1 * 2.17126629215507 + Sepal.Width * 0.495888938388551 +
                      Petal.Length * 0.829243912234806 + Petal.Width * -0.315155173326474 +
                      ifelse(Species == "versicolor", 1, 0) * -0.723561957780729 +
                      ifelse(Species == "virginica", 1, 0) * -1.02349781449083)[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

test_that("Logit links are handled correctly", {
  mod1 <- glm(Sepal.Length > 5.1 ~ Sepal.Width + Petal.Length + Petal.Width*Species,
              data=datasets::iris, family=binomial("logit"))
  res <- expression(1/(1 + exp(-1 * (1 * -28.1705053449217 + Sepal.Width * 6.14207788587817 + 
                                     Petal.Length * 4.57943291393518 + Petal.Width * -5.13144057928123 + 
                                     ifelse(Species == "versicolor", 1, 0) * -9.12661855429427 + 
                                     ifelse(Species == "virginica", 1, 0) * -7.19944090181685 + 
                                     Petal.Width * ifelse(Species == "versicolor", 1, 0) * 11.7958718975238 + 
                                     Petal.Width * ifelse(Species == "virginica", 1, 0) * 4.99663234765107))))[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

test_that("Log links are handled correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris, family=gaussian("log"))
  res <- expression(exp(1 * 1.14570615455293 + Sepal.Width * 0.0829255074863113 + 
                        Petal.Length * 0.131548951139663 + Petal.Width * -0.0493444332122325 + 
                        ifelse(Species == "versicolor", 1, 0) * -0.0920150349079109 + 
                        ifelse(Species == "virginica", 1, 0) * -0.140778672241228))[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

test_that("Inverse links are handled correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris, family=gaussian("inverse"))
  res <- expression(1/(1 * 0.27071375348109 + Sepal.Width * -0.0125750515031621 + 
                       Petal.Length * -0.0202091205105734 + Petal.Width * 0.00681801333299591 + 
                       ifelse(Species == "versicolor", 1, 0) * 0.0102475228494179 + 
                       ifelse(Species == "virginica", 1, 0) * 0.0180539702471309))[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

test_that("Cloglog links are handled correctly", {
  mod1 <- glm(Sepal.Length > 5.7 ~ Sepal.Width + Petal.Width*Species,
              data=datasets::iris, family=binomial("cloglog"))
  res <- expression(1 - exp(-exp(1 * -7.90683351552009 + Sepal.Width * 1.57511824988054 +
                                 Petal.Width * -6.84936779695922 +
                                 ifelse(Species == "versicolor", 1, 0) * -0.504799492435291 +
                                 ifelse(Species == "virginica", 1, 0) * 4.73243611546338 +
                                 Petal.Width * ifelse(Species == "versicolor", 1, 0) * 9.75613646229436 +
                                 Petal.Width * ifelse(Species == "virginica", 1, 0) * 6.72890398992262)))[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

test_that("Sqrt links are handled correctly", {
  mod1 <- glm(round(Sepal.Length) ~ Sepal.Width + Petal.Width*Species,
              data=datasets::iris, family=poisson("sqrt"))
  res <- expression((1 * 1.71211067023206 + Sepal.Width * 0.152947671170431 +
                     Petal.Width * -0.00526986214817919 +
                     ifelse(Species == "versicolor", 1, 0) * 0.256588850905108 +
                     ifelse(Species == "virginica", 1, 0) * 0.226964083721003 +
                     Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.05410529838605 +
                     Petal.Width * ifelse(Species == "virginica", 1, 0) * 0.0855903045034646)^2)[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

test_that("1/mu^2 links are handled correctly", {
  mod1 <- glm(round(Sepal.Length) ~ Sepal.Width + Petal.Width*Species,
              data=datasets::iris, family=inverse.gaussian("1/mu^2"))
  res <- expression(1/sqrt(1 * 0.0665672117432278 + Sepal.Width * -0.00754068575623855 + 
                           Petal.Width * -0.00227240145803271 +
                           ifelse(Species == "versicolor", 1, 0) * -0.0163244625050677 +
                           ifelse(Species == "virginica", 1, 0) * -0.0172540930025977 +
                           Petal.Width * ifelse(Species == "versicolor", 1, 0) * 0.00093527179856655 +
                           Petal.Width * ifelse(Species == "virginica", 1, 0) * 0.00065842246928859))[[1]]
  expect_equal(rec_round(score_expression(mod1)), rec_round(res))
})

if("mboost" %in% installed.packages())
{
  test_that("glmboost is handled correctly", {
    mod1 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                           data=datasets::iris)
    res <- expression(1 * 2.47773332391217 + Sepal.Width * 0.536390255877286 +
                        Petal.Length * 0.460907829277574 +
                        ifelse(Species == "virginica", 1, 0) * -0.0192462659183535)[[1]]
    expect_equal(rec_round(score_expression(mod1)), rec_round(res))
    
    mod2 <- mboost::glmboost(as.factor(Sepal.Length > 5.1) ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                             data=datasets::iris, family=mboost::Binomial())
    expect_error(rec_round(score_expression(mod2)))
  })
}

if("glmnet" %in% installed.packages())
{
  test_that("cv.glmnet is handled correctly", {
    mod1 <- glmnet::cv.glmnet(as.matrix(datasets::iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]),
                              datasets::iris$Sepal.Length, nfolds=nrow(datasets::iris), grouped=FALSE)
    res <- expression(1 * 2.37744529257366 + Sepal.Width * 0.556167083965938 +
                        Petal.Length * 0.499465780735234 + Petal.Width * -0.0929620855657578)[[1]]
    expect_equal(rec_round(score_expression(mod1)), rec_round(res))
    
    mod2 <- glmnet::cv.glmnet(as.matrix(datasets::iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]),
                              datasets::iris$Sepal.Length > 5.0, nfolds=nrow(datasets::iris), grouped=FALSE,
                              family="binomial")
    res <- expression(1/(1 + exp(-1 * (1 * -9.68828381601703 + Sepal.Width * 2.17597981144076 + 
                                       Petal.Length * 1.47533194385693))))[[1]]
    expect_equal(rec_round(score_expression(mod2)), rec_round(res))
    
    mod3 <- glmnet::cv.glmnet(as.matrix(datasets::iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]),
                              round(datasets::iris$Sepal.Length), nfolds=nrow(datasets::iris), grouped=FALSE,
                              family="poisson")
    res <- expression(exp(1 * 1.3340811569889 + Sepal.Width * 0.0525446668415735 + 
                          Petal.Length * 0.0709480580630341))[[1]]
    expect_equal(rec_round(score_expression(mod3)), rec_round(res))
  })
}
