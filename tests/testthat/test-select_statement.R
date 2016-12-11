context("Select statement generation")

# Always available because it's in package:stats
test_that("glm objects are handled correctly", {
  mod1 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris)
  gen <- select_statement(mod1, src_table="src")
  res <- structure(paste0("SELECT id, 1.0 * 2.17126629215507 + \"Sepal.Width\" ",
                          "* 0.495888938388551 + \"Petal.Length\" * 0.829243912234806 ",
                          "+ \"Petal.Width\" * -0.315155173326474 + CASE WHEN (\"Species\" = 'versicolor') ",
                          "THEN (1.0) ELSE (0.0) END * -0.723561957780729 + CASE WHEN (\"Species\" = 'virginica') ",
                          "THEN (1.0) ELSE (0.0) END * -1.02349781449083 FROM \"src\""), class = c("sql", "character"))
  expect_equal(gen, res)
  
  mod2 <- glm(Sepal.Length > 5.0 ~ Sepal.Width + Petal.Length + Petal.Width + Species,
              data=datasets::iris, family=binomial("logit"))
  gen <- select_statement(mod2, src_table="src")
  res <- structure(paste0("SELECT id, 1.0 / (1.0 + EXP(-1.0 * (1.0 * -40.9486616902392 ",
                          "+ \"Sepal.Width\" * 9.45599426269695 + \"Petal.Length\" * 5.37350821985281 ",
                          "+ \"Petal.Width\" * 0.643343276782682 + CASE WHEN (\"Species\" = 'versicolor') ",
                          "THEN (1.0) ELSE (0.0) END * 0.0945379643229317 + CASE WHEN (\"Species\" = 'virginica') ",
                          "THEN (1.0) ELSE (0.0) END * -7.59422356504794))) FROM \"src\""),
                   class = c("sql", "character"))
  expect_equal(gen, res)
})

# Always available because it's in package:stats
test_that("lm objects are handled correctly", {
  mod1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
             data=datasets::iris)
  gen <- select_statement(mod1, src_table="src")
  res <- structure(paste0("SELECT id, 1.0 * 2.17126629215507 + \"Sepal.Width\" ",
                          "* 0.495888938388551 + \"Petal.Length\" * 0.829243912234806 ",
                          "+ \"Petal.Width\" * -0.315155173326474 + CASE WHEN (\"Species\" = 'versicolor') ",
                          "THEN (1.0) ELSE (0.0) END * -0.723561957780729 + CASE WHEN (\"Species\" = 'virginica') ",
                          "THEN (1.0) ELSE (0.0) END * -1.02349781449083 FROM \"src\""), class = c("sql", "character"))
  expect_equal(gen, res)
})

# Only run if installed
if("arm" %in% installed.packages())
{
  test_that("Bayesglm objects are handled correctly", {
    mod1 <- arm::bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                          data=datasets::iris)
    gen <- select_statement(mod1, src_table="src")
    res <- structure(paste0("SELECT id, 1.0 * 2.16746489171907 + \"Sepal.Width\" * 0.498241952725385 ",
                            "+ \"Petal.Length\" * 0.826542543418053 + \"Petal.Width\" * -0.31800128132871 ",
                            "+ CASE WHEN (\"Species\" = 'versicolor') THEN (1.0) ELSE (0.0) END ",
                            "* -0.710871735643408 + CASE WHEN (\"Species\" = 'virginica') THEN (1.0) ",
                            "ELSE (0.0) END * -1.00574899620441 FROM \"src\""), class = c("sql", "character"))
    expect_equal(gen, res)
  })
}

# Only run if installed
if("mboost" %in% installed.packages())
{
  test_that("glmboost objects are handled correctly", {
    mod1 <- mboost::glmboost(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
                             data=datasets::iris)
    gen <- select_statement(mod1, src_table="src")
    res <- structure(paste0("SELECT id, 1.0 * 2.47773332391217 + \"Sepal.Width\" * 0.536390255877286 ",
                            "+ \"Petal.Length\" * 0.460907829277574 + CASE WHEN (\"Species\" = 'virginica') ",
                            "THEN (1.0) ELSE (0.0) END * -0.0192462659183535 FROM \"src\""),
                     class = c("sql", "character"))
    expect_equal(gen, res)
  })
}

# Only run if installed
if("glmnet" %in% installed.packages())
{
  test_that("cv.glmnet objects are handled correctly", {
    mod1 <- glmnet::cv.glmnet(as.matrix(datasets::iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]),
                              datasets::iris$Sepal.Length, nfolds=nrow(datasets::iris), grouped=FALSE)
    gen <- select_statement(mod1, src_table="src")
    res <- structure(paste0("SELECT id, 1.0 * 2.37744529257366 + \"Sepal.Width\" * 0.556167083965938 ",
                            "+ \"Petal.Length\" * 0.499465780735234 + \"Petal.Width\" * -0.0929620855657578 ",
                            "FROM \"src\""), class = c("sql", "character"))
    expect_equal(gen, res)
  })
}
