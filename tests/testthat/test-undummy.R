context("Reversing dummy expansion")

test_that("Dummies are reversed correctly", {
  fac <- c("age_bucket", "race", "state")
  
  dummies <- list(
    list("age_bucket18to29", expression(ifelse(age_bucket == "18to29", 1, 0))[[1]]),
    list("age_bucket30to45", expression(ifelse(age_bucket == "30to45", 1, 0))[[1]]),
    list("age_bucket46to60", expression(ifelse(age_bucket == "46to60", 1, 0))[[1]]),
    list("age_bucket60plus", expression(ifelse(age_bucket == "60plus", 1, 0))[[1]]),
    list("raceB", expression(ifelse(race == "B", 1, 0))[[1]]),
    list("raceH", expression(ifelse(race == "H", 1, 0))[[1]]),
    list("raceW", expression(ifelse(race == "W", 1, 0))[[1]]),
    list("stateOH", expression(ifelse(state == "OH", 1, 0))[[1]]),
    list("stateVA", expression(ifelse(state == "VA", 1, 0))[[1]]),
    list("statePA", expression(ifelse(state == "PA", 1, 0))[[1]]),
    list("stateFL", expression(ifelse(state == "FL", 1, 0))[[1]]),
    list("stateCA", expression(ifelse(state == "CA", 1, 0))[[1]]),
    list("stateMI", expression(ifelse(state == "MI", 1, 0))[[1]])
  )

  for(dm in dummies)
    expect_equal(undummy(dm[[1]], factors=fac), dm[[2]])
})