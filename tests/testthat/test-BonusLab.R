data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("lenreg rejects errounous input", {
  expect_error(linreg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris,1))
  expect_error(linreg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})


test_that("The coefficients are right", {
  
  mod_object <- ridgereg(Petal.Length~Species, data = iris, lambda=0)
  # From running MASS::lm.ridge(Petal.Length~Species, data = iris, we get 1.462, 2.798 and 4.090
  expect_equal(all.equal(mod_object$regressions_coefficients[1], 1.462), TRUE)
  expect_equal(all.equal(mod_object$regressions_coefficients[2], 2.798), TRUE)
  expect_equal(all.equal(mod_object$regressions_coefficients[3], 4.090), TRUE)
})