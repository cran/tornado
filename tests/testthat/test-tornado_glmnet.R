context("test-tornado_glmnet")

test_that("tornado glmnet works", {
  testthat::skip_if_not_installed("glmnet")

  mf <- model.frame(mpg ~ cyl*wt*hp, data = mtcars)
  mm <- model.matrix(mf, mf)
  gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  torn <- tornado(gtest, modeldata = mtcars, form = formula(mpg ~ cyl*wt*hp), s = "lambda.1se",
                  type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: glmnet")
  plot(g)
})
