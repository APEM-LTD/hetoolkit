# Test error messages

# Create example inputs
sleepstudy <- lme4::sleepstudy
model1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)


test_that("model is missing...", {
  expect_error(model_logocv(data = sleepstudy,
                        group = "Subject",
                        control = NULL),
               "'model' is missing; specify a lmerMod, lmerTest or gam model object for cross-validation")
})


test_that("data is missing...", {
  expect_error(model_logocv(model = model1,
                            group = "Subject",
                            control = NULL),
               "'data' is missing; specify dataframe or tibble containing data used for model calibration")
})

test_that("group is missing...", {
  expect_error(model_logocv(model = model1,
                            data = sleepstudy,
                            control = NULL),
               "'group' is missing; specify the name of the random grouping factor in 'model'")
})

test_that("model is lmerMod or gam...", {
  expect_error(model_logocv(model = "hello",
                            data = sleepstudy,
                            group = "Subject",
                            control = NULL),
               "'model' must be a lmerMod or gam object")
})

test_that("data is a data frame or tibble", {
  expect_error(model_logocv(model = model1,
                            data = "hello",
                            group = "Subject",
                            control = NULL),
               "'data' must be a dataframe or tibble")
})


test_that("control is a lmerControl object...", {
  expect_error(model_logocv(model = model1,
                            data = sleepstudy,
                            group = "Subject",
                            control = "hello"),
               "'control' must be a lmerControl object")
})

test_that("group is contained in data", {
  expect_error(model_logocv(model = model1,
                            data = sleepstudy,
                            group = "hello"),
               "'hello' cannot be found in 'data'")
})




