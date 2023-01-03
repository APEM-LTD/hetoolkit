# Test error messages

# Create example inputs
sleepstudy <- lme4::sleepstudy
model1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)


test_that("model is missing...", {
  expect_error(model_cv(data = sleepstudy,
                        group = "Subject",
                        k = 5,
                        r = 1,
                        control = NULL),
               "'model' is missing; specify a lmerMod, lmerTest or gam model object for cross-validation")
})


test_that("data is missing...", {
  expect_error(model_cv(model = model1,
                            group = "Subject",
                            k = 5,
                            r = 1,
                            control = NULL),
               "'data' is missing; specify dataframe or tibble containing data used for model calibration")
})

test_that("group is missing...", {
  expect_error(model_cv(model = model1,
                            data = sleepstudy,
                            k = 5,
                            r = 1,
                            control = NULL),
               "'group' is missing; specify the name of the random grouping factor in 'model'")
})

test_that("model is lmerMod or gam...", {
  expect_error(model_cv(model = "hello",
                            data = sleepstudy,
                            group = "Subject",
                            k = 5,
                            r = 1,
                            control = NULL),
               "'model' must be a lmerMod or gam object")
})

test_that("data is a data frame or tibble", {
  expect_error(model_cv(model = model1,
                            data = "hello",
                            group = "Subject",
                            k = 5,
                            r = 1,
                            control = NULL),
               "'data' must be a dataframe or tibble")
})


test_that("control is a lmerControl object...", {
  expect_error(model_cv(model = model1,
                            data = sleepstudy,
                            group = "Subject",
                            k = 5,
                            r = 1,
                            control = "hello"),
               "'control' must be a lmerControl object")
})

test_that("group is contained in data", {
  expect_error(model_cv(model = model1,
                            data = sleepstudy,
                            group = "hello",
                            k = 5,
                            r = 1,
                            control = NULL),
               "'hello' cannot be found in 'data'")
})

test_that("k is numeric", {
  expect_error(model_cv(model = model1,
                        data = sleepstudy,
                        group = "Subject",
                        k = "hello",
                        r = 1,
                        control = NULL),
               "'k' must be an integer >=1")
})

test_that("k is an integer", {
  expect_error(model_cv(model = model1,
                        data = sleepstudy,
                        group = "Subject",
                        k = 5.5,
                        r = 1,
                        control = NULL),
               "'k' must be an integer >=1")
})

test_that("k is greater than or equal to 1", {
  expect_error(model_cv(model = model1,
                        data = sleepstudy,
                        group = "Subject",
                        k = 0,
                        r = 1,
                        control = NULL),
               "'k' must be an integer >=1")
})


test_that("r is numeric", {
  expect_error(model_cv(model = model1,
                        data = sleepstudy,
                        group = "Subject",
                        k = 5,
                        r = "hello",
                        control = NULL),
               "'r' must be an integer >=1")
})

test_that("r is an integer", {
  expect_error(model_cv(model = model1,
                        data = sleepstudy,
                        group = "Subject",
                        k = 5,
                        r = 1.5,
                        control = NULL),
               "'r' must be an integer >=1")
})

test_that("r is greater than or equal to 1", {
  expect_error(model_cv(model = model1,
                        data = sleepstudy,
                        group = "Subject",
                        k = 5,
                        r = 0,
                        control = NULL),
               "'r' must be an integer >=1")
})

test_that("correct number and names of columns in output", {
  p <- model_cv(model = model1,
           data = sleepstudy,
           group = "Subject",
           k = 5,
           r = 2,
           control = NULL)
  expect_equal(colnames(p[[2]])[-c(1:3)],
               expected = c("pred_cv1","pred_cv2"))
})


