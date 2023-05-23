# Manual test needed vs RICT output

# Test Error Messages

test_that("Save directory error message is functioning", {

  env_data <- readRDS("env_data_predict.rds")

expect_error(predict_indices(env_data = env_data,
                             save_dir = "hello"),
             "Specified save directory does not exist")

})

test_that("save = logical error message is functioning", {

  env_data <- readRDS("env_data_predict.rds")

expect_error(predict_indices(env_data = env_data,
                        save = "hello"),
             "Save is not logical")

})

test_that("env_data exists...", {

  expect_error(predict_indices(env_data = "hello"),
               "Environmental data file does not exist")

})

test_that("all_indices is logical...", {

  env_data <- readRDS("env_data_predict.rds")

  expect_error(predict_indices(env_data = env_data,
                               all_indices = "hello"),
               "all_indices must be logical")
})

test_that("file_format...", {

  env_data <- readRDS("env_data_predict.rds")

  expect_error(predict_indices(env_data = env_data,
                               file_format = "hello"),
               "file_format must be specified as EDE or RICT")

})


test_that("predict_indices constructs expected output...'", {

  env_data <- readRDS("env_data_predict.rds")

  result <- predict_indices(env_data = env_data)

  compared <- readRDS("predict_data_output.rds")
  #result <- result[names(compared)]
  #expect_equivalent(result, compared)
  expect_equal(result, compared, ignore_attr = TRUE)

})
