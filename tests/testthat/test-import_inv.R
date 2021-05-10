# Test Error Messages

expect_error(import_inv(sites = data),
             "If specified, sites must be a vector")

expect_error(import_inv(sites = "34330",
                            start_date = "hello"),
             "Date should be in YYYY-MM-DD format")

expect_error(import_inv(sites = "34330",
                            end_date = "hello"),
             "Date should be in YYYY-MM-DD format")

expect_error(import_inv(sites = "34330",
                            save_dir = "hello"),
             "Specified save directory does not exist")

expect_error(import_inv(sites = "34330",
                            save = "hello"),
             "Save is not logical")

expect_error(import_inv(sites = "34330",
                            save_dwnld = "hello"),
             "Save_dwnld is not logical")

# Test start date

test_that("start_date filter works", {
  result <- import_inv(sites = "34330",
                           start_date = "2000-01-01")

  result <- min(result$SAMPLE_DATE)

  compared <- as.Date("2000-01-01")

  expect_gt(result, compared)
})

# Test end date

test_that("end_date filter works", {
  result <- import_inv(sites = "34330",
                           end_date = "2020-01-01")

  result <- max(result$SAMPLE_DATE)

  compared <- as.Date("2020-01-01")

  expect_lt(result, compared)
})


# Test output

test_that("import_inv constructs 'tibble' as expected", {
          result <- import_inv(sites = "34330")
          compared <- tibble::as_tibble(readRDS("INV_OPEN_DATA_METRICS_F.rds"))
          expect_equivalent(result, compared)
          })

