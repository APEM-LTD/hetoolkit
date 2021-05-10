# Test Error Messages

expect_error(import_env(sites = data), "If specified, sites must be a vector")

expect_error(import_env(sites = "34330",
                            save_dir = "hello"),
             "Specified save directory does not exist")

expect_error(import_env(sites = "34330",
                            save = "hello"),
             "Save is not logical")

expect_error(import_env(sites = "34330",
                            save_dwnld = "hello"),
             "Save_dwnld is not logical")

# Test output

test_that("import_env constructs 'tibble' as expected", {
  result <- import_env(sites = "34330", save = TRUE)
  compared <- tibble::as_tibble(readRDS("INV_OPEN_DATA_SITE_F.rds"))
  expect_equivalent(result, compared)
})
