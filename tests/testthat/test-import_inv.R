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

expect_error(import_inv(source = "hello"),
             "Download format must be parquet or csv, or a valid filepath must be specified")

expect_error(import_inv(source = "parquet",
                        biol_dir = "INV_OPEN_DATA_METRICS_F.rds"),
             "Set source = NULL if using biol_dir")

expect_warning(import_inv(source = NULL,
                          biol_dir = "INV_OPEN_DATA_METRICS_F.rds"),
               "In function import_inv, biol_dir argument deprecated. File paths can be specified using source.")

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
          result <- result[1:53,]
          expect_equivalent(result, compared)
          })

# Test date processing correct
test_that("Date variables not processed if already in date format", {
  result <- import_inv(source = "testdate_import_inv_dates.rds", sites = "34330", start_date = "1995-01-01")
  compared <- tibble::as_tibble(readRDS("testdata_import_inv_dates_compared.rds"))
  expect_equivalent(result, compared)
})

