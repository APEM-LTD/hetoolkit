# Test Error Messages


test_that("a station ID is defined",{
  expect_error(import_hde(sites = ),
            "Need at least one station ID")
})

test_that("warning is produced for duplicate sites",{
  expect_warning(import_hde(sites =c("5339TH","5339TH")))
})

test_that("warning is produced if site is not in hde data list",{
  expect_warning(import_hde(sites =c("5339TH", "hello", "hello2")),
                 "Could not find the following stations:  hello, hello2")
})

test_that("start date is in the correct format",{
  expect_error(import_hde(sites = "5339TH",
            start_date = "hello"),
          "Date should be in YYYY-MM-DD format")
})

test_that("end date is in the correct format",{
  expect_error(import_hde(sites = "F1707",
                        end_date = "hello"),
           "Date should be in YYYY-MM-DD format")
})

test_that("error is produced if start date is in the future",{
  expect_error(import_hde(sites = "F1707", start_date = "2030-01-01"),
                       "Start date given is in the future")
})

test_that("error is produced if end date is in the future",{
  expect_error(import_hde(sites = "F1707",
                        end_date = "2030-01-01"),
                       "End date given is in the future")
})

test_that("error is end date is before the start date",{
  expect_error(import_hde(sites = "F1707",
                        start_date = "2000-01-01",
                        end_date = "1999-01-01"),
                        "End date provided is before start date")
})

test_that("error is produced if no matching stations found in hde data",{
  expect_error(import_hde(sites = "hello"),
             "Cant find any matching HDE stations")
})


# Test Output

test_that("start_date filter works", {
  result <- import_hde(sites = "5339TH", start_date = "2000-01-01")
  result <- min(as.Date(result$date))
  compared <- as.Date("2000-01-01")
  expect_equal(result, compared)
})

test_that("end_date filter works", {
  result <- import_hde(sites = "5339TH", end_date = "2000-01-01")
  result <- max(as.Date(result$date))
  compared <- as.Date("2000-01-01")
  expect_equal(result, compared)
})

test_that("correct number of rows returned", {
  result <- import_hde(sites = "5339TH", start_date = "2020-01-01", end_date = "2020-01-05")
  n <- dim(result)[1]
  expect_equal(n, 5)
})


