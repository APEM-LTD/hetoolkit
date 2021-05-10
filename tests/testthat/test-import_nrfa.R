# Test Error Messages

test_that("Stations are defined",{
  expect_error(import_nrfa(),
            "Need at least one station ID")
})

test_that("Warning is produced for duplicate sites",{
  expect_warning(import_nrfa(sites =c("1001","1001")),
                 "Warning 1 duplicate site id detected and ignored")
})

test_that("tidyup argument is logical",{
  expect_error(import_nrfa(sites =c("1001"), tidyup = "hello"),
               "tidyup argument must be TRUE or FALSE")
})

test_that("Start date is in the correct format",{
  expect_error(import_nrfa(sites = "1001",
            start_date = "hello"),
        "Date should be in YYYY-MM-DD format")
})

test_that("End date is in the correct format",{
  expect_error(import_nrfa(sites = "1001",
                        end_date = "hello"),
           "Date should be in YYYY-MM-DD format")
})

test_that("Error is produced if start date is in the future",{
  expect_error(import_nrfa(sites = "1001",
                        start_date = "2023-01-01"),
                       "Start date is in the future")
})


test_that("Error is produced if end date is in the future",{
  expect_error(import_nrfa(sites = "1001",
                        end_date = "2023-01-01"),
                       "End date is in the future")
})

test_that("Error is produced if end date is before start date",{
  expect_error(import_nrfa(sites = "1001",
                        start_date = "2000-01-01",
                        end_date = "1999-01-01"),
                        "End date is before start date")
})


# Test Output

test_that("output contains expected number of rows", {
  output <- import_nrfa(sites = "1001", start_date = "2010-01-01", end_date = "2010-01-05")
  n <- dim(output)[1]
  expect_equal(n,5)
})

test_that("duplicate sites imported only once", {
  output <- import_nrfa(sites = c("1001", "1001"), start_date = "2010-01-01", end_date = "2010-01-05")
  n <- dim(output)[1]
  expect_equal(n,5)
})

test_that("invalid sites not included in output", {
  output <- import_nrfa(sites = c("1001", "hello"), start_date = "2010-01-01", end_date = "2010-01-05")
  n <- dim(output)[1]
  expect_equal(n,5)
})

test_that("function returns correct flow data", {
  result <- import_nrfa(sites =c("1001"), start_date = "2005-01-01", end_date = "2005-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("1001",5),
                                           date = seq(as.Date("2005-01-01"), as.Date("2005-01-05"), 1),
                                           flow = c(5.576,5.447, 6.963, 6.497, 5.972),
                                           quality = NA))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$flow <- as.double(compared$flow)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

test_that("returns NA if no data in specified date range", {
  result <- import_nrfa(sites = "2001", start_date= "1900-01-01", end_date = "1900-01-02")
  result <- as.numeric(result$flow)
  compared <- as.numeric(c(NA,NA))
  expect_equal(result, compared)
})

test_that("NAs in sites list are ignored", {
  output <- import_nrfa(sites = c("1001", NA), start_date = "2010-01-01", end_date = "2010-01-05")
  n <- dim(output)[1]
  expect_equal(n,5)
})

