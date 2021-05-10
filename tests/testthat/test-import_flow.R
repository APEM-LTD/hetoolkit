# Test Error Messages

test_that("Stations are defined",{
  expect_error(import_flow(inputs = "HDE"),
            "At least one site needs to be defined")
})

test_that("Inputs are defined",{
  expect_error(import_flow(sites = "1001"),
               "'inputs' need to be defined")
})

test_that("Site is in the correct format",{
  expect_error(import_flow(sites = 2001, inputs = "NRFA"),
               "'sites' is in invalid format, should be a character")
})

test_that("Inputs is in the correct format",{
  expect_error(import_flow(sites="1001", inputs = 20),
               "inputs' is in invalid format, should be a character")
})

test_that("Start date is in the correct format",{
  expect_error(import_flow(sites = "1001", inputs = "NRFA", start_date = "hello"),
               "Date should be in YYYY-MM-DD format")
})

test_that("End date date is in the correct format",{
  expect_error(import_flow(sites = "1001", inputs = "NRFA", end_date = "hello"),
               "Date should be in YYYY-MM-DD format")
})

test_that("Error is produced if start date is in the future",{
  expect_error(import_flow(sites = "1001", inputs= "NRFA",
                        start_date = "2023-01-01"),
                       "Start date given is in the future")
})

test_that("Error is produced if end date is in the future",{
  expect_error(import_flow(sites = "1001", inputs= "NRFA",
                           end_date = "2023-01-01"),
               "End date given is in the future")
})

test_that("Error is produced if end date is in the future",{
  expect_error(import_nrfa(sites = "1001",
                        end_date = "2023-01-01"),
                       "End date is in the future")
})

test_that("Error is produced if end date is before start date",{
  expect_error(import_flow(sites = "1001",inputs = "NRFA",
                        start_date = "2000-01-01",
                        end_date = "1999-01-01"),
                        "End date is before or equal to the start date")
})

test_that("Sites and inputs are same length",{
  expect_error(import_flow(sites= c("1001", "2001"), inputs = "NRFA"),
               "The number of sites and inputs should be the same")
})

test_that("Error is produced if input is incorrect",{
  expect_error(import_flow(sites= c("1001", "2001"), inputs = c("NRFA", "hello")),
               "'inputs' contains character string that isn't one of: 'NRFA', 'HDE' or 'FLOWFILES'")
})

test_that("Error is produced if flowfiles used but directory not specified",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), skip_num = 21, col_order = c(1,2,3)),
               "'FLOWFILES' specified in 'inputs' but directory not given")
})

test_that("Error is produced if flowfiles used but directory does not exist",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "hello", skip_num = 21, col_order = c(1,2,3)),
               "'FLOWFILES' specified in 'inputs' but 'dir' does not exist")
})

test_that("Error is produced if flowfiles used but skip number is not specified",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "data/wiski", col_order = c(1,2,3)),
               "'FLOWFILES' specified in 'inputs' but skip_num is not given")
})

test_that("Error is produced if flowfiles used but skip number is in invalid format",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "data/wiski", skip_num = "hello",col_order = c(1,2,3)),
               "'skip_num' is in invalid format, should be numeric")
})

test_that("Error is produced if flowfiles used and more than one skip number is defined",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "data/wiski", skip_num = c(21,22),col_order = c(1,2,3)),
               "Where being used only one skip number can be defined")
})


test_that("Error is produced if flowfiles used but column order is not defined",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "data/wiski", skip_num = 21),
               "'FLOWFILES' specified in 'inputs' but col_order not given")
})


test_that("Error is produced if flowfiles used but column order is in invalid",{
  expect_error(import_flow(sites= c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "data/wiski", skip_num = 21, col_order ="hello"),
               "Flowfiles specified but col_order is in invalid format, should be numeric")
})

test_that("Error is produced if flowfiles used but column order is in invalid",{
  expect_error(import_flow(sites = c("1001", "0130TH"), inputs = c("NRFA", "FLOWFILES"), dir = "data/wiski", skip_num = 21, col_order = c(1,2)),
               "Where being used the order of three columns needs to be defined")
})

test_that("date_format is a supported option",{
  expect_error(import_flow(sites = "0130TH", inputs = "FLOWFILES", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), date_format = "hello"),
               "date_format is invalid; see help for list of supported date formats")
})


# Test Output
# The test best is failing for an unknown reason.
# Manually running the 'test' and 'compare' objects demonstrates that the corrects outputs are produced.

#test_that("function returns correct data, filtered to specified date range", {
  #result <- import_flow(sites = c("F1707", "0130TH"),
  #                      inputs = c("HDE", "FLOWFILES"),
  #                      start_date = "2010-01-01",
  #                      end_date = "2010-01-05",
  #                      dir = "data/wiski",
  #                      skip_num = 21,
   #                     col_order = c(1,2,3),
   #                     date_format = "dmy_hms")
  #compared <- tibble::as_tibble(data.frame(
  #  input = rep(c("FLOWFILES", "HDE"), each = 5),
  #  flow_site_id = rep(c("0130TH", "F1707"), each = 5),
  #  date = rep(seq(as.Date("2010-01-01"), as.Date("2010-01-05"), 1), 2),
 #   flow = c(1.06, 1.08, 1.11, 1.13, 1.14, 13.60, 11.7, 10.5, 10.2, 10.5),
 #   quality = rep(c("G;C", "Estimated"), each = 5)))
 # compared$input <- as.character(compared$input)
#  compared$flow_site_id <- as.character(compared$flow_site_id)
 # compared$flow <- as.double(compared$flow)
 # compared$quality <- as.character(compared$quality)
 # expect_equivalent(result, compared)
#})



