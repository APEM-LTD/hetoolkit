# Test Error Messages

test_that("warning if duplicated site id",{
  expect_warning(import_flowfiles(sites =c("0130TH", "0130TH"), dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3)))
})

test_that("Error if directory does not exist",{
  expect_error(import_flowfiles(sites = "0130TH" , dir = "hello" , skip_num = 21, col_order = c(1,2,3)),
             "Specified directory does not exist")
})

test_that("Error if skip number is not defined",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = , col_order = c(1,2,3)),
             "The number of rows to be skipped before reading the data needs to be defined")
})

test_that("Error if col_order is not defined",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21),
             "The columns containing the data of interest need to be defined")
  })

test_that("Error if col_order is not length 3",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(0)),
               "'col_order' must have three elements")
})

test_that("Error if col_order has NA in first two elements ",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(NA,1,2)),
               "first two elements of 'col_order' can't be NA")
})

test_that("date_format is a supported option",{
  expect_error(import_flowfiles(sites = "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), date_format = "hello"),
               "date_format is invalid; see help for list of supported date formats")
})

test_that("Error if start date is in the wrong format",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), start_date="hello"),
             "Date should be in YYYY-MM-DD format")
})

test_that ("Error if end date is in the wrong format",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), end_date="hello"),
             "Date should be in YYYY-MM-DD format")
})

test_that ("Error if start date is in the future",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), start_date="2022-01-01"),
             "Start date given is in the future")
})

test_that ("Error if end date is in the future",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), end_date="2022-01-01"),
             "End date given is in the future")
})

test_that ("Error if end date is before or equal to start date",{
  expect_error(import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), start_date= "2020-01-01", end_date="2019-01-01"),
             "End date is before start date")
})

test_that ("Error if no files found for the specified sites ",{
  expect_error(import_flowfiles(sites = "hello", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3)),
               "No files found for the specified sites")
})

test_that ("Error if no files of supported formats found in dir ",{
  expect_error(import_flowfiles(dir = "data/wiski2" , skip_num = 21, col_order = c(1,2,3)),
               "No csv, xls, xlsx, all or txt files found in dir")
})

test_that("warning if files can't be found for some sites",{
  expect_warning(import_flowfiles(sites =c("0130TH", "hello", "hello2"), dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3)),
                 "Warning: No files found for the following sites: hello, hello2")
})


# Test output

test_that("function imports data from XLSX file", {
  result <- import_flowfiles(sites =c("0130TH"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hms", start_date = "2005-01-01", end_date = "2005-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("0130TH",5),
                         date = seq(as.Date("2005-01-01"), as.Date("2005-01-05"), 1),
                         flow = c(0.805, 0.786, 0.783, 0.771, 0.753),
                         quality = rep("G;C",5)))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

test_that("function imports data from XLS file", {
  result <- import_flowfiles(sites =c("0130THb"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hms", start_date = "2005-01-01", end_date = "2005-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("0130THb",5),
                                           date = seq(as.Date("2005-01-01"), as.Date("2005-01-05"), 1),
                                           flow = c(0.805, 0.786, 0.783, 0.771, 0.753),
                                           quality = rep("G;C",5)))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

test_that("function imports data from CSV file", {
  result <- import_flowfiles(sites =c("0630TH"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hm", start_date = "2005-01-01", end_date = "2005-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("0630TH",5),
                                           date = seq(as.Date("2005-01-01"), as.Date("2005-01-05"), 1),
                                           flow = c(0.575, 0.575, 0.569, 0.567, 0.562),
                                           quality = rep("G;C",5)))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

test_that("function imports data from ALL file", {
  result <- import_flowfiles(sites =c("033006"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hms", start_date = "2005-01-01", end_date = "2005-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("033006",5),
                                           date = seq(as.Date("2005-01-01"), as.Date("2005-01-05"), 1),
                                           flow = c(2.34, 2.26, 2.09, 2.04, 2),
                                           quality = rep("40 (G)  calc",5)))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

test_that("function imports data from TXT file", {
  result <- import_flowfiles(sites =c("0131TH"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hm", start_date = "2005-01-01", end_date = "2005-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("0131TH",5),
                                           date = seq(as.Date("2005-01-01"), as.Date("2005-01-05"), 1),
                                           flow = c(0.805, 0.786, 0.783, 0.771, 0.753),
                                           quality = rep("G;C",5)))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

test_that("output contains expected number of rows", {
  output <- import_flowfiles(sites = "0130TH", dir = "data/wiski",skip_num=21, col_order = c(1,2,3),start_date = "2001-01-01", end_date = "2002-01-01")
  compare <- (((as.Date(max(output$date), format="%Y/%m/%d")-(as.Date(min(output$date),"%Y/%m/%d")))+1) * length(unique(output$flow_site_id)))
  final_output <- as.numeric(compare)
  final_compare <- as.numeric(nrow(output))
  expect_equal(final_output, final_compare)
})

test_that("start_date and end_date filters work", {
  result <- import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), start_date= "2001-01-01",end_date="2002-01-01")
  result <- min(as.Date(result$date))
  compared <- as.Date("2001-01-01")
  expect_equal(result, compared)
})

test_that("returns NA if no data in specified date range", {
  result <- import_flowfiles(sites =  "0130TH", dir = "data/wiski" , skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hms", start_date= "1900-01-01", end_date = "1900-01-02")
  result <- as.numeric(result$flow)
  compared <- as.numeric(c(NA,NA))
  expect_equal(result, compared)
})

test_that("function works if csv file has only two columns", {
  result <- import_flowfiles(sites =c("0630TH_2cols"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,NA), date_format = "dmy_hm", start_date = "2005-01-01", end_date = "2005-01-05")
  result <- as.numeric(result$flow)
  compared <- c(0.575, 0.575, 0.569, 0.567, 0.562)
  expect_equal(result, compared)
})

test_that("function works if xlsx file has only two columns", {
  result <- import_flowfiles(sites =c("0130TH_2cols"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,NA), date_format = "dmy_hm", start_date = "2005-01-01", end_date = "2005-01-05")
  result <- as.numeric(result$flow)
  compared <- c(0.805, 0.786, 0.783, 0.771, 0.753)
  expect_equal(result, compared)
})

test_that("function returns NA when flow is non-numerical (CSV file)", {
  result <- import_flowfiles(sites =c("0630TH"), dir = "data/wiski", skip_num = 21, col_order = c(1,2,3), date_format = "dmy_hm", start_date = "1980-01-01", end_date = "1980-01-05")
  compared <- tibble::as_tibble(data.frame(flow_site_id = rep("0630TH",5),
                                           date = seq(as.Date("1980-01-01"), as.Date("1980-01-05"), 1),
                                           flow = NA,
                                           quality = rep("M;I",5)))
  compared$flow_site_id <- as.character(compared$flow_site_id)
  compared$flow <- as.double(compared$flow)
  compared$quality <- as.character(compared$quality)
  expect_equivalent(result, compared)
})

