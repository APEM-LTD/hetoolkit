test_that("multiplication works", {
  #   expect_equal(2 * 2, 4)
  # })

  library(dplyr)

  ### Test errors

  # "Data frame not found"
  expect_error(impute_flow(data ="hello", site_col = "flow_site_id", date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Data frame not found")

  expect_error(impute_flow(data = NULL, site_col = "flow_site_id", date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Data frame not found")

  ## SD - what if its not a data frame but data frame type data available that could be converted?

  # read in testing data
 # data_impute<-readRDS(file = "tests/testthat/data_impute.rds")
  names(data_impute)<-c("flow_site_id", "date","flow")
  data_impute$date <- as.Date(data_impute$date)

  # "Specified date column was not identified in data"
  expect_error(impute_flow(data=data_impute, site_col = "flow_site_id", date_col = "hello",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Specified date column was not identified in data")

  # "Default 'date' column was not identified in data"
  expect_error(impute_flow(data=data_impute[,c(1,3)], site_col = "flow_site_id", date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Default 'date' column was not identified in data")

  # "Duplicate dates identified"
  expect_error(impute_flow(data = rbind(data_impute,data_impute), site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Duplicate dates identified")

  # "flow data supplied is not on a daily time-step"
  data_a <- data_impute[seq(1, nrow(data_impute), 2), ]
  expect_error(impute_flow(data = data_a, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "flow data supplied is not on a daily time-step")

  # date_col not in date (yyyy-mm-dd) format
  data_x <- data_impute
  data_x$date <- as.character(data_x$date)
  expect_error(impute_flow(data = data_x, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "date_col must be of date yyyymmdd format")

  ## SD - timezone missing warning - ok?

  # "Specified site column was not identified in data"
  expect_error(impute_flow(data=data_impute, site_col = "hello", date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Specified site column was not identified in data")

  # "Default 'flow_site_id' column was not identified in data"
  expect_error(impute_flow(data=data_impute[,c(2,3)], site_col = "flow_site_id", date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Default 'flow_site_id' column was not identified in data")

  # "Specified flow column was not identified in data"
  expect_error(impute_flow(data=data_impute, site_col = "flow_site_id", date_col = "date",
                           flow_col = "hello", method = "linear", donor = NULL)
               , "Specified flow column was not identified in data")

  # "Default 'flow' column was not identified in data"
  expect_error(impute_flow(data=data_impute[,c(1,2)], site_col = "flow_site_id", date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Default 'flow' column was not identified in data")

  # "Specified flow_col is not numeric"
  data_a<-data_impute
  data_a$flow<-as.character(data_a$flow)
  expect_error(impute_flow(data = data_a, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "linear", donor = NULL)
               , "Specified flow_col is not numeric")


  # "Specified 'method' must be one of 'linear', 'exponential', or 'equipercentile'"
  expect_error(impute_flow(data = data_impute, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "hello", donor = NULL)
               , "Specified 'method' must be one of 'linear', 'exponential', or 'equipercentile'")

  # read in donor data
  #donor_data<-readRDS(file = "C:/Users/Sarah Davie/Desktop/R function writing/2022/hetoolkit_updated/tests/testthat/donor_data.rds")
  # read in equipercentile data
  #data_equipercentile<-readRDS(file = "C:/Users/Sarah Davie/Desktop/R function writing/2022/hetoolkit_updated/tests/testthat/data_equipercentile.RDS")
  names(data_equipercentile)<-c("flow_site_id", "date", "flow")
  data_equipercentile$date <- as.Date(data_equipercentile$date)


  # check warning for donor not used when adding donor to method other than equipercentile
  expect_warning(impute_flow(data = data_impute, site_col = 'flow_site_id', date_col = "date",
                             flow_col = "flow", method = "linear", donor = donor_data)
                 , "Donor stations will not be used; to use donor stations, select 'method = equipercentile'")

  # donor in list format (not data frame or tibble)
  expect_error(impute_flow(data = data_equipercentile, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "equipercentile", donor = list(c(1,3,4,8)))
               , "Donor stations must be specified in data frame or tibble format")

  # "'donor', must contain at least 2 columns; the first a list of flow sites requiring imputation, and the second a list of paired donor sites"
  expect_error(impute_flow(data = data_equipercentile, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "equipercentile", donor = donor_data[,1])
               , "'donor', must contain at least 2 columns; the first a list of flow sites requiring imputation, and the second a list of paired donor sites")

  # "A donor site was not specified for site", sep = "-", i
  expect_warning(impute_flow(data = data_equipercentile, site_col = 'flow_site_id', date_col = "date",
                             flow_col = "flow", method = "equipercentile", donor = donor_data[-3,])
                 , paste("A donor site was not specified for site",sep="-",4082))

  # "A minumum of two flow site stations are required if applying equipercentile method"
  data_a <- subset(data_equipercentile,flow_site_id == 4032)
  expect_error(impute_flow(data = data_a, site_col = 'flow_site_id', date_col = "date",
                           flow_col = "flow", method = "equipercentile", donor = donor_data[1,])
               , "A minimum of two flow stations are required if applying equipercentile method")


  # Equipercentile method cannot be applied for this site, due to insufficient overlapping data with the donor site
  data_a <-   data_equipercentile %>%
    dplyr::group_by(flow_site_id) %>%
    dplyr::slice(1:200)
  expect_warning(impute_flow(data = data_a[data_a$flow_site_id%in% c(4082,4046),], site_col = 'flow_site_id', date_col = "date",
                             flow_col = "flow", method = "equipercentile", donor = donor_data)
                 , "4046-Equipercentile method cannot be applied for this site, due to insufficient overlapping data with the donor site.")


  # Test if only NAs
  # Create dataset
  my_flow_data <- import_flow(sites = c("F1707", "1001"),
                              inputs = c("HDE", "NRFA"),
                              start_date = "2008-01-01",
                              end_date = "2010-01-05")

  # what happens if site only has NA flows?
  my_flow_data2 <- my_flow_data[1:10,]
  my_flow_data2$flow <- NA
  my_flow_data2$flow <- as.numeric(my_flow_data2$flow)

  # test warning
  expect_error( impute_flow(data = my_flow_data2, site_col = "flow_site_id", date_col = "date",
                              flow_col = "flow", method = "linear")
                 , "Only one flow site specified. Flow site contains only NAs")


})



### Test outputs ###

  test_that("impute_flow constructs expected output using 'linear'", {

    impute_test_gap$date <- as.Date(impute_test_gap$date)

    result <- impute_flow(data = impute_test_gap, site_col = "site", date_col = "date",
                          flow_col = "flow", method = "linear")

    compared <- readRDS(file = "impute_linear.rds")
    expect_equivalent(result, compared)

})


  test_that("impute_flow constructs expected output using 'exponential'", {

    impute_test_gap$date <- as.Date(impute_test_gap$date)

    result <- impute_flow(data = impute_test_gap, site_col = "site", date_col = "date",
                          flow_col = "flow", method = "exponential")
    compared <- readRDS(file = "impute_exponential.rds")
    expect_equivalent(result, compared)

  })


  test_that("impute_flow constructs expected output using 'equipercentile'", {

    data_equipercentile$date <- as.Date(data_equipercentile$date)

    result <- impute_flow(data = data_equipercentile, site_col = "site", date_col = "date",
                          flow_col = "flow", method = "equipercentile")
    compared <-  readRDS(file = "impute_equipercentile.rds")
    expect_equal(result, compared)

  })


  test_that("impute_flow constructs expected output using 'equipercentile' plus 'donor'", {

    data_equipercentile$date <- as.Date(data_equipercentile$date)

    result <- impute_flow(data = data_equipercentile, site_col = "site", date_col = "date",
                          flow_col = "flow", method = "equipercentile", donor = donor_data)
    compared <-  readRDS(file = "impute_equipercentile_donor.rds")
    expect_equal(result, compared)

  })
