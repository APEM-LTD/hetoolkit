library(lubridate)
library(dplyr)


test_that("test data is valid...", {

  expect_error(calc_flowstats(data = "hello",
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow")
               , "Data frame not found")

# data not in a data frame
  expect_error(calc_flowstats(data = list(site_col = c(1:3), date_col = c("01-01-2020","02-01-2020","03-01-2020"),flow_col = c(1:3)),
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow")
              , "Data frame not found")
})

test_that("date_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "hello",
                              flow_col = "Flow")
              , "Specified date column was not identified in data")
})

test_that("default date_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              flow_col = "Flow")
              , "Default 'date' column was not identified in data")
})

test_that("site_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "hello",
                              date_col = "Date",
                              flow_col = "Flow")
               , "Specified site column was not identified in data")
})


test_that("default site_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              date_col = "Date",
                              flow_col = "Flow")
              , "Default 'flow_site_id' column was not identified in data")
})

## test that date is a date


test_that("flow_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "hello")
              , "Specified flow column was not identified in data")
})


test_that("default flow_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date")
            , "Default 'flow' column was not identified in data")
})

test_that("flow_col is numeric...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")
  testdata_flowstats$Flow<-as.character(testdata_flowstats$Flow)

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow")
              , "Specified flow_col is not numeric")
})

test_that("Specified imputed_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              imputed_col = "hello",
                              flow_col = "Flow")
              , "Specified imputed_col was not identified in data")
})

 test_that("Specified imputed_col is valid integer within data...", {

   testdata_flowstats <- readRDS("testdata_flowstats.rds")
   testdata_flowstats$imputed <- rep(0.0,nrow(testdata_flowstats))
   testdata_flowstats$imputed[1:10] <- 0.2

   expect_error(calc_flowstats(data = testdata_flowstats,
                               site_col = "site",
                               date_col = "Date",
                             imputed_col = "imputed",
                               flow_col = "Flow")
                , "imputed_col values must be integer value of 0 or 1")
 })

 test_that("Specified imputed_col range is valid within data...", {

   testdata_flowstats <- readRDS("testdata_flowstats.rds")
   testdata_flowstats$imput <- rep(0,nrow(testdata_flowstats))
   testdata_flowstats$imput[1:10] <- 2

   expect_error(calc_flowstats(data = testdata_flowstats,
                               site_col = "site",
                               date_col = "Date",
                               imputed_col = "imput",
                               flow_col = "Flow")
                , "imputed_col values must be integer value of 0 or 1")
 })

test_that("ref_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow",
                              ref_col = "hello")
              , "Specified ref_col was not identified in data")
})

test_that("ref_col is numeric...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")
  testdata_flowstats$Ref<-as.character(testdata_flowstats$Flow)

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow",
                              ref_col = "Ref")
             ,  "Specified ref_col is not numeric")
})

test_that("duplicate dates...", {

  testdata_flowstats1 <- readxl::read_excel("flow.example_duplicate.xlsx")

  expect_error(calc_flowstats(data = testdata_flowstats1,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow")
                ,   "Duplicate dates identified")
})

test_that("win start valid format...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              win_start = "20-02-1920")
              , "win_start should be in YYYY-MM-DD format")
})

### gives a warning "All formats failed to parse. No formats found."

test_that("win start in past...", {

  data_calcfs <- readRDS("data_calcfs.rds")
  s.d<- Sys.Date()+1

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              win_start = s.d)
               , "win_start is in the future")
})

test_that("win width valid...", { # "win_width must by in day, month, or year format"

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              win_width = "6")
               , "win_width must be in day, month, or year format")
})

### doesnt like things like weekly, bi weekly - gives error when run of "Error in seq.int(0, to0 - from, by) : 'to' must be a finite number"


# "win_step must by in day, month, or year format"
test_that("win step valid...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              win_step = "6")
               , "win_step must be in day, month, or year format")
})

test_that("date_range in valid format...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              date_range = c("1920-01-01", "01-01-2020"))
               , "date_range should be in YYYY-MM-DD format")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              date_range = c("01-01-1920", "2020-01-01"))
               , "date_range should be in YYYY-MM-DD format")
})

### gives a warning "All formats failed to parse. No formats found."

test_that("correct number of dates in date_range...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              date_range = c("1920-01-01","2010-01-01", "2020-01-01"))
               , "date_range should be of maximum length 2")
})


test_that("start date before end date in date_range...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              date_range = c("2020-01-01","2010-01-01"))
               , "start date exceeds end date, please check date_range")
})


# ## this error works when running the function but test doesn't work...
 test_that("start date of date_range appropriate...", {

   data_calcfs <- readRDS("data_calcfs.rds")

   expect_error(calc_flowstats(data = data_calcfs,
                               site_col = "site",
                               date_col = "date",
                               flow_col = "flow",
                               imputed_col = "imputed",
                               ref_col =  "ref",
                               date_range = c(Sys.Date()+1,Sys.Date()+2))
                , "date_range 1 is in the future")
 })

 ## this error works when running the function but test doesn't work...
 test_that("end date of date_range appropriate...", {

   data_calcfs <- readRDS("data_calcfs.rds")

   expect_error(calc_flowstats(data = data_calcfs,
                               site_col = "site",
                               date_col = "date",
                               flow_col = "flow",
                               imputed_col = "imputed",
                               ref_col =  "ref",
                               date_range = c(Sys.Date()-1,Sys.Date()+2))
                , "date_range 2 is in the future")
 })

### can start and end date be the same?

test_that("scaling is logical...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              scaling = "hello")
               , "'scaling' is not logical")
})

test_that("q_low appropraite...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              q_low = 0)
               , "q_low must be a value between 1 and 99")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              q_low = 101)
               , "q_low must be a value between 1 and 99")

  ### this test doesn't work but error works when running the function
   expect_error(calc_flowstats(data = data_calcfs,
                               site_col = "site",
                               date_col = "date",
                               flow_col = "flow",
                               imputed_col = "imputed",
                               ref_col =  "ref",
                               q_low = 10.5)
              , "q_low must be an integer between 1 and 99")
})

test_that("q_high appropraite...", {

  data_calcfs <- readRDS("data_calcfs.rds")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              q_high = 0)
               , "q_high must be a value between 1 and 99")

  expect_error(calc_flowstats(data = data_calcfs,
                              site_col = "site",
                              date_col = "date",
                              flow_col = "flow",
                              imputed_col = "imputed",
                              ref_col =  "ref",
                              q_high = 101)
               , "q_high must be a value between 1 and 99")

  # ## this test doesn't work but error works when running the function
   expect_error(calc_flowstats(data = data_calcfs,
                               site_col = "site",
                               date_col = "date",
                               flow_col = "flow",
                               imputed_col = "imputed",
                               ref_col =  "ref",
                               q_high = 10.5)
                , "q_high must be an integer between 1 and 99")
})




#test_that("constructs expected output", {


# data_calcfs <- readRDS("data_calcfs.rds")

# flow_stats <- calc_flowstats(data = data_calcfs,
                            #site_col = "site",
                            #date_col = "date",
                            #flow_col = "flow")

# result <- flow_stats[[1]]
# compared_1 <- readRDS("data_cfs.rds")
# compared <- compared_1[[1]]

# expect_equivalent(result, compared)

#})

test_that("correct start date is generated (based on win_start)", {

  expected <- readRDS("data_cfs_sd.rds")
  data_calcfs <- readRDS("data_calcfs.rds")

  flow_stats <- calc_flowstats(data = data_calcfs,
                               site_col = "site", flow_col = "flow",
                               date_col = "date",
                               q_low = 95,
                               q_high = 50,
                               win_start = "2009-01-01",
                               win_step = "6 months",
                               win_width = "6 months")

  result <- min(flow_stats[[1]]$start_date)
  compared <- min(expected$start_date)

  expect_equivalent(result, compared)

})


test_that("correct end date is generated (based on win_start, win_width, win_step)", {

  expected <- readRDS("data_cfs_sd.rds")
  data_calcfs <- readRDS("data_calcfs.rds")

  flow_stats <- calc_flowstats(data = data_calcfs,
                               site_col = "site", flow_col = "flow",
                               date_col = "date",
                               q_low = 95,
                               q_high = 50,
                               win_start = "2009-01-01",
                               win_step = "6 months",
                               win_width = "6 months")

  result <- max(flow_stats[[1]]$end_date)
  compared <- max(expected$end_date)

  expect_equivalent(result, compared)

})


test_that("q_low changes", {

  data_calcfs <- readRDS("data_calcfs.rds")

  test1 <- calc_flowstats(data = data_calcfs,
                               site_col = "site", flow_col = "flow",
                               date_col = "date",
                               q_low = 95,
                               q_high = 50,
                               win_start = "2009-01-01",
                               win_step = "6 months",
                               win_width = "6 months")

  test2 <- calc_flowstats(data = data_calcfs,
                          site_col = "site", flow_col = "flow",
                          date_col = "date",
                          q_low = 70,
                          q_high = 50,
                          win_start = "2009-01-01",
                          win_step = "6 months",
                          win_width = "6 months")

  test1_data <- test1[[1]]
  test2_data <- test2[[2]]
  test <- isTRUE(test1_data$low_n[3] > test2_data$low_n[3])

  testthat::expect_false(test)

})

test_that("q_high changes", {

  data_calcfs <- readRDS("data_calcfs.rds")

  test1 <- calc_flowstats(data = data_calcfs,
                          site_col = "site", flow_col = "flow",
                          date_col = "date",
                          q_low = 95,
                          q_high = 10,
                          win_start = "2009-01-01",
                          win_step = "6 months",
                          win_width = "6 months")

  test2 <- calc_flowstats(data = data_calcfs,
                          site_col = "site", flow_col = "flow",
                          date_col = "date",
                          q_low = 95,
                          q_high = 50,
                          win_start = "2009-01-01",
                          win_step = "6 months",
                          win_width = "6 months")

  test1_data <- test1[[1]]
  test2_data <- test2[[2]]
  test <- isTRUE(test1_data$low_n[3] > test2_data$low_n[3])

  testthat::expect_false(test)

})

test_that("different flow stats produced when ref_col is/isn't applied", {

  data_calcfs <- readRDS("data_calcfs.rds")

  test1 <-  calc_flowstats(data = data_calcfs,
                           site_col = "site",
                           date_col = "date",
                           flow_col = "flow",
                           imputed_col = "imputed",
                           ref_col =  "ref")


  test1_data <- test1[[1]]
  test <- isTRUE(all(test1_data$Q95z == test1_data$Q95z_ref))

  testthat::expect_false(test)

})


#test_that("min_7day etc are NA if data is not daily", {

 # data_calcfs <- readRDS("data_calcfs.rds")
#
  #data_calcfs$month <- lubridate::floor_date(data_calcfs$date, "month")

  #my_data <- data_calcfs %>%
  #            group_by(site) %>%
  #            summarise(flow = mean(flow),
 #                       month = mean(month))
#
 # test1 <-  calc_flowstats(data = my_data,
#                           site_col = "site",
  #                         date_col = "month",
 #                          flow_col = "flow")
#
#
 # test1_data <- test1[[1]]
 # test <- isTRUE(TRUE %in% is.na(test1$min_7day))

#  testthat::expect_false(test)

#})


test_that("stats are are NA if n_data is not sufficient", {

 data_short <- readRDS("short_data.rds")

 test1 <-  calc_flowstats(data = data_short,
                           site_col = "site",
                         date_col = "date",
                          flow_col = "flow")


 test1_data <- test1[[1]]
 test <- isTRUE(FALSE %in% is.na(test1$Q95_z))

  testthat::expect_false(test)

})





