rm(list = ls())

library(tibble)
library(schoolmath)
library(imputeTS)
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)
library(lubridate)

data_impute <- readxl::read_excel("data/flow.example_impute.xlsx")
data_equipercentile <- readxl::read_excel("data/flow.example_impute_equipercentile.xlsx")
donor_data <- readxl::read_excel("data/donor.xlsx")

# linear
output <- impute_flow(data_impute, site_col = "site", date_col = "date", flow_col = "flow", method = "linear")

# exponential
output <- impute_flow(data_impute, site_col = "site", date_col = "date", flow_col = "flow", method = "exponential")

# eqipercentile (w/o donor)
output <- impute_flow(data_equipercentile, site_col = "site", date_col = "date", flow_col = "flow", method = "equipercentile")

#equipercentile (w/ donor)
output <- impute_flow(data_equipercentile, site_col = "site", date_col = "date", flow_col = "flow", method = "equipercentile", donor = donor_data)

#Save an object to a file
saveRDS(data_impute, file = "tests/testthat/data_impute.RDS")
#Restore the object
readRDS(file = "tests/testthat/data_impute.rds")

saveRDS(donor_data,"tests/testthat/donor_data.RDS")
readRDS(file = "tests/testthat/donor_data.rds")

saveRDS(data_equipercentile,"tests/testthat/data_equipercentile.RDS")
readRDS(file = "tests/testthat/data_equipercentile.RDS")
