rm(list = ls())

## should all be loaded by package
library(devtools)
library(tibble)
library(schoolmath)
library(imputeTS)
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggfortify)

# read-in data
data_du2v <- read.csv("data/DU2V_modelled_flows.csv")
data_impute <- readxl::read_excel("data/flow.example_impute.xlsx")
data_equipercentile <- readxl::read_excel("data/flow.example_impute_equipercentile.xlsx")
donor_data <- readxl::read_excel("data/donor.xlsx")
impute_test_original <- readxl::read_excel("data/impute_test_original.xlsx")
impute_test_gap <- readxl::read_excel("data/impute_test_gap.xlsx")

#Save to RDA or RDS for testing
save(impute_test_gap, file = "data/impute_test_gap.rda")
saveRDS(test, "tests/testthat/impute_linear.rds")
saveRDS(impute_exponential, "tests/testthat/impute_exponential.rds")
saveRDS(impute_equipercentile, "tests/testthat/impute_equipercentile.rds")
save(impute_test_original, file = "data/impute_test_original.rda")

#Save to RDA or RDS for testing
save(data_du2v, file = "data/data_du2v.rda")
saveRDS(data_du2v, "tests/testthat/data_du2v.rds")

save(data_impute, file = "data/data_impute.rda")
saveRDS(data_impute, file = "tests/testthat/data_impute.RDS")
#Restore the object
readRDS(file = "tests/testthat/data_impute.rds")

save(donor_data, file = "data/donor_data.rda")
saveRDS(donor_data,"tests/testthat/donor_data.RDS")
readRDS(file = "tests/testthat/donor_data.rds")

save(data_equipercentile, file = "data/data_equipercentile.rda")
saveRDS(data_equipercentile,"tests/testthat/data_equipercentile.RDS")
readRDS(file = "tests/testthat/data_equipercentile.RDS")

data <- readRDS(file = "data/cb_flow_data.rds")
donor <- readRDS(file = "data/cb_donor.rds")

## examples of funtion usage
# linear
data_impute$date <- as.Date(data_impute$date)
output <- impute_flow(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", method = "linear")

# exponential
output <- impute_flow(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", method = "exponential")

# eqipercentile (w/o donor)
output <- impute_flow(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", method = "equipercentile")

#equipercentile (w/ donor)
output <- impute_flow(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", method = "equipercentile", donor = donor)

