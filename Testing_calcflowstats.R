rm(list = ls())

library(tibble)
library(schoolmath)
library(imputeTS)
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)
library(lubridate)
library(CircStats)

# read-in data
# most simple dataset, 2 sites called Site1 and Site2
data_calcfs <- readxl::read_excel("data/flow.example_calc_fs.xlsx")
# must ensure dates are in date format
data_calcfs$date <- as.Date(data_calcfs$date)
saveRDS(data_calcfs, "tests/testthat/data_calcfs.rds")

# read-in data
# EA's example data
data_du2v <- read.csv("data/DU2V_modelled_flows.csv")
data_du2v$Day <- as.Date(data_du2v$Day)
saveRDS(data_du2v, "tests/testthat/data_du2v.rds")

# test function
mylist <- calc_flowstats(data = data_calcfs, site_col = "site", flow_col = "flow", date_col = "date", q_low = 95, q_high = 50)
df1 <- mylist[[1]]
df2 <- mylist[[2]]

# test function with imputed col
mylist <- calc_flowstats(data = data, site_col = "site", flow_col = "flow", date_col = "date", q_low = 95, q_high = 50, imputed_col = "imputed")
df1 <- mylist[[1]]
df2 <- mylist[[2]]

# test function using reference col
mylist <- calc_flowstats(data = data, site_col = "site", flow_col = "flow", date_col = "date", q_low = 95, q_high = 50, ref_col = "ref")
df1 <- mylist[[1]]
df2 <- mylist[[2]]
