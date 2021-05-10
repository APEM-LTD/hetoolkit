# Test Error Messages
site.model.flows<-readRDS("site.model.flows.RDS")

# is q ok?
expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT",
  q = NULL), '"q" missing, with no default.')

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT",
  q = 101), '"q" must between 1 and 99.')

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT",
  q = 0.25), '"q" must be an interger.')


# is data there and a dataframe?
expect_error(calc_rfrstats(
  data = NULL, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95
  ), "data missing, with no default.")

expect_error(calc_rfrstats(
  data = "hello", site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95
  ), "data input must be a dataframe.")


# what about data columns needed - all been listed?
expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = NULL, ref_col = "Flow_NAT", q=95), "flow_col is missing.")

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", q=95,
  ref_col = NULL), "ref_col is missing.")

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  date_col = NULL), "date_col is missing.")

expect_error(calc_rfrstats(
  data=site.model.flows, date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  site_col = NULL), "site_col is missing.")

# is the column name in the dataset?
expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", ref_col = "Flow_NAT", q=95,
  flow_col = "hello"), "hello cannot be found in input dataframe.")

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", q=95,
  ref_col = "hello"), "hello cannot be found in input dataframe.")

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  date_col = "hello"), "hello cannot be found in input dataframe.")

expect_error(calc_rfrstats(
  data=site.model.flows, date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  site_col = "hello"), "hello cannot be found in input dataframe.")

# # is the class of the column correct? ## not sure how these will work
a1 <-site.model.flows
a1$Flow_HIST <- as.character(a1$Flow_HIST)

expect_error(calc_rfrstats(
  data=a1, site_col="SITE_ID", date_col="Date_end", ref_col = "Flow_NAT", q=95,
  flow_col = "Flow_HIST"), "flow_col must be numeric.")

a2 <- site.model.flows
a2$Flow_NAT <- as.character(a2$Flow_NAT)

expect_error(calc_rfrstats(
  data=a2, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", q=95,
  ref_col = "Flow_NAT"), "ref_col must be numeric.")

a3 <- site.model.flows
a3$Date_end <- as.character(a3$Date_end)

expect_error(calc_rfrstats(
  data=a3, site_col="SITE_ID", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  date_col = "Date_end"), "date_col must be of Date class")

# test save results file errors:
expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95, save = TRUE,
  save_dir = "hello"), "Specified save directory does not exist")

expect_error(calc_rfrstats(
  data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  save = "hello"), "Save is not logical")


## test warnings
expect_warning(calc_rfrstats(
  data=rbind(site.model.flows,site.model.flows[1:50,]), site_col="SITE_ID", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95,
  date_col = "Date_end"), "there are multiple rows with the same site ID and date combination.")



## Test NA functionality
test_that("calc_rfrstats runs with NAs as expected", {
          temp<- site.model.flows
          temp$Flow_HIST[temp$SITE_ID==33889 & temp$Date_end<"1970-10-01"]<-NA
          result <- calc_rfrstats(data=temp, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95)
          compared <- tibble::as_tibble(readRDS("INV_CALC_RFRSTATS_NA_HANDELING.rds"))
          expect_equal(result, compared,ignore_attr = TRUE)
        })

## Test output
test_that("calc_rfrstats constructs 'tibble' as expected", {
          result <- calc_rfrstats(data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95)
          compared <- tibble::as_tibble(readRDS("INV_CALC_RFRSTATS_TIBBLE.rds"))
          expect_equal(result, compared,ignore_attr = TRUE)
          })
