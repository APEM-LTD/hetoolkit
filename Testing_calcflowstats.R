rm(list = ls())

IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}


library(tibble)
library(schoolmath)
library(imputeTS)
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)
library(lubridate)
library(CircStats)

data <- readxl::read_excel("data/flow.example_impute.xlsx")
data <- readxl::read_excel("data/flow.example_calc_impute.xlsx")
data$date <- as.Date(data$date)
win_width <- "6 months"
win_step <- "6 months"
win_start <- "2010-01-01"
mylist <- calc_flowstats(data = data, site_col = "site", flow_col = "flow", date_col = "date", q_low = 95, q_high = 50)
view <- mylist[[1]]



win_width <- "6 months"
width <- data.frame(win_width)
width_sep <- tidyr::separate(width, win_width, c("no", "period"))

test <- FULL.FLOW.REC %>%
  mutate(win_no_2 = win_no) %>%
  group_by(site, win_no_2) %>%
  group_modify(~ (CALCFLOWSTATS(win_no, .)))

data <- readxl::read_excel("data/flow.example_impute_equipercentile.xlsx")
donor_data <- readxl::read_excel("data/donor.xlsx")

data <- readxl::read_excel("data/flow.example_impute_equipercentile_overlap.xlsx")
donor <- readxl::read_excel("data/donor_overlap.xlsx")

output <- impute_flow(data, site_col = "site", date_col = "date", flow_col = "flow", method = "equipercentile", donor = donor_data)

# pull-in site_col

data$Date <- as.Date(data$Date)

data$Site <- dplyr::pull(data, Site)
data$Flow <- dplyr::pull(data, Flow)
data$Date <- dplyr::pull(data, Date)

for(i in unique(data$Site)){

  # filter and pull-in data

  data_f <- dplyr::filter(data, Site == i)

  data_f$Date <- dplyr::pull(data_f, Date)
  data_f$Flow <- dplyr::pull(data_f, Flow)

}
  # Check flow is numeric

  if(is.numeric( data_f$Flow) == FALSE)
  {stop("Specified flow_col is not numeric")}

  # Check for duplicate dates

  duplicates <- data_f$Date[duplicated(data_f$Date)]

  if(length(duplicates) >= 1)
  { print(duplicates)
    stop("Duplicate dates identified")}

  # Use expand grid to set-up a date range, from minimum flow date to present day

  all_dates <- expand.grid(date = seq(min(data$date), max(data$date), "days"), stringsAsFactors = FALSE)
  dim(all_dates)

  # Join all dates with observed flow data, retaining NAs where flow data is missing

  all_flow <- left_join(x = all_dates, y = data_f, by = "Date")
  all_flow <- all_flow %>% mutate(Site = "Site 2") # change to i

  # Linear method

  if(method = "linear"){

    # Filter to identify min and max dates which are not NAs

    all_flow_f <- filter(all_flow, !is.na(Flow))
    maxdate <- max(all_flow_f$Date)
    mindate <- min(all_flow_f$Date)

    # Impute flow data for the filtered dataset, and assign imputed flow to the a new column 'Flow_i'

    all_flow_i <- all_flow %>%
      filter(Date <= maxdate & Date >= mindate)

    all_flow_i$Flow_i <- na_interpolation(all_flow_i$Flow)

    # Merge datasets, so that the imputed flows sit within the full date range

    all_flow <- left_join(all_flow, all_flow_i, by = c("Date", "Site", "Flow"))

    # Flag imputed data

   all_flow <- all_flow %>%
           mutate(imputed = ifelse(is.na(Flow) & !is.na(Flow_i), 1, 0))

   # Drop extra flow column column

   all_flow$Flow <- all_flow$Flow_i
   drops <- "Flow_i"
   all_flow <- all_flow[,!(names(all_flow) %in% drops)]



    # (flag Yes or No based on NAs & numeric values in original flow column alongside numeric or NA in imputed flow column)
    # (overwrite flow column with imputed flow column and remove imputed flow column)

  }
}


data_equ <- dplyr::filter(data, site == i)

data_equ$Date <- dplyr::pull(data_equ, date_col)
data_equ$Flow <- dplyr::pull(data_equ, flow_col)

data_equ <- filter(data, Site == "Site2")
all_flow <- filter(data, Site == "Site2")

corr <- cor.test(x=all_flow$Flow, y=data_equ$Flow, method = 'spearman')
corr_rho <- as.data.frame(corr$estimate)
corr_rho$flow_site_id <- i
assign(paste(i, sep = "_", "corr_rho"), corr_rho)

}

corr_all <- c(mget(ls(pattern = "_corr_rho")))
corr_all <- Reduce('rbind', corr_all)

setDT(corr_rho)[ , .SD[which.max(corr_rho$`corr$estimate`)]]

donor <- corr_all$flow_site_id


########## equipercentile

plot(ecdf(data_0$Flow))
ecdf(data_0$Flow)(2)

raw_val <- data_0$Flow

data_0$percentile <- ecdf(data_0$Flow)(raw_val)

testing <- data_0 %>%
            mutate(., raw_val, ~ecdf(.x$Flow)(.y))

map2_dbl(data, raw_val, ~ecdf(.x$Temp)(.y))



# donor percetile
raw_val <- data$flow
data$percentile <- ecdf(data$flow)(raw_val)


all_dates <- expand.grid(date = seq(min(data$date), max(data$date), "days"), stringsAsFactors = FALSE)
dim(all_dates)

# Join all dates with observed flow data, retaining NAs where flow data is missing

donor_flow_data <- data %>% filter(site == "Site2")

original_flow <- left_join(x = all_dates, y = data_f, by = "date")
original_flow <- original_flow %>% mutate(site = "Site2") # change to i

# join donor and main data
data_percentile_flow <- left_join(x = original_flow, y = donor_flow_data, by = c("site", "date"))



# rename columns
data_percentile_flow <- data_percentile_flow %>% rename(flow = flow.x, donor_flow = flow.y)

# calculate equipercentile
data_percentile_flow <- data_percentile_flow %>%
  mutate(flow_equipercentile = quantile(flow, percentile.x, na.rm = TRUE))

# flag imputed data
all_flow <- data_percentile_flow %>%
  mutate(imputed = ifelse(is.na(flow) & !is.na(flow_equipercentile), 1, 0))



# Calculate equipercentile
# using original_flow and donor_flow_data

# donor percetile

original_flow <- data
original_flow <- data %>% filter(site == "Site2")
original_flow <- left_join(x = all_dates, y = original_flow, by = "date")

raw_val <- data$flow
data$percentile <- ecdf(data$flow)(raw_val)
donor

all_dates <- expand.grid(date = seq(min(data$date), max(data$date), "days"), stringsAsFactors = FALSE)
dim(all_dates)

# join donor and main data
data_percentile_flow <- left_join(x = original_flow, y = data, by = c("site", "date"))

# rename columns
data_percentile_flow <- data_percentile_flow %>% rename(flow = flow.x, donor_flow = flow.y)

# calculate equipercentile
data_percentile_flow <- data_percentile_flow %>%
  mutate(flow_equipercentile = quantile(flow, percentile, na.rm = TRUE))


############# P7466

biolsites <- read.csv("data/25_sites_0_data.csv")
biolsites <- biolsites$SITE
envsites <- import_env(sites = biolsites)
predict <- predict_indices(env_data = envsites)

write.csv(predict, "P7466_Indices.csv")









#####################


#' Impute missing flow data.
#'
#' @description This function infills missing records in daily flow time series for one or more sites (gauging stations) using either interpolation or an equipercentile  method. Imputation of missing flow data can improve the later estimation of flow statistics using the calc_flowstats() function and aid the visualisation of hydro-ecological relationships using the plot_hev() function.
#'
#' @usage impute_flow(data, site_col = " flow_site_id", date_col = "date", flow_col = "flow", method = "linear", donor = NULL)
#'
#' @param data A tibble or data frame containing the data to be imputed.
#' @param site_col Name of column in data containing unique flow site id. Default = "flow_site_id". Site ids are coerced to a character vector.
#' @param date_col Name of column in data containing date of flow record. Default = "date".
#' @param flow_col Name of column in data containing the measured  flow data. Default = "flow".
#' @param method Imputation method: "linear" (default), "exponential" or "equipercentile".
#' @param donor – A tibble or data frame with at least two columns: the first a list of flow sites requiring imputation, and the second a list of paired donor sites. Subsequent columns are ignored. Default = NULL. Only used when method = "equipercentile".
#'
#' @details
#' This function is intended for imputing gauged daily flow data only; it cannot be used to impute sub-daily data, and is not designed for data on coarser time steps (e.g. 10-daily or monthly).
#'
#' The function offers three imputation methods: linear interpolation, exponential interpolation and an equipercentile algorithm.
#'
#' @return A tibble containing the imputed flow data. The data are arranged in long format, with the following columns:
#'
#'    - flow_site_id (unique flow site id)
#'    - date (of flow record)
#'    - imputed (flag  indicating whether each flow value is original (0) or imputed (1))
#'    - donor_site (id of donor site used for imputation, if "equipercentile" method used)
#'    - donor_flow (measured flow at donor site on that date, if "equipercentile" method used)
#'    - any other columns in the input dataset are automatically pulled through and joined to the output data table (e.g. the ‘input’ and ‘quality’ columns from the import_flow() function).
#'
#' @examples


impute_flow <- function(data,
                        site_col = " flow_site_id",
                        date_col = "date",
                        flow_col = "flow",
                        method = "linear",
                        donor = NULL){

  if(is.data.frame(data) == FALSE){stop("Data frame not found")}

  if(isTRUE(date_col == "date") != TRUE && (date_col %in% colnames(data)) == FALSE)
  {stop("Specified date column was not identified in data")}
  if(isTRUE(date_col == "date") == TRUE && (date_col %in% colnames(data)) == FALSE)
  {stop("Default 'date' column was not identified in data")}

  if(isTRUE(site_col == "flow_site_id") != TRUE && (site_col %in% colnames(data)) == FALSE)
  {stop("Specified site column was not identified in data")}
  if(isTRUE(site_col == "flow_site_id") == TRUE && (site_col %in% colnames(data)) == FALSE)
  {stop("Default 'flow_site_id' column was not identified in data")}

  if(isTRUE(flow_col == "flow") != TRUE && (flow_col %in% colnames(data)) == FALSE)
  {stop("Specified flow column was not identified in data")}
  if(isTRUE(flow_col == "flow") == TRUE && (flow_col %in% colnames(data)) == FALSE)
  {stop("Default 'flow' column was not identified in data")}

  if(isTRUE(method %in% c("linear", "exponential", "equipercentile")) == FALSE)
  {stop("Specified 'method' must be one of 'linear', 'exponential', or 'equipercentile'")}

  if(is.null(donor) != TRUE && isTRUE(method == "equipercentile") == FALSE)
  {warning("Donor stations will not be used; to use donor stations, select 'method = regression'")}

  if(is.null(donor) != TRUE && isTRUE(method == "equipercentile") == TRUE && is.data.frame(donor) != TRUE && is_tibble(donor) != TRUE)
  {stop("Donor stations must be specified in data frame or tibble format")}

  if(is.null(donor) != TRUE && isTRUE(method == "equipercentile") == TRUE && is.data.frame(donor) == TRUE || is_tibble(donor) == TRUE && ncol(donor) < 2)
  {stop("'donor', must contain at least 2 columns; the first a list of flow sites requiring imputation, and the second a list of paired donor sites")}

  # pull-in site_col

  data$site <- dplyr::pull(data, site_col)
  data$flow <- dplyr::pull(data, flow_col)
  data$date <- dplyr::pull(data, date_col)

  if(isTRUE(method == "equipercentile") == TRUE && isTRUE(unique(data$site) < 2) == TRUE)
  {stop("A miniumum of two flow sites (stations) are required if applying equipercentile method")}

  if(isTRUE(1 %in% diff.Date(data$date)) == FALSE){Stop("flow data supplied is not on a daily time-step")}
  if(isTRUE("TRUE" %in% is.na(data$flow[data$flow < 0])) == TRUE){warning("flow data contains negative values")}

  for(i in unique(data$site)){

    # filter and pull-in data

    data_f <- dplyr::filter(data, site == "4032")

    data_f$date <- dplyr::pull(data_f, date_col)
    data_f$flow <- dplyr::pull(data_f, flow_col)

    # Check flow is numeric

    if(is.numeric( data_f$flow) == FALSE)
    {stop("Specified flow_col is not numeric")}

    # Check for duplicate dates

    duplicates <- data_f$date[duplicated(data_f$date)]

    if(length(duplicates) >= 1){stop("Duplicate dates identified")}

    # Use expand grid to set-up a date range, from minimum flow date to present day

    all_dates <- expand.grid(date = seq(min(data$date), max(data$date), "days"), stringsAsFactors = FALSE)
    dim(all_dates)


    # Equipercentile method

    if(method == "equipercentile"){

      # Join all dates with observed flow data, retaining NAs where flow data is missing

      original_flow <- left_join(x = all_dates, y = data_f, by = "date")
      original_flow <- original_flow %>% mutate(site = "4032")

      # If donor station is NOT specified

      if(is.null(donor) == TRUE){

        # pull flow data for possible donor sites for correlation test

        for(j in unique(data$site)){

          data_equ <- dplyr::filter(data, site == "Site2")

          data_equ$date <- dplyr::pull(data_equ, date_col)
          data_equ$flow <- dplyr::pull(data_equ, flow_col)

          # Join all dates with observed flow data, retaining NAs where flow data is missing

          data_equp <- left_join(x = all_dates, y = data_equ, by = "date")
          data_equp <- data_equp %>% mutate(site = "Site2")

          # test correlations

          corr <- cor.test(x=original_flow$flow, y=data_equp$flow, method = 'spearman')
          corr_rho <- as.data.frame(corr$estimate)
          corr_rho$flow_site_id <- "Site2"
          assign(paste("Site2", sep = "_", "corr_rho"), corr_rho)

        }

        corr_all <- c(mget(ls(pattern = "_corr_rho")))
        corr_all <- Reduce('rbind', corr_all)

        corr_donor <- setDT(corr_all)[ , .SD[which.max(corr_all$`corr$estimate`)]]

        donor_site <- corr_donor$flow_site_id

        donor_flow_data <- data %>%
          filter(site == donor_site)

      }

      # If donor station IS specified

      if(is.null(donor) == FALSE){

        # Identify the corresponding donor station, and filter the original input file to pull-out donor flow data

        flow_site_main <- unique(original_flow$site)

        donor_flow <- donor
        donor_flow$flow_site <- dplyr::pull(donor[,1])
        donor_flow$donor_site <- dplyr::pull(donor[,2])

        donor_flow_f <- dplyr::filter(donor_flow, flow_site == flow_site_main)

        donor_flow_data <-
          data %>%
          filter(site == donor_flow_f$donor_site)

      }

      # Check if there are sufficient overlapping records between the main flow site and the donor site

      if(length(intersect(data_f$date, donor_flow_data$date)) < 365)
      {warning(paste(i, sep = "-", "Equipercentile method cannot be applied, due to insufficient overlapping data; please select an alternative method."))}

      else {

        # Calculate equipercentile
        # using original_flow and donor_flow_data

        # donor percetile
        raw_val <- donor_flow_data$flow
        donor_flow_data$percentile <- ecdf(donor_flow_data$flow)(raw_val)

        # join donor and main data
        data_percentile_flow <- left_join(x = original_flow, y = donor_flow_data, by = c("site", "date"))

        # rename columns
        data_percentile_flow <- data_percentile_flow %>% rename(flow = flow.x, donor_flow = flow.y)

        # calculate equipercentile
        data_percentile_flow <- data_percentile_flow %>%
          mutate(flow_equipercentile = quantile(flow, percentile, na.rm = TRUE))

        # flag imputed data
        all_flow <- data_percentile_flow %>%
          mutate(imputed = ifelse(is.na(flow) & !is.na(flow_equipercentile), 1, 0))

        # specify method
        all_flow <- all_flow %>%
          mutate(imputed = ifelse(is.na(flow) & !is.na(flow_equipercentile), "Eqipercentile", ""))

        # specify donor site id
        all_flow <- all_flow %>%
          mutate(donor_site = ifelse(is.na(flow) & !is.na(flow_equipercentile), donor_flow_data$site[1], ""))

        # drop extra flow column
        drops <- c("flow_equipercentile", "percentile")
        all_flow <- all_flow[,!(names(all_flow) %in% drops)]

      }
    }


    # Select outputs and match with flow_site_id

    flow_data <- as.data.frame(all_flow)
    assign(paste(i, sep = "_", "flow_data"), flow_data)

  }

  # Merge all sites

  flow_data_all <- c(mget(ls(pattern = "_flow_data")))
  flow_data_all <- Reduce('bind_rows', flow_data_all)

  # Output

  return(flow_data_all)


}



######### Extra packages

# tibble
# schoolmath
# imputeTS
# dplyr
# zoo
