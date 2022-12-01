#' Calculate a suite of long-term and time-varying flow statistics for one or more sites.
#'
#' @description This function takes a time series of measured or modelled flows and uses a user-defined moving window to calculate a suite of time-varying flow statistics for one or more sites (stations). A smaller set of long-term statistics is also calculated. It is primarily designed to work with daily flows but can also be applied to time series data on a longer (e.g. 10-daily or monthly) time step. The data should be regularly spaced, and a common time step should be used for all sites.
#'
#' @usage calc_flowstats(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", imputed_col = “imputed”, win_start = "1995-04-01", win_width = "6 months", win_step = "6 months", date_range = NULL, q_low = 95, q_high = 70, standardise = FALSE, ref_col = NULL)
#'
#' @param data Tibble or data frame containing the flow data to be processed. Must be in long format and have, as a minimum, separate columns containing site id, date and flow (e.g. as output by the import_flow() function). If flow estimates are available for different abstraction or climate scenarios, then these must be in separate columns (see `ref_col` argument).
#' @param site_col Name of column in data containing unique flow site id. Default = "flow_site_id".
#' @param date_col Name of column in data containing date of flow record. Default = "date".  Dates must be in “yyyy-mm-dd” format.
#' @param flow_col Name of column in data containing flow data for processing (character). Default = "flow".
#' @param imputed_col Name of optional column in data specifying whether each flow value is measured (0) or imputed (1).  Default = NULL.
#' @param win_start Start date of first time window (in yyyy-mm-dd format). Default = "1995-04-01".
#' @param win_width Width of the time window, in days, weeks, months or years  (see ?seq.Date for options). Default = "6 months".
#' @param win_step The increment by which the time window moves, in days, weeks, months or years (see ?seq.Date for options). Default = "6 months".
#' @param q_low Qx flow threshold (between 1 and 99, as an integer) defining low flow events. Default = 95 (representing the long-term Q95 flow at each site).
#' @param q_high Qx flow threshold (between 1 and 99, as an integer) defining high flow events. Default = 70 (representing the long-term Q70 flow at each site).
#' @param date_range Optional vector of two dates (in yyyy-mm-dd format) defining the period of flow data to be analysed. Default = NULL . Flow records outside this range are excluded. For unbiased calculation of long-term flow statistics, it is advisable that this range spans a whole number of years (i.e c(01-01-2000, 31-12-2020))
#' @param scaling Should the time series flow data be scaled by the long-term mean flow  at each site? Default = FALSE.
#' @param ref_col Name of column in dataset containing reference flow scenario against which selected flow statistics are z-score standardised. Default = NULL.

#' @details
#'
#' The function uses the `win_start`, `win_width` and `win_step` arguments to define a moving window, which divides the flow time series into a sequence of time periods. These time periods may be contiguous, non-contiguous or overlapping (see examples below). The sequence of time periods continues up to and including the present date, even when this extends beyond the period covered by the input flow dataset, as this facilitates the subsequent joining of flow statistics and ecology data by the `join_he` function. The sequence of time periods does not extend beyond the present date, however; for example, if calculating flow statistics for each calendar year, the time periods would stop at the end of the last complete year.
#'
#' For each time period, the function calculates a suite of flow statistics, listed below. With the exception of 7-day and 30-day minimum flow statistics that require daily data, all flow statistics are calculated regardless of the time step of the flow data. Caution should be exercised, however, when analysing flow data on a coarser (e.g. monthly) time step to ensure that the statistics are meaningful and interpretable (especially those relating to the number and timing of low and high flow events, which require reasonably high frequency flow data in order to discriminate sequences of lower and higher flows).
#'
#' The function requires a minimum number of records to calculate some statistics (detailed below), otherwise an `NA` result is returned. Meeting the minimum requirement does not, however, guarantee that a statistic has been estimated to an appropriate level of precision, and users may wish to manually filter the results to eliminate potentially unreliable estimates based on sparse data.
#'
#' To ensure that estimated statistics are comparable across time periods, the flow time series data should be as complete as possible (gaps can be infilled using the `impute_flows` function). Missing values (`NA`) are ignored when calculating all statistics, including those that count the number of events when flows exceed or fall below a certain flow threshold.
#'
#' To make some statistics more comparable across sites, the scaling argument optionally allows the flow time series data to be standardised by dividing by the site’s long-term mean flow. Scaling is performed after applying the `date_range` filter, so that the long-term mean flows can be calculated over a specified number of whole years. Because this eliminates absolute differences in mean flow from site to site, scaling is most useful when the focus is on statistics measured in flow units (e.g. sd, q5, low_magnitude, low_severity, volume, min, min_7day, min_30day, max).
#'
#' Additionally, certain statistics (denoted by a `_z` suffix) are standardised using the mean and standard deviation of the estimated statistics across time periods at a given site (e.g. `q5_z = (q5 – q5_mean) / q5_sd`)). These standardised statistics are dimensionless, and so comparable across sites. Standardisation provides an alternative to scaling (described above) when one wishes flow statistics for larger and smaller watercourses to be comparable. Be aware that these standardised variables are calculated regardless of whether or not the raw flow data have been scaled (via the `scale` argument)).
#'
#' The function also includes the facility to standardise the statistics for one flow scenario (specified via `flow_col`) using mean and standard deviation  flow statistics from another scenario (specified via `ref_col`). For example, if `flow_col` = naturalised flows and `ref_col` = historical flows, then the resulting statistics can be input into a hydro-ecological model that has previously been calibrated using standardised historical flow statistics and used to make predictions of ecological status under naturalised flows.

#'
#' @return The function returns a list of two data frames. The first data frame contains a suite of time-varying flow statistics for every time period at every site. The columns are as follows:
#'
#'    - `flow_site_id`: a unique site id
#'    - `win_no`: an autonumber counting the sequence of flow time periods
#'    - `start_date`: start date of the time period (in yyyy-mm-dd format)
#'    - `end_date`: end date of the time period (in yyyy-mm-dd format)
#'    - `n_data`: the number of records with valid flows (not NA)
#'    - `n_missing`: the number of missing flow records (flow = NA)
#'    - `n_total`: the total number of flow records (sum of n_data and n_missing)
#'    - `prop_missing`: the proportion of missing flow records (n_data / n_total)
#'    - `n_imputed`: the number of flow records that have been imputed (this is calculated only if the imputed_col argument is specified)
#'    - `prop_imputed`: the proportion of flow records that have been imputed (calculated only if the imputed_col argument is specified)
#'    - `mean`: mean flow (min. records required = 2)
#'    - `sd`: the standard deviation of flows (min. records required = 2)
#'    - `Q5`: the unstandardised Q5 flow (min. records required = 20)
#'    - `Q10`: the unstandardised Q10 flow (min. records required = 10)
#'    - `Q20`: the unstandardised Q20 flow (min. records required = 5)
#'    - `Q25`: the unstandardised Q25 flow (min. records required = 4)
#'    - `Q30`: the unstandardised Q30 flow (min. records required = 4)
#'    - `Q50`: the unstandardised Q50 flow (min. records required = 2)
#'    - `Q70`: the unstandardised Q70 flow (min. records required = 4)
#'    - `Q75`: the unstandardised Q75 flow (min. records required = 4)
#'    - `Q80`: the unstandardised Q80 flow (min. records required = 5)
#'    - `Q90`: the unstandardised Q90 flow (min. records required = 10)
#'    - `Q95`: the unstandardised Q95 flow (min. records required = 20)
#'    - `Q99`: the unstandardised Q99 flow (min. records required = 100)
#'    - `Q5z`: the Q5 flow, standardised using the mean and sd of the Q5 flows across all time periods for that site, i.e. q5_z = (q5 – q5mean) / Q5sd. If ref_col is not NULL, then the Q5 is estimated for the flow_col time series, but standardised using the mean and sd parameters for the ref_col time series (i.e. q50_z = (q50 - q50mean_ref) / q50sd_ref). (min. records required = 20)
#'    - `Q10z`: as for Q5z (min. records required = 10)
#'    - `Q20z`: as for Q5z (min. records required = 5)
#'    - `Q25z`: as for Q5z (min. records required = 4)
#'    - `Q30z`: as for Q5z (min. records required = 4)
#'    - `Q50z`: as for Q5z (min. records required = 2)
#'    - `Q70z`: as for Q5z (min. records required = 4)
#'    - `Q75z`: as for Q5z (min. records required = 4)
#'    - `Q80z`: as for Q5z (min. records required = 5)
#'    - `Q90z`: as for Q5z (min. records required = 10)
#'    - `Q95z`: as for Q5z (min. records required = 20)
#'    - `Q99z`: as for Q5z (min. records required = 100)
#'    - `dry_n`: number of records with zero flow (min. records required = 2)
#'    - `dry_e`: number of events when flow drops to zero (min. records required = 28)
#'    - `dry_start`: day of year (1-366) of first zero flow record (min. records required = 8)
#'    - `dry_end`: day of year (1-366) of last zero flow record (min. records required = 28)
#'    - `dry_mid`: mean day of year (1-366) of all zero flow records (min. records required = 28)
#'    - `low_n`: number of records when flow is below the q_low threshold (min. records required = 2)
#'    - `low_e`: number of events when flow drops below the q_low threshold (min. records required = 28)
#'    - `low_start`: day of year (1-366) of first record below the q_low threshold (min. records required = 28)
#'    - `low_end`: day of year (1-366) of first record below the q_low threshold (min. records required = 28)
#'    - `low_mid`: circular  mean day of year (1-366) of all records below the q_low threshold (min. records required = 28)
#'    - `low_magnitude`: mean flow deficit below q_low (min. records required = 28)
#'    - `low_severity`: cumulative flow deficit below q_low (low_n x low_magnitude) (min. records required = 28)
#'    - `high_n`: number of records when flow is above the q_high threshold (min. records required = 2)
#'    - `high_e`: number of events when flow exceeds the q_high threshold (min. records required = 28)
#'    - `high_start`: day of year (1-366) of first record above the q_high threshold (min. records required = 28)
#'    - `high_end`: day of year (1-366) of last record above the q_high threshold (min. records required = 28)
#'    - `high_mid`: circular mean day of year (1-366) of all records above the q_high threshold (min. records required = 28)
#'    - `e_above3xq50`: number of events when flow exceeds 3 x the long-term median (Q50) flow (min. records required = 28)
#'    - `e_above5xq50`: number of events when flow exceeds 5 x the long-term median (Q50) flow (min. records required = 28)
#'    - `e_above7xq50`: number of events when flow exceeds 7 x the long-term median (Q50) flow (min. records required = 28)
#'    - `volume`: total volume discharged (sum of flows) (min. records required = 3)
#'    - `volume_z`: as for q5z (min. records required = 3)
#'    - `min`: minimum flow (min. records required = 3)
#'    - `min_z`: as for q5z (min. records required = 3)
#'    - `min_doy`: day of year (1-366) of minimum flow (min. records required = 3)
#'    - `min_7day`: minimum 7-day mean flow (min. records required = 90)
#'    - `min_7day_z`: as for q5z (min. records required = 90)
#'    - `min_7day_doy`: day of year (1-366) of midpoint of 7-day minimum flow period (min. records required = 90)
#'    - `min_30day`: minimum 30-day mean flow (min. records required = 180)
#'    - `min_30day_z`: as for q5z (min. records required = 180)
#'    - `min_30day_doy`: day of year of (1-366) of midpoint of 30-day minimum flow period (min. records required = 180)
#'    - `max`: maximum flow (min. records required = 3)
#'    - `max_z`: as for q5z (min. records required = 3)
#'    - `max_doy`: day of year (1-366) of maximum flow (min. records required = 3)
#'
#' The second data table contains long-term flow statistics. The data are arranged in long format, with the following columns:
#'    - `flow_site_id` (a unique site id)
#'    - `start_date`: start date of the long-term time period (in yyyy-mm-dd format) for which the statistics are calculated
#'    - `end_date`: end date of the long-term time period (in yyyy-mm-dd format) for which the statistics are calculated
#'    - `parameter` (long-term minimum, maximum and mean flow; long-term flow duration curve percentiles (p1 to p99); long-term base flow index (bfi = 7-day minimum flow / mean flow); and long-term mean and standard deviation of the time-varying q5 to q99, minimum flow, maximum flow and 7-day minimum flow statistics)
#'    - `value` (calculated statistic)
#'
#' @export
#'
#' @examples
#' ## calculate flow statistics for a contiguous series of summer (April to September) and winter (October to March) time periods:
#' calc_flowstats(data = flow_data,
#'                win_start =  "1995-04-01",
#'                win_width = "6 months",
#'                win_step =  "6 months")
#'
#' ## calculate flow statistics for a non-contiguous series of summer (April to September) time periods:
#' calc_flowstats(data = flow_data,
#'                win_start =  "1995-04-01",
#'                win_width = "6 months",
#'                win_step =  "1 year")
#'
#' ## calculate flow statistics for a series of overlapping 24 month time periods:
#' calc_flowstats(data = flow_data,
#'                win_start =  "1995-04-01",
#'                win_width = "24 months",
#'                win_step =  "1 month")
#'



calc_flowstats <- function(data,
                         site_col = "flow_site_id",
                         date_col = "date",
                         flow_col = "flow",
                         imputed_col = NULL,
                         win_start = "1995-04-01",
                         win_width = "6 months",
                         win_step = "6 months",
                         date_range = NULL,
                         q_low = 95,
                         q_high = 70,
                         scaling = FALSE,
                         ref_col = NULL){

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

  if(is.null(imputed_col) == FALSE && isTRUE(imputed_col %in% colnames(data)) == FALSE)
  {stop("Specified imputed_col was not identified in data")}

  if(is.null(ref_col) == FALSE && (ref_col %in% colnames(data)) == FALSE)
  {stop("Specified ref_col was not identified in data")}

  if(is.null(win_start) == FALSE && !is.na(lubridate::parse_date_time(win_start,orders="Ymd")) == FALSE)
  {stop("win_start should be in YYYY-MM-DD format")}

  if(is.null(win_start) == FALSE && lubridate::is.Date(win_start) == TRUE && isTRUE(win_start > Sys.Date()) == TRUE)
  {stop("win_start is in the future")}

  matches <- c("day", "month", "year", "week")
  if(is.null(win_width) == FALSE && grepl(matches[1], win_width) == TRUE){""} else {
    if(is.null(win_width) == FALSE && grepl(matches[2], win_width) == TRUE){""} else {
      if(is.null(win_width) == FALSE && grepl(matches[3], win_width) == TRUE){""} else {
        if(is.null(win_width) == FALSE && grepl(matches[4], win_width) == TRUE){""} else {
          stop("win_width must be in day, month, or year format")
        }
      }
    }
  }

  if(is.null(win_step) == FALSE && grepl(matches[1], win_step) == TRUE){""} else {
    if(is.null(win_step) == FALSE && grepl(matches[2], win_step) == TRUE){""} else {
      if(is.null(win_step) == FALSE && grepl(matches[3], win_step) == TRUE){""} else {
        if(is.null(win_step) == FALSE && grepl(matches[4], win_step) == TRUE){""} else {
          stop("win_step must be in day, month, or year format")
        }
      }
    }
  }

  if(is.null(date_range) == FALSE && length(date_range) == 1){
    stop("date_range is of length 1, date range must be of length 2") }

  if(is.null(date_range) == FALSE && !is.na(lubridate::parse_date_time(date_range[1],orders="Ymd")) == FALSE){
    stop("date_range should be in YYYY-MM-DD format") }

  if(is.null(date_range) == FALSE && !is.na(lubridate::parse_date_time(date_range[2],orders="Ymd")) == FALSE){
    stop("date_range should be in YYYY-MM-DD format") }

  if(is.null(date_range) == FALSE && isTRUE(length(date_range) > 2) == TRUE)
  {stop("date_range should be of maximum length 2")}
  if(is.null(date_range) == FALSE && isTRUE(date_range[1] >= date_range[2]) == TRUE)
  {stop("start date exceeds end date, please check date_range")}
  if(is.null(date_range) == FALSE && isTRUE(date_range[1] > Sys.Date()) == TRUE)
  {stop("date_range 1 is in the future")}
  if(is.null(date_range) == FALSE && isTRUE(date_range[2] > Sys.Date()) == TRUE)
  {stop("date_range 2 is in the future")}

  if(is.logical(scaling) == FALSE){stop("'scaling' is not logical")}

  if(is.null(q_low) == FALSE && dplyr::between(q_low, 1, 99) == FALSE)
  {stop("q_low must be a value between 1 and 99")}

  if(dplyr::between(q_low, 1, 99) == TRUE && testInteger(q_low) == FALSE)
  {stop("q_low must be an integer between 1 and 99")}

  if(is.null(q_high) == FALSE && dplyr::between(q_high, 1, 99) == FALSE)
  {stop("q_high must be a value between 1 and 99")}

  if(dplyr::between(q_high, 1, 99) == TRUE && testInteger(q_high) == FALSE)
  {stop("q_high must be an integer between 1 and 99")}


  # pull-in data
  data_1 <- data
  data_1$site <- dplyr::pull(data_1, site_col)
  data_1$date <- dplyr::pull(data_1, date_col)
  data_1$flow <- dplyr::pull(data_1, flow_col)
  data_1 <- data_1 %>% dplyr::arrange(., site, date)

  # Check flow is numeric
  if(is.numeric(data_1$flow) == FALSE)
  {stop("Specified flow_col is not numeric")}

  # Check for duplicate dates in each site
  duplicates<-data_1$date[duplicated(data.table::data.table(data_1),by=c("site","date"))]

  if(length(duplicates) >= 1)
  { print(duplicates)
   stop("Duplicate dates identified")}

  # filter by date_range
  if(is.null(date_range) == FALSE){
    data_1 <- data_1 %>% dplyr::filter(date <= date_range[2] & date >= date_range[1])
  }

  # Create a large data frame with all data windows, for all sites, up to present day
  sites <- data.frame(site = unique(data_1$site))
  expand_dates <- expand.grid(start_date = seq(as.Date(win_start), Sys.Date(), paste("+", sep = "", win_step)), stringsAsFactors = FALSE)

  # get numeric win_width
  width <- data.frame(win_width)
  width_sep <- tidyr::separate(width, win_width, c("no", "period"))
  width_no <- as.numeric(width_sep$no)

  # if win_width is in days
  if(is.null(win_width) == FALSE && grepl("day", win_width) == TRUE){
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% lubridate::days(width_no) - days(1),
                                                win_no = 1:nrow(.))
  }

  # if win_width is in weeks
  if(is.null(win_width) == FALSE && grepl("week", win_width) == TRUE){
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% lubridate::weeks(width_no) - days(1),
                                                win_no = 1:nrow(.))
  }

  # if win_width is in months
  if(is.null(win_width) == FALSE && grepl("month", win_width) == TRUE){
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% months(width_no) - days(1),
                                    win_no = 1:nrow(.))
  }

  # if win_width is in years
  if(is.null(win_width) == FALSE && grepl("year", win_width) == TRUE){
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% lubridate::years(width_no) - days(1),
                                         win_no = 1:nrow(.))
  }

  # assign full set of moving windows to each site
  win_dates <- dplyr::full_join(all_dates, sites, by = character())
  win_dates$win_no <- as.character(win_dates$win_no)

  all_win_dates <- win_dates %>% dplyr::rowwise() %>%
    dplyr::do(data.frame(site =.$site,
                  date = seq(.$start_date, .$end_date, by="days"),
                  win_no =.$win_no,
                  win_start_date = .$start_date,
                  win_end_date = .$end_date))

  # if scaling = TRUE, standardise flow data by the mean (for each site)
  if(scaling == TRUE){
    data_1 %>%
      dplyr::group_by(site) %>%
      dplyr::mutate(mean_f = (mean(flow, na.rm = TRUE))) %>%
      dplyr::mutate(flow = flow / mean_f) %>%
      dplyr::select(-mean_f)
    }

  # join window dates to flow data for CalcFlowStats
  my_data_cf <- dplyr::left_join(all_win_dates, data_1, by = c("site", "date"))

  # drop columns not required for flow calcs (these will be re-joined at the end)
  my_data_cf_2 <- my_data_cf %>% dplyr::select(site, date, win_no, flow)
  my_data_lt <- data_1 %>% dplyr::select(site, date, flow)


  # calculate time-varying flow statistics
  STATS1 <- my_data_cf_2 %>% CalcFlowStats()

  # calculate long-term flow statistics
  long_data <- suppressWarnings(CreateLongData(my_data_lt, STATS1))

  # calculate remaining time-varying flow statistics (durations/events, 7/30-day mins)
  flow_stats <- suppressWarnings(CreateFlowStats(STATS1, long_data, my_data_cf_2, data_1, q_high = q_high, q_low = q_low))

  # join time-varying statistics with win_dates, rename site column and drop unwanted columns
  df1 <- dplyr::left_join(win_dates, flow_stats, by=c("site", "win_no")) %>%
    dplyr::rename(flow_site_id = site) %>%
    dplyr::select(-Q5mean, -Q5sd, -Q10mean, -Q10sd, -Q20mean, -Q20sd,
                  -Q25mean, -Q25sd, -Q30mean, -Q30sd, -Q50mean, -Q50sd,
                  -Q70mean, -Q70sd, -Q75mean, -Q75sd, -Q80mean, -Q80sd, -Q90mean, -Q90sd,
                  -Q95mean, -Q95sd, -Q99mean, -Q99sd, -min_7day_mean, -min_7day_sd,
                  -min_30day_mean, -min_30day_sd, -min_mean, -min_sd, -max_mean, -max_sd,
                  -vol_mean, -vol_sd)

  # re-order columns
  if(is.null(imputed_col) == TRUE){
    col_order <- c("flow_site_id", "win_no", "start_date", "end_date", "n_data", "n_missing", "n_total", "prop_missing", "mean", "sd", "Q5", "Q10", "Q20", "Q25", "Q30", "Q50", "Q70", "Q75", "Q80", "Q90", "Q95", "Q99", "Q5z", "Q10z", "Q20z", "Q25z", "Q30z", "Q50z", "Q70z", "Q75z", "Q80z", "Q90z", "Q95z", "Q99z", "dry_n", "dry_e", "dry_start", "dry_end", "dry_mid", "low_n", "low_e", "low_start", "low_end", "low_mid", "low_magnitude", "low_severity", "high_n", "high_e", "high_start", "high_end", "high_mid", "e_above3xq50", "e_above5xq50", "e_above7xq50", "volume", "vol_z", "min", "min_z", "min_doy", "min_7day", "min_7day_z", "min_7day_doy", "min_30day", "min_30day_z", "min_30day_doy", "max", "max_z", "max_doy")
    df1 <- df1[, col_order]
  }

  if(is.null(imputed_col) == FALSE){

    # pull-in imputed col
    my_data <- my_data_cf
    my_data$imputed <- dplyr::pull(my_data, imputed_col)

    test_impute <- data_1
    test_impute$imputed <- dplyr::pull(test_impute, imputed_col)
    if(isTRUE(all(test_impute$imputed == as.integer(test_impute$imputed))) == FALSE)
    {stop ("imputed_col values must be integer value of 0 or 1")}
    if(isTRUE(range(test_impute$imputed)[2]>1) == TRUE)
    {stop ("imputed_col values must be integer value of 0 or 1")}
    if(isTRUE(range(test_impute$imputed)[1]<0) == TRUE)
    {stop ("imputed_col values must be integer value of 0 or 1")}

    # count imputed data
    count_imputed <- my_data %>%
    dplyr::group_by(site, win_no) %>%
    dplyr::count(., imputed) %>%
    dplyr::rename(n_imputed = n,
                  flow_site_id = site)

    # add in count of imputed flow records
    df1 <- dplyr::left_join(df1, count_imputed, by = c("flow_site_id", "win_no")) %>%
      dplyr::mutate(n_imputed = ifelse(imputed == 1, n_imputed, 0)) %>%
      dplyr::select(-imputed) %>%
      dplyr::mutate(prop_imputed = (n_imputed/n_data))

     # re-order cols
    col_order <- c("flow_site_id", "win_no", "start_date", "end_date", "n_data", "n_missing", "n_total", "prop_missing", "n_imputed", "prop_imputed", "mean", "sd", "Q5", "Q10", "Q20", "Q25", "Q30", "Q50", "Q70", "Q75", "Q80", "Q90", "Q95", "Q99", "Q5z", "Q10z", "Q20z", "Q25z", "Q30z", "Q50z", "Q70z", "Q75z", "Q80z", "Q90z", "Q95z", "Q99z", "dry_n", "dry_e", "dry_start", "dry_end", "dry_mid", "low_n", "low_e", "low_start", "low_end", "low_mid", "low_magnitude", "low_severity", "high_n", "high_e", "high_start", "high_end", "high_mid", "e_above3xq50", "e_above5xq50", "e_above7xq50", "volume", "vol_z", "min", "min_z", "min_doy", "min_7day", "min_7day_z", "min_7day_doy", "min_30day", "min_30day_z", "min_30day_doy", "max", "max_z", "max_doy")
    df1 <- df1[, col_order]
  }

  # add min & max dates to long_data, to create df2 for final function output
  # get start and end dates for long term stats
  minmax_dates <- data_1 %>% dplyr::group_by(site) %>%
    dplyr::summarise(start_date = min(date),
                     end_date = max(date))
  # join with long_data
  long_data_2 <- long_data %>% dplyr::full_join(minmax_dates, by = "site") %>%
    dplyr::rename(flow_site_id = site) %>%
    dplyr::select(-win_no)
  # re-order cols
  cols_order <- c("flow_site_id", "start_date", "end_date", "parameter", "value")
  long_data_2 <- long_data_2[, cols_order]
  # rename
  df2 <- long_data_2

  # Remov flow stats where the minimum data requirements have not been met

  df1.1 <- df1 %>%
    dplyr::mutate(mean = ifelse(n_data >= 2, mean, NA),
                  sd = ifelse(n_data >= 2, sd, NA),
                  Q5 = ifelse(n_data >= 20, Q5, NA),
                  Q10 = ifelse(n_data >= 10, Q10, NA),
                  Q20 = ifelse(n_data >= 5, Q20, NA),
                  Q25 = ifelse(n_data >= 4, Q25, NA),
                  Q30 = ifelse(n_data >= 4, Q30, NA),
                  Q50 = ifelse(n_data >= 2, Q50, NA),
                  Q70 = ifelse(n_data >= 4, Q70, NA),
                  Q75 = ifelse(n_data >= 4, Q75, NA),
                  Q80 = ifelse(n_data >= 5, Q80, NA),
                  Q90 = ifelse(n_data >= 10, Q90, NA),
                  Q95 = ifelse(n_data >= 20, Q95, NA),
                  Q99 = ifelse(n_data >= 100, Q99, NA),
                  Q5z = ifelse(n_data >= 20, Q5z, NA),
                  Q10z = ifelse(n_data >= 10, Q10z, NA),
                  Q20z = ifelse(n_data >= 5, Q20z, NA),
                  Q25z = ifelse(n_data >= 4, Q25z, NA),
                  Q30z = ifelse(n_data >= 4, Q30z, NA),
                  Q50z = ifelse(n_data >= 2, Q50z, NA),
                  Q70z = ifelse(n_data >= 4, Q70z, NA),
                  Q75z = ifelse(n_data >= 4, Q75z, NA),
                  Q80z = ifelse(n_data >= 5, Q80z, NA),
                  Q90z = ifelse(n_data >= 10, Q90z, NA),
                  Q95z = ifelse(n_data >= 20, Q95z, NA),
                  Q99z = ifelse(n_data >= 100, Q99z, NA),
                  dry_n = ifelse(n_data >= 2, dry_n, NA),
                  dry_start = ifelse(n_data >= 28, dry_start, NA),
                  dry_end = ifelse(n_data >= 28, dry_end, NA),
                  dry_mid = ifelse(n_data >= 28, dry_mid, NA),
                  low_n = ifelse(n_data >= 2, low_n, NA),
                  low_start = ifelse(n_data >= 28, low_start, NA),
                  low_end = ifelse(n_data >= 28, low_end, NA),
                  low_mid = ifelse(n_data >= 28, low_mid, NA),
                  low_magnitude = ifelse(n_data >= 28, low_magnitude, NA),
                  low_severity = ifelse(n_data >= 28, low_severity, NA),
                  high_n = ifelse(n_data >= 2, high_n, NA),
                  high_start = ifelse(n_data >= 28, high_start, NA),
                  high_end = ifelse(n_data >= 28, high_end, NA),
                  high_mid = ifelse(n_data >= 28, high_mid, NA),
                  e_above3xq50  = ifelse(n_data >= 28, e_above3xq50, NA),
                  e_above5xq50  = ifelse(n_data >= 28, e_above5xq50, NA),
                  e_above7xq50  = ifelse(n_data >= 28, e_above7xq50, NA),
                  volume = ifelse(n_data >= 3, volume, NA),
                  vol_z = ifelse(n_data >= 3, vol_z, NA),
                  min = ifelse(n_data >= 3, min, NA),
                  min_z = ifelse(n_data >= 3, min_z, NA),
                  min_doy = ifelse(n_data >= 3, min_doy, NA),
                  min_7day = ifelse(n_data >= 90, min_7day, NA),
                  min_7day_z = ifelse(n_data >= 90, min_7day_z, NA),
                  min_7day_doy = ifelse(n_data >= 90, min_7day_doy, NA),
                  min_30day = ifelse(n_data >= 180, min_30day, NA),
                  min_30day_z = ifelse(n_data >= 180, min_30day_z, NA),
                  min_30day_doy = ifelse(n_data >= 180, min_30day_doy, NA),
                  max = ifelse(n_data >= 3, max, NA),
                  max_z = ifelse(n_data >= 3, max_z, NA),
                  max_doy = ifelse(n_data >= 3, max_doy, NA))

  df1.1$win_no <- as.numeric(df1.1$win_no)
  df1.1 <- arrange(df1.1, flow_site_id, win_no)

  ### create final data to return
  final_list <- list(df1.1, df2)

  ### if a ref_col is specified
  # calculate ref values using ref_col
  if(is.null(ref_col) == FALSE){

    # pull-in data
    data_ref <- data
    data_ref$site <- dplyr::pull(data_ref, site_col)
    data_ref$date <- dplyr::pull(data_ref, date_col)
    data_ref$flow <- dplyr::pull(data_ref, ref_col)

    if(is.numeric(data_ref$flow) == FALSE)
    {stop("Specified ref_col is not numeric")}

    # filter by date_range
    if(is.null(date_range) == FALSE){
      data_ref <- data_ref %>% dplyr::filter(date <= date_range[2] & date >= date_range[1])
    }

    # join window dates to ref data
    my_data_ref <- dplyr::left_join(all_win_dates, data_ref, by = c("site", "date"))

    # drop columns not required for flow calcs
    my_data_ref_2 <- my_data_ref %>% dplyr::select(site, date, win_no, flow)

    # calculate time varying flow statistics
    STATS1_ref <- CalcFlowStats(my_data_ref_2)

    # Get QXz_adj
    # select the data we need from _ref dataset and rename
    ref_data <- STATS1_ref %>%
      dplyr::select(site, win_no, Q5mean, Q5sd, Q10mean, Q10sd, Q20mean, Q20sd,
                    Q25mean, Q25sd, Q30mean, Q30sd, Q50mean, Q50sd, Q70mean, Q70sd,
                    Q75mean, Q75sd, Q80mean, Q80sd, Q90mean, Q90sd, Q95mean, Q95sd,
                    Q99mean, Q99sd, min_mean, min_sd, max_mean, max_sd,
                    min_30day_mean, min_30day_sd, min_7day_mean, min_7day_sd,
                    vol_mean, vol_sd)

    ref_data <-  ref_data %>% dplyr::rename(flow_site_id = site,
                                            Q5mean_ref = Q5mean,
                                            Q5sd_ref = Q5sd,
                                            Q10mean_ref = Q10mean,
                                            Q10sd_ref = Q10sd,
                                            Q20mean_ref = Q20mean,
                                            Q20sd_ref = Q20sd,
                                            Q25mean_ref = Q25mean,
                                            Q25sd_ref = Q25sd,
                                            Q30mean_ref = Q30mean,
                                            Q30sd_ref = Q30sd,
                                            Q50mean_ref = Q50mean,
                                            Q50sd_ref = Q50sd,
                                            Q70mean_ref = Q70mean,
                                            Q70sd_ref = Q70sd,
                                            Q75mean_ref = Q75mean,
                                            Q75sd_ref = Q75sd,
                                            Q80mean_ref = Q80mean,
                                            Q80sd_ref = Q80sd,
                                            Q90mean_ref = Q90mean,
                                            Q90sd_ref = Q90sd,
                                            Q95mean_ref = Q95mean,
                                            Q95sd_ref = Q95sd,
                                            Q99mean_ref = Q99mean,
                                            Q99sd_ref = Q99sd,
                                            minmean_ref = min_mean,
                                            minsd_ref = min_sd,
                                            maxmean_ref = max_mean,
                                            maxsd_ref = max_sd,
                                            volmean_ref = vol_mean,
                                            volsd_ref = vol_sd,
                                            min_day30mean_ref = min_30day_mean,
                                            min_day30sd_ref = min_30day_sd,
                                            min_day7mean_ref = min_7day_mean,
                                            min_day7sd_ref = min_7day_sd)

    # calculate _adj values
    df1_ALL <- dplyr::left_join(x = df1, y = ref_data,
                                by = c("flow_site_id", "win_no"), na.rm  = TRUE)

    # Get Q5z_adj
    Q5 <- df1_ALL$Q5
    Q5mean_ref <- df1_ALL$Q5mean_ref
    Q5sd_ref <- df1_ALL$Q5sd_ref
    df1_ALL$Q5z_ref <- (Q5 - Q5mean_ref) / Q5sd_ref

    # Get Q10z_adj
    Q10 <- df1_ALL$Q10
    Q10mean_ref <- df1_ALL$Q10mean_ref
    Q10sd_ref <- df1_ALL$Q10sd_ref
    df1_ALL$Q10z_ref <- (Q10 - Q10mean_ref) / Q10sd_ref

    # Get Q20z_adj
    Q20 <- df1_ALL$Q20
    Q20mean_ref <- df1_ALL$Q20mean_ref
    Q20sd_ref <- df1_ALL$Q20sd_ref
    df1_ALL$Q20z_ref <- (Q20 - Q20mean_ref) / Q20sd_ref

    # Get Q25z_adj
    Q25 <- df1_ALL$Q25
    Q25mean_ref <- df1_ALL$Q25mean_ref
    Q25sd_ref <- df1_ALL$Q25sd_ref
    df1_ALL$Q25z_ref <- (Q25 - Q25mean_ref) / Q25sd_ref

    # Get Q30z_adj
    Q30 <- df1_ALL$Q30
    Q30mean_ref <- df1_ALL$Q30mean_ref
    Q30sd_ref <- df1_ALL$Q30sd_ref
    df1_ALL$Q30z_ref <- (Q30 - Q30mean_ref) / Q30sd_ref

    # Get Q50z_adj
    Q50 <- df1_ALL$Q50
    Q50mean_ref <- df1_ALL$Q50mean_ref
    Q50sd_ref <- df1_ALL$Q50sd_ref
    df1_ALL$Q50z_ref <- (Q50 - Q50mean_ref) / Q50sd_ref

    # Get Q70z_adj
    Q70 <- df1_ALL$Q70
    Q70mean_ref <- df1_ALL$Q70mean_ref
    Q70sd_ref <- df1_ALL$Q70sd_ref
    df1_ALL$Q70z_ref <- (Q70 - Q70mean_ref) / Q70sd_ref

    # Get Q75z_adj
    Q75 <- df1_ALL$Q75
    Q75mean_ref <- df1_ALL$Q75mean_ref
    Q75sd_ref <- df1_ALL$Q75sd_ref
    df1_ALL$Q75z_ref <- (Q75 - Q75mean_ref) / Q75sd_ref

    # Get Q80z_adj
    Q80 <- df1_ALL$Q80
    Q80mean_ref <- df1_ALL$Q80mean_ref
    Q80sd_ref <- df1_ALL$Q80sd_ref
    df1_ALL$Q80z_ref <- (Q80 - Q80mean_ref) / Q80sd_ref

    # Get Q90z_adj
    Q90 <- df1_ALL$Q90
    Q90mean_ref <- df1_ALL$Q90mean_ref
    Q90sd_ref <- df1_ALL$Q90sd_ref
    df1_ALL$Q90z_ref <- (Q90 - Q90mean_ref) / Q90sd_ref

    # Get Q95z_adj
    Q95 <- df1_ALL$Q95
    Q95mean_ref <- df1_ALL$Q95mean_ref
    Q95sd_ref <- df1_ALL$Q95sd_ref
    df1_ALL$Q95z_ref <- (Q95 - Q95mean_ref) / Q95sd_ref

    # Get Q99z_adj
    Q99 <- df1_ALL$Q99
    Q99mean_ref <- df1_ALL$Q99mean_ref
    Q99sd_ref <- df1_ALL$Q99sd_ref
    df1_ALL$Q99z_ref <- (Q99 - Q99mean_ref) / Q99sd_ref

    # Get vol_z_adj
    vol <- df1_ALL$volume
    volmean_ref <- df1_ALL$volmean_ref
    volsd_ref <- df1_ALL$volsd_ref
    df1_ALL$vol_z_ref <- (vol - volmean_ref) / volsd_ref

    # Get min_z_adj
    min <- df1_ALL$min
    minmean_ref <- df1_ALL$minmean_ref
    minsd_ref <- df1_ALL$minsd_ref
    df1_ALL$min_z_ref <- (min - minmean_ref) / minsd_ref

    # Get max_z_adj
    max <- df1_ALL$max
    maxmean_ref <- df1_ALL$maxmean_ref
    maxsd_ref <- df1_ALL$maxsd_ref
    df1_ALL$max_z_ref <- (max - maxmean_ref) / maxsd_ref

    # Get min_day7_z_adj
    min_7day <- df1_ALL$min_7day
    min_day7mean_ref <- df1_ALL$min_day7mean_ref
    min_day7sd_ref <- df1_ALL$min_day7sd_ref
    df1_ALL$min_7day_z_ref <- (min_7day - min_day7mean_ref) / min_day7sd_ref

    # Get min_day30_z_adj
    min_30day <- df1_ALL$min_30day
    min_day30mean_ref <- df1_ALL$min_day30mean_ref
    min_day30sd_ref <- df1_ALL$min_day30sd_ref
    df1_ALL$min_30day_z_ref <- (min_30day - min_day30mean_ref) / min_day30sd_ref

    df1_ALL <- df1_ALL %>%
                dplyr::select(-Q5mean_ref, -Q5sd_ref, -Q10mean_ref, -Q10sd_ref, -Q20mean_ref,
                              -Q20sd_ref, -Q25mean_ref, -Q25sd_ref, -Q30mean_ref, -Q30sd_ref,
                              -Q50mean_ref, -Q50sd_ref, -Q70mean_ref, -Q70sd_ref, -Q75mean_ref,
                              -Q75sd_ref, -Q80mean_ref, -Q80sd_ref, -Q90mean_ref, -Q90sd_ref,
                              -Q95mean_ref, -Q95sd_ref,-Q99mean_ref, -Q99sd_ref, -minmean_ref,
                              -minsd_ref, -maxmean_ref, -maxsd_ref, -volmean_ref, -volsd_ref,
                              -min_day30mean_ref, -min_day30sd_ref, -min_day7mean_ref, -min_day7sd_ref)


    df1_ALL.1 <- df1_ALL %>%
      dplyr::mutate(mean = ifelse(n_data >= 2, mean, NA),
                    sd = ifelse(n_data >= 2, sd, NA),
                    Q5 = ifelse(n_data >= 20, Q5, NA),
                    Q10 = ifelse(n_data >= 10, Q10, NA),
                    Q20 = ifelse(n_data >= 5, Q20, NA),
                    Q25 = ifelse(n_data >= 4, Q25, NA),
                    Q30 = ifelse(n_data >= 4, Q30, NA),
                    Q50 = ifelse(n_data >= 2, Q50, NA),
                    Q70 = ifelse(n_data >= 4, Q70, NA),
                    Q75 = ifelse(n_data >= 4, Q75, NA),
                    Q80 = ifelse(n_data >= 5, Q80, NA),
                    Q90 = ifelse(n_data >= 10, Q90, NA),
                    Q95 = ifelse(n_data >= 20, Q95, NA),
                    Q99 = ifelse(n_data >= 100, Q99, NA),
                    Q5z = ifelse(n_data >= 20, Q5z, NA),
                    Q10z = ifelse(n_data >= 10, Q10z, NA),
                    Q20z = ifelse(n_data >= 5, Q20z, NA),
                    Q25z = ifelse(n_data >= 4, Q25z, NA),
                    Q30z = ifelse(n_data >= 4, Q30z, NA),
                    Q50z = ifelse(n_data >= 2, Q50z, NA),
                    Q70z = ifelse(n_data >= 4, Q70z, NA),
                    Q75z = ifelse(n_data >= 4, Q75z, NA),
                    Q80z = ifelse(n_data >= 5, Q80z, NA),
                    Q90z = ifelse(n_data >= 10, Q90z, NA),
                    Q95z = ifelse(n_data >= 20, Q95z, NA),
                    Q99z = ifelse(n_data >= 100, Q99z, NA),
                    dry_n = ifelse(n_data >= 2, dry_n, NA),
                    dry_start = ifelse(n_data >= 28, dry_start, NA),
                    dry_end = ifelse(n_data >= 28, dry_end, NA),
                    dry_mid = ifelse(n_data >= 28, dry_mid, NA),
                    low_n = ifelse(n_data >= 2, low_n, NA),
                    low_start = ifelse(n_data >= 28, low_start, NA),
                    low_end = ifelse(n_data >= 28, low_end, NA),
                    low_mid = ifelse(n_data >= 28, low_mid, NA),
                    low_magnitude = ifelse(n_data >= 28, low_magnitude, NA),
                    low_severity = ifelse(n_data >= 28, low_severity, NA),
                    high_n = ifelse(n_data >= 2, high_n, NA),
                    high_start = ifelse(n_data >= 28, high_start, NA),
                    high_end = ifelse(n_data >= 28, high_end, NA),
                    high_mid = ifelse(n_data >= 28, high_mid, NA),
                    e_above3xq50  = ifelse(n_data >= 28, e_above3xq50, NA),
                    e_above5xq50  = ifelse(n_data >= 28, e_above5xq50, NA),
                    e_above7xq50  = ifelse(n_data >= 28, e_above7xq50, NA),
                    volume = ifelse(n_data >= 3, volume, NA),
                    vol_z = ifelse(n_data >= 3, vol_z, NA),
                    min = ifelse(n_data >= 3, min, NA),
                    min_z = ifelse(n_data >= 3, min_z, NA),
                    min_doy = ifelse(n_data >= 3, min_doy, NA),
                    min_7day = ifelse(n_data >= 90, min_7day, NA),
                    min_7day_z = ifelse(n_data >= 90, min_7day_z, NA),
                    min_7day_doy = ifelse(n_data >= 90, min_7day_doy, NA),
                    min_30day = ifelse(n_data >= 180, min_30day, NA),
                    min_30day_z = ifelse(n_data >= 180, min_30day_z, NA),
                    min_30day_doy = ifelse(n_data >= 180, min_30day_doy, NA),
                    max = ifelse(n_data >= 3, max, NA),
                    max_z = ifelse(n_data >= 3, max_z, NA),
                    max_doy = ifelse(n_data >= 3, max_doy, NA),
                    Q5z_ref = ifelse(n_data >= 20, Q5z_ref, NA),
                    Q10z_ref = ifelse(n_data >= 10, Q10z_ref, NA),
                    Q20z_ref = ifelse(n_data >= 5, Q20z_ref, NA),
                    Q25z_ref = ifelse(n_data >= 4, Q25z_ref, NA),
                    Q30z_ref = ifelse(n_data >= 4, Q30z_ref, NA),
                    Q50z_ref = ifelse(n_data >= 2, Q50z_ref, NA),
                    Q70z_ref = ifelse(n_data >= 4, Q70z_ref, NA),
                    Q75z_ref = ifelse(n_data >= 4, Q75z_ref, NA),
                    Q80z_ref = ifelse(n_data >= 5, Q80z_ref, NA),
                    Q90z_ref = ifelse(n_data >= 10, Q90z_ref, NA),
                    Q95z_ref = ifelse(n_data >= 20, Q95z_ref, NA),
                    Q99z_ref = ifelse(n_data >= 100, Q99z_ref, NA),
                    vol_z_ref = ifelse(n_data >= 3, vol_z_ref, NA),
                    min_z_ref = ifelse(n_data >= 3, min_z_ref, NA),
                    max_z_ref = ifelse(n_data >= 3, max_z_ref, NA),
                    min_7day_z_ref = ifelse(n_data >= 90, min_7day_z_ref, NA),
                    min_30day_z_ref = ifelse(n_data >= 180, min_30day_z_ref, NA)
                    )

    df1_ALL.1$win_no <- as.numeric(df1_ALL.1$win_no)
    df1_ALL.1 <- arrange(df1_ALL.1, flow_site_id, win_no)

    # Create a list of output_1 (with adj QXz values) and output_2
    final_list_ref <- list(df1_ALL.1, df2)

  }

  if(is.null(ref_col) == TRUE){return(final_list)}
  if(is.null(ref_col) == FALSE){return(final_list_ref)}

}


####################################################################
## CalcFlowStats
# Required as part of calc_flowstats (main function)
# calculates Qstats, min, max, volume, min_7day, min_30day
# calculates mean & sd for all parameters, then calculates z-stats
# Output passed to CreateFlowData

CalcFlowStats <- function (flowts) {

  # calculate missing data before NAs are removed
  MISSING <- flowts %>%
    dplyr::group_by(site, win_no) %>%
    dplyr::summarise(n_data = sum(!is.na(flow)),
                     n_missing = sum(is.na(flow)),
                     n_total = n_missing + n_data,
                     prop_missing = n_missing / n_total)

  # remove NAs and calculate flow stats for each site by time period (win_no)
  flowts <- flowts %>%
    dplyr::group_by(site, win_no) %>%
    dplyr::filter(!is.na(flow)) %>%
      dplyr::summarise(n_data = length(flow),
                       mean = mean(flow),
                       sd = sd(flow),
                       Q5 = quantile(flow, probs=0.95, na.rm=TRUE),
                       Q10 = quantile(flow, probs=0.9, na.rm=TRUE),
                       Q20 = quantile(flow, probs=0.8, na.rm=TRUE),
                       Q25 = quantile(flow, probs=0.75, na.rm=TRUE),
                       Q30 = quantile(flow, probs=0.7, na.rm=TRUE),
                       Q50 = quantile(flow, probs=0.5,na.rm=TRUE),
                       Q70 = quantile(flow, probs=0.3,na.rm=TRUE),
                       Q75 = quantile(flow, probs=0.25, na.rm=TRUE),
                       Q80 = quantile(flow, probs=0.2, na.rm=TRUE),
                       Q90 = quantile(flow, probs=0.1,na.rm=TRUE),
                       Q95 = quantile(flow, probs=0.05,na.rm=TRUE),
                       Q99 = quantile(flow, probs=0.01,na.rm=TRUE),
                       volume = sum(flow),
                       min = min(flow, na.rm = TRUE),
                       min_7day = min(zoo::rollmean(flow, 7), na.rm = TRUE),
                       min_30day = min(zoo::rollmean(flow, 30), na.rm = TRUE),
                       max = max(flow, na.rm = TRUE),
                       e_above3xq50 = riisbiggs2(flow, Q50, 3),
                       e_above5xq50 = riisbiggs2(flow, Q50, 5),
                       e_above7xq50 = riisbiggs2(flow, Q50, 7)) %>%
                       dplyr::ungroup() %>%
                       dplyr::group_by(site) %>%
                       dplyr::mutate(Q5mean = mean(Q5),
                              Q10mean = mean(Q10),
                              Q20mean = mean(Q20),
                              Q25mean = mean(Q25),
                              Q30mean = mean(Q30),
                              Q50mean = mean(Q50),
                              Q70mean = mean(Q70),
                              Q75mean = mean(Q75),
                              Q80mean = mean(Q80),
                              Q90mean = mean(Q90),
                              Q95mean = mean(Q95),
                              Q99mean = mean(Q99),
                              vol_mean = mean(volume),
                              min_mean = mean(min),
                              min_7day_mean = mean(min_7day),
                              min_30day_mean = mean(min_30day),
                              max_mean = mean(max),
                              Q5sd = sd(Q5),
                              Q10sd = sd(Q10),
                              Q20sd = sd(Q20),
                              Q25sd = sd(Q25),
                              Q30sd = sd(Q30),
                              Q50sd = sd(Q50),
                              Q70sd = sd(Q70),
                              Q75sd = sd(Q75),
                              Q80sd = sd(Q80),
                              Q90sd = sd(Q90),
                              Q95sd = sd(Q95),
                              Q99sd = sd(Q99),
                              vol_sd = sd(volume),
                              min_sd = sd(min),
                              min_7day_sd = sd(min_7day),
                              min_30day_sd = sd(min_30day),
                              max_sd = sd(max),
                              Q5z = (Q5-Q5mean)/Q5sd,
                              Q10z = (Q10-Q10mean)/Q10sd,
                              Q20z = (Q20-Q20mean)/Q20sd,
                              Q25z = (Q25-Q25mean)/Q25sd,
                              Q30z = (Q30-Q30mean)/Q30sd,
                              Q50z = (Q50-Q50mean)/Q50sd,
                              Q70z = (Q70-Q70mean)/Q70sd,
                              Q75z = (Q75-Q75mean)/Q75sd,
                              Q80z = (Q80-Q80mean)/Q80sd,
                              Q90z = (Q90-Q90mean)/Q90sd,
                              Q95z = (Q95-Q95mean)/Q95sd,
                              Q99z = (Q99-Q99mean)/Q99sd,
                              vol_z = (volume - vol_mean) / vol_sd,
                              min_z = (min - min_mean) / min_sd,
                              min_7day_z = (min_7day - min_7day_mean) / min_7day_sd,
                              min_30day_z = (min_30day - min_30day_mean) / min_30day_sd,
                              max_z = (max - max_mean) / max_sd) %>%
                        dplyr::full_join(MISSING)

    return(flowts)

}

###########################################################
## CreateLongData
# Required as part of calc_flowstats (main function)
# output then used in CreateFlowData
# calculates BFI
# calculates flow duration curve
# calculates 'long_data' required for CreateFlowData

CreateLongData <- function(flow.data, statsData) {

  # rename data
  flow_data <- flow.data
  QSTATS1 <- statsData

  # calculate base flow index, if any stations have flows with 0's, make that bfi NA
  # if there are no 0s in the flow data then use calc_bfi to calculate base flow index
  # if flow cantains 0s then return NA
  bfi <- flow_data %>%
    dplyr::filter(!is.na(flow)) %>%
    dplyr::mutate(x=flow) %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(bfi = if(0 %in% x == FALSE ){ bfi= calc_bfi(.$x) }else{ print ("flow contains 0's, returning NA")
      bfi=NA}) %>%
    dplyr::mutate(win_no = "Annual") %>%
    tidyr::gather(-site, -win_no, key = parameter, value = value)

  # calculate long-term flow duration curve percentile 1:99
  # returns a warning that calculation is ignoring missing values- even when NA are removed
  FlowDurationCurve <- flow_data %>%
    dplyr::filter(!is.na(flow)) %>%
    dplyr::group_by(site) %>%
    dplyr::group_modify(~ (fasstr::calc_longterm_percentile(data = flow_data, dates = date, values = flow, percentiles=c(1:99), transpose = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(win_no="Annual") %>%
    setNames(., c("site", "parameter", "value", "win_no")) %>%
    dplyr::mutate(parameter=as.character(parameter))

  # create a long dataframe containing just means and sd for each Q value, BFI and flow duration curve
  long_data <- QSTATS1 %>%
    dplyr::filter(!is.na("Q30")) %>%
    # make dataframe long
    tidyr::gather(-site, -win_no, key = stat, value = stat_value) %>%  # select just the stats we want
    dplyr::filter(stat== "Q5"| stat== "Q10"| stat== "Q20"| stat== "Q25"| stat== "Q30"| stat== "Q50" | stat== "Q70" |  stat== "Q75" |stat== "Q80"|stat== "Q90"| stat== "Q95"| stat== "Q99"| stat == "vol"| stat== "min"| stat== "min_7day"| stat== "min_7day"| stat== "max") %>%
    dplyr::group_by(stat, site) %>%
    # calculate the mean and sd for each statistic by site
    dplyr::summarise(mean = mean(stat_value, na.rm=TRUE), sd = sd(stat_value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(sd, mean, key = id, value = value) %>%                   # gather mean and sd columns into a column with values and key column (ie mean or sd)
    dplyr::do(within(.,  parameter <- paste(stat, id, sep=""))) %>%                     # concatenate stat (ie Q10) and id (ie mean or sd) columns (example: Q10mean, Q70mean)
    dplyr::select(-stat, -id) %>%
    dplyr::mutate(win_no = "Q_Annual") %>%
    dplyr::bind_rows(bfi)  %>%
    dplyr::bind_rows(FlowDurationCurve)      # remove the stat and id columns

  return(long_data)

}


#######################################################################################
## CreateFlowStats
# Required for calc_flowstats (main function)
# calculates durations above/below qhigh/qlow, and zero flow durations
# Calculates the minimum/maximum flow date as DOY, plus mean 30/70day_min DOY

CreateFlowStats <- function(stats_data, long.data, station_data, original_data, q_high, q_low) {

  # rename inputs
  QSTATS1 <- stats_data
  long_data <- long.data
  flow_data <- station_data
  q_high <- as.numeric(q_high); q_low <- as.numeric(q_low)
  data_1 <- original_data

  # filter data for calculating zero events, events above/below q_high/q_low
  thres_data <- flow_data %>%
    dplyr::filter(!is.na(flow))

  # find zero flow days
  zeros <- thres_data %>%
    dplyr::group_by(site, win_no) %>%
    dplyr::do(zero_eventsDuration(.))
  colnames(zeros) <- c("site", "win_no", "dry_n", "dry_e", "dry_start", "dry_end", "dry_mid")   # rename columns

  # calculate QHigh and QLow
  # needed to calculate durations above and below threshold value
  high <- (100 - q_high)
  low <- (100 - q_low)
  Q_high <- paste("P", sep = "", high)
  Q_low <- paste("P", sep = "", low)
  Qhigh <- long_data %>%
    dplyr::filter(parameter == Q_high)
  Qlow <- long_data %>%
    dplyr::filter(parameter == Q_low)
  Qvals <- rbind(Qhigh, Qlow)
  Qvals2<- Qvals %>%
    tidyr::spread(., key=parameter, value=value) %>%
    dplyr::select(-win_no)
  colnames(Qvals2) <- c("site", "qLow", "qHigh")
  thres_dataQ <- dplyr::left_join(thres_data, Qvals2, by = "site")

  # calculate duration/events above q_low and q_high
  duration_above <- thres_dataQ %>%
    dplyr::group_by(site, win_no, qHigh) %>%
    dplyr::do(find_eventDuration(.,  unique(.$qHigh), "high")) %>%
    ungroup() %>%
    dplyr::select(-qHigh)
  colnames(duration_above) <- c("site", "win_no", "high_n", "high_e", "high_start", "high_end", "high_mid")   # rename columns

  duration_below <- thres_dataQ %>%
    dplyr::group_by(site, win_no, qLow) %>%
    dplyr::do(find_eventDuration(.,  unique(.$qLow), "low")) %>%
    ungroup %>%
    dplyr::select(-qLow)
  colnames(duration_below) <- c("site", "win_no", "low_n", "low_e", "low_start", "low_end", "low_mid", "low_magnitude", "low_severity")   # rename columns
  Durations <- dplyr::full_join(duration_above, duration_below, by=c("site", "win_no"))

  if(isTRUE(1 %in% diff.Date(data_1$date)) == TRUE){
  # calculate min, max, min 7/30day rolling mean DOY
  # min plus 7day_min
  doy1 <- thres_data %>%
    dplyr::group_by(site, win_no) %>%
    dplyr::do(find_doy(.,  "low", 7))
  colnames(doy1) <- c("site", "win_no", "min_doy", "min_7day_doy")

  # max plus 30day_min
  doy2 <- thres_data %>%
    dplyr::group_by(site, win_no) %>%
    dplyr::do(find_doy(.,  "high", 30))
  colnames(doy2) <- c("site", "win_no", "max_doy", "min_30day_doy")
  minmax_doy <- dplyr::full_join(doy1, doy2, by=c("site", "win_no"))
  }

  else {
    minmax_doy <- thres_data %>% dplyr::group_by(site, win_no) %>%
      dplyr::mutate(min_doy = NA,
                     min_7day_doy = NA,
                     max_doy = NA,
                     min_30day_doy = NA)
  }

  # Join QSTATS1 with Duration (dataframe with durations/events above/below)
  QSTATS1.2<- dplyr::full_join(QSTATS1, Durations, by=c("site", "win_no"))
  # Then join with zeros
  QSTATS1.3<- dplyr::full_join(QSTATS1.2, zeros, by=c("site", "win_no"))
  # Then join with minmax_doy
  QSTATS1.4<- dplyr::full_join(QSTATS1.3, minmax_doy, by=c("site", "win_no"))

  # create final dataset
  QSTATS_final<- QSTATS1.4

  return(QSTATS_final)
}

##############################################################################

############### FUNCTIONS USED IN CalcFlowStats ##############################
## riisbiggs2
# Required for CalcFlowStats
# Calculates the number of times a flow threshold is exceeded

riisbiggs2 <- function(datavector, threshold, multiplier) {
  # code checked 26/6/2008. Numbers of times threshold exceeded are all turned into annual averages. Lets leave it like that for now
  # original riis / biggs work was all based on annual values
  # if we are looking at different time windows in the same analysis, this would need some more thought
  # difference between 182 and 183 days probably not worth worrying about

  # in the future, absolute values might be more useful in some cases.
  # re-checked 21/10/2013

  nyears <- length(datavector)/365.25
  medvec5 <- as.numeric(datavector > multiplier*threshold) # create vector of TRUE / FALSE depending on whether flow is > threshold
  medvec5 <- medvec5[!is.na(medvec5)] # NAs should have been removed already

  test <- rle(medvec5)
  # length(test$lengths[test$values==1])/nyears
  length(test$lengths[test$values==1])
}

############### FUNCTIONS USED IN CreateLongData ##############################

###############################################
## calc_bfi
# Required for CreateLongData

calc_bfi <- function(x) {

  day7mean <- zoo::rollmean(x, 7, align = "right")
  min7day <- min(day7mean)
  meanflow <- mean(x)
  calc_bfi <- min7day/meanflow
  return(calc_bfi)

}

###############################################

############### FUNCTIONS USED IN CreateFlowStats ##############################

###############################################
## find_eventDuration
# Required for CreateFlowStats
# Calculates the duration in days above or below a flow threshold, plus magnitude and severity of flow deficits below a given threshold

find_eventDuration <- function(x, threshold ,type, pref) {
  # find events above/below a given threshold

  # find high or low flow events
  flowEvents <- find_events(x, threshold=threshold, type=type)
  flowEvents <- stats::na.omit(flowEvents)  # remove NAs

  # find number of records
  n_records <- length(flowEvents$event)

  # find number of events
  n_events <- length(unique(flowEvents$event))

  # if n_events > 0, find start, end & mid dates, otherwise skip & assign NAs
  if(n_events > 0){

      flowEvents$yday <- lubridate::yday(flowEvents$date)

      # first low/high flow day
      n_start <- flowEvents %>% dplyr::arrange(date) %>% dplyr::slice(1)
      n_start <- n_start$yday

      # last low/high flow day
      n_end <- flowEvents %>% dplyr::arrange(date)
      n_end <- utils::tail(n_end, n=1)
      n_end <- n_end$yday

      # average low flow day
      conv <- 2*pi/365 ## doy -> radians
      n_mid1 <- circ.mean(conv*(flowEvents$yday-1))/conv
      n_mid <- round((n_mid1 + 365) %% 365,0)

  } else {

      n_start <- NA; n_end <- NA; n_mid <- NA

  }

  find_eventDurations <- data.frame(n_records, n_events, n_start, n_end, n_mid)

  # calculate mean and cumulative deficit for low flow events
  if(type == "low" && n_events > 0){
      flowEvents$deficit <- (threshold - flowEvents$flow)
      mean_deficit <- mean(flowEvents$deficit, na.rm = TRUE)
      cumulative_deficit <- sum(flowEvents$deficit, na.rm = TRUE)

      find_eventDurations <- data.frame(n_records, n_events, n_start, n_end, n_mid, mean_deficit, cumulative_deficit)
  }

  return(find_eventDurations)
}

#################################################
## find_events
# Required for find_eventDuration
# Finds events above/below qhigh/qlow

#' find_events(x,threshold)
find_events <- function(flow_data, threshold, type="high") {

  x <- flow_data

  x <- x %>% dplyr::filter(!is.na(flow))

  if(type=="high")
  {
    #Calculate flows in excess of threshold
    #And define high flow as positive number

    x$event <- ifelse(x$flow > threshold,T,F)
  } else {
    x$event <- ifelse(x$flow < threshold,T,F)
    ###Remove first "event" since the time at strt of year before flood is not between 2 floods
  }


  #Calculate run lengths of T of F values to classify events
  runLengths <- rle(x$event)
  runLengths <- data.frame(lengths = runLengths$lengths,
                           values = runLengths$values,
                           eventNum = NA)

  #Make sequence of numbers to number events
  events <- 1:sum(runLengths$values==T)

  #Number events
  runLengths$eventNum[runLengths$values==T] <- events
  eventVector <- rep(runLengths$eventNum,runLengths$lengths)

  #Bind events to X
  x$event <- eventVector

  flowEvents <- x


  return(flowEvents)
}

#################################################
## zero_eventsDuration
# same as find_eventDuration, but for zero flows
# Calculates the duration in days that flow are 0
# used in CreateFlowStats

zero_eventsDuration <- function(flow_data) {
  # find events above/below a given threshold
  zeroEvents <- zero_events(flow_data)

  zeroEvents <- stats::na.omit(zeroEvents)  # remove NAs
  # find number of records
  n_records <- length(zeroEvents$event)

  # find number of events
  n_events <- length(unique(zeroEvents$event))

  zeroEvents$yday <- lubridate::yday(zeroEvents$date)

  # first low flow day
  dry_start <- zeroEvents %>% dplyr::arrange(date) %>% dplyr::slice(., 1)
  dry_start <- dry_start$yday

  # last low flow day
  dry_end <- zeroEvents %>% dplyr::arrange(date)
  dry_end <- utils::tail(dry_end, n=1)
  dry_end <- dry_end$yday

  # average low flow day
  dry_mid <- mean(zeroEvents$yday)

  if(n_events == 0){
    dry_start <- NA
    dry_end <- NA
    dry_mid <- NA
  }

  find_zeroDurations <- data.frame(n_records, n_events, dry_start, dry_end, dry_mid)

  return(find_zeroDurations)
}

#####################################################################
## zero_events
# effectively the same as find_events, but for 0 flows
# finds zero-flow events
# required for zero_eventsDuration

zero_events <- function(flow_data) {

  x <- flow_data

  x <- x %>% dplyr::filter(!is.na(flow))

  x$Obs_0 <- ifelse(x$flow == 0, T, F)

  #Calculate run lengths of T of F values to classify events
  runLengths <- rle(x$Obs_0)
  runLengths <- data.frame(lengths = runLengths$lengths,
                           values = runLengths$values,
                           eventNum = NA)

  #Make sequence of numbers to number events
  events <- 1:sum(runLengths$values==T)

  #Number events
  runLengths$eventNum[runLengths$values==T] <- events
  eventVector <- rep(runLengths$eventNum,runLengths$lengths)

  #Bind events to X
  x$Obs_0 <- eventVector

  zeroEvents <- x

  return(zeroEvents)
}

#####################################################################
## find_doy
# used in CreateFlowStats
# finds the minimum or maximum flow date as DOY
# finds day of year (1-366, as midpoint) of 7-day or 30-day minimum flow period

find_doy <- function(flow_data, type, nday) {

  # pull-in data
  x <- flow_data
  x2 <- x %>% dplyr::filter(!is.na(flow))

  ## find min or max flow
  if(type == "low"){
    flow_day <- x2 %>% dplyr::filter(flow == min(x2$flow))}
  if(type == "high"){
    flow_day <- x2 %>% dplyr::filter(flow == max(x2$flow))}

  # find first low or high flow day
  flow_day2 <- flow_day %>% dplyr::arrange(date) %>% head(., n = 1)
  # convert to DOY
  flow_day2$yday <- lubridate::yday(flow_day2$date)
  # first low or high flow day
  m_day <- as.numeric(unique(flow_day2$yday))

  ## find 7day or 30day rolling mean
  if(nday == 7){

    if(length(x2) > 7) {
    NAs <- c(NA, NA, NA, NA, NA, NA)
    minNday <- data.frame(roll_mean = c(NAs, zoo::rollmean(x2$flow, 7)))
    x2$roll_mean <- minNday$roll_mean

    }

    if(length(x2) < 7) {

      min_day <- NA
    }

  }

  if(nday == 30){

    if(length(x2) > 30) {
    NAs <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    minNday <- data.frame(roll_mean = c(NAs, zoo::rollmean(x2$flow, 30)))
    x2$roll_mean <- minNday$roll_mean
    }

    if(length(x2) < 30) {

      min_day <- NA
    }

  }

    if(length(x2) > 7) {

  # filter to find date of lowest mean flow
  low_flow <- x2 %>% dplyr::filter(roll_mean == min(roll_mean, na.rm = TRUE)) %>%
    dplyr::arrange(date) %>% head(., n = 1)

  # convert to DOY
  if(nday == 7){
    low_flow$yday <- lubridate::yday(low_flow$date - lubridate::days(3))
  }
  if(nday == 30){
    low_flow$yday <- lubridate::yday(low_flow$date) - 14.5
    if(isTRUE(-1 %in% sign(low_flow$yday)) == TRUE) {
      low_flow$yday <- 365 - abs(low_flow$yday)
    }
  }
  # first mean low flow day
  min_day <- as.numeric(low_flow$yday)

    }

  # combine low/high flow DOY and 7/30day mean min flow DOY
  find_doy <- data.frame(m_day, min_day)

  return(find_doy)

}

##############################################################
## check whether a column if formatted as date

IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}


##############################################################
## find circular mean

circ.mean <- function (x) {
  sinr <- sum(sin(x))
  cosr <- sum(cos(x))
  circmean <- atan2(sinr, cosr)
  circmean
}

##############################################################
## test if integer

testInteger <- function(x){
  test <- all.equal(x, as.integer(x), check.attributes = FALSE)
  if(test == TRUE){ return(TRUE) }
  else { return(FALSE) }
}
