#' Calculate a suite of long-term and seasonal flow statistics for one or more sites.
#'
#' @description This function takes a time series of measured or modelled flows and uses a user-defined moving window to calculate a suite of time-varying flow statistics for one or more sites (stations). A smaller set of long-term statistics is also calculated. It is primarily designed to work with daily flows but can also be applied to time series data on a longer (e.g. 10-daily or monthly) time step. The data should be regularly spaced, and a common time step should be used for all sites.
#'
#' @usage calc_flowstats(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", imputed_col = “imputed”, win_start = "1995-04-01", win_width = "6 months", win_step = "6 months", date_range = NULL, q_low = 95, q_high = 70, standardise = FALSE, ref_col = NULL)
#'
#'@param data Tibble or data frame containing the flow data to be processed. Must be in long format and contain, as a minimum site id, date and flow (e.g. as output by the import_flow() function).
#'@param site_col Name of column in data containing unique flow site id. Default = "flow_site_id".
#'@param date_col Name of column in data containing date of flow record. Default = "date".
#'@param flow_col Name of column in data containing flow data for processing (character). Default = "flow".
#'@param imputed_col Name of optional column in data specifying whether each flow value is measured (0) or imputed (1).  Default = NULL.
#'@param win_start Start date of first time window (in yyyy-mm-dd format). Default = "1995-04-01".
#'@param win_width Width of the time window, in days, weeks, months, quarters or years  (see ?seq.Date for options). Default = "6 months".
#'@param win_step The increment by which the time window moves, in days, weeks, months, quarters or years (see ?seq.Date for options). Default = "6 months".
#'@param q_low Qx flow threshold (between 1 and 99) defining low flow events. Default = 95 (representing the long-term Q95 flow at each site).
#'@param q_high Qx flow threshold (between 1 and 99) defining high flow events. Default = 70 (representing the long-term Q70 flow at each site).
#'@param date_range Optional vector of two dates (in yyyy-mm-dd format) defining the period of flow data to be analysed. Default = NULL . Flow records outside this range are excluded. For unbiased calculation of long-term flow statistics, it is advisable that this range spans a whole number of years.
#'@param scaling Should the time series flow data be scaled by the long-term mean flow  at each site? Default = FALSE.
#'@param ref_col Name of column in dataset containing reference flow scenario against which selected flow statistics are z-score standardised. Default = NULL.

#' @details
#'
#'
#' @return calc_flowstats returns a list of two data frames. The first data frame contains a suite of time-varying flow statistics for every 6 month winter/summer period at every site. The columns are as follows:
#'
#'    - flow_site_id: a unique site id.
#'    - start_date: start date of the time period (in yyyy-mm-dd format).
#'    - end_date: end date of the time period (in yyyy-mm-dd format).
#'    - n_data: the number of records with valid flows (not NA).
#'    - n_missing: the number of missing flow (flow = NA).
#'    - n_total: the total number of flow records (sum of n_data and n_missing).
#'    - prop_missing: the proportion of missing flow records.
#'    - n_imputed: the number of flow records that have been imputed (calculated only if the imputed_col argument is specified).
#     - prop_imputed: the proportion of flow records that have been imputed (calculated only if the imputed_col argument is specified).
#'UPDATE FLOW STATS AFTER THIS POINT
#'    - Q10: the unstandardised Q10 flow in a 6-month winter or summer period.
#'    - Q30: the unstandardised Q30 flow in a 6-month winter or summer period.
#'    - Q50: the unstandardised Q50 flow in a 6-month winter or summer period.
#'    - Q70: the unstandardised Q70 flow in a 6-month winter or summer period.
#'    - Q90: the unstandardised Q70 flow in a 6-month winter or summer period.
#'    - Q95: the unstandardised Q95 flow in a 6-month winter or summer period.
#'    - Q99: the unstandardised Q99 flow in a 6-month winter or summer period.
#'    - Q10mean: the seasonal (winter or summer) Q10 flow, averaged across years.
#'    - Q10sd: the standard deviation of the seasonal (winter or summer) Q10 flows.
#'    - Q10z: the Q10 flow in a six month period, standardised using the above mean and sd. If ref_col = NULL, then the mean and sd parameters are calculated using the same flow time series (i.e. Q10z = (Q10 - Q10mean) / Q10sd); if not, the mean and sd parameters are calculated from the ref_col flow time series (i.e. Q10z = (Q10 - Q10mean_ref) / Q10sd_ref).
#'    - Q30mean: (as for Q10)
#'    - Q30sd: (as for Q10)
#'    - Q30z: (as for Q10)
#'    - Q50mean: (as for Q10)
#'    - Q50sd: (as for Q10)
#'    - Q50z: (as for Q10)
#'    - Q70mean: (as for Q10)
#'    - Q70sd: (as for Q10)
#'    - Q70z: (as for Q10)
#'    - Q95: (as for Q10)
#'    - Q95mean: (as for Q10)
#'    - Q95sd: (as for Q10)
#'    - Q95z: (as for Q10)
#'    - zero:	Number of zero flow days
#'    - mean:	mean flow
#'    - sd: standard deviation of flows
#'    - lsd: standard deviation of the log flow series
#'    - n:	number of days (182 or 183)
#'    - e3: number of events when flows exceed 3 x the long-term median (Q50) flow
#'    - e5: number of events when flows exceed 5 x the long-term median (Q50) flow
#'    - e7: number of events when flows exceed 7 x the long-term median (Q50) flow
#'    - missing: number of missing records (NAs)
#'    - durationAbove: number of records above the long-term Q70
#'    - nEventsAbove: number of events when flows exceed the long-term Q70
#'    - durationBelow: number of records below the long-term Q95
#'    - nEventsBelow: number of events when flows exceed the long-term Q95
#'    - e3mean: e3, averaged across years
#'    - e5mean: e5, averaged across years
#'    - e7mean: e7, averaged across years
#'    - durationAboveMean: durationAbove, averaged across years
#'    - durationBelowMean: durationBelow, averaged across years
#'    - nEventsAboveMean: nEventsAbove, averaged across years
#'    - nEventsBelowMean: nEventsBelow, averaged across years
#'
#' The second data table contains long-term flow statistics. The data are arranged in long format, with the following columns:
#' - flow_site_id (a unique site id)
#' - start_date: start date of the long-term time period (in yyyy-mm-dd format) for which the statistics are calculated.
#' - end_date: end date of the long-term time period (in yyyy-mm-dd format) for which the statistics are calculated.
#' - parameter  (minimum, maximum and mean flow; flow duration curve percentiles (p1 to p99); base flow index (bfi); and seasonal means and standard deviations for Q50, Q30, Q50, Q75 and Q95).
#' - value (calculated statistic).
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

  if(is.null(win_start) == FALSE && IsDate(win_start, "%Y-%m-%d") == FALSE)
  {stop("win_start should be in YYYY-MM-DD format")}

  if(is.null(win_start) == FALSE && IsDate(win_start, "%Y-%m-%d") == TRUE && isTRUE(win_start > Sys.Date()) == TRUE)
  {stop("win_start is in the future")}

  matches <- c("day", "month", "year")
  if(is.null(win_width) == FALSE && grepl(matches[1], win_width) == TRUE){""} else {
    if(is.null(win_width) == FALSE && grepl(matches[2], win_width) == TRUE){""} else {
      if(is.null(win_width) == FALSE && grepl(matches[3], win_width) == TRUE){""} else {
          stop("win_width must by in day, month, or year format")
        }
      }
    }

  if(is.null(win_step) == FALSE && grepl(matches[1], win_step) == TRUE){""} else {
    if(is.null(win_step) == FALSE && grepl(matches[2], win_step) == TRUE){""} else {
      if(is.null(win_step) == FALSE && grepl(matches[3], win_step) == TRUE){""} else {
          stop("win_step must by in day, month, or year format")
        }
      }
    }

  if(is.null(date_range) == FALSE && IsDate(date_range[1], "%Y-%m-%d") == TRUE){""} else {
    if(is.null(date_range) == FALSE && IsDate(date_range[2], "%Y-%m-%d") == TRUE){""} else {
      if(is.null(date_range) == TRUE){""} else {
       stop("date_range should be in YYYY-MM-DD format")
      }
    }
  }
  if(is.null(date_range) == FALSE && isTRUE(length(date_range > 2)) == TRUE)
  {"date_range should be of maximum length 2"}
  if(is.null(date_range) == FALSE && IsDate(date_range[1], "%Y-%m-%d") == TRUE && IsDate(date_range[2], "%Y-%m-%d") == TRUE && isTRUE(date_range[1] >= date_range[2]) == TRUE)
  {stop("start date exceed end date, please check date_range")}
  if(is.null(date_range) == FALSE && IsDate(date_range[1], "%Y-%m-%d") == TRUE && isTRUE(date_range[1] > Sys.Date()) == TRUE)
  {stop("date_range[1] is in the future")}
  if(is.null(date_range) == FALSE && IsDate(date_range[2], "%Y-%m-%d") == TRUE && isTRUE(date_range[2] > Sys.Date()) == TRUE)
  {stop("date_range[2] is in the future")}

  if(is.logical(scaling) == FALSE){stop("'scaling' is not logical")}

  if(is.null(q_low) == FALSE && between(q_low, 1, 99) == FALSE)
  {stop("q_low must be a value between 1 and 99")}

  if(is.null(q_high) == FALSE && between(q_low, 1, 99) == FALSE)
  {stop("q_high must be a value between 1 and 99")}


  # pull-in data
  data_1 <- data
  data_1$date <- dplyr::pull(data_1, date_col)
  data_1$flow <- dplyr::pull(data_1, flow_col)

  # Check flow is numeric
  if(is.numeric(data_1$flow) == FALSE)
  {stop("Specified flow_col is not numeric")}

  # Check for duplicate dates
  duplicates <- data_1$Date[duplicated(data_1$Date)]

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
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% lubridate::days(width_no),
                                         win_no = 1:nrow(.))
  }

  # if win_width is in months
  if(is.null(win_width) == FALSE && grepl("month", win_width) == TRUE){
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% months(width_no),
                                    win_no = 1:nrow(.))
  }

  # if win_width is in years
  if(is.null(win_width) == FALSE && grepl("year", win_width) == TRUE){
    all_dates <- expand_dates %>% dplyr::mutate(end_date = start_date %m+% lubridate::years(width_no),
                                         win_no = 1:nrow(.))
  }

  # assign full set of moving windows to each site
  win_dates <- dplyr::full_join(all_dates, sites, by = character())
  win_dates$win_no <- as.character(win_dates$win_no)

  all_win_dates <- win_dates %>% dplyr::rowwise() %>%
    do(data.frame(site =.$site,
                  date = seq(.$start_date, .$end_date, by="days"),
                  win_no =.$win_no,
                  win_start_date = .$start_date,
                  win_end_date = .$end_date))

  # if scaling = TRUE, standardise flow data by the mean (for each site)
  if(scaling == TRUE){

    means <- data_1 %>% dplyr::group_by(site) %>%
                        dplyr::summarise(mean_f = (mean(flow, na.rm = TRUE)))
    data_1 <- data_1 %>% dplyr::left_join(means, by = "site") %>%
                         dplyr::mutate(flow = flow / mean_f) %>%
                         dplyr::select(-mean_f)
    }

  # join window dates to flow data
  my_data <- dplyr::left_join(all_win_dates, data_1, by = c("site", "date"))

  # drop columns not required for flow calcs (these will be re-joined at the end)
  my_data_2 <- my_data %>% dplyr::select(site, date, win_no, flow)

  # format and rename data in-line with APPLYFLOWSTATS functions
  test.flow.rec <-  my_data_2
  temp <- cbind(DAY=as.integer(lubridate::day(test.flow.rec$date)),
                MONTH=as.integer(lubridate::month(test.flow.rec$date)),
                YEAR=as.integer(lubridate::year(test.flow.rec$date)))
  FULL.FLOW.REC <- cbind(test.flow.rec, temp)
  names(FULL.FLOW.REC) <- casefold(names(FULL.FLOW.REC))

  # if imputed_col is specified, count imputed values
  if(is.null(imputed_col) == FALSE){

  my_data$imputed <- dplyr::pull(my_data, imputed_col)
  count_imputed <- my_data %>%
                     dplyr::group_by(site, win_no) %>%
                     dplyr::count(., imputed) %>%
                     dplyr::rename(n_imputed = n)
  }

  # calculate flow statistics
  mylist <- APPLYFLOWSTATS(FULL.FLOW.REC, q_high, q_low)

  # extract flow stats from list
  df1_a <- mylist[[1]]
  df2 <- mylist[[2]]
  df3 <- mylist[[3]]

  # join main data with win_dates and rename cols
  df1.1 <- dplyr::left_join(win_dates, df1_a, by=c("site", "win_no"))
  df1.2 <- df1.1 %>% dplyr::rename(max = high,
                               max_z = highz,
                               min = low,
                               min_z = lowz,
                               min_30day = day30,
                               min_30day_z = day30z,
                               min_7day = day7,
                               min_7day_z = day7z,
                               vol_z = volz,
                               n_data = n,
                               n_missing = missing) %>%
                        dplyr::mutate(n_total = (n_missing + n_data),
                               prop_missing = (n_missing/n_data))

  # get rid of mean & sd
  df1.3 <- df1.2 %>%
    dplyr::select(-Q5mean, -Q5sd, -Q10mean, -Q10sd, -Q20mean, -Q20sd,
                  -Q25mean, -Q25sd, -Q30mean, -Q30sd, -Q50mean, -Q50sd,
                  -Q70mean, -Q70sd, -Q75mean, -Q75sd, -Q80mean, -Q80sd, -Q90mean, -Q90sd,
                  -Q95mean, -Q95sd, -Q99mean, -Q99sd, -day7mean, -day7sd,
                  -day30mean, day30sd, lowmean, -lowsd, -highmean, -highsd,
                  -volmean, -volsd)

  if(is.null(imputed_col) == TRUE){
  # re-order cols
  col_order <- c("site", "win_no", "start_date", "end_date", "n_data", "n_missing", "n_total", "prop_missing", "mean", "sd", "lsd", "Q5", "Q10", "Q20", "Q25", "Q30", "Q50", "Q70", "Q75", "Q80", "Q90", "Q95", "Q99", "Q5z", "Q10z", "Q20z", "Q25z", "Q30z", "Q50z", "Q70z", "Q75z", "Q80z", "Q90z", "Q95z", "Q99z", "dry_n", "dry_e", "dry_start", "dry_end", "dry_mid", "low_n", "low_e", "low_start", "low_end", "low_mid", "low_magnitude", "low_severity", "high_n", "high_e", "high_start", "high_end", "high_mid", "vol", "vol_z", "min", "min_z", "min_doy", "min_7day", "min_7day_z", "min_7day_doy", "min_30day", "min_30day_z", "min_30day_doy", "max", "max_z", "max_doy")
  df1 <- df1.3[, col_order]
  }

  if(is.null(imputed_col) == FALSE){
  # add in count of imputed flows
  df1.4 <- dplyr::left_join(df1.3, count_imputed, by = c("site", "win_no")) %>%
    dplyr::mutate(n_imputed = ifelse(imputed == 1, n_imputed, 0)) %>%
    dplyr::select(-imputed) %>%
    dplyr::mutate(prop_imputed = (n_imputed/n_data))
  # re-order cols
  col_order <- c("site", "win_no", "start_date", "end_date", "n_data", "n_missing", "n_total", "prop_missing", "n_imputed", "prop_imputed", "mean", "sd", "lsd", "Q5", "Q10", "Q20", "Q25", "Q30", "Q50", "Q70", "Q75", "Q80", "Q90", "Q95", "Q99", "Q5z", "Q10z", "Q20z", "Q25z", "Q30z", "Q50z", "Q70z", "Q75z", "Q80z", "Q90z", "Q95z", "Q99z", "dry_n", "dry_e", "dry_start", "dry_end", "dry_mid", "low_n", "low_e", "low_start", "low_end", "low_mid", "low_magnitude", "low_severity", "high_n", "high_e", "high_start", "high_end", "high_mid", "vol", "vol_z", "min", "min_z", "min_doy", "min_7day", "min_7day_z", "min_7day_doy", "min_30day", "min_30day_z", "min_30day_doy", "max", "max_z", "max_doy")
  df1 <- df1.4[, col_order]

  }

  # create final data to return
  final_list <- list(df1, df2, df3)

  ### if a ref_col is specified
  # calculate ref values using ref_col
  if(is.null(ref_col) == FALSE){

    # pull-in data
    data_ref <- data
    data_ref$date <- dplyr::pull(data_ref, date_col)
    data_ref$flow <- dplyr::pull(data_ref, ref_col)

    # filter by date_range
    if(is.null(date_range) == FALSE){
      data_ref <- data_ref %>% dplyr::filter(date <= date_range[2] & date >= date_range[1])
    }

    # join window dates to ref data
    my_data_ref <- dplyr::left_join(all_win_dates, data_ref, by = c("site", "date"))

    # drop columns not required for flow calcs
    my_data_ref_2 <- my_data_ref %>% dplyr::select(site, date, win_no, flow)

    # format and rename data in-line with APPLYFLOWSTATS functions
    test.flow.rec_ref <-  my_data_ref_2
    FULL.FLOW.REC_ref <- cbind(test.flow.rec_ref, temp)
    names(FULL.FLOW.REC_ref) <- casefold(names(FULL.FLOW.REC_ref))

    # calculate flow statistics
    mylist_ref <- APPLYFLOWSTATS(FULL.FLOW.REC_ref, q_high, q_low)
    # pull-in main data
    df1_ref <- mylist_ref[[1]]
    #rename stats
    df1_ref <- df1_ref %>% dplyr::rename(max = high,
                          max_z = highz,
                          min = low,
                          min_z = lowz,
                          min_30day = day30,
                          min_30day_z = day30z,
                          min_7day = day7,
                          min_7day_z = day7z,
                          vol_z = volz)

    # Get QXz_adj
    # select the data we need from _ref dataset and rename
    ref_data <- df1_ref %>%
      dplyr::select(site, win_no, Q5mean, Q5sd, Q10mean, Q10sd, Q20mean, Q20sd,
                    Q25mean, Q25sd, Q30mean, Q30sd, Q50mean, Q50sd, Q70mean, Q70sd,
                    Q75mean, Q75sd, Q80mean, Q80sd, Q90mean, Q90sd, Q95mean, Q95sd,
                    Q99mean, Q99sd, lowmean, lowsd, highmean, highsd,
                    day30mean, day30sd, day7mean, day7sd, volmean, volsd)

    ref_data <-  ref_data %>% dplyr::rename(Q5mean_ref = Q5mean,
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
                                            minmean_ref = lowmean,
                                            minsd_ref = lowsd,
                                            maxmean_ref = highmean,
                                            maxsd_ref = highsd,
                                            volmean_ref = volmean,
                                            volsd_ref = volsd,
                                            min_day30mean_ref = day30mean,
                                            min_day30sd_ref = day30sd,
                                            min_day7mean_ref = day7mean,
                                            min_day7sd_ref = day7sd)

    # calculate _adj values
    df1_ALL <- dplyr::left_join(x = df1, y = ref_data,
                                by = c("site", "win_no"), na.rm  = TRUE)

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
    vol <- df1_ALL$vol
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
    df1_ALL$min_30day_z_ref <- (min_7day - min_day7mean_ref) / min_day7sd_ref

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

    # Create a list of output_1 (with adj QXz values) and output_2
    final_list_ref <- list(df1_ALL, df2, df3)

  }

  if(is.null(ref_col) == TRUE){return(final_list)}
  if(is.null(ref_col) == FALSE){return(final_list_ref)}

}


######################################################

# Required as part of APPLYFLOWSTATS

CALCFLOWSTATS <- function (group1, group2, flowts) {

#calculate missing data before NAs are removed
MISSING <- flowts %>% dplyr::group_by(site, win_no) %>%
  dplyr::summarise(MISSING=sum(is.na(flow))) #amount of missing flow data in each group
flowts <- subset(flowts, !is.na(flow))  # remove NAs so stats can be calculated
median <- quantile(flowts$flow, probs=0.5, na.rm=TRUE)  # calculate the median flow

#calculate flow stats for each year by season
flowts<- flowts %>%  dplyr::group_by(site, win_no) %>%
    dplyr::summarise(q5 = quantile(flow, probs=0.95, na.rm=TRUE),
                     q10 = quantile(flow, probs=0.9, na.rm=TRUE),
                     q20 = quantile(flow, probs=0.8, na.rm=TRUE),
                     q25 = quantile(flow, probs=0.75, na.rm=TRUE),
                     q30 = quantile(flow, probs=0.7, na.rm=TRUE),
                     q50 = quantile(flow, probs=0.5,na.rm=TRUE),
                     q70 = quantile(flow, probs=0.3,na.rm=TRUE),
                     q75 = quantile(flow, probs=0.25, na.rm=TRUE),
                     q80 = quantile(flow, probs=0.2, na.rm=TRUE),
                     q90 = quantile(flow, probs=0.1,na.rm=TRUE),
                     q95 = quantile(flow, probs=0.05,na.rm=TRUE),
                     q99 = quantile(flow, probs=0.01,na.rm=TRUE),
               #ZERO= sum(flow==0),
               volume = sum(flow),
               min = min(flow, na.rm = TRUE),
               #min_day = find_mDate(., "low"),
               min_7day = min(rollmean(flow, 7), na.rm = TRUE),
               #min_7day_doy = find_minNday(., 7),
               min_30day = min(rollmean(flow, 30), na.rm = TRUE),
               #min_30day_doy = find_minNday(., 30),
               max = max(flow, na.rm = TRUE),
               #max_day = find_mDate(., "high"),
               CMEAN=mean(flow,na.rm=TRUE),
               CSD= sqrt(var(flow, na.rm=TRUE)),
               LSD= sqrt(var(log(flow), na.rm=TRUE)),
              N= sum(!(is.na(flow)))) %>%
              #EVENTS3= riisbiggs2(flow, median, 3),
              #EVENTS5= riisbiggs2(flow, median, 5),
              #EVENTS7= riisbiggs2(flow, median, 7))
  dplyr::filter(!is.na({{group1}})) %>% dplyr::full_join(MISSING)

  flowts$N <- flowts$N + flowts$MISSING

  return(flowts)

}

###########################################################

# Required as part of APPLYFLOWSTATS

CreateLongData<- function(flow.data, statsData) {

# rename data
FULL.FLOW.REC <- flow.data; QSTATS1 <- statsData

# calculate base flow index, if any stations have flows with 0's, make that bfi NA
# if there are no 0s in the flow data then use calc_bfi to calculate base flow index
# if flow cantains 0s then return NA
  bfi<- FULL.FLOW.REC %>%
    dplyr::filter(!is.na(flow)) %>%
    dplyr::mutate(x=flow) %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(bfi = if(0 %in% x == FALSE ){ bfi= calc_bfi(.$x) }else{ print ("flow contains 0's, returning NA")
      bfi=NA}) %>%
    dplyr::mutate(win_no = "Annual") %>%   # season = all
    tidyr::gather(-site, -win_no, key = parameter, value = value)

  ### flow duration curve- returns a warning that calculation is ignoring missing values- even when NA are removed

  FlowDurationCurve <- FULL.FLOW.REC %>%
    dplyr::filter(!is.na(flow)) %>%
    dplyr::group_by(site) %>%
    # calc percentiles 1:99
    group_modify(~ (fasstr::calc_longterm_percentile(data = FULL.FLOW.REC, dates = date, values = flow, percentiles=c(1:99), transpose = TRUE))) %>%
    #dplyr::do(fasstr::calc_longterm_percentile(data = STATION.FLOW.REC.SP, dates = date, values = flow, percentiles=c(1:99), transpose = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(win_no="Annual") %>%
    #old: `colnames<-`(c("parameter", "value", "season")) %>%
    setNames(., c("site", "parameter", "value", "win_no")) %>%
    dplyr::mutate(parameter=as.character(parameter)) # season= ALL

    ## create a long dataframe containing just means and sd for each Q value, BFI and flow duration curve
  long_data <- QSTATS1 %>%
    dplyr::filter(!is.na("Q30")) %>%
    # make dataframe long
    tidyr::gather(-site, -win_no, key = stat, value = stat_value) %>%               # select just the stats we want
    dplyr::filter(stat== "Q5"| stat== "Q10"| stat== "Q20"| stat== "Q25"| stat== "Q30"| stat== "Q50" | stat== "Q70" |  stat== "Q75" |stat== "Q80"|stat== "Q90"| stat== "Q95"| stat== "Q99"| stat == "vol"| stat== "low"| stat== "day7"| stat== "day30"| stat== "high") %>%
    dplyr::group_by(stat, site) %>%
    dplyr::summarise(mean = mean(stat_value, na.rm=TRUE), sd = sd(stat_value, na.rm = TRUE)) %>%     # calculate the mean and sd for each stat by season
    dplyr::ungroup() %>%
    tidyr::gather(sd, mean, key = id, value = value) %>%                   # gather mean and sd coloums into a coloumn with values and key coloumn (ie mean or sd)
    dplyr::do(within(.,  parameter <- paste(stat, id, sep=""))) %>%                     # concatenate stat (ie Q10) and id (ie mean or sd) coloumns (example: Q10mean, Q70mean)
    dplyr::select(-stat, -id) %>%
    dplyr::mutate(win_no = "Q_Annual") %>%
    dplyr::bind_rows(bfi)  %>%
    dplyr::bind_rows(FlowDurationCurve)      # remove the stat and id coloumns



  }

########################################################

APPLYFLOWSTATS <- function(FULL.FLOW.REC, q_low, q_high) {

# run CALCFLOWSTATS
gennames <- c("Q5", "Q10", "Q20", "Q25", "Q30", "Q50","Q70", "Q75", "Q80", "Q90", "Q95","Q99", "vol", "low", "day7", "day30", "high", "mean", "sd","lsd", "n", "missing")   # for naming coloumns
# calculate flow stats and add a period coloumn
QSTATS1 <- FULL.FLOW.REC %>% CALCFLOWSTATS(site, win_no, .) %>%
  #old: `colnames <-`(c("season", "water.year", gennames)) %>%
  setNames(., c("site", "win_no", gennames)) %>%
  dplyr::ungroup()

QSTATS1$win_no<- as.character(QSTATS1$win_no)
QSTATS1$win_no<- as.factor(QSTATS1$win_no)
QSTATS1

# calculate flow duration curve/bfi/means/sd
long_data<- CreateLongData(FULL.FLOW.REC, QSTATS1)

# flow stats- Q values, durations/events, missing data, number of 0's, dry events, 7/30daymin...
qlow <- as.numeric(q_low)
qhigh <- as.numeric(q_high)
standizedData<- CreateFlowStats(QSTATS1, long_data, FULL.FLOW.REC, qhigh, qlow)

flowdata <- FULL.FLOW.REC  # used to calculate missing data

# return a list of 3 dataframes:
 # long_data are static variables
      # Created using CreateLongData
 # standizedData - time varying stats
      # Created using CALCFLOWSTATS, then CreateFlowStats
 # flow data - data to input, with additional columns.
      # Created using DataProcessing
mylist <- list(standizedData, long_data, flowdata)

# return
return(mylist)

}

# -----------------------------------------------------------------------

# Required for CALCFLOWSTATS
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
} # end of function
# ------------------------------------------------------------------------

# Required for CreateFlowStats
find_eventDuration <- function(x, threshold ,type, pref) {
  # find events above/below a given threshold

  # find high or low flow events
  flowEvents <- find_events(x, threshold=threshold, type=type)

  flowEvents <- stats::na.omit(flowEvents)  # remove NAs
  # find the length of each event (ie the duration above/below threshold)
  #find_eventDurations <- dplyr::summarize(dplyr::group_by(flowEvents,event),
                                          #duration = length(event))
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
  n_mid1 <- CircStats::circ.mean(conv*(flowEvents$yday-1))/conv
  n_mid <- (n_mid1 + 365) %% 365

  } else {n_start <- NA; n_end <- NA; n_mid <- NA}

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


#######################################################################################

# Required for APPLYFLOWSTATS

CreateFlowStats<- function(stats_data, long.data, station_data, q_high, q_low) {

QSTATS1<- stats_data; long_data<- long.data; FULL.FLOW.REC<- station_data
q_high <- as.numeric(q_high); q_low <- as.numeric(q_low)

### calculate standardized values
# data frame needs to be long to use groupby, calculate standardized values
long_data2<- long_data %>%
  tidyr::spread(., key=parameter, value=value) %>%
  dplyr::select(site, win_no, 4:11, 111:136) %>%
  dplyr::filter(win_no != "Annual") %>%
  dplyr::select(-win_no)

# make the mean coloumns wide, remove the word mean from the parameter coloumn (Q10mean becomes Q10)
mean_df<- long_data2 %>%
  dplyr::select(Q5mean, Q10mean, Q20mean, Q25mean, Q30mean, Q50mean, Q70mean, Q75mean, Q80mean, Q90mean, Q95mean, Q99mean, volmean, lowmean, day7mean, day30mean, highmean, site) %>%
  tidyr::gather(-site, key=parameter, value=mean) %>%
  tidyr::separate(., col=parameter, c("parameter", NA), sep = "m")

# same as above but for sd, then join the coloumn of means with the sd data frame
all_df<- long_data2 %>%
  dplyr::select(Q5sd, Q10sd, Q20sd, Q25sd, Q30sd, Q50sd, Q70sd, Q75sd, Q80sd, Q90sd, Q95sd, Q99sd, volsd, lowsd, day7sd, day30sd, highsd, site) %>%
  tidyr::gather(-site, key=parameter, value=sd) %>%
  tidyr::separate(., col=parameter, c("parameter", NA), sep = "s") %>%
  dplyr::full_join(mean_df, by=c("site", "parameter"))

# calculate z scores
QSTATS1.1<- QSTATS1 %>%
  tidyr::gather(-site, -win_no, key = parameter, value = stat_value) %>%     # make dataframe long
  dplyr::filter(parameter== "Q5" | parameter== "Q10"| parameter== "Q20"| parameter== "Q25"| parameter== "Q30"| parameter== "Q50" | parameter== "Q70" | parameter== "Q75"| parameter== "Q80"| parameter== "Q85"| parameter== "Q90"| parameter== "Q95" |parameter== "Q99"| parameter == "vol"| parameter== "low"| parameter== "day7"| parameter== "day30"| parameter== "high") %>%   # keep just Q values
  dplyr::full_join(all_df, by=c("site", "parameter")) %>%
  dplyr::mutate(z= (stat_value-mean)/sd)   # standarize values

# filter data for calculating zero events, events above/below q_high/q_low
thres_data <- FULL.FLOW.REC %>%
  dplyr::filter(!is.na(flow))

# find zero flow days
zeros <- thres_data %>%
  dplyr::group_by(site, win_no) %>%
  dplyr::do(find_zeroDuration(.))
colnames(zeros) <- c("site", "win_no", "dry_n", "dry_e", "dry_start", "dry_end", "dry_mid")   # rename columns


#calculate QHigh and QLow
##needed calculate durations above and below threshold value
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
colnames(Qvals2) <- c("site", "qHigh", "qLow")
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

# calculate min, max, min rolling mean day of year
# min plus 7day_min
minmax_doy1 <- thres_data %>%
  dplyr::group_by(site, win_no) %>%
  dplyr::do(find_doy(.,  "low", 7))
colnames(minmax_doy1) <- c("site", "win_no", "min_doy", "min_7day_doy")

# max plus 30day_min
minmax_doy2 <- thres_data %>%
  dplyr::group_by(site, win_no) %>%
  dplyr::do(find_doy(.,  "high", 30))
colnames(minmax_doy2) <- c("site", "win_no", "max_doy", "min_30day_doy")
minmax_doy <- dplyr::full_join(minmax_doy1, minmax_doy2, by=c("site", "win_no"))

# create a wide data set with all standarized values
d1<- QSTATS1.1  %>%
  tidyr::gather(mean, sd, z, key= id, value = value) %>%     # make data long with id (mean, sd, Qz) and corrisponding values
  dplyr::do(within(.,  parameter <- paste(parameter, id, sep=""))) %>%                  # concatenate id (mean, Qz ect) with stat(Q10, Q50 ect) into a parameter coloumn
  dplyr::select(-stat_value, -id) %>%
  tidyr::spread(key=parameter, value = value)  # select only the coloumns we want and make data wide, a coloumns for each Qvalue+mean ect

# dataframe with unstandarized Q values (QSTAT1) with data with standarized z values (d1)
QSTATS1.2<- dplyr::full_join(d1, QSTATS1, by=c("site", "win_no")) %>%
  dplyr::filter(!is.na(mean))
# Then join with Duration (dataframe with durations/events above/below)
QSTATS1.3<- dplyr::full_join(QSTATS1.2, Durations, by=c("site", "win_no"))
# Then join with zeros
QSTATS1.4<- dplyr::full_join(QSTATS1.3, zeros, by=c("site", "win_no"))
# Then join with minmax_doy
QSTATS1.5<- dplyr::full_join(QSTATS1.4, minmax_doy, by=c("site", "win_no"))

# win_no means for stats
# newstat<- QSTATS1.5 %>%
#  dplyr::group_by(site, win_no) #%>%
#  #dplyr::summarise(e3mean= mean(e3),
                   #e5mean=mean(e5),
                   #e7mean=mean(e7)#,
                   #durationAboveMean=mean(durationAbove),
                   #durationBelowMean=mean(durationBelow),
                   #nEventsAboveMean=mean(nEventsAbove),
                   #nEventsBelowMean=mean(nEventsBelow)
                   #)

# create final dataset
QSTATS_final<- QSTATS1.5
#QSTATS_final_win <- dplyr::left_join(win_dates, QSTATS_final, by=c("site", "win_no"))

  return(QSTATS_final)
}

##############################################


###############################################

# calc_longterm_percentile from fasstr
# Required for CreateLongData

##############################################

# calc_bfi
# Required for CreateLongData

calc_bfi <- function(x) {

  day7mean <- RcppRoll::roll_mean(x, 7, align = "right")
  min7day <- min(day7mean)
  meanflow <- mean(x)
  calc_bfi <- min7day/meanflow
  return(calc_bfi)
}


#################################################
# find_events
# Required for find_eventDuration

#' find_events(x,threshold)
find_events <- function(FULL.FLOW.REC, threshold, type="high") {

  x <- FULL.FLOW.REC

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
# find_0S
# effectively the same as find_events, but for 0 flows
zero_events <- function(FULL.FLOW.REC) {

  x <- FULL.FLOW.REC

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
### find_zeroDuration
## same as find_eventDuration, but for zero flows

find_zeroDuration <- function(FULL.FLOW.REC) {
  # find events above/below a given threshold
  zeroEvents <- zero_events(FULL.FLOW.REC)

  zeroEvents <- stats::na.omit(zeroEvents)  # remove NAs
  # find number of records
  n_records <- length(zeroEvents$event)

  # find number of events
  n_events <- length(unique(zeroEvents$event))

  zeroEvents$yday <- lubridate::yday(zeroEvents$date)

  # first low flow day
  dry_start <- zeroEvents %>% arrange(date) %>% dplyr::slice(., 1)
  dry_start <- dry_start$yday

  # last low flow day
  dry_end <- zeroEvents %>% arrange(date)
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
### find_mDay
# used in find_doy
# find the minimum and maximum flow dates

find_mDay <- function(FULL.FLOW.REC, type) {

  x <- FULL.FLOW.REC

  x2 <- x %>% dplyr::filter(!is.na(flow))

  if(type == "low"){
    flow_day <- x2 %>% dplyr::filter(flow == min(flow))}

  if(type == "high"){
    flow_day <- x2 %>% dplyr::filter(flow == max(flow))}

  # first low flow day
  flow_day2 <- flow_day %>% arrange(date) %>% dplyr::slice(., 1)

  # find number of events
  flow_day2$yday <- lubridate::yday(flow_day2$date)

  # first low flow day
  m_day <- as.character(unique(flow_day2$yday))

  return(m_day)
}


#####################################################################
### find_mNday
# used in find_doy
# find the minimum 7-day or 30-day mean flow

find_mNDay <- function(FULL.FLOW.REC, nday) {

if(nday == 7){
minNday <- FULL.FLOW.REC %>%  dplyr::summarise(min_Nday = min(zoo::rollmean(flow, 7), na.rm = TRUE),
                              date = date)
}

if(nday == 30){
    minNday <- FULL.FLOW.REC %>%  dplyr::summarise(min_Nday = min(zoo::rollmean(flow, 30), na.rm = TRUE),
                                                   date = date)
  }

  # filter to find lowest flow
  low_flow <- minNday %>% dplyr::filter(min_Nday == min(min_Nday)) %>%
    dplyr::arrange(date)
  low_flow2 <- utils::head(low_flow, 1)

  # create doy
  low_flow2$yday <- lubridate::yday(low_flow2$date)

  # first low flow day
  min_day <- as.character(low_flow2$yday)

  return(min_day)
}

######################################################################
## find_doy
# uses find_mDay and find mNDay to find the DOY (365) of miniumum flows


find_doy <- function(FULL.FLOW.REC, type, nday){

if(type == "low"){
  min_doy <- find_mDay(FULL.FLOW.REC, "low")
  N7day_doy <- find_mNDay(FULL.FLOW.REC, 7)

  find_doy1 <- data.frame(min_doy, N7day_doy)

}

if(type == "high"){
  max_doy <- find_mDay(FULL.FLOW.REC, "high")
  N30day_doy <- find_mNDay(FULL.FLOW.REC, 30)

  find_doy1 <- data.frame(max_doy, N30day_doy)

}

  return(find_doy1)

}


##############################################################
###IsDate

IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}


