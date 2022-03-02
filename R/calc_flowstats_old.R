#' Calculate a suite of long-term and seasonal flow statistics for one or more sites.
#'
#' @description calc_flowstats uses a time series of measured or modeled flows to calculate a suite of long-term and seasonal (winter and summer) flow statistics for one or more sites (stations). It is primarily designed to work with mean daily flows (e.g. as produced by import_flow), but can also be applied to time series data on a longer (e.g. monthly) time step. Regardless, the data should be regularly spaced and the same time step should be used for all sites.
#'
#' @usage calc_flowstats(data = data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", ref_col = NULL)
#'
#' @param data Name of data frame or tibble containing the flow data to be processed.
#'  Must be in long format and contain columns for flow site id, date and flow (see output from import_flows).
#' @param sites_col Name of column in 'data' containing unique flow site id. Default = "flow_site_id".
#' @param date_col Name of column in 'data' containing date of flow record.  Default = "date".
#' @param flow_col Name of column in 'data' containing flow data for processing. Default = "flow".
#' @param ref_col Name of column in 'data' containing flow data for reference scenario against which flow statistics are standardised. Default: NULL.
#'
#' @details
#' The function is hard-wired to exclude any flow data before 1st April 1985.  Must be in long format and contain columns named "flow_site_id", "Date" and "Flow" (see output from import_flows).
#'
#' Winter is defined as 1st October to 31st March, and Summer as 1st April to 30th September. There is no minimum number of records required in each 6 month period; if desired, the outputs can be filtered afterwards to eliminate potentially unreliable results based on sparse data.
#'
#' Selected flow percentiles (Q10, Q30, Q50, Q70 and Q95) are calculated for each 6 month winter/summer, and then standardized using the long-term mean and standard deviation of these statistics. If calculating flow statistics for two different flow scenarios (e.g. historical and naturalised), then the function includes the facility to standardise the flow percentile statistics for one scenario (specified via flow_col) using mean and standard deviation of flow statistics from the other scenario (specified via ref_col). For example, if flow_col = naturalised flows and ref_col = historical flows, then the resulting statistics can be input into a hydro-ecological model calibrated using historical flow data and used to make predictions of ecological status under naturalised flows.
#'
#' @return calc_flowstats returns a list of two data frames. The first data frame contains a suite of time-varying flow statistics for every 6 month winter/summer period at every site. The columns are as follows:
#'
#'    - flow_site_id (a unique site id; this is the standardised column header for flow sites)
#'    - season (S = summer, W = winter)
#'    - water_year (year starting 1st October; e.g. 2018 = 01/10/2018 to 30/09/2019)
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
#' - season (S = summer, W = winter, A = all)
#' - parameter (base flow index (bfi), flow duration curve percentiles (p1 to p99), and seasonal means and standard deviations for Q10, Q30, Q50, Q75 and Q95).
#' - value (calculated statistic)
#'
#' @export
#'
#' @examples
#' ## Calculate a suite of long-term and seasonal flow statistics for all sites listed in site_col:
#' calc_flowstats(data = flow_data,
#'                 site_col = "flow_site_id",
#'                 date_col = "date",
#'                 flow_col = "flow")
#'
#' ## Calculate a suite of long-term and seasonal flow statistics for all sites listed in site_col, and standardise QXX flow statistics using flow data specified in ref_col:
#' calc_flowstats(data = data,
#'                site_col = "flow_site_id",
#'                date_col = "date",
#'                flow_col = "HistoricalFlow",
#'                ref_col = "NaturalisedFlow")


calc_flowstats_old <- function(data,
                         site_col = "flow_site_id",
                         date_col = "date",
                         flow_col = "flow",
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

  if(is.null(ref_col) == FALSE && (ref_col %in% colnames(data)) == FALSE)
  {stop("Specified ref_col was not identified in data")}

  # pull-in site_col

  data$site <- dplyr::pull(data, site_col)

  for(i in unique(data$site)){

  # filter and pull-in data

  data_f <- dplyr::filter(data, site == i)

  data_f$Date <- dplyr::pull(data_f, date_col)
  data_f$Flow <- dplyr::pull(data_f, flow_col)

  # Check flow is numeric

  if(is.numeric( data_f$Flow) == FALSE)
  {stop("Specified flow_col is not numeric")}

  # Check for duplicate dates

  duplicates <- data_f$Date[duplicated(data_f$Date)]

  if(length(duplicates) >= 1)
  { print(duplicates)
    stop("Duplicate dates identified")}

  # run doForOneStation on Date & Flow Cols

  dfos_data <- data_f %>% dplyr::select(Date, Flow)

  dfos_output <- doForOneStation_old(dfos_data)

  # Select outputs and match with flow_site_id

  dfos_output_a <- as.data.frame(dfos_output[[1]])

  dfos_output_a$flow_site_id  <- i

  assign(paste(i, sep = "_", "dfos_output_1"), dfos_output_a)

  dfos_output_b <- as.data.frame(dfos_output[[2]])

  dfos_output_b$flow_site_id  <- i

  assign(paste(i, sep = "_", "dfos_output_2"), dfos_output_b)

  }

  # merge outputs for all sites

  dfos_output_1 <- c(mget(ls(pattern = "_dfos_output_1")))
  dfos_output_2 <- c(mget(ls(pattern = "_dfos_output_2")))

  merge_dfos_output_1 <- Reduce('rbind', dfos_output_1)
  merge_dfos_output_2 <- Reduce('rbind', dfos_output_2)

  # if required, process ref_col flows
  # repeats the process above, but using ref_col as the 'Flow' column
  # allows for calculation of QXz

  if(is.null(ref_col) == FALSE){

    for(i in unique(data$site)){

      data_ref <- dplyr::filter(data, site == i)

      data_ref$Date <- dplyr::pull(data_ref, date_col)
      data_ref$Flow <- dplyr::pull(data_ref, ref_col)

      dfos_data_ref <- data_ref %>% dplyr::select(Date, Flow)

      dfos_output_ref <- doForOneStation_old(dfos_data_ref)

      dfos_output_a_ref <- as.data.frame(dfos_output_ref[[1]])

      dfos_output_a_ref$flow_site_id  <- i

      assign(paste(i, sep = "_", "dfos_output_1_ref"), dfos_output_a_ref)

      dfos_output_b_ref <- as.data.frame(dfos_output_ref[[2]])

      dfos_output_b_ref$flow_site_id  <- i

      assign(paste(i, sep = "_", "dfos_output_2_ref"), dfos_output_b_ref)

    }

    dfos_output_1_ref <- c(mget(ls(pattern = "dfos_output_1_ref")))
    dfos_output_2_ref <- c(mget(ls(pattern = "dfos_output_2_ref")))

    merge_dfos_output_1_ref <- Reduce('rbind', dfos_output_1_ref)
    merge_dfos_output_2_ref <- Reduce('rbind', dfos_output_2_ref)


    # Get Q10z_adj

    # select the data we need from _ref dataset

    ref_data <- merge_dfos_output_1_ref %>%
                dplyr::select(flow_site_id, water.year, season, Q10mean, Q10sd, Q30mean, Q30sd, Q50mean, Q50sd, Q70mean, Q70sd, Q95mean, Q95sd)

    ref_data <-  ref_data %>% dplyr::rename(Q10mean_ref = Q10mean,
                                      Q10sd_ref = Q10sd,
                                      Q30mean_ref = Q30mean,
                                      Q30sd_ref = Q30sd,
                                      Q50mean_ref = Q50mean,
                                      Q50sd_ref = Q50sd,
                                      Q70mean_ref = Q70mean,
                                      Q70sd_ref = Q70sd,
                                      Q95mean_ref = Q95mean,
                                      Q95sd_ref = Q95sd)

    # merge ref_data with original data

    merge_dfos_output_ALL <- dplyr::left_join(x = merge_dfos_output_1, y = ref_data,
                                 by = c("flow_site_id", "water.year", "season"),
                                 na.rm  = TRUE)

    # Get Q10z_adj

    Q10 <- merge_dfos_output_ALL$Q10
    Q10mean <- merge_dfos_output_ALL$Q10mean
    Q10sd <- merge_dfos_output_ALL$Q10sd
    Q10mean_ref <- merge_dfos_output_ALL$Q10mean_ref
    Q10sd_ref <- merge_dfos_output_ALL$Q10sd_ref
    merge_dfos_output_ALL$Q10z <- (Q10 - Q10mean_ref) / Q10sd_ref

    # Get Q30z_adj

    Q30 <- merge_dfos_output_ALL$Q30
    Q30mean <- merge_dfos_output_ALL$Q30mean
    Q30sd <- merge_dfos_output_ALL$Q30sd
    Q30mean_ref <- merge_dfos_output_ALL$Q30mean_ref
    Q30sd_ref <- merge_dfos_output_ALL$Q30sd_ref
    merge_dfos_output_ALL$Q30z <- (Q30 - Q30mean_ref) / Q30sd_ref

    # Get Q50z_adj

    Q50 <- merge_dfos_output_ALL$Q50
    Q50mean <- merge_dfos_output_ALL$Q50mean
    Q50sd <- merge_dfos_output_ALL$Q50sd
    Q50mean_ref <- merge_dfos_output_ALL$Q50mean_ref
    Q50sd_ref <- merge_dfos_output_ALL$Q50sd_ref
    merge_dfos_output_ALL$Q50z <- (Q50 - Q50mean_ref) / Q50sd_ref

    # Get Q70z_adj

    Q70 <- merge_dfos_output_ALL$Q70
    Q70mean <- merge_dfos_output_ALL$Q70mean
    Q70sd <- merge_dfos_output_ALL$Q70sd
    Q70mean_ref <- merge_dfos_output_ALL$Q70mean_ref
    Q70sd_ref <- merge_dfos_output_ALL$Q70sd_ref
    merge_dfos_output_ALL$Q70z <- (Q70 - Q70mean_ref) / Q70sd_ref

    # Get Q95z_adj

    Q95 <- merge_dfos_output_ALL$Q95
    Q95mean <- merge_dfos_output_ALL$Q95mean
    Q95sd <- merge_dfos_output_ALL$Q95sd
    Q95mean_ref <- merge_dfos_output_ALL$Q95mean_ref
    Q95sd_ref <- merge_dfos_output_ALL$Q95sd_ref
    merge_dfos_output_ALL$Q95z <- (Q95 - Q95mean_ref) / Q95sd_ref

    merge_dfos_output_ALL <- merge_dfos_output_ALL %>%
      dplyr::rename(water_year = water.year)

    merge_dfos_output_ALL <- subset(merge_dfos_output_ALL, select=c(flow_site_id, 1:45, 47:56))

    # Create a list of output_1 (with adj QXz values) and output_2
    dfos_output_ALL <- list(merge_dfos_output_ALL, merge_dfos_output_2)

    # return
    return(dfos_output_ALL)

  }

  if(is.null(ref_col) == TRUE){

  merge_dfos_output_1 <-  merge_dfos_output_1 %>%
    dplyr::rename(water_year = water.year)

  merge_dfos_output_1 <- subset(merge_dfos_output_1, select=c(flow_site_id, 1:45))

  # Create a list of output_1 and output_2
  dfos_output_ALL <- list(merge_dfos_output_1, merge_dfos_output_2)

  # return
  return(dfos_output_ALL)

  }

}

################################################################################################################

#' @export
#'

doForOneStation_old <- function(test.flow.rec) {
# Function is only passed a flow series at this point
# with Date in column 1 and Flowin column 2

# Divides data to Season (Winter/Summer)
  # Trims data to start 1/Oct to 31st/March OR 1st Apr to 31 Sept
  # Gets rid data of pre-1985

# Cut down flow data so it starts and ends on a season boundary
FirstLastDate <- test.flow.rec %>%
  dplyr::filter(!is.na(Date)) %>%
  dplyr::summarise(first=lubridate::ymd(min(Date)),
                   last=lubridate::ymd(max(Date)),
                   fy= lubridate::year(first),
                   fm=lubridate::month(first),
                   ly= lubridate::year(last),
                   lm=lubridate::month(last))

standard.period.start <- lubridate::ymd(paste("1985","04","01", sep="-"))
head(test.flow.rec)

#  If flows start pre-1985 - Trim them to start in April 1985
  if(FirstLastDate$first < standard.period.start){
    test.flow.rec <- subset(test.flow.rec, Date >= standard.period.start)
    print('flows start prior to 1985, trimming them to start on 1/4/1985')
  }else{
    print('Minor warning: flow series starts after the start of the standard period (1/4/1985)')
  }

#  If flows start post-1985 - Trim them to start on first season boundary
 # Warning to Check end season boundary
  if(FirstLastDate$first > lubridate::ymd(paste(FirstLastDate$fy,"04","01", sep="-")) & FirstLastDate$first < lubridate::ymd(paste(FirstLastDate$fy,"10","01", sep="-"))){
    test.flow.rec <- subset(test.flow.rec, Date >= lubridate::ymd(paste(FirstLastDate$fy,"10","01", sep="-")))
    print('Trimming flow series to start in October')
  } else {
    test.flow.rec <- subset(test.flow.rec, Date >= lubridate::ymd(paste(FirstLastDate$fy,"04","01", sep="-")))
    print('Trimming flow series to start in April')
  }

  tail(test.flow.rec)
  if(FirstLastDate$last >= lubridate::ymd(paste(FirstLastDate$ly,"04","01", sep="-")) & FirstLastDate$last < lubridate::ymd(paste(FirstLastDate$ly,"09","01", sep="-"))){
    test.flow.rec <- subset(test.flow.rec, Date < lubridate::ymd(paste(FirstLastDate$ly,"04","01", sep="-")))
    print('Trimming flow series to end in March')
  } else {
    test.flow.rec <- subset(test.flow.rec, Date < lubridate::ymd(paste(FirstLastDate$ly,"10","01", sep="-")))
    print('Trimming flow series to end in September')
  }

  print(head(test.flow.rec,30))
  print(tail(test.flow.rec,30))

# If statement checks there is adequate flow records
  # (at least 1 year's worth of 2 weekly sampling)
if(nrow(test.flow.rec) > 1){

temp <- cbind(DAY=as.integer(lubridate::day(test.flow.rec$Date)),
              MONTH=as.integer(lubridate::month(test.flow.rec$Date)),
              YEAR=as.integer(lubridate::year(test.flow.rec$Date)))

FULL.FLOW.REC <- cbind(test.flow.rec, temp)

names(FULL.FLOW.REC) <- casefold(names(FULL.FLOW.REC))

# keep NAs as they are used to calculated missing values in the CALCFLOWSTATS function

# Use APPLYFLOWSTATS

STATS <- APPLYFLOWSTATS_old(FULL.FLOW.REC)

# Return

return(STATS)

# Otherwise - there is not enough flow data; do nothing.

} else {
print("not enough flow data"); flush.console()

}

}

##########################################################################################

# Required for APPLYFLOWSTATS

DataProcessing_old <- function(full.flow.rec) {
  FULL.FLOW.REC<-  full.flow.rec

  FULL.FLOW.REC$sp <- FALSE

  FULL.FLOW.REC$wyear <- FULL.FLOW.REC$year
  FULL.FLOW.REC$wyear[FULL.FLOW.REC$month<=9] <- FULL.FLOW.REC$wyear[FULL.FLOW.REC$month<=9] -1

  FULL.FLOW.REC$sp[FULL.FLOW.REC$wyear>=1984] <- TRUE
  # water year for end of SP amended to 2016, ie will include flows to Sept 2017
  # water year for standard period amended to 1984 (= calendar year 1985)
  # 25/1/2019. Max Water Year removed from subsetting here
  # This is because it's too confusing to update this every year

  # create season variables
  FULL.FLOW.REC$season[FULL.FLOW.REC$month<=9 & FULL.FLOW.REC$month>=4] <- "Summer"
  FULL.FLOW.REC$season[FULL.FLOW.REC$month>=10 | FULL.FLOW.REC$month<=3] <- "Winter"

  FULL.FLOW.REC$season <- factor(FULL.FLOW.REC$season, levels=c("Winter","Summer"), ordered=TRUE)

  STATION.FLOW.REC<- FULL.FLOW.REC

  STATION.FLOW.REC.SP<- STATION.FLOW.REC %>%
    dplyr::filter(sp==TRUE)


  return(STATION.FLOW.REC.SP)

}




######################################################

# Required as part of APPLYFLOWSTATS

CALCFLOWSTATS_old <- function (group1, group2, flowts) {

#calculate missing data before NAs are removed
MISSING<- flowts %>% dplyr::group_by({{group1}}, {{group2}}) %>%
  dplyr::summarise(MISSING=sum(is.na(flow))) #amount of missing flow data in each group
flowts <- subset(flowts, !is.na(flow))  # remove NAs so stats can be calculated
median <- quantile(flowts$flow, probs=0.5, na.rm=TRUE)  # calculate the median flow

#calculate flow stats for each year by season
flowts<- flowts %>%  dplyr::group_by({{group1}}, {{group2}}) %>%
    dplyr::summarise(q10 = quantile(flow, probs=0.9, na.rm=TRUE),
                     q30 = quantile(flow, probs=0.7, na.rm=TRUE),
                     q50 = quantile(flow, probs=0.5,na.rm=TRUE),
                     q70 = quantile(flow, probs=0.3,na.rm=TRUE),
                     q90 = quantile(flow, probs=0.1,na.rm=TRUE),
                     q95 = quantile(flow, probs=0.05,na.rm=TRUE),
                     q99 = quantile(flow, probs=0.01,na.rm=TRUE),
               ZERO= sum(flow==0),
               CMEAN=mean(flow,na.rm=TRUE),
               CSD= sqrt(var(flow, na.rm=TRUE)),
               LSD= sqrt(var(log(flow), na.rm=TRUE)),
              N= sum(!(is.na(flow))),
              EVENTS3= riisbiggs2(flow, median, 3),
              EVENTS5= riisbiggs2(flow, median, 5),
              EVENTS7= riisbiggs2(flow, median, 7)) %>%
  dplyr::filter(!is.na({{group1}})) %>% dplyr::full_join(MISSING)

  flowts$N <- flowts$N + flowts$MISSING

  return(flowts)

}

###########################################################

# Required as part of APPLYFLOWSTATS

CreateLongData_old <- function(flow.data, statsData) {

# rename data
STATION.FLOW.REC.SP<- flow.data; QSTATS1<- statsData

# calculate base flow index, if any stations have flows with 0's, make that bfi NA
# if there are no 0s in the flow data then use calc_bfi to calculate base flow index
# if flow cantains 0s then return NA
  bfi<- STATION.FLOW.REC.SP %>%
    dplyr::filter(!is.na(flow)) %>%
    dplyr::mutate(x=flow) %>%
    dplyr::summarise(bfi= if(0 %in% x == FALSE ){ bfi= calc_bfi(x) }else{ print ("flow contains 0's, returning NA")
      bfi=NA}) %>%
    dplyr::mutate(season= "Annual") %>%   # season = all
    tidyr::gather(-season, key = parameter, value = value)

  ### flow duration curve- returns a warning that calculation is ignoring missing values- even when NA are removed

  # remove NAs
  FlowDurationCurve<- STATION.FLOW.REC.SP %>%
    dplyr::filter(!is.na(flow)) %>%
    # calc percentiles 1:99
    dplyr::do(fasstr::calc_longterm_percentile(data=., dates = date, values = flow, percentiles=c(1:99), transpose = TRUE)) %>%
    dplyr::mutate(season="Annual") %>%
    #old: `colnames<-`(c("parameter", "value", "season")) %>%
    setNames(., c("parameter", "value", "season")) %>%
    dplyr::mutate(parameter=as.character(parameter)) # season= ALL

  ## create a long dataframe containing just means and sd for each Q value, BFI and flow duration curve
  long_data <- QSTATS1 %>%
    # make dataframe long
    tidyr::gather(-season, -water.year, -period, key = stat, value = stat_value) %>%               # select just the stats we want
    dplyr::filter(stat== "Q10"| stat== "Q30"| stat== "Q50" | stat== "Q70" | stat== "Q95") %>%
    dplyr::group_by(stat, season) %>%
    dplyr::summarise(mean=mean(stat_value, na.rm=TRUE), sd=sd(stat_value, na.rm=TRUE)) %>%     # calculate the mean and sd for each stat by season
    dplyr::ungroup() %>%
    tidyr::gather(mean, sd, key= id, value = value) %>%                   # gather mean and sd coloums into a coloumn with values and key coloumn (ie mean or sd)
    dplyr::do(within(.,  parameter <- paste(stat, id, sep=""))) %>%                     # concatenate stat (ie Q10) and id (ie mean or sd) coloumns (example: Q10mean, Q70mean)
    dplyr::select(-stat, -id) %>%
    dplyr::bind_rows(bfi)  %>%
    dplyr::bind_rows(FlowDurationCurve)      # remove the stat and id coloumns



  }

########################################################

APPLYFLOWSTATS_old <- function(STATION.FLOW.REC) {

# call dataprocessing function
# does some data processing and creates season variables
STATION.FLOW.REC.SP <- DataProcessing_old(STATION.FLOW.REC)

gennames <- c("Q10", "Q30", "Q50","Q70", "Q90", "Q95","Q99", "zero", "mean", "sd","lsd", "n", "e3", "e5", "e7", "missing")   # for naming coloumns
# calculate flow stats and add a period coloumn
QSTATS1 <- STATION.FLOW.REC.SP %>% CALCFLOWSTATS_old(season, wyear, .) %>%
  dplyr::mutate(period="SPY") %>%
  #old: `colnames <-`(c("season", "water.year", gennames, "period")) %>%
  setNames(., c("season", "water.year", gennames, "period")) %>%
  dplyr::ungroup()

QSTATS1$season<- as.character(QSTATS1$season)
QSTATS1

long_data<- CreateLongData_old(STATION.FLOW.REC.SP, QSTATS1)  # calculate flow duration curve/bfi/means/sd

standizedData<- CreateFlowStats_old(QSTATS1, long_data, STATION.FLOW.REC.SP) # flow stats- Q values, durations/events, missing data, number of 0's...

flowdata <- STATION.FLOW.REC.SP  # used to calculate missing data

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

riisbiggs2_old <- function(datavector, threshold, multiplier) {
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

find_eventDuration_old <- function(x, threshold ,type, pref) {
  # find events above/below a given threshold
  flowEvents <- find_events_old(x=x$flow, threshold=threshold ,type="type")

  flowEvents_old <- na.omit(flowEvents)  # remove NAs
  # find the length of each event (ie the duration above/below threshold)
  find_eventDurations <- dplyr::summarize(dplyr::group_by(flowEvents,event),
                                          duration = length(event))

  if(nrow(find_eventDurations_old) > 0) {
      if(pref=="mean")
      {
        # if mean, then find the mean duration above/below threshold
        find_eventDurations<- find_eventDurations %>%
          dplyr::summarise(duration=mean(duration,na.rm=TRUE), nEvents=max(event))

      } else if (pref=="sum")
      {
        # if sum, then find the total number of days above/below the threshold
        find_eventDurations<- find_eventDurations %>%
          dplyr::summarise(duration=sum(duration,na.rm=TRUE), nEvents=max(event))
        }

    # if there are no events above/below then retun a data.frame
  } else {find_eventDurations <- data.frame(duration=0, nEvents=0)}

  return(find_eventDurations)
}


#######################################################################################

# Required for APPLYFLOWSTATS

CreateFlowStats_old <- function(stats_data, long.data, station_data) {

QSTATS1<- stats_data; long_data<- long.data; STATION.FLOW.REC.SP<- station_data

### calculate standardized values
# data frame needs to be long to use groupby, calculate standardized values
long_data2<- long_data %>%
  tidyr::spread(., key=parameter, value=value) %>%
  dplyr::select(season, 102: 111) %>%
  dplyr::filter(season != "Annual")   # make data long and select just the mean & sd coloumns

# make the mean coloumns wide, remove the word mean from the parameter coloumn (Q10mean becomes Q10)
mean_df<- long_data2 %>%
  dplyr::select(Q10mean, Q30mean, Q50mean, Q70mean, Q95mean, season) %>%
  tidyr::gather(-season, key=parameter, value=mean) %>%
  tidyr::separate(., col=parameter, c("parameter", NA), sep = "m")

# same as above but for sd, then join the coloumn of means with the sd data frame
all_df<- long_data2 %>%
  dplyr::select(Q10sd, Q30sd, Q50sd, Q70sd, Q95sd, season) %>%
  tidyr::gather(-season, key=parameter, value=sd) %>%
  tidyr::separate(., col=parameter, c("parameter", NA), sep = "s") %>%
  dplyr::full_join(mean_df, by=c("season", "parameter"))

# calculate z scores
QSTATS1.1<- QSTATS1 %>%
  tidyr::gather(-season, -water.year, -period, key = parameter, value = stat_value) %>%     # make dataframe long
  dplyr::filter(parameter== "Q10"| parameter== "Q30"| parameter== "Q50" | parameter== "Q70" | parameter== "Q95") %>%   # keep just Q values
  dplyr::full_join(all_df, by=c("season", "parameter")) %>%
  dplyr::mutate(z= (stat_value-mean)/sd)   # standarize values

#calculate Q95 and Q70
##needed calculate durations above and below threshold value
Q95<- long_data %>%
  dplyr::filter(parameter == "P5") %>%
  dplyr::select(value); Q95 <- as.numeric(Q95)
Q70<- long_data %>%
  dplyr::filter(parameter == "P30") %>%
  dplyr::select(value); Q70 <- as.numeric(Q70)
thres_data <- STATION.FLOW.REC.SP %>%
  dplyr::filter(!is.na(flow))

# calculate duration/events above Q70 and below Q95
duration_above<- thres_data %>%
  dplyr::group_by(season, wyear) %>%
  dplyr::do(find_eventDuration(.,  Q70, "high", "sum"))

colnames(duration_above) <- c("season", "water.year", "durationAbove", "nEventsAbove")   # rename columns
duration_below <- thres_data %>%
  dplyr::group_by(season, wyear) %>%
  dplyr::do(find_eventDuration(.,  Q95, "low", "sum"))
colnames(duration_below) <- c("season", "water.year", "durationBelow", "nEventsBelow")
Durations<- dplyr::full_join(duration_above, duration_below, by=c("water.year", "season"))

# create a wide data set with all standarized values
d1<- QSTATS1.1  %>%
  tidyr::gather(mean, sd, z, key= id, value = value) %>%     # make data long with id (mean, sd, Qz) and corrisponding values
  dplyr::do(within(.,  parameter <- paste(parameter, id, sep=""))) %>%                  # concatenate id (mean, Qz ect) with stat(Q10, Q50 ect) into a parameter coloumn
  dplyr::select(-stat_value, -id, -period) %>%
  tidyr::spread(key=parameter, value = value)  # select only the coloumns we want and make data wide, a coloumns for each Qvalue+mean ect

# dataframe with unstandarized Q values (QSTAT1) with data with standarized z values (d1)
QSTATS1.2<- dplyr::full_join(d1, QSTATS1, by=c("water.year", "season")) %>%
  dplyr::filter(!is.na(mean))
# Then join with Duration (dataframe with durations/events above/below)
QSTATS1.3<- dplyr::full_join(QSTATS1.2, Durations, by=c("water.year", "season"))

# season means for stats
newstat<- QSTATS1.3 %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(e3mean= mean(e3),
                   e5mean=mean(e5),
                   e7mean=mean(e7),
                   durationAboveMean=mean(durationAbove),
                   durationBelowMean=mean(durationBelow),
                   nEventsAboveMean=mean(nEventsAbove),
                   nEventsBelowMean=mean(nEventsBelow))

# create final dataset
QSTATS_final<- dplyr::full_join(QSTATS1.3, newstat, by=c("season"))

  return(QSTATS_final)
}

##############################################


###############################################

# calc_longterm_percentile from fasstr
# Required for CreateLongData

##############################################

# calc_bfi
# Required for CreateLongData

calc_bfi_old <- function(x) {

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
find_events_old <- function(x,threshold,type="high") {

  x <- data.frame(flow = x)

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

  flowEvents <- x[c("flow","event")]


  return(flowEvents)
}




