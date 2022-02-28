#' Link biology samples with time-varying flow statistics for paired biology and flow sites.
#'
#' @description
#' This function joins biology sample data with time-varying flow statistics for one or more antecedent (lagged) time periods (as calculated by the `calc_flowstats` function) to create a combined dataset for hydro-ecological modelling.
#'
#' @usage
#' join_he(biol_data, flow_stats, mapping = NULL, method = "A" , lags = 0, join_type = "add_flows")
#'
#' @param biol_data Data frame or tibble containing the processed biology data. Must contain the following columns: biol_site_id and date.
#' @param flow_stats Data frame or tibble containing the calculated time-varying flow statistics, by site and time period and win_no (as produced by the `calc_flowstats` function ). Must contain the following columns: flow_site_id, start_date and end_date. The function joins all the variables in `flow_stats` to the biology samples, so it is advisable to manually drop any flow statistics which are not of interest before applying the function.
#' @param mapping Data frame or tibble containing paired biology sites IDs and flow site IDs. Must contain columns named biol_site_id and flow_site_id. These columns must not contain any NAs. Default = `NULL`, which assumes that paired biology and flow sites have identical ids, so mapping is not required.
#' @param method Choice of method for linking biology samples to flow statistics for antecedent time periods. Using method = "A" (default), lag 0 is defined for each biology sample as the most recently finished flow time period; using method = "B", lag 0 is defined as the most recently started flow time period. See below for details.
#' @param lags Vector of lagged flow time periods of interest. Values must be zero or positive, with larger values representing longer time lags (i.e. an increasing time gap between the flow time period and the biology sample date). Default = 0. See below for details.
#' @param join_type To add flow statistics to each biology sample, choose "add_flows" (default); this produces a dataset of biological metrics (response variables) and flow statistics (predictor variables) for hydro-ecological modelling. To add biology sample data to flow statistics for each time period, choose "add_biol"; this produces a time series of flow statistics with associated biological metrics which can be used to assess the coverage of historical flow conditions using the `plot_rngflows` function .
#'
#' @details
#' `biol_data` and `flow_stats` may contain more sites than listed in `mapping`, but any sites not listed in `mapping` will be filtered out. If `mapping = NULL`, then biology site and flow sites with matching ids will be paired automatically.
#'
#' The `calc_flowstats` function uses a moving window approach to calculate a time-varying flow statistics for a  sequence of time periods which can be either: (i) contiguous (i.e. each time period is followed immediately by the next one), (ii) non-contiguous (i.e. there is a gap between one time period at the next), or (iii) over-lapping (i.e. the next time period stats before the previous one has finished).
#'
#' To describe the antecedent flow conditions prior to each biology sample, the time periods are labelled relative to the date of the biology sample, with lag 0 representing either the most recently finished (method = "A") or most recently started (method = "B") flow time period. The time period immediately prior to the Lag 0 time period is the Lag 1 period, and the period immediately prior to that is the Lag 2 period, and so on.
#'
#' As an example, suppose we have a biology sample dated 15 September 2020 and that flow statistics are available for a sequence of contiguous 12 month periods (each 1 January to 31 December). Using Method "A", the Lag 0 period for that biology sample would be January to December 2019 (the most recently finished time period), the Lag 1 period would be January to December 2018, the Lag 2 period would be January to December 2017, and so on. Similarly, using Method "B", the Lag 0 period for that biology sample would be January to December 2020 (the most recently started time period), the Lag 1 period would be January to December 2019, the Lag 2 period would be January to December 2018, and so on.
#'
#' As a second example, suppose we again have a biology sample dated 15 September 2020 and that flow statistics are available for a sequence of overlapping 6 month periods (i.e. February to July 2020, March to August 2020, April to September 2020, and so on). Using Method "A", the Lag 0 period for that biology sample would be March to August 2020 (the most recently finished time period), the Lag 1 period would be February to July 2020, the Lag 2 period would be January to June 2020, and so on. Similarly, using Method "B", the Lag 0 period for that biology sample would be would be September 2000 to February 2021 (the most recently started time period), the Lag 1 period would be 1 August 2000 to January 2021, the Lag 2 period would be July to December 2020, and so on.

#' @return `join_he` returns a tibble containing the linked biology data and flow statistics.
#'
#' @export
#'
#' @examples
#' ## input datasets for a single site
#' flows <- data.frame(time_period = c(1,2,3,4), q95 = c(0.2, 0.6, 0.8, 0.5)); flows
#' time_period q95
#'           1 0.2
#'           2 0.6
#'           3 0.8
#'           4 0.5
#'
#' biol <- data.frame(id = c(1,2,3), time_period = c(2,4,4), life = c(0.3, 0.9, 0.4)); biol
#' id time_period life
#' 1           2  0.3
#' 2           4  0.9
#' 3           4  0.4
#'
#' ## Add flow statistics to each biology sample
#' join_he(biol, flows, lags = c(0,1), join_type ="add_flows")
#'
#' id time_period life q95_0 q95_1
#' 1           2  0.3   0.6   0.2
#' 2           4  0.9   0.5   0.8
#' 3           4  0.4   0.5   0.8
#'
## Add biology sample data to flow statistics for each time period
#' join_he(biol, flows, lags = c(0,1), join_type ="add_biol")
#'
#' time_period q95_0 q95_1 id life
#'           1   0.2    NA  NA   NA
#'           2   0.6   0.2   1  0.3
#'           3   0.8   0.6  NA   NA
#'           4   0.5   0.8   2  0.9
#'           4   0.5   0.8   3  0.4



join_he <- function(biol_data,
                    flow_stats,
                    mapping = NULL,
                    method = "A",
                    lags = 0,
                    join_type = "add_flow"){


  if(is.data.frame(biol_data) == FALSE){stop("biol_data is invalid format")}

  if("biol_site_id" %in% colnames(biol_data) == FALSE)
  {stop("biol_site_id column was not identified in biol_data")}

  if("date" %in% colnames(biol_data) == FALSE)
  {stop("date column was not identified in biol_data")}

  if(nrow(biol_data) == 0)
  {stop("biol_data is of length 0")}

  if(is.data.frame(flow_stats) == FALSE){stop("flow_stats is invalid format")}

  if("flow_site_id" %in% colnames(flow_stats) == FALSE)
  {stop("flow_site_id column was not identified in flow_stats")}

  if("start_date" %in% colnames(flow_stats) == FALSE)
  {stop("start_date column was not identified in flow_stats")}

  if("end_date" %in% colnames(flow_stats) == FALSE)
  {stop("end_date column was not identified in flow_stats")}

  if(is.null(mapping) == FALSE && is.data.frame(mapping) == FALSE)
  {stop("Mapping data frame is invalid")}

  if(is.null(mapping) == FALSE && "flow_site_id" %in% colnames(mapping) == FALSE)
  {stop("flow_site_id column was not identified in mapping")}

  if(is.null(mapping) == FALSE && "biol_site_id" %in% colnames(mapping) == FALSE)
  {stop("biol_site_id column was not identified in mapping")}

  if(isTRUE(any(is.na(mapping))) == TRUE)
  {stop("mapping contains NAs")}

  if(testInteger(lags) == FALSE) {stop("lags must be an integer")}

  if(method != "A" && method != "B")
  {stop("method must be specified using 'A' or 'B'")}

  if(join_type != "add_flows" && join_type != "add_biol")
  {stop("join_type must be specified using 'add_flows' or 'add_biol'")}


  ## get flow stats
  flow_stats <- flow_stats
  biol_data <- biol_data

  if(lubridate::is.Date(biol_data$date) == FALSE) {stop("date_col must be yyyy-mm-dd date format")}

  ## get mapping
  if(is.null(mapping) == FALSE){

    biol_data <- dplyr::filter(biol_data, biol_site_id %in% unique(mapping$biol_site_id))
    flow_stats <- dplyr::filter(flow_stats, flow_site_id %in% unique(mapping$flow_site_id))

  }

  # if mapping is not specified, create mapping
  ## assumes paired biology and flow sites have identical ids
  if(is.null(mapping) == TRUE){

    biol_site_id <- unique(biol_data$biol_site_id)
    flow_site_id <- biol_site_id

    mapping <- data.frame(biol_site_id, flow_site_id)

  }


  ## flow data processing
  ## create lagged flow data - applied even when lags = 0
  if(is.null(lags) == FALSE){

    # ensure win_no and lags are numeric
    flow_stats$win_no <- as.numeric(flow_stats$win_no)
    as.numeric(lags)

    # create a template dataset with flow_site_id, win_no, start_date, and end_date - to be used later in joining dataframes
    flow_stats_temp <- flow_stats %>% dplyr::select(flow_site_id, win_no, start_date, end_date)

    # pull-in lagged flow - loops through for each lag
    for(i in unique(lags)){

      # create lag win_no
      flow_stats_lag <- flow_stats %>%
        mutate(lag = i,
               win_no = win_no + lag) %>%
                dplyr::select(flow_site_id, win_no)

      # join flow stats for each lag
      flow_stats_lag_1 <- left_join(flow_stats_lag, flow_stats, by = c("flow_site_id", "win_no"))

      # remove identifiers
      flow_stats_lag_2 <- flow_stats_lag_1 %>% dplyr::select(-flow_site_id, -start_date, -end_date)

      # rename cols with lag_x
      as.character(lags)
      lag_name <- as.character(paste("lag", sep = "_", i))
      colnames(flow_stats_lag_2) <- paste(colnames(flow_stats_lag_2), lag_name, sep = "_")

      # assign to a dataframe
      assign(paste(i, sep = "_", "flow_stats_lag"), flow_stats_lag_2)

    }

    # collate and bind all lagged flow stats
    flow_stats_lag_all <- c(mget(ls(pattern = "_flow_stats_lag")))
    flow_stats_lag_all_2 <- Reduce('bind_cols', flow_stats_lag_all)

    # all flow data
    flow_stats_all <- cbind(flow_stats_temp, flow_stats_lag_all_2)

  }


  ## process and join biology data
    if(method == "A"){

      # add mapping
      biol_data_2 <- biol_data %>%
        dplyr::left_join(mapping, by = "biol_site_id")

      # index biology and flow stats to find the nearest flow window and create date_list
      date_indx <- survival::neardate(biol_data_2$flow_site_id, flow_stats$flow_site_id,
                                      biol_data_2$date, flow_stats$end_date, best = "prior",
                                      nomatch = NA_integer_)
      date_list <- data.frame(flow_site_id = flow_stats[date_indx, "flow_site_id"],
                              win_no = flow_stats[date_indx, "win_no"],
                              date = biol_data_2["date"],
                              biol_site_id = biol_data_2["biol_site_id"])

      # join date_list to biol_data
      biol_data_3 <- biol_data_2 %>% left_join(date_list, by = c("biol_site_id", "flow_site_id", "date"))

     if(join_type == "add_flows"){

      # join flow stats to biology data
      join_data <- biol_data_3 %>% left_join(flow_stats_all, by = c("flow_site_id","win_no"))

     }

      if(join_type == "add_biol"){

      # join biology data to flow stats
      join_data <- flow_stats_all %>% left_join(biol_data_3, by = c("flow_site_id","win_no"))

      }


  }

  if(method == "B"){

    # add mapping
    biol_data_2 <- biol_data %>%
      dplyr::left_join(mapping, by = "biol_site_id")

    # index biology and flow stats to find the nearest flow window and create date_list
    date_indx <- survival::neardate(biol_data_2$flow_site_id, flow_stats$flow_site_id,
                                     biol_data_2$date, flow_stats$start_date, best = "prior",
                                     nomatch = NA_integer_)
    date_list <- data.frame(flow_site_id = flow_stats[date_indx, "flow_site_id"],
                            win_no = flow_stats[date_indx, "win_no"],
                            date = biol_data_2["date"],
                            biol_site_id = biol_data_2["biol_site_id"])

    # join date_list to biol_data
    biol_data_3 <- biol_data_2 %>% left_join(date_list, by = c("biol_site_id", "flow_site_id", "date"))

    join_data <- biol_data_3 %>% left_join(flow_stats_all, by = c("flow_site_id","win_no"))


    if(join_type == "add_flows"){

      # join flow stats to biology data
      join_data <- biol_data_3 %>% left_join(flow_stats_all, by = c("flow_site_id","win_no"))

    }

    if(join_type == "add_biol"){

      # join biology data to flow stats
      join_data <- flow_stats_all %>% left_join(biol_data_3, by = c("flow_site_id","win_no"))

    }


    }


  return(join_data)

}


##############################################################
## test if integer

testInteger <- function(x){
  test <- all.equal(x, as.integer(x), check.attributes = FALSE)
  if(test == TRUE){ return(TRUE) }
  else { return(FALSE) }
}

