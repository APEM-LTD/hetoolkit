#' Impute missing flow data.
#'
#' @description This function infills missing records in daily flow time series for one or more sites (gauging stations) using either interpolation or an equipercentile  method. Imputation of missing flow data can improve the later estimation of flow statistics using the calc_flowstats() function and aid the visualisation of hydro-ecological relationships using the plot_hev() function.  Note, it is advisable to consult a hydrologist if your data contains extensive/lengthy gaps.
#'
#' @usage impute_flow(data, site_col = " flow_site_id", date_col = "date", flow_col = "flow", method = "linear", donor = NULL)
#'
#' @param data A tibble or data frame containing the data to be imputed.
#' @param site_col Name of column in data containing unique flow site id. Default = "flow_site_id". Site ids are coerced to a character vector.
#' @param date_col Name of column in data containing date of flow record. Default = "date".  Dates must be in “yyyy-mm-dd” format.
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
#' @export
#'
#' @examples
#' ## impute flow statistics using 'linear' method
#' impute_flow(data_impute,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "linear")
#'
#' ## impute flow statistics using 'exponential' method
#' impute_flow(data_impute,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "exponential")
#'
#' impute flow statistics using 'equipercentile' method, without specifying the donor station to be used
#' impute_flow(data_equipercentile,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "equipercentile")
#'
#' impute flow statistics using 'equipercentile' method, without specifying the donor station to be used
#' impute_flow(data_equipercentile,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "equipercentile",
#'             donor = donor_data)
#'


impute_flow <- function(data,
                        site_col = "flow_site_id",
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
  {warning("Donor stations will not be used; to use donor stations, select 'method = equipercentile'")}

  if(is.null(donor) != TRUE && isTRUE(method == "equipercentile") == TRUE && is.data.frame(donor) != TRUE && tibble::is_tibble(donor) != TRUE)
  {stop("Donor stations must be specified in data frame or tibble format")}

  # rename for later and pull-in site_col
  data <- data %>% dplyr::rename("site" = site_col,
                                 "flow" = flow_col,
                                 "date" = date_col)

  if(isTRUE(method == "equipercentile") == TRUE && isTRUE(length(unique(data$site)) < 2) == TRUE)
  {stop("A minimum of two flow stations are required if applying equipercentile method")}

  if(isTRUE(1 %in% diff.Date(data$date)) == FALSE){stop("flow data supplied is not on a daily time-step")}
  if(isTRUE("FALSE" %in% is.na(data$flow[data$flow < 0])) == TRUE){warning("flow data contains negative values")}

  # rename so original data remains unchanged
  data_1 <- data

  # if 'method' and 'imputed columns feature in the original data, drop these (they will be added back in at the end)
  if(isTRUE("method" %in% colnames(data)) == TRUE | "imputed" %in% colnames(data)){
    drop_vars <- c("method", "imputed")
    data_1 <- data[,!(names(data) %in% drop_vars)]
  }

  data_2 <- data_1 %>% dplyr::select(site, date, flow)

  for(i in unique(data_2$site)){

    # filter and pull-in data
    data_f <- dplyr::filter(data_2, site == i)

    data_f$date <- dplyr::pull(data_f, date_col)
    data_f$flow <- dplyr::pull(data_f, flow_col)

    if(lubridate::is.Date(data$date) == FALSE) {stop("date_col must be of date yyyymmdd format")}

    # check whether full dataset is NAs
    # skip site if only NAs
    if("FALSE" %in% is.na(data_f$flow) == FALSE){

      warning(paste0("flow column contains only NAs for site", sep = " ", i))

      if(length(unique(data$site) < 2)){
        stop("Only one flow site specified. Flow site contains only NAs") } else {
      next

        }
    }

    # Check flow is numeric
    if(is.numeric(data_f$flow) == FALSE)
    {stop("Specified flow_col is not numeric")}

    # Check for duplicate dates
    duplicates <- data_f$date[duplicated(data_f$date)]
    if(length(duplicates) >= 1){stop("Duplicate dates identified")}

    # Use expand grid to set-up a date range, from minimum flow date to present day
    all_dates <- expand.grid(date = seq(min(data$date), max(data$date), "days"), stringsAsFactors = FALSE)
    dim(all_dates)

    # Linear method

    if(method == "linear"){

      # Join all_dates with observed flow data, retaining NAs where flow data is missing
      original_flow <- dplyr::left_join(x = all_dates, y = data_f, by = "date")
      original_flow <- original_flow %>% dplyr::mutate(site = i)

      # Filter to identify min and max dates which are not NA
      original_flow_f <- dplyr::filter(original_flow, !is.na(flow))
      maxdate <- max(original_flow_f$date)
      mindate <- min(original_flow_f$date)

      # Impute flow data for the filtered dataset, and assign imputed flow to the a new column 'flow_i'
      original_flow_i <- original_flow %>%
        dplyr::filter(date <= maxdate & date >= mindate)

      original_flow_i$flow_i <- na_interpolation(original_flow_i$flow)

      imp <- original_flow_i$flow_i
      if("TRUE" %in% is.na(original_flow_i$flow) == TRUE){
      plot_1 <- ggplot_na_imputations(original_flow_i$flow, imp)
      ggplot2::ggsave(paste0(getwd(), sep = "/", i, "_Imputed_Values.png"), plot = plot_1)
      }

      # Merge original and imputed flow datasets
      all_flow <- dplyr::left_join(original_flow, original_flow_i, by = c("date", "site", "flow"))

      # Flag imputed data
      all_flow <- all_flow %>%
        dplyr::mutate(imputed = ifelse(is.na(flow) & !is.na(flow_i), 1, 0))

      # Specify imputation method
      all_flow <- all_flow %>%
        dplyr::mutate(method = ifelse(imputed == 1, "Linear", NA))

      # Drop extra flow column column
      all_flow$flow <- all_flow$flow_i
      drops <- "flow_i"
      all_flow <- all_flow[,!(names(all_flow) %in% drops)]

    }

    # Exponential method

    if(method == "exponential"){

      # Join all dates with observed flow data, retaining NAs where flow data is missing
      original_flow <- dplyr::left_join(x = all_dates, y = data_f, by = "date")
      original_flow <- original_flow %>% dplyr::mutate(site = i)

      # Error if 0 follows NA or vice versa
      pattern1 <- "0-NA"
      pattern2 <- "NA-0-"
      original_data <- as.numeric(original_flow$flow)
      data_as_string <- paste(original_data, collapse="-")

      test1 <- gregexpr(pattern1,data_as_string)[[1]][1]
      test2 <- gregexpr(pattern2,data_as_string)[[1]][1]
      if(isTRUE(-1 %in% sign(test1) == FALSE) | isTRUE(-1 %in% sign(test2) == FALSE)){
        warning("NA value follows or precedes a 0 value, exponential method cannot be applied in these instances")
      next }

          # Filter to identify min and max dates which are not NA
          original_flow_f <- dplyr::filter(original_flow, !is.na(flow))
          maxdate <- max(original_flow_f$date)
          mindate <- min(original_flow_f$date)

          # Impute flow data for the filtered dataset, and assign imputed flow to the a new column 'flow_i'
          original_flow_i <- original_flow %>%
            dplyr::filter(date <= maxdate & date >= mindate)

          original_flow_i$flow_i <- na_interpolation(log(original_flow_i$flow))
          original_flow_i$flow_i <- exp(original_flow_i$flow_i)

          imp <- original_flow_i$flow_i
          if("TRUE" %in% is.na(original_flow_i$flow) == TRUE){
          plot_1 <- ggplot_na_imputations(original_flow_i$flow, imp)
          ggplot2::ggsave(paste0(getwd(), sep = "/", i, "_Imputed_Values.png"), plot = plot_1)
          }

          # Merge original and imputed flow datasets
          all_flow <- dplyr::left_join(original_flow, original_flow_i, by = c("date", "site", "flow"))

          # Flag imputed data
          all_flow <- all_flow %>%
            dplyr::mutate(imputed = ifelse(is.na(flow) & !is.na(flow_i), 1, 0))


          # Specify method
          all_flow <- all_flow %>%
            dplyr::mutate(method = ifelse(imputed == 1, "Exponential", NA))

          # Combine observed and imputed flow columns and drop extra flow column column
          all_flow$flow <- all_flow$flow_i
          drops <- "flow_i"
          all_flow <- all_flow[,!(names(all_flow) %in% drops)]

  }


    # Equipercentile method

    if(method == "equipercentile"){

      if(length(unique(data$site)) < 2){stop("equipercentile method requires a miniumum of two flow sites")}

      # Join all dates with observed flow data, retaining NAs where flow data is missing
      original_flow <- dplyr::left_join(x = all_dates, y = data_f, by = "date")
      original_flow <- original_flow %>% dplyr::mutate(site = i)

      # If donor station is NOT specified

      if(is.null(donor) == TRUE){

        # pull flow data for possible donor sites for correlation test
        for(j in unique(data$site)){

          data_equ <- dplyr::filter(data, site == j)
          data_equ$date <- dplyr::pull(data_equ, date_col)
          data_equ$flow <- dplyr::pull(data_equ, flow_col)

          # Join all dates with observed flow data, retaining NAs where flow data is missing
          data_equp <- dplyr::left_join(x = all_dates, y = data_equ, by = "date")
          data_equp <- data_equp %>% dplyr::mutate(site = j)

          # test correlations
          corr <- suppressWarnings(stats::cor.test(x=original_flow$flow, y=data_equp$flow, method = 'spearman'))
          corr_rho <- as.data.frame(corr$estimate)
          as.data.frame(corr_rho)
          corr_rho$flow_site_id <- j
          assign(paste(j, sep = "_", "corr_rho"), corr_rho)

        }

        # Identify most strongly correlated site (this will be the donor site)
        corr_all <- c(mget(ls(pattern = "_corr_rho")))
        corr_all <- Reduce('bind_rows', corr_all)
        corr_donor <- corr_all[corr_all$flow_site_id != i,]
        corr_donor_final <- corr_donor %>%
          filter(corr_donor$`corr$estimate` == max(corr_donor$`corr$estimate`))

        donor_site <- corr_donor_final$flow_site_id

        # filter the original dataset to get donor site flow data
        donor_flow_data <- data %>%
          dplyr::filter(site == donor_site)

      }

      # If donor station IS specified
      if(is.null(donor) == FALSE){

        # check donor data has a least 2 columns
        if(ncol(donor) < 2)
        {stop("'donor', must contain at least 2 columns; the first a list of flow sites requiring imputation, and the second a list of paired donor sites")}

        # Identify the corresponding donor station
        donor_flow <- donor
        donor_flow$flow_site <- donor_flow[,1]
        donor_flow$donor_site <- donor_flow[,2]

        if(unique(original_flow$site) %in% unique(donor_flow$flow_site) == FALSE)
        {warning(paste("A donor site was not specified for site", sep = "-", i))

          all_flow <- original_flow
          all_flow$imputed <- 0

          donor_flow_data <- NULL

          }

        else {

          flow_site_main <- unique(original_flow$site)
          donor_flow_f <- dplyr::filter(donor_flow, flow_site == flow_site_main)

          # filter the original dataset to get donor site flow data
          donor_flow_data <-
            data %>%
            dplyr::filter(site == donor_flow_f$donor_site)
        }

      }

      if(is.null(donor_flow_data) == FALSE){

      # Check if there are sufficient overlapping records between the main flow site and the donor site
      if(length(intersect(data_f$date, donor_flow_data$date)) < 365)
      {warning(paste(i, sep = "-", "Equipercentile method cannot be applied for this site, due to insufficient overlapping data with the donor site."))
        all_flow <- original_flow
        all_flow$imputed <- 0
      }


      else {

        # Calculate percentile values for donor data
        raw_val <- donor_flow_data$flow
        donor_flow_data$percentile <- stats::ecdf(donor_flow_data$flow)(raw_val)

        # only keep necessary column from donor_flow_data
        donor_flow_data <- donor_flow_data %>% dplyr::select(site, date, flow, percentile)

        # join donor data containing percentile values with main flow data
        data_percentile_flow <- dplyr::left_join(x = original_flow, y = donor_flow_data, by = c("date"))

        # rename columns
        data_percentile_flow <- data_percentile_flow %>% dplyr::rename(flow = flow.x,
                                                                       donor_flow = flow.y,
                                                                       site = site.x,
                                                                       donor_site = site.y)

        data_percentile_flow <- data_percentile_flow %>% dplyr::mutate(site = i)


        # calculate equipercentile for main flow data, based on donor percentile values
        data_percentile_flow <- data_percentile_flow %>%
          dplyr::mutate(flow_equipercentile = stats::quantile(flow,
                                                              percentile, na.rm = TRUE))

        imp <- data_percentile_flow$flow_equipercentile
        if("TRUE" %in% is.na(data_percentile_flow$flow) == TRUE){
        plot_1 <- ggplot_na_imputations(data_percentile_flow$flow, imp)
        ggplot2::ggsave(paste0(getwd(), sep = "/", i, "_Imputed_Values.png"), plot = plot_1)
        }

        # Flag imputed data
        all_flow <- data_percentile_flow %>%
          dplyr::mutate(imputed = ifelse(is.na(flow) & !is.na(flow_equipercentile), 1, 0))


        # Specify method
        all_flow <- all_flow %>%
          dplyr::mutate(method = ifelse(imputed == 1, "Exponential", NA))

        # incorporate imputed flow to flow column
        all_flow <- all_flow %>% dplyr::mutate(flow = ifelse(is.na(flow), flow_equipercentile, flow))

      }

      }
    }


    # Select outputs and match with flow_site_id
    flow_data <- as.data.frame(all_flow)
    assign(paste(i, sep = "_", "flow_data"), flow_data)

  }

  # Merge all sites
  flow_data_all <- c(mget(ls(pattern = "_flow_data")))
  flow_data_all <- Reduce('bind_rows', flow_data_all)

  flow_data_all <- flow_data_all[!is.na(flow_data_all$imputed),]

  drops <- c("percentile", "flow_equipercentile")
  flow_data_all <- flow_data_all[,!(names(flow_data_all) %in% drops)]

  # If present in original data, add back in original imputed and method columns
  if(isTRUE("method" %in% colnames(data)) == TRUE | "imputed" %in% colnames(data)){

    keep_vars <- c("site", "date", "method", "imputed")
    join_data <- data[,(names(data) %in% keep_vars)]

    flow_data_all <- dplyr::left_join(x = flow_data_all, y = join_data, by = c("site", "date"))
    flow_data_all <- flow_data_all %>% dplyr::mutate(method.x = ifelse(is.na(method.x), method.y, method.x))
    flow_data_all <- flow_data_all %>% dplyr::mutate(imputed.x = case_when(imputed.x == 0 & imputed.y == 1 ~ 1,
                                                                           imputed.x == 1 & imputed.y == 0 ~ 1,
                                                                           TRUE ~ 0))

    drop_vars2 <- c("method.y", "imputed.y")
    flow_data_all <- flow_data_all[,!(names(flow_data_all) %in% drop_vars2)]
    flow_data_all <- flow_data_all %>% dplyr::rename(method = method.x,
                                                     imputed = imputed.x)
  }


  # add back in any other columns
  # drop flow col from original data - might not match if different decimal places
  data_extra <- data_1 %>% dplyr::select(-flow)
  flow_data_all <- flow_data_all %>%
    dplyr::left_join(data_extra, by = c("site", "date")) %>%
    dplyr::rename(flow_site_id = site)


  # Output
  return(flow_data_all)


}
