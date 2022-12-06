#' Impute missing flow data.
#'
#' @description This function imputes (infills) missing records in daily flow time series for one or more sites (gauging stations) using either interpolation or an equipercentile  method. Imputation of missing flow data can improve the later estimation of flow statistics using the calc_flowstats() function and aid the visualisation of hydro-ecological relationships using the plot_hev() function.  Note, although this function provides automated tools for imputing missing records, it is advisable to consult a hydrologist if your data contains extensive/lengthy gaps, to ensure that the methods and results are trustworthy.
#'
#' @usage impute_flow(data, site_col = "flow_site_id", date_col = "date", flow_col = "flow", method = "linear", donor = NULL)
#'
#' @param data A tibble or data frame containing the data to be imputed, plus flow data for donor sites.
#' @param site_col Name of column in `data` containing unique flow site id. Default = "flow_site_id". Site ids are coerced to a character vector.
#' @param date_col Name of column in `data` containing the date of each flow record. Default = "date".  Dates must be in "yyyy-mm-dd" format.
#' @param flow_col Name of column in `data` containing the measured flow data. Default = "flow".
#' @param method Imputation method: "linear" (default), "exponential" or "equipercentile". See below for details.
#' @param donor – A tibble or data frame with at least two columns: the first a list of flow sites requiring imputation, and the second a list of paired donor sites. Subsequent columns are ignored. Default = NULL. Only used when `method = equipercentile`.  Note, the date ranges of the donor site and the flow site of interest must overlap by a minimum of 365 records (i.e. 1 year).
#'
#' @details
#' This function is intended for imputing gauged daily flow data only; it cannot be used to impute sub-daily data, and is not designed for data on coarser time steps (e.g. 10-daily or monthly).
#'
#' The function offers three imputation methods: linear interpolation, exponential interpolation and an equipercentile algorithm.
#'
#' The default `linear` method uses linear (straight line) interpolation to impute missing flow values. It is therefore unable to infill gaps at the beginning or end of a time series (these flow values remain `NA`).
#'
#' The `exponential` method assumes that flow changes exponentially with time, and so produces imputed values with an accelerating rate of change in flow on the rising limb of the hydrograph, and a decelerating rate of change on the descending limb. Specifically, the flow on day t (Q<sub>t</sub>) is a function of the flow on the previous day (`Q<sub>t-1</sub>`) and the exponential decay constant (`λ`): `Q<sub>t</sub> = Q<sub>t-1</sub> e<sup>-λ</sup>`. For example, a flow time series with a 3 day gap – `10, NA, NA, NA, 6` – has an exponential decay constant λ = ln(10/6) / 4 = 0.1277. The interpolated value for day 2 is therefore `10<sup>e-0.1277</sup> = 8.801`, and the full interpolated time series is: `10.000, 8.801, 7.746, 6.817, 6.000`. Like the `linear` method, however, it is unable to infill gaps at the beginning or end of a time series. Furthermore, the `exponential` method will fail if the flow value immediately before or after a gap is 0 (or negative).
#'
#' The `equipercentile` method uses measured flows at a donor site to estimate missing flows at a target site. Specifically, the percentile value of the donor flow on any given day is assumed equal to the percentile value of the target flow. Gaps are infilled by calculating the donor flow percentile values and using the existing target flow data to derive the flow equivalent to this percentile value at the target site (for details see Hughes and Smakhtin 1996).
#'
#' The donor site to be used for each target site can be specified by the user (via the `donors` argument). If `donors = NULL`, then the function finds the site in `data` whose flows are most strongly correlated (using Spearman’s rank correlation coefficient) with those at the target site, and uses that as the donor site. Note that this automated method does not guarantee that the donor site identified will be suitable; indeed, the donor site may be very unsuitable if none of the other sites are climatologically and hydrologically similar to the target site. To mitigate the risk of poor imputation, the function requires that paired target and donor sites have a minimum of 365 overlapping measured flow records. If this condition is not met and a donor site cannot be identified, then the function provides a warning message listing the sites affected.
#'
#' The interpolation methods have the benefit of simplicity and typically perform best when used to infill relatively short gaps – i.e. intervals where it is reasonable to assume that flows are stable or changing linearly or exponentially over time. The equipercentile method can be better than interpolation at infilling longer gaps, during which flows may change abruptly in response to rainfall events, but its performance is critically dependent on the suitability of the donor site. Donor sites should be hydrologically similar to, and have flows which are strongly correlated with, the target site. If these conditions are not met, then the equipercentile method can produce very imprecise or biased imputed values. When a suitable donor site is used, the equipercentile method has been demonstrated to be superior to many other imputation techniques (including catchment area scaling, long-term mean scaling, and linear regression methods using a single donor site) and to perform similarly to multiple regression using two donor sites (Harvey et al. 2012).
#'
#' The function applies just one, chosen method at a time, with no default to fall-back methods. If the first method fails to infill all the gaps, then the function can be run a second time, with a different chosen method, to try to infill the remaining gaps, and so on. This iterative approach provides flexibility to determine the sequence in which methods are applied.
#'
#' The `linear` and `exponential` methods can be applied to a single site, but the `equipercentile` method requires a minimum of two sites (each site acting as a donor for the other).
#'
#' When processing data for multiple sites, it is recommended that the sites have flow data that span a common time period. This is not essential, however, as the function identifies the earliest and latest dates (across all sites), and ‘expands’ the output dataset to cover all dates for all sites. If flow cannot be imputed for a certain date, then the flow value returned is `NA`.
#'
#' @return A tibble containing the imputed flow data. The data are arranged in long format, with the following columns:
#'
#'    - flow_site_id (unique flow site id)
#'    - date (of flow record)
#'    - imputed (flag indicating whether each flow value is original (0) or imputed (1))
#'    - donor_site (id of donor site used for imputation when using the `equipercentile` method)
#'    - donor_flow (measured flow at donor site on that date, when using the `equipercentile` method )
#'    - any other columns in the input dataset are automatically pulled through and joined to the output data table (e.g. the `input` and `quality` columns from the `import_flow()` function).
#'
#' @references
#'
#' Harvey, C. L., Dixon, H., and Hannaford, J. (2012) An appraisal of the performance of data-infilling methods for application to daily mean river flow records in the UK. Hydrology Research, 43(5), 618-636.
#' Hughes, D.A., and Smakhtin, V. (1996) Daily flow time series patching or extension: A spatial interpolation approach based on flow duration curves. Hydrological Sciences Journal, 41, 851-871.
#'#'
#' @export
#'
#' @examples
#' ## impute flow statistics using 'linear' method
#' impute_flow(data = data_impute,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "linear")
#'
#' ## impute flow statistics using 'exponential' method
#' impute_flow(data = data_impute,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "exponential")
#'
#' impute flow statistics using 'equipercentile' method, without specifying the donor station to be used
#' impute_flow(data = data_equipercentile,
#'             site_col = "flow_site_id",
#'             date_col = "date",
#'             flow_col = "flow",
#'             method = "equipercentile")
#'
#' impute flow statistics using 'equipercentile' method, without specifying the donor station to be used
#' impute_flow(data = data_equipercentile,
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

    if(lubridate::is.Date(data$date) == FALSE) {stop("date_col must be of date (yyyy-mm-dd) format")}

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
          dplyr::mutate(method = ifelse(imputed == 1, "Equipercentile", NA))

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
