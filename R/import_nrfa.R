#' Importing flow data from the National River Flow Archive (NRFA)
#'
#' @description
#' The import_nrfa function downloads mean daily flow data from the NRFA for a user-defined set of sites (gauging stations). Data can be optionally filtered by date.
#'
#' @usage
#' import_nrfa(sites, start_date = "1985-01-01", end_data = Sys.Date(), tidyup = TRUE)
#'
#' @param sites Vector of site (station) IDs to extract data for. NAs and blanks ("") are ignored
#' @param start_date Start date for flow data extraction (YYYY-MM-DD format). Default = "1985-01-01".
#' @param end_date End date for flow data extraction (YYYY-MM-DD format). Default = today's date.
#' @param tidyup Whether or not csv files downloaded by the function to the users working directory should be deleted afterwards. Default = TRUE.
#'
#' @details The National River Flow Archive (NRFA; <http://nrfa.ceh.ac.uk>) is the main archive of river flow data for the United Kingdom, and contains data from over 1500 gauging stations. This function downloads flow data from the NRFA as a series of CSV files, one per site, and collates them into a single data set.
#'
#' The function initially imports flow data for all dates and then filters out records that are before start_date or after end_date. If the data does not span the entire range of dates provided, additional records are created and the flow and quality values defined as NA on these dates.
#'
#' A warning message is produced for every site that cannot be found on NRFA.
#'
#' If a site ID is duplicated in the 'sites' argument, that site is only searched for once and a warning message is produced.
#'
#' @return A tibble containing daily flow data for the specified sites, with the following columns: flow-site_id, date, and flow (in m3/s). (The NRFA database does not contain information about the quality of the flow data.)
#'
#' @export
#'
#' @examples
#' # Import data for selected sites and dates (and keep downloaded CSV files for inspection)
#' # import_nrfa(sites = c("1001", "2001"), start_date = "2000-01-01", end_date = "2019-12-31", tidyup = FALSE)
#'
#' # Returns flow = NA if site exists but no data available for the specified date range
#' # import_nrfa(sites = c("1001"), start_date = "1900-01-01", end_date = "1900-01-05")
#'
#' # Returns data only for sites that are present in NRFA; see warning messages for missing sites
#' # import_nrfa(sites = c("1001", "hello"), start_date = "2010-01-01", end_date = "2010-01-05")




import_nrfa <- function(sites, start_date = "1985-01-01", end_date = Sys.Date(), tidyup = FALSE){

  # stop if no station ID or list of station IDs is provided to the function
  if(missing(sites)) {
    stop("Need at least one station ID")
  }

  # remove NAs and blanks
  sites <- sites[is.na(sites) == FALSE]
  sites <- sites[sites != ""]

  # remove duplicates
  original_sites <- length(sites)
  sites <- unique(sites)
  if(original_sites > length(sites)){
    removed_sites <- (original_sites - length(sites))
    warning(print(paste('Warning ', removed_sites, ' duplicate site id detected and ignored', sep="")))
  }

  # stop if tidyup is invalid format
  if(is.logical(tidyup) == FALSE){
    stop("tidyup argument must be TRUE or FALSE")
  }

  # stop if start date is provided in incorrect format
  if(IsDate(start_date, "%Y-%m-%d") == FALSE){
    stop("Date should be in YYYY-MM-DD format")
  }

  # stop if end date is provided in incorrect format
  if(IsDate(end_date, "%Y-%m-%d") == FALSE){
    stop("Date should be in YYYY-MM-DD format")
  }

  # stop if start date provided is in the future
  if(start_date > Sys.Date()){
    stop("Start date is in the future")
  }

  # stop if end data provided is in the future
  if(end_date > Sys.Date()){
    stop("End date is in the future")
  }

  # stop if end date provided is before start date
  if(end_date < start_date){
    stop ("End date is before start date")
  }

  # import data from NRFA
  flow_data <- purrr::map_dfr(sites, ~get_nrfa_data(., tidyup))

  # stop if no data retrieved
  if(dim(flow_data)[1] == 0){
    stop("URL does not seem to exist for these NRFA site(s); no data retrieved")
  }

  # format flow and quality columns
  flow_data$flow <- as.numeric(flow_data$flow)
  flow_data$quality <- as.character(flow_data$quality)

  # list sites that are present in NRFA
  #sites_with_data <- sites[sites %in% flow_data$Station]
  sites_with_data <- sites[sites %in% flow_data$flow_site_id]

  # filter date by date
  flow_data <- flow_data %>%
    dplyr::filter(flow_data$date >= start_date & flow_data$date <= end_date)

  # Create dataset running from start_date to end_date for every site
  full_grid <- expand.grid(flow_site_id = sites_with_data, date = seq(from = lubridate::date(start_date), to = lubridate::date(end_date), by = 1))

  complete_data <- dplyr::left_join(full_grid, flow_data, by = c("flow_site_id", "date"))

  complete_data <- complete_data[order(complete_data$flow_site_id,complete_data$date),]

  return(tibble::as_tibble(complete_data))

}

##################### helper functions ###########################

# get_nrfa_data: Download and import data for one site (station)

get_nrfa_data <- function(my.site, tidyup) {

  url.stub <- 'https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gdf&station='

  data_nrfa <- tryCatch(
    {
      suppressWarnings(download.file(paste0(url.stub, my.site), destfile = paste0(my.site, ".csv")))

      # was originally hard-wired to skip = 19, but some sites have <19 or > 19 metadata rows
      #nrfa_file <- readr::read_csv(paste0(my.site, '.csv'), col_names = FALSE, skip = 19)

      # instead check data to identify final metadata row and filter data accordingly
      nrfa_file <- suppressWarnings(readr::read_csv(paste0(my.site, '.csv'), col_names = FALSE))
      nrfa_file <- nrfa_file[ ,c(1,2)]
      # identify final metadata row (assumed to always have "last" in the second column)
      nrfa_file$X3 <- ifelse(nrfa_file$X2 == "last", 1, 0)
      # set X3 = 0 where missing data results in NAs
      nrfa_file$X3[is.na(nrfa_file$X3)] <- 0
      # identify and exclude metadata rows
      nrfa_file <- nrfa_file %>%
        dplyr::mutate(X4 = cumsum(X3)) %>%
        dplyr::filter(X4 >= 1)
      #drop final metadata row and redundant X3 and X4 columns
      nrfa_file <- nrfa_file[-1, -c(3,4)]

      # Add flow_site_id and quality columns
      #nrfa_file$Station <- my.site
      nrfa_file$flow_site_id <- my.site
      nrfa_file$quality <- NA_character_

      # format and name columns
      nrfa_file <- nrfa_file %>%
        dplyr::rename("date" = "X1", "flow" = "X2")

      nrfa_file$date <- as.Date(nrfa_file$date)
      nrfa_file$flow <- as.numeric(nrfa_file$flow)
      nrfa_file$quality <- as.character(nrfa_file$quality)

      # delete the downloaded CSV file (warnings suppressed to reduce clutter in console)
      if(tidyup == TRUE){
        suppressWarnings(unlink(paste0(my.site, '.csv')))
      }

      # return this file
      nrfa_file <- nrfa_file

    },

    # Choose a return value in case of error
    error=function(cond) {

      message(paste("URL does not seem to exist for site:", my.site, ", site name is probably incorrect."))

      # create date vector of length 0
      a <- as.Date("2020-01-01"); a <- a[-1]
      nrfa_file <- data.frame(date = a, flow = double(0), flow_site_id = character(0), quality = character(0))

      return(nrfa_file)

    },

    # warning=function(cond) {
    # message(paste("URL does not seem to exist for site:", my.site, ", site name is probably incorrect."))

    finally={}

  )

  return(data_nrfa)

}



