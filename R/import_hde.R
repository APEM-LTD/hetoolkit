#' Import daily mean flow data from Hydrology Data Explorer (HDE)
#'
#' @description
#' This function retrieves daily mean flow data from the EA's hydrology data explorer (HDE) for a user-defined set of sites (gauging stations). Date can be optionally filtered by date.
#'
#' @usage
#' import_hde (sites, start_date = "1985-01-01", end_date = Sys.Date()))
#'
#' @param sites Vector of site (station) IDs to filter by. NAs and blanks ("") are ignored.
#' @param start_date Start date for flow data extraction (YYYY-MM-DD format);
#'  Default = 01/01/1985.
#' @param end_date End date for flow data extraction (YYYY-MM-DD format);
#'  Default = today's date.
#'
#' @details
#' This function provides access to Environment Agency open hydrology data via the Hydrology Data Explorer (https://environment.data.gov.uk/hydrology/).
#'
#' The function initially imports flow data for all dates and then filters out records that are before start_date or after end_date. If the data does not span the entire range of dates provided, additional records are created and the flow and quality values defined as NA on these dates.
#'
#' A warning message is produced for every site that cannot be found on HDE.
#'
#' If a site ID is duplicated in the 'sites' argument, that site is only searched for once and a warning message is produced.
#'
#' @return A tibble containing daily flow data for the specified sites, with the following columns: flow_site_id, date, flow (in m3/s) and quality.
#'
#' @examples
#' # Import data for selected sites and dates
#' # import_hde(sites = c("F1707", "030028"), start_date = "2000-01-01", end_date = "2019-12-31")
#'
#' # Returns flow = NA if site exists but no data available for the specified date range
#' # import_hde(sites = c("F1707"), start_date = "1900-01-01", end_date = "1900-01-05")
#'
#' # Returns data only for sites that are present in NRFA; see warning messages for missing sites
#' # import_hde(sites = c("F1707", "hello"), start_date = "2010-01-01", end_date = "2010-01-05")
#'
#' # Returns an error if none of the sites are present in HDE
#' # import_hde(sites = c("hello", "hello2"))



import_hde <- function(sites,
                      start_date ="1985-01-01",
                      end_date = Sys.Date()){

  #stop if no station ID or list of station IDs is provided to the function
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
          warning(print(paste('Warning: ', removed_sites, ' duplicate site ID(s) detected and ignored', sep="")))
      }

  # stop if start date is provided in incorrect format
  if(IsDate(start_date, "%Y-%m-%d") == FALSE){
    stop ("Date should be in YYYY-MM-DD format")
  }

  # stop if end date is provided in incorrect format
  if(IsDate(end_date, "%Y-%m-%d") == FALSE){
    stop("Date should be in YYYY-MM-DD format")
  }

  # stop if start date provided is in the future
  if(start_date > Sys.Date()){
    stop("Start date given is in the future")
  }

  # stop if end data provided is in the future
  if(end_date > Sys.Date()){
    stop("End date given is in the future")
  }

  # stop is end date provided is before start date
  if(end_date < start_date){
    stop ("End date provided is before start date")
  }

  # get list of every station in HDE
  stations_global <- get_hde_stations()
  if(length(stations_global) == 0) {
    stop("Can't find a list of HDE stations")
  }

  # create list of only user defined stations in global list
  stations_hde <- sites[sites %in% stations_global$wiskiID]

  # warning if no stations are found in global list
  stations_cant_find <- sites[!(sites %in% stations_global$wiskiID)]

  #check and stop if none of the defined stations are present in the HDE station list
  if(length(stations_hde) == 0 ) {
    stop("Cant find any matching HDE stations")
  } else if(length(stations_cant_find) > 0 ){
        warning(print(paste('Could not find the following stations: ', paste(stations_cant_find, sep="", collapse = ", "))))
  }

  # import data from HDE
  hde_flow_data <- purrr::map_dfr(stations_hde, ~get_hde_flows(., stations_global, start_date, max.date = end_date))


  # Create dataset running from start_date to end_date for every site
  full_grid <- expand.grid(flow_site_id = stations_hde, date = seq(from = lubridate::date(start_date), to = lubridate::date(end_date), by = 1))

  complete_data <- dplyr::left_join(full_grid, hde_flow_data, by = c("flow_site_id" = "wiskiID", "date"))

  names(complete_data)[names(complete_data) == "value"] <- "flow"
  complete_data <- complete_data[order(complete_data$flow_site_id,complete_data$date),]

  return (tibble::as_tibble(complete_data))

}


####################### import_hde helper functions ######################

# get_hde_flows

get_hde_flows <- function(my.station, global.stations, min.date, max.date = date(today()), SAFE = TRUE, PEEKDATA = TRUE){

  my.station.url <- get_hde_url(my.station, global.stations)
  full.url <- paste0(my.station.url, "/readings.csv?mineq-date=", min.date, "&&maxeq-date=", max.date )
  print(paste('getting flow data from:', my.station))
  #print(paste('url:', full.url)) ; flush.console()

  # value column gets read in error as logical by default, so force to read as numeric
  # maybe this is when have NAs at the beginning

  if(SAFE == TRUE){
    print('Using SAFE mode, downloading file first')
    download.file(full.url, destfile = 'temp_download.csv')
    if (file.info('temp_download.csv')$size >0){
      flow.series <- readr::read_csv('temp_download.csv', col_types = readr::cols(value = readr::col_double()) ) %>%
        dplyr::mutate(wiskiID = my.station) %>% dplyr::select(wiskiID, date, value, quality) #, everything())
      # if file is of length zero then just don't read it
      # optional, remove temp file
    } else{
      # create empty df
      flow.series = tibble::tibble(wiskiID = character(), date = lubridate::ymd(), value = double(), quality = character())
    }
  } else {
    print('Using direct access mode')
    # unsafe method, will fail with empty file
    flow.series <- readr::read_csv(full.url, col_types = cols(value = col_double()) ) %>%
      mutate(wiskiID = my.station) %>%
      dplyr::select(wiskiID, date, value, quality) #, everything())
  }

  if(PEEKDATA == TRUE){
    print(head(flow.series))
    print(tail(flow.series))
  }

  flush.console()

  return(flow.series)

}

############################################################################

# get_hde_stations

# download the HDE stations table and make a column which has the url for daily mean flows for each station

get_hde_stations <- function(){

  stations <- download.file("http://environment.data.gov.uk/hydrology/id/stations.csv", destfile = "stations.csv", mode ="wb")

  stations <- readr::read_csv("stations.csv")

  stations <- stations %>%
    dplyr::select(wiskiID, label, stationReference, riverName, easting, northing, measures) %>%
    tidyr::separate(measures, c("col1", "col2"), sep = "\\|", fill = 'right') %>%
    tidyr::pivot_longer(cols = c(col1, col2), names_to = 'col') %>%
    dplyr::filter(stringr::str_detect(value, '86400'))

  return(stations)

}

############################################################################

# get_hde_url

# returns a single value which is a url string, given a HDE station id

get_hde_url <- function(my.station, global.stations){

  global.stations %>%
    dplyr::filter(wiskiID == my.station) %>%
    dplyr::select(value) %>%
    dplyr::pull()

}

