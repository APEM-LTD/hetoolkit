#' Import flow data from NRFA, HDE and local files
#'
#' @description
#' `import_flow` is a high-level function that calls `import_nrfa`, `import_hde` and `import_flowfiles` to import data for a user-defined list of sites.
#'
#' @usage
#'  import_flow(sites = NULL, inputs = NULL, start_date = 1900-01-01, end_date = Sys.Date, dir = NULL, skip_num = NULL, col_order = NULL)
#'
#' @param sites Vector of site (station) IDs to extract data for.
#' @param inputs Vector of flow data inputs, must be either "NRFA", "HDE" or "FLOWFILES"
#' @param start_date Start date for flow data extraction (YYYY-MM-DD format). Default = `1900-01-01`.
#' @param end_date End date for flow data extraction (YYYY-MM-DD format). Default = today's date.
#' @param dir Path to local files. Ignored if `inputs` doesn't contain "FLOWFILES".
#' @param skip_num For local files, defines the number of rows to skip before starting to read data. Ignored if `inputs` doesn't contain "FLOWFILES".
#' @param col_order For local files, defines which columns contain the data of interest: `date`, `flow` and, if available, `quality`. Ignored if `inputs` doesn't contain "FLOWFILES".
#' @param date_format The order of the year (y), month (m), day (d), hour (h), minute (m) and second (s) elements in the flow file. Default = "dmy". See `?import_flowfiles` for more information. Ignored if `inputs` doesn't contain "FLOWFILES".
#'
#' @details
#' import_flow requires a list of site (station) ids (called `sites`), and a corresponding list (called `inputs`) specifying where to source the data for each site (either "NRFA", "HDE" or, for local files, "FLOWFILES"). `sites` and `inputs` must be of equal length. Any records where `sites` or `inputs` is NA are dropped.
#'
#' If a `sites`-`inputs` combination is defined more than once, the duplicate(s) are ignored and a warning message is produced.
#'
#' If a site id is listed twice, for two different inputs (e.g. NRFA and HDE), then the data will be imported from both sources and a warning message is produced.
#'
#' @return A tibble containing daily flow data for the specified sites, with the following columns: `input` ("HDE", "NRFA" or "FLOWFILES"), `flow_site_id`, `date`, `flow` and `quality`.
#'
#' @export
#'
#' @examples
#' # Import data for selected sites and dates
#' # import_flow(sites = c("F1707", "1001", "0130TH"),
#' #            inputs = c("HDE", "NRFA", "FLOWFILES"),
#' #            start_date = "2010-01-01",
#' #            end_date = "2010-01-05",
#' #            dir = "data/wiski",
#' #            skip_num = 21,
#' #            col_order = c(1,2,3))
#'
#' # If no data to be imported from local files, then 'dir', 'skip_num', 'col_order' and date_format do not need to be specified
#' # import_flow(sites = c("F1707", "1001"),
#' #           inputs = c("HDE", "NRFA"),
#' #            start_date = "2010-01-01",
#' #            end_date = "2010-01-05")
#' #
#' # Duplicate sites-inputs combinations are dropped.
#' # import_flow(sites = c("F1707", "1001", "1001"),
#' #            inputs = c("HDE", "NRFA", "NRFA"),
#' #            start_date = "2010-01-01",
#' #            end_date = "2010-01-05")



import_flow <- function(sites, inputs, start_date = "1985-01-01", end_date = Sys.Date(), dir = NULL, skip_num = NULL, col_order = NULL, date_format = "dmy"){

  # stop if sites are not defined
  if(missing(sites)) {
    stop("At least one site needs to be defined")
  }

  # stop if inputs are not defined
  if(missing(inputs)) {
    stop("'inputs' need to be defined")
  }

  # stop if sites is not a character
  if(is.character(sites) == FALSE) {
    stop("'sites' is in invalid format, should be a character")
  }

  # stop if inputs is not a character
  if(is.character(inputs) == FALSE) {
    stop("'inputs' is in invalid format, should be a character")
  }

  # stop if length of sites is unequal to length of inputs
  if(length(sites) != length(inputs)){
    stop ("The number of sites and inputs should be the same")
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
  if(start_date>Sys.Date()){
    stop("Start date given is in the future")
  }

  # stop if end data provided is in the future
  if(end_date>Sys.Date()){
    stop("End date given is in the future")
  }

  # stop if end date provided is before start date
  if(end_date <= start_date){
    stop ("End date is before or equal to the start date")
  }

  # stop if inputs contains invalid text
  names <- c("NRFA", "HDE", "FLOWFILES")
  match <- inputs %in% names
  if(sjmisc::str_contains(match, "FALSE")){
    stop("'inputs' contains character string that isn't one of: 'NRFA', 'HDE' or 'FLOWFILES'")
  }

  # stop if flowfiles specified and directory is not specified
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (is.null(dir) == TRUE)) {
    stop("'FLOWFILES' specified in 'inputs' but directory not given")
  }

  # stop if flowfiles specified and directory does not exist
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (file.exists(dir) == FALSE)) {
    stop("'FLOWFILES' specified in 'inputs' but 'dir' does not exist")
  }

  # stop if flowfiles specified and skip number is not defined
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (is.null(skip_num) == TRUE)) {
    stop("'FLOWFILES' specified in 'inputs' but skip_num is not given")
  }

  #stop if flowfiles specified and skip number is in invalid format
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (is.numeric(skip_num) == FALSE)) {
    stop("'skip_num' is in invalid format, should be numeric")
  }

  # stop if flowfiles specified and more than one skip number is defined
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (length(skip_num) >1)) {
    stop("Where being used only one skip number can be defined")
  }

  # stop if flowfiles specified and column order is not defined
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (is.null(col_order) == TRUE)) {
    stop("'FLOWFILES' specified in 'inputs' but col_order not given")
  }

  # stop if flowfiles specified and column order is in invalid format
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (is.numeric(col_order) == FALSE)) {
    stop("Flowfiles specified but col_order is in invalid format, should be numeric")
  }

  # stop if flowfiles specified and column order is wrong length
  if(sjmisc::str_contains(inputs, "FLOWFILES") && (length(col_order) !=3)) {
    stop("Where being used the order of three columns needs to be defined")
  }

  # drop rows where sites or inputs is NA
  sites_inputs <- data.frame(cbind(sites,inputs))
  sites_inputs <- sites_inputs[complete.cases(sites_inputs), ]
  nrows <- dim(sites_inputs)[1]

  # check for duplicate site-inputs records, and print warning if there is
  sites_inputs2 <- dplyr::distinct(sites_inputs)
  nrows2 <- dim(sites_inputs2)[1]
  if(nrows2 < nrows){
    warning(print(paste0("Warning: ",nrows - nrows2," duplicate sites-inputs combination(s) identified and dropped")))
    }

  # check for duplicate site ids, then print list of any found
  dup_sites <- sites_inputs2 %>%
              dplyr::mutate(n = 1) %>%
              tidyr::pivot_wider(names_from = "inputs", values_from = "n")

  ncols <- dim(dup_sites)[2]

  dup_sites <- dup_sites %>%
    dplyr::mutate(TOTAL = rowSums(.[2:ncols], na.rm = TRUE)) %>%
    dplyr::filter(TOTAL > 1)

  if(dim(dup_sites)[1] > 0){
    warning(print(paste0("Warning: data for the following sites will be imported from more than one source:")))
    print(dup_sites)
    }


  # split list of sites into three, depending on inputs
  sites_hde <- subset(sites_inputs2, inputs == "HDE", select = sites)
  sites_nrfa <- subset(sites_inputs2, inputs == "NRFA", select = sites)
  sites_flowfiles <- subset(sites_inputs2, inputs == "FLOWFILES", select = sites)

  # import data for HDE sites
  if(dim(sites_hde)[1] > 0){
    data_hde <- import_hde(sites = sites_hde$sites,
                           start_date = start_date,
                           end_date = end_date)
    data_hde$input <- "HDE"
  } else {
    data_hde <- data.frame()
  }

  # import data for NRFA sites
  if(dim(sites_nrfa)[1] > 0){
    data_nrfa <- import_nrfa(sites = sites_nrfa$sites,
                            start_date = start_date,
                            end_date = end_date)
    data_nrfa$input <- "NRFA"
  } else {
    data_nrfa <- data.frame()
  }

  # import data from local files
  if(dim(sites_flowfiles)[1] > 0){
    data_flowfiles <- import_flowfiles(sites = sites_flowfiles$sites,
                                      start_date = start_date,
                                      end_date = end_date,
                                      dir = dir,
                                      skip_num = skip_num,
                                      col_order = col_order,
                                      date_format = date_format)
    data_flowfiles$input <- "FLOWFILES"
  } else {
    data_flowfiles <- data.frame()
  }


  # bind data together
  data_all <- rbind(data_hde, data_nrfa, data_flowfiles)

  sites_with_data <- sites[sites %in% data_all$flow_site_id]
  stations_cant_find <- sites[!(sites %in% data_all$flow_site_id)]

  # stop if none of the user defined sites are present in the station list
  if(length(sites_with_data) == 0 ) {
  stop("Cant find any data for the defined sites within the specified date range")
  } else if(length(stations_cant_find) > 0 ) {
   warning(print(paste('No data found for the date range given for the following sites: ', stations_cant_find)))
  }

  # sort data by site, then date
  data_all <- data_all %>%
    dplyr::arrange(flow_site_id, date)

  # reorder columns so that input is moved from last to first
  data_all <- data_all[, c(5,1,2,3,4)]

  # form site id as character
  data_all$flow_site_id <- as.character(data_all$flow_site_id)

  return (tibble::as_tibble(data_all))

}



