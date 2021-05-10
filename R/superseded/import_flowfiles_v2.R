#' Import flow data from local files
#'
#' @description
#' This function imports flow data from one or more local files, one per site, named in the format 'siteID.filextension'. Supported file formats are: csv, txt, all, xls and xlsx.
#'
#' @usage
#' import_flowfiles(sites = NULL, dir, skip_num, col_order, start_date = "1985-01-01", end_date = Sys.Date())
#'
#' @param sites Vector of site IDs (= file names, without file extension). Default = NULL (import all files (of supported formats) in dir).
#' @param dir Path to folder containing flow files.
#' @param skip_num Number of rows (including any column headers) to skip before starting to read data. As an example, if the data has three rows of metadata, then a row of column headers, set skip_num = 4.
#' @param col_order Define numbers of columns containing the date, flow and quality data. If no quality data, the third element can be set to NA.
#' @param start_date Start date for flow data extraction (YYYY-MM-DD format). Default = 1985-01-01.
#' @param end_date End date for flow data extraction (YYYY-MM-DD format). Default = today's date.
#'
#' @details
#' #' All files must be stored in the same directory and must have the same structure (i.e. the same number of header rows, and with the date, flow and quality columns in the same position). A mix of different file formats is allowed. Only the first worksheet is imported from xlsx files.
#'
#' If 'sites' is not NULL, then the function uses the information in 'sites' to search 'dir' for all possible files (e.g. 0130TH.csv, 0130TH.txt, 0130TH.all, 0130TH.xls and 0130TH.xlsx). If 'sites' is not NULL, then all csv, txt, all, xls and xlsx files in 'dir' are imported. All other folders and file types are ignored.
#'
#' If a site has two files in different formats (e.g. 0130TH.csv and 0130TH.all), then the data will be imported from both files. Be aware that this could result in duplicate records in the output file.
#'
#' The function initially imports flow data for all dates in the flow file(s) and then filters out records that are before start_date or after end_date. If the data does not span the entire range of dates provided, additional records are created and, the flow and quality values defined as NA on these dates.
#'
#' If a site ID is duplicated in the 'sites' argument, that site is only searched for once in the data and a warning message is produced.
#'
#' @return A tibble containing flow data for the specified sites/files, with the following columns: flow-site_id, date, flow and (if available) quality.
#'
#' @examples
#' # Import data for selected sites and dates
#' import_flowfiles(sites = c("0130TH", "033006"), dir = "data/wiski", col_order = c(1,2,3), skip_num = 21, start_date = "2010-01-01", end_date = "2010-01-05")
#'
#' # Returns flow = NA if site exists but no data available for the specified date range
#' import_flowfiles(sites = c("0130TH", "033006"), dir = "data/wiski", col_order = c(1,2,3), skip_num = 21, start_date = "1900-01-01", end_date = "1900-01-05")



import_flowfiles <- function(sites = NULL, dir, skip_num, col_order, date_format = "dmy", start_date = "1985-01-01", end_date = Sys.Date()){

  # remove any duplicate site ids
  if(is.null(sites) == FALSE){
    original_sites <- length(sites)
    sites <- unique(sites)
      if(original_sites > length(sites)){
        removed_sites <- (original_sites-length(sites))
        warning(print(paste('Warning: ', removed_sites, ' duplicate site identifications detected and ignored', sep="")))
      }
  }

  # stop if file directory does not exist
    if(file.exists(dir) == FALSE) {stop("Specified directory does not exist")}

  # stop if the number of rows to be skipped is not defined
    if(missing(skip_num)) {
    stop("The number of rows to be skipped before reading the data needs to be defined")
    }

  # stop if the data in column one is not defined
    if(missing(col_order)) {
      stop("The columns containing the data of interest need to be defined")
    }

  # stop if col_order doesn't have three elements
   if(length(col_order) != 3){
    stop ("'col_order' must have three elements")
   }

  # stop if col_order has NA in first two elements
  if(is.na(col_order[1]) || is.na(col_order[2])){
    stop ("first two elements of 'col_order' can't be NA")
  }

  #stop if start date is provided in incorrect format
    if(IsDate(start_date, "%Y-%m-%d") == FALSE){
      stop("Date should be in YYYY-MM-DD format")
      }

  #stop if end date is provided in incorrect format
    if(IsDate(end_date, "%Y-%m-%d") == FALSE){
      stop("Date should be in YYYY-MM-DD format")
      }

  #stop if start date provided is in the future
    if(start_date>Sys.Date()){
      stop("Start date given is in the future")
      }

  # stop if end data provided is in the future
    if(end_date>Sys.Date()){
      stop("End date given is in the future")
      }

  # stop is end date provided is before start date
    if(end_date <= start_date){
      stop ("End date is before or equal to start date")
      }

  # shorten col_order if necessary
    if(is.na(col_order[3])){
      col_order <- col_order[1:2]
      }

  # generate list of files to process
    all_files <- list.files(path = dir)

    if(is.null(sites) == TRUE) {
      all_exts <- tools::file_ext(all_files)
      files_list <- all_files[all_exts %in% c("csv","xls","xlsx","txt","all")]
      sites <- tools::file_path_sans_ext(files_list)
    } else {
      poss_files <- expand.grid(name = sites, extension = c("csv","xls","xlsx","txt","all"))
      poss_files <- paste0(poss_files$name, "." , poss_files$extension)
      files_list <- poss_files[poss_files %in% all_files]
    }


    i <- 0

  # Clear tibble for each run of the function
    datalist = list()


  # Read in files
    for (item in files_list) {

        i <- i + 1
        file_name <- tools::file_path_sans_ext(item)
        file_ext <- tools::file_ext(item)
        flow_file <- data.frame()

        if(file_ext=="csv") {
          flow_file <- readr::read_csv(paste0(dir,"/",item), skip = skip_num, col_names = FALSE, col_types = cols(.default = "c"))[,col_order]
          colnames(flow_file) <- c("X1", "X2", "X3")
          flow_file$Station <- file_name
        }

        if(file_ext=="txt"){
          flow_file <- readr::read_tsv(paste0(dir,"/",item), skip = skip_num, col_names = FALSE, col_types = cols(.default = "c"))[,col_order]
          colnames(flow_file) <- c("X1", "X2", "X3")
          flow_file$Station <- file_name
        }

        if(file_ext=="all"){
          flow_file <- readr::read_delim(paste0(dir,"/", item), skip = skip_num, delim = c(","), col_names = FALSE, col_types = cols(.default = "c"))[,col_order]
          colnames(flow_file) <- c("X1", "X2", "X3")
          flow_file$Station <- file_name
        }

        if(file_ext=="xlsx" | file_ext=="xls"){
          flow_file <- readxl::read_excel(paste0(dir,"/",item), skip = skip_num, col_names = FALSE)[,col_order]
          colnames(flow_file) <- c("X1", "X2", "X3")
          flow_file$Station <- file_name
        }

    datalist[[i]] <- flow_file

    }

  # Join files to form final data set
    final_data <- do.call(rbind, datalist)

  # Re-name column headings in final dataset, and add quality column if required
    if(length(col_order)==2){
      colnames(final_data)<-c("date", "flow", "flow_site_id")
      final_data <- final_data[,c(3,1,2)]
      final_data$quality <- "NA"
    } else {
      colnames(final_data) <- c("date", "flow", "quality", "flow_site_id")
      final_data <- final_data[,c(4,1,2,3)]
    }

  # format date column

    #OLD: final_data$date <- anytime::anydate(final_data$date)

    if(date_format == "dmy")     {final_data$date <- lubridate::dmy(final_data$date)}
    if(date_format == "dmy_h")   {final_data$date <- lubridate::dmy_h(final_data$date)}
    if(date_format == "dmy_hm")  {final_data$date <- lubridate::dmy_hm(final_data$date)}
    if(date_format == "dmy_hms") {final_data$date <- lubridate::dmy_hms(final_data$date)}

    if(date_format == "mdy")     {final_data$date <- lubridate::mdy(final_data$date)}
    if(date_format == "mdy_h")   {final_data$date <- lubridate::mdy_h(final_data$date)}
    if(date_format == "mdy_hm")  {final_data$date <- lubridate::mdy_hm(final_data$date)}
    if(date_format == "mdy_hms") {final_data$date <- lubridate::mdy_hms(final_data$date)}

    if(date_format == "ymd")     {final_data$date <- lubridate::ymd(final_data$date)}
    if(date_format == "ymd_h")   {final_data$date <- lubridate::ymd_h(final_data$date)}
    if(date_format == "ymd_hm")  {final_data$date <- lubridate::ymd_hm(final_data$date)}
    if(date_format == "ymd_hms") {final_data$date <- lubridate::ymd_hms(final_data$date)}

    if(date_format == "ydm")     {final_data$date <- lubridate::ydm(final_data$date)}
    if(date_format == "ydm_h")   {final_data$date <- lubridate::ydm_h(final_data$date)}
    if(date_format == "ydm_hm")  {final_data$date <- lubridate::ydm_hm(final_data$date)}
    if(date_format == "ydm_hms") {final_data$date <- lubridate::ydm_hms(final_data$date)}

  # convert to date (not date-time)
    final_data$date <- lubridate::floor_date(final_data$date, unit = "day")

  # force flow to be numeric (text converted to NA)
    final_data$flow <- as.numeric(final_data$flow)

  # Filter data set by start and end date
    final_data <- final_data %>% filter(final_data$date >= start_date & final_data$date <= end_date)

  # Create dataset running from start_date to end_date for every site
    full_grid <- expand.grid(flow_site_id = sites, date = seq(from = lubridate::date(start_date), to = lubridate::date(end_date),by = 1))
    full_grid <- tibble::as_tibble(full_grid)

    complete_data <- dplyr::left_join(full_grid, final_data, by = c("flow_site_id", "date"))
    complete_data <- complete_data[order(complete_data$flow_site_id,complete_data$date),]

  # Format columns
    complete_data$date <- as.Date(complete_data$date)
    complete_data$flow <- as.numeric(complete_data$flow)

  return (tibble::as_tibble(complete_data))

}


# helper function to format dates
reformat_dates <- function(x, date_format){

  if(date_format == "dmy")     {y <- lubridate::dmy(x)}
  if(date_format == "dmy_h")   {y <- lubridate::dmy_h(x)}
  if(date_format == "dmy_hm")  {y <- lubridate::dmy_hm(x)}
  if(date_format == "dmy_hms") {y <- lubridate::dmy_hms(x)}

  if(date_format == "mdy")     {y <- lubridate::mdy(x)}
  if(date_format == "mdy_h")   {y <- lubridate::mdy_h(x)}
  if(date_format == "mdy_hm")  {y <- lubridate::mdy_hm(x)}
  if(date_format == "mdy_hms") {y <- lubridate::mdy_hms(x)}

  if(date_format == "ymd")     {y <- lubridate::ymd(x)}
  if(date_format == "ymd_h")   {y <- lubridate::ymd_h(x)}
  if(date_format == "ymd_hm")  {y <- lubridate::ymd_hm(x)}
  if(date_format == "ymd_hms") {y <- lubridate::ymd_hms(x)}

  if(date_format == "ydm")     {y <- lubridate::ydm(x)}
  if(date_format == "ydm_h")   {y <- lubridate::ydm_h(x)}
  if(date_format == "ydm_hm")  {y <- lubridate::ydm_hm(x)}
  if(date_format == "ydm_hms") {y <- lubridate::ydm_hms(x)}

  return(y)

}

