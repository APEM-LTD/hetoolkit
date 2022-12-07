#' Import macroinvertebrate data in Ecology Data Explorer (EDE) format
#'
#' @description
#' The `import_inv` function imports macroinvertebrate sampling data from the Environment Agency's Ecology and Fish Data Explorer. The data can either be downloaded from <https://environment.data.gov.uk/ecology-fish/downloads/INV_OPEN_DATA.zip> or read in from a local csv or rds file. The data can be optionally filtered by site ID and sample date.
#'
#' @usage
#' import_inv(source = "parquet", sites = NULL, start_date = NULL, end_date = NULL, save = FALSE, save_dwnld = FALSE, save_dir = getwd()biol_dir = NULL)
#'
#' @param source File format for the download from EDE OR path to local .csv or .rds file containing biology data. Default = "parquet".
#' @param sites Vector of site ids to filter by.
#' @param start_date Start date for data extraction (YYYY-MM-DD format). Default = NULL.
#' @param end_date End date for data extraction (YYYY-MM-DD format). Default = NULL.
#' @param save Specifies if filtered biology data should be saved as rds file (for future use). Default = FALSE.
#' @param save_dir Path to folder where biology data is to be saved. Default = Current working directory.
#' @param save_dwnld Specifies whether or not downloaded biology data should be saved. Default = FALSE.
#' @param biol_dir Deprecated. Path to local .csv or .rds file containing biology data; or NULL to download directly from EDE. Default = NULL.
#'
#' @details
#' If saving a copy of the downloaded data, the name of the rds file is hard-wired to: INV_OPEN_DATA_METRICS_ALL.RDS. If saving after filtering on site or date, the name of the rds file is hard-wired to: INV_OPEN_DATA_METRICS_F.RDS.
#'
#'  Downloaded raw data files (in .csv and .parquet format) will be automatically removed from the working directory following completed execution of the function.
#'
#'  The function will modify the output from EDE, renaming "SITE_ID" as "biol_site_id" (standardised column header for biology sites).
#'
#' @return Tibble containing biology data
#'
#' @export
#'
#' @examples
#' # Download data for all sites and save as .rds file for future use:
#' # import_inv(save_dwnld = TRUE, save_dir = "mydata")
#'
#' # Read in local .rds file and filter on selected sites and dates:
#' # import_inv(biol_dir = "mydata/INV_OPEN_DATA_METRICS_ALL.rds",
#' #                  sites = c("34310", "34343"),
#' #                  start_date = "1995-01-01",
#' #                  end_date = Sys.Date())
#'
#' # Read in local .csv file, filter on selected sites, and save results as rds file:
#' # import_inv(biol_dir = "mydata/INV_OPEN_DATA_METRICS.csv",
#' #                  sites = c("34310", "34343"),
#' #                  save = TRUE)


import_inv <- function(source = "parquet",
                           sites = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           save = FALSE,
                           save_dwnld = FALSE,
                           save_dir = getwd(),
                           biol_dir = NULL)

  # Errors
  if(is.null(sites) == FALSE && is.vector(sites) == FALSE)
    {stop("If specified, sites must be a vector")}
  if(is.null(start_date) == FALSE && IsDate(start_date, "%Y-%m-%d") == FALSE)
    {stop("Date should be in YYYY-MM-DD format")}
  if(is.null(end_date) == FALSE && IsDate(end_date, "%Y-%m-%d") == FALSE)
    {stop("Date should be in YYYY-MM-DD format")}
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}
  if(is.logical(save_dwnld) == FALSE) {stop("Save_dwnld is not logical")}
  if(is.null(source) == FALSE && source %in% c("parquet", "csv") == FALSE && grepl(source,"\\.csv$|\\.rds$") == FALSE)
    {stop("Download format must be parquet or csv, or a valid filepath must be specified (.csv or .rds)")}

  if(is.null(source) == FALSE) {

    if(source == "parquet") {
      # Download biology data from EDE
      downloader::download("https://environment.data.gov.uk/ecology-fish/downloads/INV_OPEN_DATA_METRICS.parquet",
                           destfile = 'INV_OPEN_DATA_SITE.parquet',
                           mode = 'wb')

      col_types <- readr::cols(
        REPLICATE_CODE = readr::col_character()
      )

      # read parquet
      inv_metrics <- arrow::read_parquet("INV_OPEN_DATA_SITE.parquet",
                                         col_select = NULL,
                                         as_data_frame = TRUE)

      # Optional download
      if(isTRUE(save_dwnld) == TRUE){

        saveRDS(inv_metrics, paste0(save_dir,
                                    "/INV_OPEN_DATA_METRICS_ALL.rds"))

      }
    }

    if(source == "csv") {

      # Download biology data from EDE
      downloader::download("https://environment.data.gov.uk/ecology-fish/downloads/INV_OPEN_DATA_METRICS.csv.gz",
                           dest = "INV_OPEN_DATA_METRICS.csv.gz", mode="wb")

      col_types <- readr::cols(
        REPLICATE_CODE = readr::col_character()
      )

      # readcsv
      inv_metrics <- readr::read_csv("INV_OPEN_DATA_METRICS.csv.gz",
                                     col_types = col_types)

      # Optional download
      if(isTRUE(save_dwnld) == TRUE){

        saveRDS(inv_metrics, paste0(save_dir,
                                    "/INV_OPEN_DATA_METRICS_ALL.rds"))

      }

    }

  #}

  # Read-in csv option
  if((is.null(biol_dir) == FALSE && grepl("csv", biol_dir) == TRUE) || (grepl(".csv", source) == TRUE)) {

    if(file.exists(source) == FALSE || file.exists(biol_dir) == FALSE)
      {stop("Specified file directory does not exist")}

    # readcsv
    inv_metrics <- readr::read_csv(source)
    inv_metrics <- readr::read_csv(biol_dir)


  }

    # Read-in rds option
  if((is.null(biol_dir) == FALSE && grepl("rds", biol_dir) == TRUE) || (grepl("rds", source) == TRUE)) {

    if(file.exists(source) == FALSE || file.exists(biol_dir) == FALSE)
      {stop("Specified file directory does not exist")}

    # readcsv
    inv_metrics <- readr::read_rds(source)
    inv_metrics <- readr::read_rds(biol_dir)


    }

  if(is.null(sites) == FALSE) {

  # convert to integers, dates, factors
  inv_metrics_f <- inv_metrics %>% dplyr::mutate(
    SITE_ID = as.character(SITE_ID),
    SAMPLE_ID = as.character(SAMPLE_ID),
    SAMPLE_VERSION = as.integer(SAMPLE_VERSION),
    #  REPLICATE_CODE = col_character(),
    SAMPLE_TYPE = factor(SAMPLE_TYPE),
    SAMPLE_TYPE_DESCRIPTION = factor(SAMPLE_TYPE_DESCRIPTION),
    SAMPLE_METHOD = factor(SAMPLE_METHOD),
    SAMPLE_METHOD_DESCRIPTION = factor(SAMPLE_METHOD_DESCRIPTION),
    SAMPLE_REASON = factor(SAMPLE_REASON),
    ANALYSIS_ID = as.integer(ANALYSIS_ID),
    ANALYSIS_TYPE = factor(ANALYSIS_TYPE),
    ANALYSIS_TYPE_DESCRIPTION = factor(ANALYSIS_TYPE_DESCRIPTION),
    ANALYSIS_METHOD = factor(ANALYSIS_METHOD),
    ANALYSIS_METHOD_DESCRIPTION = factor(ANALYSIS_METHOD_DESCRIPTION),
    IS_THIRD_PARTY_DATA = factor(IS_THIRD_PARTY_DATA),
    WATERBODY_TYPE = factor(WATERBODY_TYPE)
  )

  # convert to date (skip for parquet)
  if(source != "parquet") {
    inv_metrics_f <- inv_metrics %>%
      dplyr::mutate(SAMPLE_DATE = lubridate::dmy(SAMPLE_DATE),
                    DATE_OF_ANALYSIS = lubridate::dmy(DATE_OF_ANALYSIS))
  }

  # Filter by sites (vector of specified Site IDs)
  inv_metrics_f1 <- dplyr::filter(inv_metrics_f, SITE_ID %in% sites)

  # Identify missing sites
  a <- unique(inv_metrics_f1$SITE_ID)
  b <- unique(as.character(sites))
  c <- b[!b %in% a]
  if(isFALSE(length(c) == 0)) {warning(paste0("Biology Site Not Found:", c))}

  if(is.null(start_date) == FALSE) {

  # Convert to tbl_time
  inv_metrics_f1 <- tibbletime::as_tbl_time(inv_metrics_f1, index = SAMPLE_DATE)

  # Filter by start_date and end_date
  inv_metrics_f1 <- dplyr::filter(inv_metrics_f1, SAMPLE_DATE >= start_date)

  }

  if(is.null(end_date) == FALSE) {


    # Filter by start_date and end_date
    inv_metrics_f1 <- dplyr::filter(inv_metrics_f1, SAMPLE_DATE <= end_date)

  }

  # save copy to disk in rds format if needed
  if (save == TRUE) {saveRDS(inv_metrics_f1, paste0(save_dir, "/INV_OPEN_DATA_METRICS_F.rds"))}

  if(source == "parquet") {file.remove("INV_OPEN_DATA_SITE.parquet")}
  if(source == "csv") {file.remove("INV_OPEN_DATA_METRICS.csv.gz")}

  inv_metrics_f1 <- inv_metrics_f1 %>% dplyr::rename(biol_site_id = SITE_ID)

  # create Season and Year Columns
  inv_metrics_f1$Month <- lubridate::month(inv_metrics_f1$SAMPLE_DATE)
  inv_metrics_f1$Year <- lubridate::year(inv_metrics_f1$SAMPLE_DATE)
  inv_metrics_f1$Season <- ifelse((inv_metrics_f1$Month >= 3) & (inv_metrics_f1$Month <= 5), "Spring",
                             ifelse((inv_metrics_f1$Month >= 6) & (inv_metrics_f1$Month <= 8), "Summer",
                                    ifelse((inv_metrics_f1$Month >= 9) & (inv_metrics_f1$Month <= 11), "Autumn", "Winter")))

  return(tibble::as_tibble(inv_metrics_f1))

  }

}
