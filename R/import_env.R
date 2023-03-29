#' Import environmental base data
#'
#' @description
#' The `import_env` function imports environmental base data for macroinvertebrate sampling sites from the Environment Agency's Ecology and Fish Data Explorer. The data can either be downloaded from https://environment.data.gov.uk/ecology-fish/downloads/INV_OPEN_DATA.zip or read in from a local csv or rds file. The data can be optionally filtered by site ID.
#'
#' @usage
#' import_env(env_dir = NULL, sites = NULL, save = FALSE, save_dwnld = FALSE, save_dir = getwd())
#'
#' @param env_dir Path to local csv or rds file containing environmental data. If NULL (default), then data is downloaded from data.gov.uk.
#' @param sites Vector of site ids to filter on.
#' @param save Specifies if imported environmental data should be saved
#'  as rds file (for future use); Default = FALSE.
#' @param save_dir Path to folder where imported environmental data is to be saved;
#'  Default = Current working directory.
#' @param save_dwnld Specifies if downloaded biology data should be saved.
#'
#' @details If saving a copy of the downloaded data, the name of the rds file is hard-wired to: INV_OPEN_DATA_SITES_ALL.rds. If saving after filtering on site, the name of the rds file is hard-wired to: INV_OPEN_DATA_SITE_F.rds.
#'
#'  Downloaded raw data files (in .csv and .zip format) will be automatically removed from the working directory following completed execution of the function.
#'
#'  The function will modify the output from EDE, renaming `SITE_ID` to `biol_site_id` (hetoolkit's standardised column header for biology sites).
#'
#' @return Tibble containing environmental data
#'
#' @export
#'
#' @examples
#' # Download data for all sites and save as .rds file for future use:
#'   # import_env(save_dwnld = TRUE, save_dir = "mydata")
#'
#' # Read in local .rds file and filter on selected sites:
#'   # import_env(env_dir = "mydata/INV_OPEN_DATA_SITES_ALL.rds",
#'   #                sites = c("34310", "34343"))
#'
#' # Read in local .csv file and filter on selected sites:
#'   # import_env(env_dir = "mydata/INV_OPEN_DATA_SITES_ALL.csv",
#'   #               sites = c("34310", "34343"))



import_env <- function(env_dir = NULL,
                       sites = NULL,
                       save = FALSE,
                       save_dwnld = FALSE,
                       save_dir = getwd()){

  # Errors
  if(is.null(sites) == FALSE && is.vector(sites) == FALSE)
    {stop("If specified, sites must be a vector")}
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}
  if(is.logical(save_dwnld) == FALSE) {stop("Save_dwnld is not logical")}
  if(is.null(env_dir) == FALSE && file.exists(env_dir) == FALSE)
  {stop("Specified file directory does not exist")}


  if(is.null(env_dir) == TRUE) {

    # Download envrionmental data from EDE
    downloader::download("https://environment.data.gov.uk/ecology-fish/downloads/INV_OPEN_DATA_SITE.csv.gz",
                         dest = "INV_OPEN_DATA_SITE.csv.gz", mode="wb")

    # read csv convert to integers, dates, factors
    inv_sites <- readr::read_csv("INV_OPEN_DATA_SITE.csv.gz",
                                 guess_max = 50000, col_types = readr::cols(
                                   SITE_ID = readr::col_character(),
                                   SITE_VERSION = readr::col_integer(),
                                   AGENCY_AREA = readr::col_character(),
                                   CATCHMENT = readr::col_character(),
                                   WATERBODY_TYPE = readr::col_character(),
                                   WATERBODY_TYPE_DESCRIPTION = readr::col_character(),
                                   WATER_BODY = readr::col_character(),
                                   NGR_PREFIX = readr::col_character(),
                                   EASTING = readr::col_character(),
                                   NORTHING = readr::col_character(),
                                   NGR_10_FIG = readr::col_character(),
                                   FULL_EASTING = readr::col_integer(),
                                   FULL_NORTHING = readr::col_integer(),
                                   WFD_WATERBODY_ID = readr::col_character(),
                                   BASE_DATA_DATE = readr::col_date(format = "%d/%m/%Y"),
                                   MIN_SAMPLE_DATE = readr::col_date(format = "%d/%m/%Y"),
                                   MAX_SAMPLE_DATE = readr::col_date(format = "%d/%m/%Y"),
                                   ECN_SITE_INV = readr::col_logical(),
                                   COUNT_OF_SAMPLES = readr::col_integer()
                                 ))


      if(isTRUE(save_dwnld) == TRUE){

      saveRDS(inv_sites, paste0(save_dir,
                                  "/INV_OPEN_DATA_SITES_ALL.rds"))

    }

  }

    if(is.null(env_dir) == FALSE && grepl("csv", env_dir) == TRUE){

    if(file.exists(env_dir) == FALSE)
      {stop("Specified file directory does not exist")}

    inv_sites <- readr::read_csv(env_dir,
                                 guess_max = 50000, col_types = readr::cols(
                                   SITE_ID = readr::col_character(),
                                   SITE_VERSION = readr::col_integer(),
                                   AGENCY_AREA = readr::col_character(),
                                   CATCHMENT = readr::col_character(),
                                   WATERBODY_TYPE = readr::col_character(),
                                   WATERBODY_TYPE_DESCRIPTION = readr::col_character(),
                                   WATER_BODY = readr::col_character(),
                                   NGR_PREFIX = readr::col_character(),
                                   EASTING = readr::col_character(),
                                   NORTHING = readr::col_character(),
                                   NGR_10_FIG = readr::col_character(),
                                   FULL_EASTING = readr::col_integer(),
                                   FULL_NORTHING = readr::col_integer(),
                                   WFD_WATERBODY_ID = readr::col_character(),
                                   BASE_DATA_DATE = readr::col_date(format = "%d/%m/%Y"),
                                   MIN_SAMPLE_DATE = readr::col_date(format = "%d/%m/%Y"),
                                   MAX_SAMPLE_DATE = readr::col_date(format = "%d/%m/%Y"),
                                   ECN_SITE_INV = readr::col_logical(),
                                   COUNT_OF_SAMPLES = readr::col_integer()
                                 ))

    }


    if(is.null(env_dir) == FALSE && grepl("rds", env_dir) == TRUE){

    if(file.exists(env_dir) == FALSE) {stop("Specified file directory does not exist")}

    inv_sites <- readr::read_rds(env_dir)
    }

  if(is.null(sites) == FALSE) {

  # Filter by sites (vector of specified Site IDs)
  inv_sites_1 <- dplyr::filter(inv_sites, SITE_ID %in% sites)

  # Identify missing sites
  a <- unique(inv_sites_1$SITE_ID)
  b <- unique(as.character(sites))
  c <- b[!b %in% a]
  if(isFALSE(length(c) == 0)) {warning(paste0("Biology Site Not Found:", c))}

  # save copy to disk in rds format if needed
  if(save == TRUE){saveRDS(inv_sites_1, paste0(save_dir, '/INV_OPEN_DATA_SITE_F.rds'))}

  # remove zip file, if downloaded
  if(is.null(env_dir)==TRUE){
      file.remove("INV_OPEN_DATA_SITE.csv.gz")
  }

  inv_sites_1 <- inv_sites_1 %>% dplyr::rename(biol_site_id = SITE_ID)

  return(tibble::as_tibble(inv_sites_1))

  }

}
