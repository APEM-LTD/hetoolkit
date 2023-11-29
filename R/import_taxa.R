#' Importing macroinvertebrate abundance data from the EA Ecology and Fish Data Explorer
#'
#' @description
#' The import_taxa function imports macroinvertebrate taxon abundance information from the Environment Agency's Ecology and Fish Data Explorer (EDE). The function downloads and merges two files: one listing taxon abundances by sample, and one with details of each taxon (e.g. full taxon name, taxonomic rank). The data can be optionally filtered by site and/or sample ID, and the filtered data saved as a .rds file.
#'
#' @usage
#' import_taxa(sites = NULL,
#'             samples = NULL,
#'             save = FALSE)
#'
#' @param sites Vector of site ids to filter by. Default = NULL
#' @param samples Vector of sample ids to filter by. Default = NULL
#' @param save Specifies whether (`TRUE`) or not (`FALSE`) the filtered data should be saved as an rds file (for future use, or audit trail). Default = `FALSE`.
#'
#' @details
#' If saving a copy of the filtered taxa data, the name of the rds file is hard-wired to `INV_OPEN_DATA_TAXA_F.RDS`.
#'
#' Downloaded raw data files will be automatically removed from the working directory following completed execution of the function.
#'
#' The function automatically modifies the output from EDE, renaming "SITE_ID" to "biol_site_id" (`hetoolkit`'s standardised column header for biology site ids).
#'
#' Please note that that approaches to sample processing, such as abundance recording, have changed over time. For details, please see page 5 of \href{https://environment.data.gov.uk/api/file/download?fileDataSetId=66c79b9a-c855-4b65-a894-0668bbbd1843&fileName=Freshwater_river_macroinvertebrate_surveys_(Biosys).pdf}{this pdf document}
#'
#' @return A tibble containing the imported macroinvertebrate taxonomic abundance data.
#'
#' @export
#'
#' @examples
#'
#' # Import data from EDE for specific sites
#' taxa_data <- import_taxa(sites = c("34310", "34343"))
#'
#' # Import data and match on other macroinvertebrate data
#' biol_data <- hetoolkit::import_inv(sites = c("34310", "34343"))
#' sample_IDs <- unique(biol_data$SAMPLE_ID)
#'
#' taxa_data <- import_taxa(samples = sample_IDs)
#'
#' full_data <- taxa_data %>%
#'   dplyr::left_join(biol_data)
#'


import_taxa <- function(sites = NULL,
                        samples = NULL,
                        save = FALSE){

  ## Errors and warnings

  if(is.null(sites) == FALSE && is.vector(sites) == FALSE) {
      stop("If specified, sites must be a character vector")
  }

  if(is.null(sites) == FALSE && length(sites) == 0) {
    stop("No sites specified in input")
  }

  if(is.null(sites) == FALSE && is.character(sites) == FALSE) {
    stop("If specified, sites must be a character vector")
  }

  if(is.null(samples) == FALSE && is.vector(samples) == FALSE) {
    stop("If specified, samples must be a character vector")
  }

  if(is.null(samples) == FALSE && length(samples) == 0) {
    stop("No samples specified in input")
  }

  if(is.null(samples) == FALSE && is.character(samples) == FALSE) {
    stop("If specified, samples must be a character vector")
  }

  if(is.logical(save) == FALSE) {
    stop("save is not logical")
  }

  if(any(is.na(sites)) == TRUE) {
    warning("sites contains NAs")
  }

  if(any(is.na(samples)) == TRUE) {
    warning("samples contains NAs")
  }


  ## Download abundance data
  downloader::download("https://environment.data.gov.uk/ecology-fish/downloads/INV_OPEN_DATA_TAXA.parquet",
                       destfile = 'INV_OPEN_DATA_TAXA.parquet',
                       mode = 'wb')

  col_types <- readr::cols(
    REPLICATE_CODE = readr::col_character()
  )

  # read parquet
  inv_abunds <- arrow::read_parquet("INV_OPEN_DATA_TAXA.parquet",
                                     col_select = NULL,
                                     as_data_frame = TRUE)

  ## Filter abundance data
  if(is.null(sites) == FALSE) {
    inv_abunds_f <- inv_abunds %>%
      dplyr::filter(SITE_ID %in% sites)
  }

  if(is.null(samples) == FALSE) {
    inv_abunds_f <- inv_abunds %>%
      dplyr::filter(SAMPLE_ID %in% samples)
  }

  ## Warning if no rows remain in dataset
  if(nrow(inv_abunds_f) == 0){
    warning("No rows in output dataset")
  }

  ## Download taxa information
  downloader::download("https://environment.data.gov.uk/ecology-fish/downloads/OPEN_DATA_TAXON_INFO.zip",
                       dest = "OPEN_DATA_TAXON_INFO.zip", mode="wb")

  # col_types <- readr::cols(
  #   REPLICATE_CODE = readr::col_character()
  # )

  utils::unzip ("OPEN_DATA_TAXON_INFO.zip")

  # read csv
  inv_taxa <- readr::read_csv("OPEN_DATA_TAXON_INFO.csv",
                              col_types = col_types)

  ## Merge files
  inv_taxa_data <- inv_abunds_f %>%
    dplyr::left_join(inv_taxa, by = "TAXON_LIST_ITEM_KEY")

  ## Format data
  inv_taxa_data <- inv_taxa_data %>%
    dplyr::mutate(SITE_ID = as.character(SITE_ID),
                  SAMPLE_ID = as.character(SAMPLE_ID),
                  SAMPLE_TYPE = factor(SAMPLE_TYPE),
                  SAMPLE_METHOD = factor(SAMPLE_METHOD),
                  ANALYSIS_ID = as.integer(ANALYSIS_ID),
                  ANALYSIS_TYPE = factor(ANALYSIS_TYPE),
                  ANALYSIS_METHOD = factor(ANALYSIS_METHOD),
                  WATERBODY_TYPE = factor(WATERBODY_TYPE),
                  TAXON_NAME = factor(TAXON_NAME),
                  TAXON_VERSION_KEY = factor(TAXON_VERSION_KEY),
                  PREFERRED_TAXON_NAME = factor(PREFERRED_TAXON_NAME),
                  PARENT_TAXON_NAME = factor(PARENT_TAXON_NAME),
                  TAXON_TYPE = factor(TAXON_TYPE),
                  TAXON_GROUP_NAME = factor(TAXON_GROUP_NAME),
                  NON_NATIVE_SP = factor(NON_NATIVE_SP),
                  PROTECTED_TAXA = factor(PROTECTED_TAXA)) %>%
    dplyr::rename(biol_site_id = SITE_ID)

  # Identify missing sites
  if(is.null(sites) == FALSE){
    a <- unique(inv_taxa_data$biol_site_id)
    b <- unique(as.character(sites))
    c <- b[!b %in% a]
    if(isFALSE(length(c) == 0)) {warning(paste0("Biology Site Not Found:", c, "\n"))}
  }

  # Identify missing samples
  if(is.null(samples) == FALSE){
    d <- unique(inv_taxa_data$SAMPLE_ID)
    e <- unique(as.character(samples))
    f <- e[!e %in% d]
    if(isFALSE(length(f) == 0)) {warning(paste0("Sample ID Not Found:", f, "\n"))}
  }

  ## Save a copy to disk in .rds format if required
  if(save == TRUE) {saveRDS(wq_metrics, paste0(getwd(), "/INV_OPEN_DATA_TAXA_F.RDS"))}

  ## Delete downloaded files
  file.remove("INV_OPEN_DATA_TAXA.parquet")
  file.remove("OPEN_DATA_TAXON_INFO.zip")

  # RETURN
  return(tibble::as_tibble(inv_taxa_data))

  ### FUNCTION END

}
