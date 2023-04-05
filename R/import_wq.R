#' Importing water quality data from the EA Water Quality Archive database
#'
#' @description
#' The `import_wq` function imports water quality (WQ) data from the Environment Agency’s Water Quality Archive database. The data can either be downloaded automatically in .csv format or read in from a previously saved .csv or .rds file. The data can be optionally filtered by WQ site ID, determinand and/or sample date, and the filtered data saved as a .rds file.
#'
#' @usage
#' import_wq(source = NULL,
#'           sites,
#'           dets = "default",
#'           start_date = "2020-01-01",
#'           end_date = Sys.Date(),
#'           save = TRUE)
#'
#' @param source Either NULL, indicating the data should be downloaded from WIMS; or a path specifying a csv file with the same columns as would be in the downloaded file. Default = NULL.
#' @param sites A list containing WQ sites for which data is to be imported.
#' @param dets Sets the list of determinants to be imported. If “default”, a specific list of determinands will be imported (see details). If “all” is specified, all determinants are imported. Alternatively, a vector of determinand codes (specified as integers) can be supplied. Default = “default”.
#' @param start_date The date of the first record to be imported. Default = "2020-01-01"
#' @param end_date The date of the last record to be imported. If a date in the future is specified, end_date will be replaced with Sys.Date(). Default Sys.Date()
#' @param save A logical value confirming whether or not to save a copy of the filtered import file to the current working directory. Default = TRUE.
#'
#' @details
#' The Water Quality Archive only has data since 2000. Data from 1999 and before is not available using this function.
#'
#' The default list of determinands to import (specified using dets = “default”) is: pH, temperature, conductivity at 25C, ammoniacal nitrogen as N, nitrogen total oxidised as N, nitrate as N, nitrite as N, ammonia un-ionised as N, alkalinity to Ph 4.5 as CaCO3, orthophosphate reactive as P, oxygen % dissolved saturation, oxygen dissolved as O2, chlorophyll, and suspended solids, and turbidity (6396). These are coded as determinand IDs: 61, 76, 77, 111, 116, 117, 118, 119, 162, 180, 9901, 9924, 19, 135, 6396.
#'
#' There are over 7000 determinands in the Water Quality Archive. To download a complete list in .csv format see https://environment.data.gov.uk/water-quality/def.csv/determinands. See also https://environment.data.gov.uk/water-quality/view/doc/reference#api-determinands for alternative ways to find determinands.
#'
#' If supplying a list of determinand IDs to the function, these must be specified as integers (ie, no leading zeros).
#'
#' If saving a copy of the downloaded data, the name of the rds file is hard-wired to WQ_OPEN_DATA_METRICS.RDS. The file will save to the current working directory.
#'
#' Downloaded raw data files will be automatically removed from the working directory following completed execution of the function.
#'
#' The function automatically modifies the downloaded data by updating the column names, fixing the formats of the data as required and selecting only the required columns. There is no processing of the data.
#'
#' Note that the WQ site IDs used in WIMS do not match the biol_site_id or flow_site_ids used in other functions in this package. For use in the wider HE Toolkit workflow, links must be made between wq_site_id in the returned file and at least one of biol_site_id or flow_site_id.
#'
#' @return
#' A tibble containing the imported water quality data. The data are arranged in long format, with the following columns:
#' - wq_site_id (unique WQ site id)
#' - date (of WQ record)
#' - det_label (details of which determinant is measured in the record)
#' - det_id (ID number of the determinand, returned as integers)
#' - result (measured value of determinand)
#' - unit (the unit of measurement for the determinand)
#' - qualifier (character value “<” indicating whether the result is below the limit of quantification for the relevant determinant). Will be NA if the true measurement has been returned.
#'
#' @export
#'
#' @examples
#' # Bulk download of data for a single site and save as .rds file for future use:
#' temp <- import_wq(sites = "SW-60250424")
#'
#' # Import previously downloaded data in .csv format:
#' \dontrun{
#' temp <-import_wq(source = "data/example_import_wq.csv", sites = "SW-60250424", save = FALSE)
#' }

import_wq <- function(source = NULL,
                      sites,
                      dets = "default",
                      start_date = "2020-01-01",
                      end_date = Sys.Date(),
                      save = TRUE) {
  ### Function Start

  ## Errors and warnings
  if(is.null(source) == FALSE && is.character(source) == FALSE) {
    stop("Source must be either NULL to run download or a valid file path in string format")
  }

  # if(is.null(source) == FALSE && file.exists(source) == FALSE) {
  #   stop("Source must be either NULL (to run download) or a valid file path in string format")
  # }

  if(is.null(source) == FALSE && grepl("\\.csv$|\\.rds$", source) == FALSE) {
    stop("If importing an existing file, it must be in .csv or .rds format")
  }

  if(missing(sites) == TRUE) {
    stop("A list of site IDs must be specified")
  }

  if(is.character(sites) == FALSE) {
    stop("Site must be a list of strings")
  }

  if(("default" %in% dets || "all" %in% dets) == FALSE && is.numeric(dets) == FALSE) {
    stop("dets must be set to default, all or a numeric vector of determinand IDs")
  }

  if(is.null(start_date) == FALSE && IsDate(start_date, "%Y-%m-%d") == FALSE) {
    stop("Start date should be in YYYY-MM-DD format")
  }

  if(is.null(end_date) == FALSE && IsDate(end_date, "%Y-%m-%d") == FALSE) {
    stop("End date should be in YYYY-MM-DD format")
  }

  if((as.Date(start_date) > Sys.Date()) == TRUE) {
    stop("Start data cannot be in the future")
  }

  if((end_date <= start_date) == TRUE) {
    stop("End date must be after start date")
  }

  if(is.logical(save) == FALSE) {
    stop("Save must be logical")
  }

  for (i in sites) {
    if (is.na(i) == TRUE) {
      warning("Site list contains NAs")
    }
  }

  if((end_date > Sys.Date()) == TRUE) {
    warning("End date is in the future. End date will be set to the current date")
  }

  if((as.Date(start_date) < as.Date("2000-01-01")) == TRUE) {
    warning("Data not availble from the WQA database before year 2000")
  }


  # SETTINGS
  ## Reset end date if in future
  if((end_date > Sys.Date()) == TRUE) {
    end_date <- Sys.Date()
  }

  ## Set default dets (if not specified by the user)
  if ("default" %in% dets) {
    dets <- c(61, 76, 77, 111, 116, 117, 118, 119, 162, 180, 9901, 9924, 19, 135, 6396)
  }


  # RUN DOWNLOAD OR OPEN FILE

  # if downloading from WIMS
  if(is.null(source) == TRUE) {

    # Set 4-digit determinand codes
    query_dets <- formatC(dets, width = 4, format = "d", flag = "0")

    # Build list of determinands for queries
    det <- as.character("")

    for (i in query_dets) {
      i <- paste0("&determinand=", i)
      det <- paste0(det, i)
    }

    # Overwrite if using all determinands
    if ("all" %in% dets) {
      det <- ""
    }

    ## Build df and query for each site and start/end dates

    df <- expand.grid(site = sites, start = start_date, end = end_date)
    web.stub <- "https://environment.data.gov.uk/water-quality/"
    web.addr.meas <- paste0(web.stub, "data/measurement.csv")
    df <- df %>%
      dplyr::mutate(query = paste0(web.addr.meas,
                                   "?startDate=", as.character(start_date),
                                   "&endDate=", as.character(end_date),
                                   "&samplingPoint=", site,
                                   det,
                                   "&_limit=99999")) %>%
      dplyr::select(query)

    #Set up blank file for appending
    wq_metrics <- data.frame(`@id` = character(),
                             sample.samplingPoint = character(),
                             sample.samplingPoint.notation = character(),
                             sample.samplingPoint.label = character(),
                             sample.sampleDateTime = as.Date(character()),
                             determinand.label = character(),
                             determinand.definition  = character(),
                             determinand.notation = character(),
                             resultQualifier.notation = logical(),
                             result = double(),
                             codedResultInterpretation.interpretation = logical(),
                             determinand.unit.label = character(),
                             sample.sampledMaterialType.label = character(),
                             sample.isComplianceSample = logical(),
                             sample.purpose.label = character(),
                             sample.samplingPoint.easting = double(),
                             sample.samplingPoint.northing = double())

    for (q in df$query){
      downloader::download(q,
                           destfile = "WQ_DATA_DOWNLOAD.csv",
                           mode = 'wb')

      col_types <- readr::cols()
      #)

      # readcsv
      dwnld <- readr::read_csv("WQ_DATA_DOWNLOAD.csv",
                               col_types = col_types)

      wq_metrics<-rbind(wq_metrics, dwnld)
    }
  }

  # If opening flat file
  if(is.null(source) == FALSE) {

    # csv format
    if(grepl("\\.csv$", source) == TRUE) {
      wq_metrics <- readr::read_csv(source, show_col_types = FALSE)
    }

    # rds format
    if(grepl("\\.rds$", source) == TRUE) {
      wq_metrics <- readr::read_rds(source)
    }

    # Check file has correct header names. Should match those coming from the database download.
    exp_names <- c("@id", "sample.samplingPoint", "sample.samplingPoint.notation", "sample.samplingPoint.label", "sample.sampleDateTime",
                   "determinand.label", "determinand.definition", "determinand.notation", "resultQualifier.notation", "result",
                   "codedResultInterpretation.interpretation", "determinand.unit.label", "sample.sampledMaterialType.label",
                   "sample.isComplianceSample", "sample.purpose.label", "sample.samplingPoint.easting", "sample.samplingPoint.northing")
    err_count = 0

    for (i in exp_names) {
      if (i %in% names(wq_metrics) == FALSE){
        err_count <- err_count+1
        warning(paste0("Required column header missing: ", i))
      }
    }

    if ((err_count > 0) == TRUE) {
      stop("Imported file is missing required column headers.")
    }

  }


  # FORMAT/CALCULATIONS/FILTER

  # Rename columns
  colnames(wq_metrics) <- c("ID", "wq_site_full", "wq_site_id", "wq_site_name", "sample_date", "det_label", "det_desc",
                    "det_id", "qualifier", "result", "interp", "unit", "sample", "compliance","sample_purpose",
                    "sample_east", "sample_north")

  # Update formats and keep only required variables
  wq_metrics <- wq_metrics %>%
    dplyr::mutate(date = lubridate::date(sample_date),
                  det_id = as.integer(det_id)) %>%
    dplyr::select(c(wq_site_id, date, det_label, det_id, result, unit, qualifier))

  # Identify missing determinands
  a <- unique(wq_metrics$det_id)
  b <- unique(as.character(dets))
  c <- b[!b %in% a]
  if(isFALSE(length(c) == 0)) {warning(paste0("Water quality determinand Not Found:", c, "\n"))}


  # Identify missing sites
  x <- unique(wq_metrics$wq_site_id)
  y <- unique(as.character(sites))
  z <- y[!y %in% x]
  if(isFALSE(length(z) == 0)) {warning(paste0("Water quality site Not Found:", z, "\n"))}


  # SAVE A COPY TO DISK IN RDS FORMAT IF REQUIRED
  if(save == TRUE) {saveRDS(wq_metrics, paste0(getwd(), "/WQ_DATA_METRICS.rds"))}


  # DELETE DOWNLOADED FILE
  if(is.null(source) == TRUE) {file.remove("WQ_DATA_DOWNLOAD.csv")}


  # RETURN
  return(tibble::as_tibble(wq_metrics))


  ### Function End
}
