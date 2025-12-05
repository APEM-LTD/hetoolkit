#' Importing water quality data from the EA Water Quality Explorer
#'
#' @description
#' The `import_wq` function imports water quality (WQ) data from the Environment Agency’s Water Quality Explorer database (https://environment.data.gov.uk/water-quality-beta). The data can either be obtained automatically or read in from a previously saved .csv or .rds file. The data can be optionally filtered by WQ site ID, determinand and/or sample date, and the filtered data saved as a .rds file.
#'
#' @usage
#' import_wq(source = NULL,
#'           sites,
#'           dets = "default",
#'           start_date = "2020-01-01",
#'           end_date = Sys.Date(),
#'           save = TRUE)
#'
#' @param source Either NULL, indicating the data should be imported from Water Quality Explorer; or a path specifying a csv file with the same columns as would be in the downloaded file. Default = NULL.
#' @param sites A list containing WQ sites for which data is to be imported.
#' @param dets Sets the list of determinants to be imported. If “default”, a specific list of determinands will be imported (see details). If “all” is specified, all determinants are imported. Alternatively, a vector of determinand codes (specified as integers) can be supplied. Default = “default”.
#' @param start_date The date of the first record to be imported. Default = "2020-01-01"
#' @param end_date The date of the last record to be imported. If a date in the future is specified, end_date will be replaced with Sys.Date(). Default = Sys.Date()
#' @param save A logical value confirming whether or not to save a copy of the filtered import file to the current working directory. Default = TRUE.
#'
#' @details
#' The Water Quality Explorer only has data collected from 2000 onwards. Data from 1999 and earlier are not available using this function.
#'
#' The default list of determinands to import (specified using dets = “default”) is: pH, temperature, conductivity at 25C, ammoniacal nitrogen as N, nitrogen total oxidised as N, nitrate as N, nitrite as N, ammonia un-ionised as N, alkalinity to Ph 4.5 as CaCO3, orthophosphate reactive as P, oxygen % dissolved saturation, oxygen dissolved as O2, chlorophyll, and suspended solids, and turbidity (6396). These are coded as determinand IDs: 61, 76, 77, 111, 116, 117, 118, 119, 162, 180, 9901, 9924, 19, 135, 6396.
#'
#' There are over 7000 determinands in the Water Quality Explorer. To download a complete list in .csv format see https://environment.data.gov.uk/water-quality/def.csv/determinands. See also https://environment.data.gov.uk/water-quality/view/doc/reference#api-determinands for alternative ways to find determinands.
#'
#' If supplying a list of determinand IDs to the function, these may be specified as integers (ie, no leading zeros) or 4-digit character strings (ie, numbers enclosed in quotes, eg: c("0061", "9901")).
#'
#' If saving a copy of the data straight from the data explorer, the name of the rds file is hard-wired to WQ_DATA_METRICS.RDS. The file will save to the current working directory.
#'
#' The function automatically modifies the imported data by updating the column names, fixing the formats of the data as required and selecting only the required columns. There is no processing of the data.
#'
#' Note that the WQ site IDs used in WIMS do not match the biol_site_id or flow_site_ids used in other functions in this package. For use in the wider HE Toolkit workflow, links must be made between wq_site_id in the returned file and at least one of biol_site_id or flow_site_id.
#'
#' @return
#' A tibble containing the imported water quality data. The data are arranged in long format, with the following columns:
#' - wq_site_id (unique WQ site id)
#' - wq_site_name (name of WQ site)
#' - easting (of site)
#' - northing (of site)
#' - EA area
#' - date_time (of WQ record)
#' - det_id (ID number of the determinand)
#' - determinand (name of determinand)
#' - result (measured value of determinand)
#' - unit (the unit of measurement for the determinand)
#' - qualifier (character value “<” indicating whether the result is below the limit of quantification for the relevant determinant). Will be NA if the true measurement has been returned.
#' - observation (where the 'result' recorded in the Water Quality Data Explorer is a character description- e.g. 'Present')
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

  if(("default" %in% dets || "all" %in% dets) == FALSE
     && (all(grepl("[0-9]{4}", dets)) == FALSE && is.numeric(dets) == FALSE)) {
    stop("dets must be set to default, all or a vector of determinand IDs. String IDs must be 4-digits long.")
  }

  # if(("default" %in% dets || "all" %in% dets) == FALSE && is.numeric(dets) == FALSE) {
  #   stop("dets must be set to default, all or a numeric vector of determinand IDs")
  # }

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
    warning("End date is in the future. End date has reverted to the current date")
  }

  if((as.Date(start_date) < as.Date("2000-01-01")) == TRUE) {
    warning("Data not available from the WQA database before year 2000")
  }


  # SETTINGS
  ## Reset end date if in future
  if((end_date > Sys.Date()) == TRUE) {
    end_date <- Sys.Date()
  }


  # RUN DOWNLOAD OR OPEN FILE

  # if downloading from WIMS
  if(is.null(source) == TRUE) {

    ## Set default dets (if not specified by the user)
    if ("default" %in% dets) {
      dets <- c(61, 76, 77, 111, 116, 117, 118, 119, 162, 180, 9901, 9924, 19, 135, 6396)
    }

    # Set 4-digit determinand codes
    det_list <- formatC(dets, width = 4, format = "d", flag = "0")

    # create empty list
    all_points_data_list <- list()

    # run query
    for (site_id in sites) {
      point_data <- fetch_wq_data(
        sampling_point_id = site_id,
        initial_date_from = start_date,
        initial_date_to = end_date
      )

      if (nrow(point_data) > 0) {
        point_data <- point_data |> dplyr::mutate(SamplingPointID = site_id)
        all_points_data_list <- append(all_points_data_list, list(point_data))
      }
    }

    wq_metrics <- bind_rows(all_points_data_list)

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

    ## Set default dets (if not specified by the user)
    if ("default" %in% dets) {
      dets <- c(61, 76, 77, 111, 116, 117, 118, 119, 162, 180, 9901, 9924, 19, 135, 6396)
    }

    # Set 4-digit determinand codes
    det_list <- formatC(dets, width = 4, format = "d", flag = "0")

    # Check file has correct header names. Should match those coming from the database download.
    exp_names <- c("id", "samplingPoint.notation", "samplingPoint.prefLabel", "samplingPoint.easting", "samplingPoint.northing",
                   "samplingPoint.region","samplingPoint.area","samplingPoint.subArea", "samplingPoint.samplingPointStatus",
                   "samplingPoint.samplingPointType", "phenomenonTime", "samplingPurpose", "sampleMaterialType", "determinand.notation",
                   "determinand.prefLabel", "result", "unit")
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

  # Check at least some data returned
  if(nrow(wq_metrics) == 0) {
    stop("No data returned for selected sites and dates; check site and determinand IDs are correct and periods of record coincide with specified date range")
  }

  # Rename columns
  colnames(wq_metrics) <- c("id", "wq_site_id", "wq_site_name", "easting", "northing",
                            "region","area","subarea", "status",
                            "type", "date_time", "sampling_purpose", "material_type", "det_id",
                            "determinand", "result_char", "unit")

  # Keep only required variables
  wq_metrics <- wq_metrics |> dplyr::select(c(wq_site_id, wq_site_name, easting, northing, area, date_time, det_id, determinand, result_char, unit))

  # Filter by determinands of interest
  if(" all" %in% det_list == FALSE){
    wq_metrics <- wq_metrics |> dplyr::filter(det_id %in% det_list)
  }

  # Sort out result variable: separate out qualifier, make numeric and shift character results to different column
  # Can't use grepl("[0-9]",...) + parse_number as some descriptive results also contain numbers (e.g. 'heavy rain in last 24 hours') so use workaround
  wq_metrics <- wq_metrics |> dplyr::mutate(qualifier = stringr::str_extract(result_char, "<"), result_char = stringr::str_replace(result_char, "([<])", ""))

  wq_metrics <- wq_metrics |>
    dplyr::mutate(result = case_when(!unit == "Coded Result" | unit == "PRESENT/NOT FOUND" ~ suppressWarnings(as.numeric(result_char)),
                                                                                                                TRUE ~ NA),
                                       observation = case_when(unit == "Coded Result" | unit == "PRESENT/NOT FOUND" ~ readr::parse_character(result_char),
                                                                                                               TRUE ~ NA)) |>
    dplyr:: select(-result_char)


  # Identify missing determinands
  # a <- unique(as.character(wq_metrics$det_id))
  a <- unique(formatC(wq_metrics$det_id, width = 4, format = "d", flag = "0"))
  b <- unique(formatC(dets, width = 4, format = "d", flag = "0"))
  c <- b[!b %in% a]
  if(isFALSE(length(c) == 0) & !" all" %in% c) {warning(paste0("Water quality determinand not found:", c, "\n"))}


  # Identify missing sites
  x <- unique(wq_metrics$wq_site_id)
  y <- unique(as.character(sites))
  z <- y[!y %in% x]
  if(isFALSE(length(z) == 0)) {warning(paste0("Water quality site not found:", z, "\n"))}

  # Summary message
  cat("Total of", nrow(unique(wq_metrics)), "unique records retrieved", "\n")

  # SAVE A COPY TO DISK IN RDS FORMAT IF REQUIRED
  if(save == TRUE) {saveRDS(wq_metrics, paste0(getwd(), "/WQ_DATA_METRICS.rds"))}

  # RETURN
  return(tibble::as_tibble(wq_metrics))

  ### Function End
}
