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


  # RUN DOWNLOAD OR OPEN FILE

  # if downloading from WIMS
  if(is.null(source) == TRUE) {

    ## Set default dets (if not specified by the user)
    if ("default" %in% dets) {
      dets <- c(61, 76, 77, 111, 116, 117, 118, 119, 162, 180, 9901, 9924, 19, 135, 6396)
    }

    # Set 4-digit determinand codes
    det_list <- formatC(dets, width = 4, format = "d", flag = "0")

    ## Build df and query for each site and start/end dates

    df <- expand.grid(site = sites, start = start_date, end = end_date)
    web.stub <- "https://environment.data.gov.uk/water-quality/data/observation"
    df <- df |>
      dplyr::mutate(query = paste0(web.stub,
                                   "?dateFrom=", as.character(start_date),
                                   "&dateTo=", as.character(end_date),
                                   "&pointNotation=", site,
                                   "&limit=2500")) |>
      dplyr::select(query)

    # create empty lists
    req <- list()
    req_paged <- list()
    resps <- list()
    all_pages <- list()

    for (q in df$query){
      # Set throttling to limit per minute requests to safely avoid exceeding API constraints (forbidden error)
      req[[q]] <- httr2::request(q) |> httr2::req_throttle(capacity = 75, fill_time_s = 60) |>
                                      httr2::req_headers("Accept" = "text/csv",
                                                          "Accept-Crs" = "http://www.opengis.net/def/crs/EPSG/0/27700",
                                                          "CSV-Header" = "present",
                                                          "API-Version" = "1")

      # API requests are now limited to 2500 records- to get whole datasets need to perform iterative requests, following `Link: rel="next"` automatically

      req_paged[[q]] <- req[[q]] |>
        httr2::req_url_query(limit = 2500) |> httr2::req_method("POST") |> httr2::req_error(is_error= \(resp) FALSE)

      resps[[q]] <- httr2::req_perform_iterative(
        req_paged[[q]],
        next_req = httr2::iterate_with_link_url(rel = "next"),
        max_reqs = Inf, progress = paste0(q, ":")
      )

        # Remove requests with no data returned
        resps[[q]] <- resps[[q]][lapply(resps[[q]], httr2::resp_status_desc) == "OK"]

      # Parse and combine CSV pages
      all_pages[[q]] <- lapply(resps[[q]], function(r) readr::read_csv(httr2::resp_body_string(r), show_col_types = FALSE))
      # Drop empty pages, if any
      all_pages[[q]] <- Filter(function(df) nrow(df) > 0, all_pages[[q]])

      # Convert to df
      complete_list <- unlist(all_pages, recursive = FALSE)
      wq_metrics <- do.call("rbind", complete_list)

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
    exp_names <- c("id", "samplingPoint.notation", "samplingPoint.prefLabel", "samplingPoint.easting", "samplingPoint.northing",
                   "samplingPoint.region","samplingPoint.area","samplingPoint.subArea", "samplingPointStatus",
                   "samplingPointType", "phenomenonTime", "samplingPurpose", "sampleMaterialType", "determinand.notation",
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
  if(is.null(wq_metrics) == TRUE) {
    stop("No data returned for selected sites and dates; check site IDs are correct and periods of record coincide with specified date range")
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
  # Can't use grepl("[0-9]",...) + parse_number as some descriptions contain numbers (e.g. 'heavy rain in last 24 hours') so use workaround
  wq_metrics <- wq_metrics |> dplyr::mutate(qualifier = stringr::str_extract(result_char, "<"), result_char = stringr::str_replace(result_char, "([<])", ""))

  if(is.character(wq_metrics$result_char) == TRUE){
  wq_metrics <- wq_metrics |> dplyr::mutate(result = case_when(!unit == "Coded Result" | unit == "PRESENT/NOT FOUND" ~ suppressWarnings(as.numeric(result_char)),
                                                                                                                TRUE ~ NA),
                                       observation = case_when(unit == "Coded Result" | unit == "PRESENT/NOT FOUND" ~ readr::parse_character(result_char),
                                                                                                               TRUE ~ NA)) |>
    dplyr:: select(-result_char)}

  else if(is.character(wq_metrics$result_char) == FALSE){
    wq_metrics <- wq_metrics |> dplyr::mutate(result = result_char, observation = NA) |> dplyr::select(-result_char)
  }


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

  # SAVE A COPY TO DISK IN RDS FORMAT IF REQUIRED
  if(save == TRUE) {saveRDS(wq_metrics, paste0(getwd(), "/WQ_DATA_METRICS.rds"))}

  # RETURN
  return(tibble::as_tibble(wq_metrics))


  ### Function End
}
