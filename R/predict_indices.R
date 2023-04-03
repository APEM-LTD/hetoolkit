#' Calculating expected scores for macroinvertebrate indices using the RICT2 model
#'
#' @description
#' The `predict_indices` function mirrors the functionality of the RICT model available on the MS Azure platform (<https://gallery.azure.ai/Experiment/RICT-package-2>). Specifically, it uses  environmental (ENV) data from Ecology Data Explorer to generate expected scores under minimally impacted reference conditions for 80 indices, plus probabilities for RIVPACS end-groups. The prediction functionality applies the 'rict_predict()' function from the AquaMetrics RICT package (https://github.com/aquaMetrics/rict). No classification is undertaken.
#'
#' @usage
#' predict_indices(env_data = x, save = FALSE, save_dir = getwd())
#'
#' @param env_data A data frame or tibble containing site-level environmental data in Environment Agency Ecology Data Explorer format (as produced by the import_env function).
#' @param file_format Format in which env_data is supplied: "EDE" - environmental data is formatted as downloaded from the EA's Ecology Data Explorer; "RICT" - environmental data is in the RICT template format.
#' @param save Specifies whether or not expected indices data should be saved as a rds file (for future use); Default = TRUE.
#' @param save_dir Path to folder where expected indices data is to be saved; Default = Current working directory.
#' @param all_indices Boolean. TRUE - Return all indices in output (default); FALSE - only returns WHPT indices (arguement passed to rict::rict_predict).
#'
#' @details
#' All data validation and transformation (conversion) are done in this function using functions predefined in HelperFunctionsv1.R. Predictions are made using PredictionfunctionsV2.R.
#'
#' The function will modify the standard RICT output, renaming "SITE" as "biol_site_id" (standardised column header for biology sites).
#'
#' @return Tibble containing expected scores for macroinvertebrate indices plus end-group probabilities. The RICT Technical Specification and the RIVPACS IV End Group Descriptions are available at <https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/User%20Guides.aspx>
#'
#' @references
#' FBA, 2020. River Invertebrate Classification Tool (RICT2) User Guide V1.5 (2020) Available at: <https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/User%20Guides.aspx>
#'
#' @export
#'
#' @examples
#' # Generate expected scores for macroinvertebrate indices, using environmental data downloaded from Ecology Data Explorer for site(s) of interest.
#' # Save dataset as .RDS file.
#'  predict_indices(env_data = env_data,
#'                   file_format = "EDE",
#'                   save = TRUE)


predict_indices <- function(env_data,
                            file_format = "EDE",
                            save = FALSE,
                            save_dir = getwd(),
                            all_indices = TRUE){

  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.object(env_data) == FALSE) {stop("Environmental data file does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}
  if(is.logical(all_indices) == FALSE){stop("all_indices must be logical")}
  if(isTRUE((file_format == "EDE") | isTRUE(file_format == "RICT")) == FALSE)
    {stop("file_format must be specified as EDE or RICT")}


  # Model
  model <- c("RIVPACS IV GB") # Changed in AZURE/RSTUDIO

  # Get data
  raw.input.data <- env_data

  if(isTRUE(file_format == "EDE") == TRUE){

  # check substrate variables for presence of NAs
    raw.input.data %>%
      dplyr::select(BOULDERS_COBBLES, PEBBLES_GRAVEL, SAND, SILT_CLAY) %>%
      dplyr::summarise(across(.fns = (~sum(is.na(.x)))))

    # replace NAs, if required
    raw.input.data$BOULDERS_COBBLES[is.na(raw.input.data$BOULDERS_COBBLES)] <- 0

  # Add RICT Format File Columns
  raw.input.data$SPR_SEASON_ID <- 1
  raw.input.data$SUM_SEASON_ID <- 2
  raw.input.data$AUT_SEASON_ID <- 3
  raw.input.data$velocity <- NA

  raw.input.data$"SPR_TL2_WHPT_ASPT (ABW,DISTFAM)" <- NA
  raw.input.data$"SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)" <- NA
  raw.input.data$"SPR_NTAXA_BIAS" <- NA
  raw.input.data$"SUM_TL2_WHPT_ASPT (ABW,DISTFAM)" <- NA
  raw.input.data$"SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)" <- NA
  raw.input.data$"SUM_NTAXA_BIAS" <- NA
  raw.input.data$"AUT_TL2_WHPT_ASPT (ABW,DISTFAM)" <- NA
  raw.input.data$"AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)" <- NA
  raw.input.data$"AUT_NTAXA_BIAS" <- NA

  # Initial Data Formatting
  ##Rename the column 1 from ï..SITE to "SITE"
  #colnames(raw.input.data)[1]  <- c("SITE") # change AZURE
  #raw.input.data$SITE_ID <- as.character(raw.input.data$SITE_ID) # change AZURE

  # check for length <5, add a "0" to get proper Easting/Northing 5 digit codes
  raw.input.data$EASTING  <- getCorrectCodes(raw.input.data$EASTING)   # Change AZURE
  raw.input.data$NORTHING <- getCorrectCodes(raw.input.data$NORTHING)  # Change AZURE

  # Change all column names to uppercase
  names(raw.input.data) <- toupper(names(raw.input.data)) # change AZURE
  #head(colnames(raw.input.data), 28) #ok
  #tail(colnames(raw.input.data), 28)

  # Get all the bioligical data
  namesBiological <-    c(colnames(raw.input.data)[1],colnames(raw.input.data)[2],colnames(raw.input.data)[3],"SPR_SEASON_ID", "SPR_TL2_WHPT_ASPT (ABW,DISTFAM)","SPR_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SPR_NTAXA_BIAS", "SUM_SEASON_ID", "SUM_TL2_WHPT_ASPT (ABW,DISTFAM)","SUM_TL2_WHPT_NTAXA (ABW,DISTFAM)", "SUM_NTAXA_BIAS", "AUT_SEASON_ID", "AUT_TL2_WHPT_ASPT (ABW,DISTFAM)","AUT_TL2_WHPT_NTAXA (ABW,DISTFAM)","AUT_NTAXA_BIAS")

  biologicalData <- raw.input.data[,namesBiological]
  head(biologicalData, 4)

  raw.input.data <- raw.input.data %>%
    dplyr::rename(WATERBODY = WATER_BODY,
                  NGR = NGR_PREFIX,
                  BOULDER_COBBLES = BOULDERS_COBBLES,
                  HARDNESS = TOTAL_HARDNESS,
                  MEAN_WIDTH = WIDTH,
                  MEAN_DEPTH = DEPTH,
                  SITE = BIOL_SITE_ID) %>%
    dplyr::mutate(YEAR = as.integer(format(Sys.Date(), "%Y")))

  }

  final_input_data <- data.frame(raw.input.data)

  predict_data <- rict::rict_predict(final_input_data, all_indices = all_indices)

  predict_data <- predict_data %>% dplyr::rename(biol_site_id = SITE)

  return(tibble::as_tibble(predict_data))

}
