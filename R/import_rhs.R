#' Importing River Habitat Survey (RHS) data from the EA's Open Data portal
#'
#' @description
#' The `import_rhs` function imports River Habitat Survey (RHS) data. The data can either be downloaded from from data.gov.uk (<https://environment.data.gov.uk/portalstg/sharing/rest/content/items/b82d3ef3750d49f6917fff02b9341d68/data>) or read in from a local xlsx or rds file. Data can be optionally filtered by survey ID.
#'
#' @usage
#' import_rhs(source = NULL,
#'            surveys = NULL,
#'            save = FALSE,
#'            save_dwnld = FALSE,
#'            save_dir = getwd(),
#'            rhs_dir = NULL)
#'
#' @param source Path to local .xlsx or .rds file containing RHS data. If NULL (default), then RHS data is downloaded from data.gov.uk.
#' @param surveys Vector of survey ids to filter on. Default = NULL
#' @param save Specifies whether (TRUE) or not (FALSE) the filtered data should be saved as an .rds file (for future use, or audit trail). Default = FALSE.
#' @param save_dir Path to folder where RHS data is to be saved. Applies to both filtered and unfiltered data; Default = Current working directory.
#' @param save_dwnld Specifies whether (TRUE) or not (FALSE) the unfiltered file download should be saved, in .rds format. Default = FALSE.
#' @param rhs_dir Deprecated. Path to local .xlsx or .rds file containing RHS data. If NULL (default), then RHS data is downloaded from data.gov.uk.
#'
#' @details This function downloads all data from the RHS database and optionally filters on a specified set of survey IDs.
#'
#' The save options allow the raw downloaded file (save_dwnld = TRUE) or the filtered file (save = TRUE) to be saved to the working directory.
#'
#' If saving a copy of the downloaded data, the name of the .rds file is hard-wired to: RHS_survey_summary_ALL.rds. If saving after filtering on site, the name of the rds file is hard-wired to: RHS_survey_summary_F.rds.
#'
#' Downloaded raw data files (in .xlsx and .zip format) are automatically removed from the working directory following completed execution of the function.
#'
#' @return Tibble containing RHS data.
#'
#' @export
#'
#' @examples
#' # Download data for all surveys and save as .rds file for future use:
#' # import_rhs(save_dwnld = TRUE, save_dir = "mydata")
#'
#' # Read in local .rds file and filter on selected dates:
#' # import_env(source = "mydata/RHS_survey_summary_ALL.rds",
#' #                  surveys = c("34310", "34343"))
#'
#' # Read in local .xlsx file and filter on selected sites:
#' # import_rhs(source = "mydata/RHS_ALL.xlsx",
#' #                  surveys = c("34310", "34343"))


import_rhs <- function(source = NULL,
                       surveys = NULL,
                       save = FALSE,
                       save_dwnld = FALSE,
                       save_dir = getwd(),
                       rhs_dir = NULL){

  # error messages
  if(is.null(surveys) == FALSE && is.vector(surveys) == FALSE)
    {stop("If specified, surveys must be a vector")}
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}
  if(is.null(source) == FALSE && file.exists(source) == FALSE)
    {stop("Specified file directory does not exist")}
  if(is.logical(save_dwnld) == FALSE) {stop("Save_dwnld is not logical")}

  if(is.null(rhs_dir) == FALSE && is.null(source) == FALSE)
    {stop("Set source = NULL if using rhs_dir to specify file paths. Alternatively, file paths can be specified using source")}

  if(is.null(rhs_dir) == FALSE)
    {warning("In function import_rhs, rhs_dir argument deprecated. File paths can be specified using source.")
    source <- rhs_dir}

  if(is.null(source) == TRUE) {

    # Download RHS data from open data
    downloader::download("https://environment.data.gov.uk/portalstg/sharing/rest/content/items/b82d3ef3750d49f6917fff02b9341d68/data",
                         dest = "River_Habitat_Survey_-_Survey_Details_and_Summary_Results_(2).zip",
                         mode="wb")

    # Unzup file
    utils::unzip ("River_Habitat_Survey_-_Survey_Details_and_Summary_Results_(2).zip")

    # Read excel file
    rhs <- readxl::read_excel("River Habitat Survey - Survey Details and Summary Results.xlsx",
                              guess_max = 50000,
                              sheet = "Data",
                              .name_repair = "universal")

    if(isTRUE(save_dwnld) == TRUE){

      saveRDS(rhs, paste0(save_dir,
                                  "/RHS_survey_summary_ALL.rds"))

    }

  }

  if(is.null(source) == FALSE && grepl("xlsx", source) == TRUE){

    if(file.exists(source) == FALSE)
    {stop("Specified file directory does not exist")}

    # Read excel file
    rhs <- readxl::read_excel(source,
                              guess_max = 50000,
                              .name_repair = "universal")

  }

  if(is.null(source) == FALSE && grepl("rds", source) == TRUE){

    if(file.exists(source) == FALSE)
    {stop("Specified file directory does not exist")}

    # readcsv
    rhs <- readr::read_rds(source)


  }

  if(is.null(surveys) == FALSE){

    # convert to integers, dates, factors
    rhs <- rhs %>% dplyr::mutate(
      SURVEY_ID = as.character(SURVEY_ID)
    )

    # Filter by surveys (vector of specified survey IDs)
    rhs_1 <- dplyr::filter(rhs, SURVEY_ID %in% surveys)

    rhs_1 <- rhs_1 %>% distinct(SURVEY_ID, .keep_all = TRUE)

    # Warning, if rhs Sites are not found
    if(isTRUE(length(unique(rhs_1$SURVEY_ID)) ==
              length(unique(surveys))) == FALSE)
    {warning("Some RHS Surveys were not found - review specified sites.")}

    # Identify missing sites
    a <- unique(rhs_1$SURVEY_ID)
    b <- unique(as.character(surveys))
    c <- b[!b %in% a]
    if(isFALSE(length(c) == 0)) {warning(paste0("RHS survey ID not found:", c))}

  } else {

    rhs_1 <- rhs

  }

  # reassign variable names for back-compatibility
  if(is.null(source) == TRUE){
    rhs_1 <- rhs_1 %>%
      dplyr::select(!c(SURVEY_YEAR, RSN_SURVEY))

    colnames_original <- c(
      "Survey.ID", "Survey.Status", "Location", "Site.Id..Site.", "NGR.Site", "Survey.Date", "River..Site.","Survey.Form.Id",
      "Spot.One.Is.At", "Surveyor.Id", "Adverse.Conditions", "Number.Of.Photos", "Significant.Tributary",
      "Site.Surveyed.From.Description", "Bed.is.Visible.Description","Valley.Form.Description", "Dist.Flat.Valley.Bottom",
      "Natural.Terraces", "Predom.Channel.Desc", "Predominant.Flow.Type", "No.Pools", "No.Riffles", "No.Unvegetated.Point.Bars",
      "No.Vegetated.Point.Bars", "Realigned.Channel.Description", "OverDeepened.Channel.Description", "Water.Impounded.Description",
      "Trees.Left.Description", "Trees.Right.Description", "Left.Banktop.Height", "Left.Bth.Equals.Bfh", "Left.Embanked.Height",
      "Channel.Bankfull.Width", "Channel.Water.Depth", "Channel.Water.Width", "Right.Banktop.Height", "Right.Bth.Equals.Bfh",
      "Right.Embanked.Height", "Trashline.Height", "Trashline.Width", "Bed.Material.Description",
      "Location.of.Measure.Description", "Hogweed.Bankface.Decoded", "Hogweed.Banktop.Decoded", "Knotweed.Bankface.Decode",
      "Knotweed.Banktop.Decode", "Balsam.Bankface.Decode", "Balsam.Banktop.Decode", "Other.Plant.Name",
      "Other.Plant.Bankface.Decode", "Other.Plant.Banktop.Decode", "Alders", "Diseased.Alders", "HMS.Score", "HMS.Class", "HQA",
      "HQA.Adjusted", "PCA1", "PCA2", "Hms.Berms.Embnkmnts.Sub.Score", "Hms.Bridges.Sub.Score", "Hms.Fords.Sub.Score",
      "Hms.Ofall.Dflector.Sub.Score", "Hms.Poaching.Sub.Score", "Hms.Rforced.Bnk.Bed.Sub.Score", "Hms.Rsctned.Bnk.Bed.Sub.Score",
      "Hqa.Adj.Flow.Types.Sub.Score", "Hqa.Adj.Istr.Chl.Veg.Sub.Score", "Hqa.Bank.Features.Sub.Score", "Hqa.Bank.Veg.Sub.Score",
      "Hqa.Chnl.Features.Sub.Score", "Hqa.Chnl.Substrates.Sub.Score", "Hqa.Flow.Types.Sub.Score", "Hqa.In.Str.Chnl.Veg.Sub.Score",
      "Hqa.Land.Use.Sub.Score", "Hqa.Special.Features.Sub.Score", "Hqa.Trees.Assoc.Ftes.Sub.Score"
    )

    colnames(rhs_1) <- colnames_original
  }


  # save copy to disk in rds format if needed
  if (save == TRUE) {saveRDS(rhs_1, paste0(save_dir, '/RHS_survey_summary_F.rds'))}

  if(is.null(rhs_dir) == TRUE){
    unlink("River Habitat Survey - Survey Details and Summary Results.xlsx")
    }

  unlink("River_Habitat_Survey_-_Survey_Details_and_Summary_Results_(2).zip")

  return(tibble::as_tibble(rhs_1))

}
