#' Import RHS data
#'
#' @description
#' The `import_rhs` function imports River Habitat Survey (RHS) data. The data can either be downloaded from from data.gov.uk (<https://environment.data.gov.uk/portalstg/sharing/rest/content/items/b82d3ef3750d49f6917fff02b9341d68/data>) or read in from a local xlsx or rds file. Data can be optionally filtered by survey ID.
#'
#' @usage
#' import_rhs(rhs_dir = NULL, surveys = NULL, save = FALSE, save_dwnld = FALSE, save_dir = getwd())
#'
#' @param rhs_dir Path to local file containing RHS data.
#' @param surveys Vector of survey ids to filter on.
#' @param save Specifies if imported RHS data should be saved to working directory
#'  as rds file (for future use); Default = FALSE.
#' @param save_dir Path to folder where RHS data is to be saved;
#'  Default = Current working directory.
#' @param save_dwnld Specifies if downloaded RHS data should be saved.
#'
#' @details If saving a copy of the downloaded data, the name of the rds file is hard-wired to: RHS_survey_summary_ALL.rds. If saving after filtering on site, the name of the rds file is hard-wired to: RHS_survey_summary_F.rds.
#'
#' Downloaded raw data files (in xlsx and zip format) are automatically removed from the working directory following completed execution of the function.
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
#' # import_env(rhs_dir = "mydata/RHS_survey_summary_ALL.rds",
#' #                  surveys = c("34310", "34343"))
#'
#' # Read in local .xlsx file and filter on selected sites:
#' # import_rhs(rhs_dir = "mydata/RHS_ALL.xlsx",
#' #                  surveys = c("34310", "34343"))


import_rhs <- function(rhs_dir = NULL,
                       surveys = NULL,
                       save = FALSE,
                       save_dwnld = FALSE,
                       save_dir = getwd()){

  # error messages
  if(is.null(surveys) == FALSE && is.vector(surveys) == FALSE)
    {stop("If specified, surveys must be a vector")}
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}
  if(is.null(rhs_dir) == FALSE && file.exists(rhs_dir) == FALSE)
    {stop("Specified file directory does not exist")}
  if(is.logical(save_dwnld) == FALSE) {stop("Save_dwnld is not logical")}


  if(is.null(rhs_dir) == TRUE) {

    # Down RHS data from open data
    downloader::download("https://environment.data.gov.uk/portalstg/sharing/rest/content/items/b82d3ef3750d49f6917fff02b9341d68/data",
                         dest = "River_Habitat_Survey_-_Survey_Details_and_Summary_Results_(2).zip",
                         mode="wb")

    # Unzup file
    utils::unzip ("River_Habitat_Survey_-_Survey_Details_and_Summary_Results_(2).zip")

    # Read excel file
    rhs <- readxl::read_excel("River Habitat Survey - Survey Details and Summary Results.xlsx",
                              guess_max = 50000,
                              .name_repair = "universal")

    if(isTRUE(save_dwnld) == TRUE){

      saveRDS(rhs, paste0(save_dir,
                                  "/RHS_survey_summary_ALL.rds"))

    }

  }

  if(is.null(rhs_dir) == FALSE && grepl("xlsx", rhs_dir) == TRUE){

    if(file.exists(rhs_dir) == FALSE)
    {stop("Specified file directory does not exist")}

    # Read excel file
    rhs <- readxl::read_excel(rhs_dir,
                              guess_max = 50000,
                              .name_repair = "universal")

  }

  if(is.null(rhs_dir) == FALSE && grepl("rds", rhs_dir) == TRUE){

    if(file.exists(rhs_dir) == FALSE)
    {stop("Specified file directory does not exist")}

    # readcsv
    rhs <- readr::read_rds(rhs_dir)


  }

  if(is.null(surveys) == FALSE){

  # convert to integers, dates, factors
  rhs <- rhs %>% dplyr::mutate(
    Survey.ID = as.character(Survey.ID)
  )

  # Filter by surveys (vector of specified survey IDs)
  rhs_1 <- dplyr::filter(rhs, Survey.ID %in% surveys)

  rhs_1 <- rhs_1 %>% distinct(Survey.ID, .keep_all = TRUE)

  # Warning, if rhs Sites are not found
  if(isTRUE(length(unique(rhs_1$Survey.ID)) ==
            length(unique(surveys))) == FALSE)
  {warning("Some RHS Surveys were not found - review specified sites.")}

  # Identify missing sites
  a <- unique(rhs_1$Survey.ID)
  b <- unique(as.character(surveys))
  c <- b[!b %in% a]
  if(isFALSE(length(c) == 0)) {warning(paste0("RHS survey ID not found:", c))}

  # save copy to disk in rds format if needed
  if (save == TRUE) {saveRDS(rhs_1, paste0(save_dir, '/RHS_survey_summary_F.rds'))}

  if(is.null(rhs_dir) == TRUE){
  unlink("River Habitat Survey - Survey Details and Summary Results.xlsx")}

  unlink("River_Habitat_Survey_-_Survey_Details_and_Summary_Results_(2).zip")

  return(tibble::as_tibble(rhs_1))

  }

}
