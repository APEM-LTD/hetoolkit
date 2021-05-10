#' Calculate residual flow ratios.
#'
#' @description Uses modelled flow data to calculate percentile statistics under a chosen
#' scenario (e.g. historical or recent actual) as a ratio of a reference scenario (usually
#' naturalised flows), for one or more sites.
#'
#' @usage
#' calc_rfrstats(data = NULL,
#'               site_col = NULL,
#'               date_col = NULL,
#'               flow_col = NULL,
#'               ref_col = NULL,
#'               q = NULL
#'               save_as = FALSE
#'               save_dir = getwd()
#'               )
#'
#' @param data Name of dataframe or tibble containing the flow data to be processed.
#' Must be in long format (i.e. separate columns for site_id, date, and each flow scenario).
#' @param site_col Name of column in data containing unique flow site id.
#' @param date_col Name of column in data containing date of flow record in the format of yyyy/mm/dd. See examples for a simple example to reformating the date if needed.
#' @param flow_col Name of column in data containing flow values for the scenario of interest
#' (e.g. historical, recent actual).
#' @param ref_col Name of column in data containing flows under the reference (e.g.
#' naturalised) scenario.
#' @param q Required percentile statistic (between 1 and 99).
#' @param save Specifies if results should be saved as rds file (for future use); Default = FALSE.
#' @param save_dir Path to folder where results are to be saved; Default = Current working directory.
#'
#' @details For each water year (e.g. 1 October one year to 30 September the next) at each site, the
#' chosen flow percentile (q) is calculated under the scenario of interest (qx)
#' and the reference scenario (qx_ref), where qx takes its name depending on q
#' (e.g. Q95). The residual flow ratio (RFR) is then calculated as: rfrx = qx / qx_ref.
#'
#' A q value of 95 represents the flow that is exceeded 95% of the time (i.e. 5th percentile).
#'
#' In addition, the number of data points in each water year is calculated for
#' both the scenario of interest (n) and the reference scenario (n_ref).
#'
#' Modelled flows can be on any time step (e.g. 1, 10, 30 days, or monthly) but should
#' be at approximately regular time intervals and the same for the both scenarios. The
#' function does not require a minimum number of records in each water year, but more
#' extreme percentiles calculated using sparse data may not be meaningful. Any missing
#' records (NAs) are ignored when calculating summary statistics. The user should check
#' n and n_ref to identify any water years with incomplete data coverage.
#'
#' @return A tibble containing the processed flow statistics for every combination of site
#' and water year.
#'
#' @examples
#' # Example of altering date format to the required yyyy-mm-dd format
#' x <- c("09-01-2001", "09-01-2002", "09-01-2003")
#' as.Date(x,format="%d-%m-%Y")
#'
#' # Example 1
#' # load site model flow data
#' site.model.flow<-data(site.model.flow,package="hetoolkit")
#'
#' # run
#' # calc_rfrstats(data = site.model.flow,
#' #              site_col = "SITE_ID",
#' #              date_col = "Date_end",
#' #              flow_col = "Flow_HIST",
#' #              ref_col = "Flow_NAT",
#' #              q = 75,
#' #              save = FALSE,
#' #              save_dir = getwd())




# use site.model.flows.rds as an example dataset
# note that flows may not be daily (more likely monthly, or every 10 days).
# Flows do not have to cover a full year; this is for the user to check.

# work flow...
#-------------

calc_rfrstats<- function(data = NULL,
                         site_col = NULL,
                         date_col = NULL,
                         flow_col = NULL,
                         ref_col = NULL,
                         q = NULL,
                         save = FALSE,
                         save_dir = getwd()){
  # Errors:
  # make sure q is entered correctly
  if(is.null(q)) {stop(paste0('"q" missing, with no default.'))}
  if(IsWholeNumber(q)==FALSE) {stop(paste0('"q" must be an interger.'))}
  if((0<q & q<100)==FALSE) {stop(paste0('"q" must between 1 and 99.'))}

  # make sure the right data had been input:
  if(is.null(data)) {stop("data missing, with no default.")}
  if(!is.data.frame(data)) {stop("data input must be a dataframe.")}

  if(is.null(flow_col)) {stop("flow_col is missing.")}
  if(is.null(ref_col)) {stop("ref_col is missing.")}
  if(is.null(site_col)) {stop("site_col is missing.")}
  if(is.null(date_col)) {stop("date_col is missing.")}

  if(flow_col %in% colnames(data) == FALSE) {stop(paste0(flow_col," cannot be found in input dataframe."))}
  if(ref_col %in% colnames(data) == FALSE) {stop(paste0(ref_col," cannot be found in input dataframe."))}
  if(site_col %in% colnames(data) == FALSE) {stop(paste0(site_col," cannot be found in input dataframe."))}
  if(date_col %in% colnames(data) == FALSE) {stop(paste0(date_col," cannot be found in input dataframe."))}

  # make sure saving options are viable:
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}

  # load required packages:
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(lubridate, tidyverse)

  # format input data
  data <- data %>% dplyr::rename(site_col = site_col, date_col = date_col, flow_col = flow_col, ref_col = ref_col)
  data <- subset(data, select = c(site_col, date_col,flow_col, ref_col))

  # make sure input data in the correct format
  if(!class(data$flow_col)=="numeric") {stop("flow_col must be numeric.")}
  if(!class(data$ref_col)=="numeric") {stop("ref_col must be numeric.")}
  if(!class(data$date_col)=="Date") {stop("date_col must be of Date class")}


  if(nrow(data)!=length(unique(paste0(data$site_col,data$date_col)))){warning("there are multiple rows with the same site ID and date combination.")}

  # Use lubridate::date() to format date_col as date
  data$date_col<-  lubridate::date(data$date_col)

  # Create water_year column based on a 1st October year end, e.g. water year 2018 = 01-10-2018 to 30-09-2019
  data$water_year<-ifelse(data$date_col <  lubridate::date(paste0(format(data$date_col,"%Y"),'-10-01')), format(data$date_col,"%Y"),as.numeric(format(data$date_col,"%Y"))+1)

  # generate summary statistics for flow and reference flow by site and referece year: n, n_ref, q at user specified leve, and q_ref at user specified level
  out<-dplyr::left_join(data %>% dplyr::group_by(site_col,water_year) %>% dplyr::summarise_at("flow_col", list(~length(which(!is.na(flow_col))),~quantile(flow_col,probs=1-(q/100), na.rm=TRUE))),
                        data %>% dplyr::group_by(site_col,water_year) %>% dplyr::summarise_at("ref_col", list(~length(which(!is.na(ref_col))),~quantile(ref_col,probs=1-(q/100), na.rm=TRUE)))
                        ,by = c("site_col","water_year"))

  ## ratio rfr = qx / qx_ref
  out$rfr<- out$quantile.x / out$quantile.y

  ## rename columns in output table
  names(out)<-c("site_col", "water_year", "n",  paste0("q",q), "n_ref", paste0("q",q,"_ref"),"rfr")

  # save copy to disk in rds format if needed
  if (save == TRUE) {saveRDS(out, paste0(save_dir, "/RFR_stats_results.rds"))}

  return(out)
}

#
# supporting functions:
#----------------------

# IsWholeNumber <-
#   function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# IsDate <- function(x, date.format = NULL) {
#   formatted = try(as.Date(x, date.format), silent = TRUE)
#   is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
#   is_date[is.na(x)] = NA  # Insert NA for NA in x
#   return(is_date)
# }
#

##################
#
#run function:
#-------------

# final<- calc_rfrstats(data=site.model.flows, site_col="SITE_ID", date_col="Date_end", flow_col = "Flow_HIST", ref_col = "Flow_NAT", q=95)

##################
#
# getwd()
# setwd("C:/Users/Sarah Davie/Desktop/R function writing")
# site.model.flows<-readRDS("site.model.flows.rds")
# View(site.model.flows)
#

