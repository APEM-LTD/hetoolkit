#' Link biology data to six-monthly flow statistics for paired biology and flow sites.
#'
#' @description
#' The join_he function links biology data to six-monthly flow statistics (from calc_flowstats) to create dataset for hydro-ecological modelling. Includes option to lag selected flow statistics.
#'
#' @usage
#' join_he(biol_data = biol_data, flow_stats = flow_stats, mapping = NULL, LS1 = FALSE, LS2 = FALSE, lag_vars = c("Q10z", "Q95z"))
#'
#' @param biol_data Data frame or tibble containing the processed biology data. Must contain the following columns: biol_site_id, Year and Season. Seasons must be named "Spring" and "Autumn". See Details below.
#' @param flow_stats Data frame (first element of list returned by the 'calc_flowstats' function), containing the processed flow statistics. Must contain the following columns: flow_site_id, water_year and season.
#' @param mapping Data frame or tibble containing paired biology and flow site IDs. Must contain columns named biol_site_id and flow_site_id. These columns must not contain any NAs. Default = NULL, used when paired biology and flow sites are assumed to have identical ids, and so mapping not required.
#' @param LS1 Logical value indicating whether or not to also link biology samples to flow statistics for the summer period of the previous year. Default = FALSE.
#' @param LS2 Logical value indicating whether or not to also link biology samples to flow statistics for the summer period of the year before last. Default = FALSE.
#' @param lag_vars List of flow variables from 'flow_stats' to be lagged if LS1 and/or LS2 = TRUE. Default = two commonly-used flow statistics: Q10z and Q95z.
#'
#' @details
#' join_he is not intended to join biology data to residual flow ratio statistics (from calc_rfrstats) because these are on an annual time step, rather than six-monthly, and therefore easy to join manually.
#'
#' 'biol_data' and 'flow_stats' may contain more sites than listed in 'mapping', but any sites not listed in 'mapping' will be filtered out.
#'
#' 'biol_data' must contain the following columns: biol_site_id, Year and Season. Seasons must be named "Spring" and "Autumn".
#'
#' join_he joins spring (March to May) biology samples to flow statistics for the preceding winter (October to March) period, and autumn (September to November) samples to  flow statistics for the preceding summer (April to September) period. Any winter (December to February) or summer (June to August) biology samples are dropped. All flow statistics in 'flow_stats' are joined; any superfluous variables must be dropped manually, either before or after executing the join_he function.
#'
#' If LS1 = TRUE, spring biology samples are also joined to flow statistics for the summer period of the previous year. These flow statistics are renamed with the suffix 'LS1'. Only the flow statistics listed in 'lag_vars' are joined in this way.
#'
#' If LS2 = TRUE, spring biology samples are also joined to flow statistics for the summer period of the year before last. These flow statistics are renamed with the suffix 'LS2'. Only the flow statistics listed in 'lag_vars' are joined in this way.
#'
#' To facilitate subsequent data visualisation and modelling, the function uses expand.grid to generate all combinations of biol_site_id, season (spring, autumn) and year (ranging from the earliest to the latest year in biol_data). Left joins are used to link the biology data and flow statistics to this expanded grid.
#'
#' It is recommended that any replicate or duplicate biology samples collected from a site in the same season and year are averaged out or eliminated before executing this function.
#'
#' @return join_he returns a tibble containing processed biology data linked to processed flow statistics.
#'
#' @export
#'
#' @examples
#' ## Join processed biology data and processed flow statistics - no mapping specified because paired flow and biology sites have identical ids. Do not lag flow statistics.
#' # join_he(biol_data = biol_all,
#' #          flow_stats = flowstats_1,
#' #          mapping = NULL,
#' #          LS1 = FALSE,
#' #          LS2 = FALSE)
#'
#' ## Join processed biology data and processed flow statistics using mapping specified. Link biology data to summer flows in previous year - flow variables to lag specified.
#'
#' # join_he(biol_data = biol_all,
#' #          flow_stats = flowstats_1,
#' #          mapping = SiteList,
#' #          LS1 = TRUE,
#' #          LS2 = FALSE,
#' #          lag_vars = c("Q10z", "Q95z", "Q70z"))
#'
#' ## Join processed biology and processed flow statistics
#' ### Includes an example of user-create mapping data frame.
#'
#' # flow_data <- import_hde(sites = c("AA", "BB"))
#' # flow_stats <- calc_flowstats(flow_data)
#' # biol_data <- import_biology(sites = c("XX", "YY"))
#' # mapping <- data.frame(biol_site_id = c("XX", "YY"), flow_site_id = c("BB", "AA"))
#' # join_he(biol_data = biol_data,
#' #        flow_stats = flow_stats,
#' #        mapping = mapping)


join_he <- function(biol_data,
                    flow_stats,
                    mapping = NULL,
                    LS1 = FALSE,
                    LS2 = FALSE,
                    lag_vars = c("Q10z", "Q95z")){


  if(is.data.frame(biol_data) == FALSE){stop("biol_data is invalid format")}

  if("biol_site_id" %in% colnames(biol_data) == FALSE)
  {stop("biol_site_id column was not identified in biol_data")}

  if("Season" %in% colnames(biol_data) == FALSE)
  {stop("Season column was not identified in biol_data")}

  if("Year" %in% colnames(biol_data) == FALSE)
  {stop("Year column was not identified in biol_data")}

  if(nrow(biol_data) == 0)
  {stop("biol_data is of length 0")}

  if(is.data.frame(flow_stats) == FALSE){stop("flow_stats is invalid format")}

  if("flow_site_id" %in% colnames(flow_stats) == FALSE)
  {stop("flow_site_id column was not identified in flow_stats")}

  if("water_year" %in% colnames(flow_stats) == FALSE)
  {stop("water_year column was not identified in flow_stats")}

  if("season" %in% colnames(flow_stats) == FALSE)
  {stop("season column was not identified in flow_stats")}

  if(is.null(mapping) == FALSE && is.data.frame(mapping) == FALSE)
  {stop("Mapping data frame is invalid")}

  if(is.null(mapping) == FALSE && "flow_site_id" %in% colnames(mapping) == FALSE)
  {stop("flow_site_id column was not identified in mapping")}

  if(is.null(mapping) == FALSE && "biol_site_id" %in% colnames(mapping) == FALSE)
  {stop("biol_site_id column was not identified in mapping")}

  if(isTRUE(any(is.na(mapping))) == TRUE)
  {stop("mapping contains NAs")}

  if(is.logical(LS1) == FALSE) {stop("LS1 is not logical")}
  if(is.logical(LS2) == FALSE) {stop("LS2 is not logical")}

  if(LS1 == FALSE && lag_vars %in% colnames(flow_stats) == FALSE)
  {stop("lag_var was not identified in flow_stats")}

  if(LS2 == FALSE && lag_vars %in% colnames(flow_stats) == FALSE)
  {stop("lag_var was not identified in flow_stats")}


  # get flow stats

  flow_stats <- flow_stats

  # filter data using mapping

  if(is.null(mapping) == FALSE){

    biol_data <- dplyr::filter(biol_data, biol_site_id %in% unique(mapping$biol_site_id))
    flow_stats <- dplyr::filter(flow_stats, flow_site_id %in% unique(mapping$flow_site_id))

  }

  # if mapping is not specified, create mapping
  ## assumes paired biology and flow sites have identical ids

  if(is.null(mapping) == TRUE){

    biol_site_id <- unique(biol_data$biol_site_id)
    flow_site_id <- biol_site_id

    mapping <- data.frame(biol_site_id, flow_site_id)

  }

  # biol data processing

  # Here we take the ecology data frame and create dummy rows for those years and seasons where we don't have ecology samples (quite alot)
  all.combinations <- expand.grid(biol_site_id = unique(biol_data$biol_site_id), Year = min(biol_data$Year):max(biol_data$Year), Season = c("Spring", "Autumn"), stringsAsFactors = FALSE)
  dim(all.combinations)

  # LS1 & LS2 = FALSE

  if(LS1 == FALSE && LS2 == FALSE){

    biol_data_1 <- biol_data %>%
      dplyr::right_join(all.combinations) %>%
      dplyr::arrange(biol_site_id, Year, desc(Season)) %>%
      dplyr::mutate(FlowSeason = dplyr::case_when(Season == 'Autumn' ~ 'Summer',
                                        Season == 'Spring' ~ 'Winter'))

  }

  # LS1 = TRUE

  if(LS1 == TRUE && LS2 == FALSE){

    biol_data_1 <- biol_data %>%
      dplyr::right_join(all.combinations) %>%
      dplyr::arrange(biol_site_id, Year, desc(Season)) %>%
      dplyr::mutate(FlowSeason = dplyr::case_when(Season == 'Autumn' ~ 'Summer',
                                        Season == 'Spring' ~ 'Winter')) %>%
      dlyr::mutate(FlowSeason.L1 = 'Summer')

  }

  # LS1 & LS2 = TRUE

  if(LS1 == TRUE && LS2 == TRUE){

    biol_data_1 <- biol_data %>%
      dplyr::right_join(all.combinations) %>%
      dplyr::mutate(Year.2000 = Year - 2000) %>%
      dplyr::arrange(biol_site_id, Year, desc(Season)) %>%
      dplyr::mutate(FlowSeason = dplyr::case_when(Season == 'Autumn' ~ 'Summer',
                                        Season == 'Spring' ~ 'Winter')) %>%
      dplyr::mutate(FlowSeason.L1 = 'Summer') %>%
      dplyr::mutate(FlowSeason.L2 = 'Summer')

  }

  # Add FlowYear column to biology data

  biol_data_1$FlowYear <- biol_data_1$Year - 1

  if(LS1 == TRUE){
    biol_data_1$FlowYear.L1 <- biol_data_1$Year - 2
  }

  if(LS2 == TRUE){
    biol_data_1$FlowYear.L2 <- biol_data_1$Year - 3
  }

  # add mapping to biology data

  biol_data_2 <- biol_data_1 %>%
    dplyr::left_join(mapping, by = "biol_site_id")

  # divide dataset to spring and autumn

  biol_data_aut <- biol_data_2 %>% dplyr::filter(Season == 'Autumn')
  biol_data_spr <- biol_data_2 %>% dplyr::filter(Season == 'Spring')

  # flow data processing

  flow.stats.summer <- subset(flow_stats, season =='Summer')
  flow.stats.summer <- flow.stats.summer[,c('flow_site_id','season','water_year', lag_vars)]

  excluded_vars <- c("flow_site_id", "water_year", "season")

  info_cols <- dplyr::select(flow.stats.summer, flow_site_id, water_year, season)

  to_rename <- dplyr::select(flow.stats.summer, -excluded_vars)

  #flow.stats.summer.L1 <- to_rename %>% rename_all( ~ paste0("LS1_", .))
  flow.stats.summer.L1 <- to_rename %>% dplyr::rename_all( ~ paste0( .,"LS1"))
  flow.stats.summer.L1 <- data.frame(info_cols, flow.stats.summer.L1)

  #flow.stats.summer.L2 <- to_rename %>% rename_all( ~ paste0("LS2_", .))
  flow.stats.summer.L2 <- to_rename %>% dplyr::rename_all( ~ paste0( .,"LS2"))
  flow.stats.summer.L2 <- data.frame(info_cols, flow.stats.summer.L2)


  if(LS1 == FALSE && LS2 == FALSE){

    dat_spr <- biol_data_spr %>%
      dplyr::left_join(flow_stats, by=c("flow_site_id", "FlowYear" = "water_year", "FlowSeason" = "season"))

    dat_aut <- biol_data_aut %>%
      dplyr::left_join(flow_stats, by=c("flow_site_id", "FlowYear" = "water_year", "FlowSeason" = "season"))


  }

  if(LS1 == TRUE && LS2 == FALSE){

    dat_spr <- biol_data_spr %>%
      dplyr::left_join(flow_stats, by=c("flow_site_id", "FlowYear" = "water_year", "FlowSeason" = "season")) %>%
      dplyr::left_join(flow.stats.summer.L1, by=c("flow_site_id", "FlowYear.L1" = "water_year"))

    dat_aut <- biol_data_aut %>%
      dplyr::left_join(flow_stats, by=c("flow_site_id", "FlowYear" = "water_year", "FlowSeason" = "season")) %>%
      dplyr::left_join(flow.stats.summer.L1, by=c("flow_site_id", "FlowYear.L1" = "water_year"))
  }

  if(LS1 == TRUE && LS2 == TRUE){

    dat_spr <- biol_data_spr %>%
      dplyr::left_join(flow_stats, by=c("flow_site_id", "FlowYear" = "water_year", "FlowSeason" = "season")) %>%
      dplyr::left_join(flow.stats.summer.L1, by=c("flow_site_id", "FlowYear.L1" = "water_year")) %>%
      dplyr::left_join(flow.stats.summer.L2, by=c("flow_site_id", "FlowYear.L2" = "water_year"))


    dat_aut <- biol_data_aut %>%
      dplyr::left_join(flow_stats, by=c("flow_site_id", "FlowYear" = "water_year", "FlowSeason" = "season")) %>%
      dplyr::left_join(flow.stats.summer.L1, by=c("flow_site_id", "FlowYear.L1" = "water_year")) %>%
      dplyr::left_join(flow.stats.summer.L2, by=c("flow_site_id", "FlowYear.L2" = "water_year"))

  }

  # join spring and autumn datasets back together
  he_data <- rbind(dat_spr, dat_aut)

  # format as tibble
  he_data <- tibble::as_tibble(he_data)

  return(he_data)

}
