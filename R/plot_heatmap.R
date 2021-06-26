#' Heatmap plot
#'
#' @description The plot_heatmap function is designed to visualise and summarise gaps in time series data. It plots time series data for multiple sites as a tiled heatmap, and optionally produces tabular summaries of data completeness by time period and site. Although designed for application with flow time series data, it can be applied to any type of numerical data, with or without a time dimension.
#'
#' @usage
#' plot_heatmap(data, x, y, fill, colour = "viridis", lab.x = x, lab.y = y, lab.legend = fill, dual = FALSE, list_out = TRUE, limits = FALSE, save = FALSE, save_dir = getwd(), ...)
#'
#' @param data Name of dataframe or tibble containing data to be processed. Must be in long format (i.e. separate columns for x and y). If list_out is set to TRUE for the missingness information tables, user must supply an expanded and pre-sorted dataset to calculate missing time steps, where data is sorted by y and then x (i.e. sites sorted into date order for all possible date time steps). Function sorts "x" data into increasing timesteps by default.
#' @param x Name of column containing variable to be used to define x-axis of heatmap (ideally a time series variable e.g. date or year).
#' @param y Name of column containing variable to be used to define y-axis of heatmap (ideally a spatial grouping factor e.g. site_id).
#' @param fill Name of column containing variable to be plotted (e.g. flow, WHPT O/E) (continuous variate, integer, or double).
#' @param colour A character string indicating the colorramp option to use. Limited to the four viridis package options: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D") or "cividis" (or "E"). Default = "viridis".
#' @param lab.x Character string to be used on the x axis. Default is name of x variable.
#' @param lab.x Character string to be used on the y axis. Default is name of y variable.
#' @param lab.legend Character string to be used on the legend title. Default is name of fill column variable.
#' @param dual Logical value determining whether or not to include a histogram of % missingness alongside the heatmap. Default = FALSE.
#' @param list_out logical value specifying whether or not to produce tabular summaries of data completeness. Default = TRUE.
#' @param limits Logical value specifying whether or not to trim the data to plot 30 x by 20 y values into one tile. Default = FALSE.
#' @param save Logical value specifying whether or not output plot should be saved to file. Default = FALSE.
#' @param save_dir Path to folder where plot should be saved. Default = Current working directory.
#' @param ... Provision to include additional ggplot plotting and saving arguments, including for example: theme, file type, width and size. See ?theme and ?ggsave for details.
#'
#' @details Requires data in long format (e.g. x, y, fill columns). x and y can be any numeric or categorical data; fill must be numeric.
#'
#' Large datasets may be difficult to visualise clearly using a single heatmap, and need to be subset prior to executing the function.
#'
#' If x (or y) is a time series, the user should check that the time series is complete - e.g. a complete sequence of dates should be present in the data, even if the fill variable is NA on some dates. See example below illustration use of expand.grid prior to remedy gaps prior to executing the function.
#'
#' The examples below illustrate how the data can be aggregated (e.g. to different time steps, e.g. year, season, month) and summarised (e.g. using mean, min, max, sd, count) prior to executing the function to explore patterns and gaps in the data.
#'
#' @return A list of three elements:
#' 1. A ggplot tiled heatmap with x (e.g. time) and y (e.g. site) axes. If dual = TRUE, the ggplot object includes a marginal histogram of % missingness. The plot can be optionally saved as a png file.
#' If list_out is set to TRUE the list will also contain:
#' 2. A tibble summary of data completeness for each value or level of x: number and proportion of missing observations, and total number of observations.
#' 3. A tibble summary of data completeness for each value or level of y: number and proportion of missing observations, total observations, the number of gaps (sequences of missing time steps) in the dataset, and what the shortest and longest run of missing time steps are. The later three will be NA if there are no missing time steps.
#'
#' @export
#'
#' @examples
#'
#' # read in example mean daily flow dataset
#' # df <- readRDS("data/FLOW_DATA.rds")
#'
#' # set the date column data to date format
#' # df$date <- lubridate::date(df$date)
#'
#' ## Basic application: plotting flow rate by site and date, providing tables of proportion of missing flow values by date and site along with the heatmap plot
#'
#' # a <- plot_heatmap(data = flow_data, x = "date", y = "flow_site_id", fill = "flow")
#'
#' ## view heatmap
#' # a[[1]]
#' # amend ggplot elements, such as the title
#' ## a[[1]] + labs(title = "this adds a new title to the plot")
#' ## tibble table of missing flow values by date (variable x)
#' # a[[2]]
#' ## tibble table of missing flow values by site (variable y)
#' # a[[3]]
#'
#' ## setting dual = TRUE added a marginal histogram displaying % missingness
#' # a <- plot_heatmap(data = df, x = "date", y = "sites", fill = "flow", limits = TRUE, dual= TRUE)
#'
#' ## to plot this device, the code needed is slightly different:
#' # gridExtra::grid.arrange(a[[1]])
#'
#' ## Presence-absence heatmap plot, showing whether or not flow was measured on each date at each site. Using dual=TRUE includes a marginal barchart to visualise, for each site, the proportion of days that do not have a daily flow value.
#'
#' # df$presence <- "1"
#' # df$presence[is.na(df$flow)]<- NA
#' # a <- plot_heatmap(data = df, x = "date", y = "sites", fill = "presence", dual = TRUE)
#' # gridExtra::grid.arrange(a[[1]])
#'
#' ## Identifying missing variables.
#' ## If certain dates are completely missing from an input dataset, they will not show in the heatmap plot unless they are explicitly added as missing values. In this example we expand the x time step to include missing days and re-plot to demonstrate how this can be accounted for. If not using some form of date, a list of unique options of x and or y will need to be specified as a vector similar to that produced by unique(df$sites).
#'
#' ## artificially remove some specific days from the data
#' # temp1 <- df[!df$date %in% lubridate::date(c("2000-02-07", "2000-02-08", "2000-12-12", "2001-09-19")),]
#'
#' ## use expand.grid() to re-create the missing days
#' # new <- expand.grid(date = seq(min(temp1$date, na.rm=TRUE), max(temp1$date, na.rm=TRUE), 1), sites = unique(temp1$sites))
#'
#' ## join the flow data to the new expanded grid to generate NA flow values on the four missing days
#' # temp <- dplyr::full_join(temp1, new, by=c("date","sites"))
#' # temp[temp$date %in% lubridate::date(c("2000-02-07","2000-02-08", "2000-12-12","2001-09-19")),]
#' # temp <- temp[order(temp$date),]
#'
#' ## heatmap showing missing daily flow records (zoom in RStudio to see all four missing days)
#' # a <- plot_heatmap(data = temp, x = "date", y = "sites", fill = "flow")
#' # a[[1]]
#'
#' # rm(list= c("new","temp", "temp1"))
#'
#' ## Calculate and then plot monthly mean flows
#'
#' # artificially remove some data
#' # temp <- df
#' # temp$flow[temp$date %in% lubridate::date(c("2000-02-07","2000-02-08","2000-12-12","2001-09-19"))] <- NA
#' # temp$flow[temp$sites %in% c("SS60F015","029004") & temp$date %in% lubridate::date(c("2000-01-02","2001-06-22","2001-10-12"))] <- NA
#'
#' ## calculate monthly mean flows
#' # temp$month <- lubridate::month(df$date)
#' # temp$year <- lubridate::year(df$date)
#' # temp1 <- temp %>% dplyr::group_by(month, year, sites) %>%
#' # dplyr::summarise(across(.cols= "flow", list(mean = ~mean(flow, na.rm = TRUE), missing = ~length(which(is.na(flow))), total = ~length(flow), perc_missing = ~(length(which(is.na(flow)))/length(flow))*100), .names = "{.fn}"))
#' # temp1$yy_mm <- paste(temp1$year,temp1$month,sep="_")
#'
#' ## heatmap showing monthly mean flows
#' # a <- plot_heatmap(data = temp1, x = "yy_mm", y = "sites", fill = "mean",dual = TRUE)
#' # gridExtra::grid.arrange(a[[1]])
#'
#' ## heatmap showing percentage completeness of the daily flow data in each month
#' # a <- plot_heatmap(data = temp1, x = "yy_mm", y = "sites", fill = "perc_missing")
#' # a[[1]]
#'
#' ## every month has a mean flow, so no missing records at this time step
#' # a[[3]]
#'
#' # rm(list=c("temp","temp1"))



plot_heatmap <- function(data,
                        x,
                        y,
                        fill,
                        colour = "viridis",
                        lab.x = x,
                        lab.y = y,
                        lab.legend = fill,
                        limits = FALSE,
                        dual = FALSE,
                        list_out = TRUE,
                        save = FALSE,
                        save_dir = getwd(),
                        ...){

  # Errors:
    # make sure the right data had been input:
    if(is.null(data)) {stop("data missing, with no default")}
    if(!is.data.frame(data)) {stop("data input must be a dataframe")}

    # x is not specified, missing from data, invalid format, or contains NAs
    if(is.null(x)) {stop("x is missing, please specify")}
    if(x %in% colnames(data) == FALSE) {stop("x cannot be found in input dataframe")}

    # y is not specified, missing from data or invalid format
    if(is.null(y)) {stop("y is missing, please specify")}
    if(y %in% colnames(data) == FALSE) {stop("y cannot be found in input dataframe")}

    # fill is not specified or missing from data
    if(is.null(fill)) {stop("fill is missing, please specify")}
    if(fill %in% colnames(data) == FALSE) {stop("fill cannot be found in input dataframe")}

    # colour is not valid colour scheme
    if(colour %in% c("magma" ,"A", "inferno","B", "plasma", "C", "viridis","D", "cividis","E") == FALSE) {stop("colour input not valid colour scheme")}

    # check logical values provided for logical imput settings
    if(is.logical(limits)==FALSE) {stop("logical value of limits variable must be provided")}
    if(is.logical(list_out)==FALSE) {stop("logical value of list_out variable must be provided")}
    if(is.logical(dual)==FALSE) {stop("logical value of dual variable must be provided")}

    # check save settings are valid
    if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
    if(is.logical(save) == FALSE) {stop("Save is not logical")}


  # format input data
  data <- tibble::as_tibble(data)
  data <- data %>% dplyr::rename(x = all_of(x),y = all_of(y), fill = all_of(fill))
  data <- subset(data, select = c(x, y, fill))

  # error if NAs in x or y
  if(anyNA(data$x)) {stop("x contains NAs, NAs not a valid value of x")}
  if(anyNA(data$y)) {stop("y contains NAs, NAs not a valid value of y")}

  # order the data
  data <- data[order(data$x),]

  # replace any empty character strings ("") in the dataset with an NA
  #data <- data %>% dplyr::mutate_all(list(~dplyr::na_if(.,"")))
  data <- data %>% dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"")))


  # limit size of the data set if limits = TRUE
  if(limits == TRUE) {
    data <- subset(data, x %in% unique(data$x)[1:30] & y %in% unique(data$y)[1:20])
    warning("limits set to TRUE, dataset being trimmed to a maximum of the first 30 unique x values and first 20 unique y values, no effect if less than 30 x and 20 y occur within the dataset. If trimming is not wanted set 'limits' = FALSE")
  }

  # for plotting breaks - convert x into a factor if more than 10 unique values
  if (length(unique(data$x)) > 10) {data$x <- as.factor(data$x)}

  # plot heatmap
  if(length(unique(data$fill))<3) {data$fill<- as.character(data$fill)} # handle as discrete variable, particularly relevant for presence/absence data recorded as 0 or 1

  p <- ggplot2::ggplot(data, aes(x=x, y=y, fill=fill)) +
        geom_tile() +
        scale_x_discrete(breaks=levels(data$x)[seq(1, nlevels(data$x), length.out = 5)]) +
        theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-10,0,-10,-10)) +  #axis.text.x = element_text(angle = -90),
        labs(title = NULL, subtitle = NULL, x = stringr::str_to_title(lab.x), y = stringr::str_to_title(lab.y), fill = stringr::str_to_title(lab.legend))

  if(typeof(data$fill)=="double") {
      p <- p + viridis::scale_fill_viridis(option = colour, na.value="white")
    } else {
      p <- p + viridis::scale_fill_viridis(option = colour, discrete = TRUE, na.value="white")
    }

    out <- list()
    out[[1]] <- p

  # create data tables
  if(list_out == TRUE || dual==TRUE) {
  #  options(dplyr.summarise.inform=F)
    out[[2]] <- data %>%
      dplyr::group_by(x) %>%
      dplyr::summarise(across(.cols= "fill", list(missing = ~length(which(is.na(fill))), total = ~length(fill), prop_missing = ~length(which(is.na(fill)))/length(fill)), .names = "{.fn}"))

    out[[3]] <- data %>%
      dplyr::group_by(y) %>%
      dplyr::summarise(across(.cols= "fill", list(missing = ~length(which(is.na(fill))), total = ~length(fill), prop_missing = ~length(which(is.na(fill)))/length(fill)), .names = "{.fn}"))

    miss <- data %>%
      dplyr::group_by(y) %>%
      naniar::miss_var_run( var = fill) %>%
      group_by(y,is_na) %>%
      dplyr::summarise(number_of_gaps = n(), smallest_gap = min(run_length), biggest_gap = max(run_length))

    out[[3]] <- dplyr::left_join(out[[3]],subset(miss, is_na=="missing", select = c(y,number_of_gaps, smallest_gap, biggest_gap)), by = "y")

    rm(miss)

  #  options(dplyr.summarise.inform=T)
  }

  # add the histogram of missingness to the heatmap
  if(dual == TRUE) {
    right.plot <- ggplot2::ggplot(out[[3]], aes(x=y,y=prop_missing)) +
      geom_bar(stat="identity") +
      scale_x_discrete(labels=NULL) +
      labs(x = NULL, y = "Proportion missing") +
      coord_flip()

    legend <- g_legend(p + theme(legend.position = "bottom"))

    lay <- rbind(c(1,1,1,2),
                 c(1,1,1,2),
                 c(1,1,1,2),
                 c(1,1,1,2),
                 c(1,1,1,2),
                 c(1,1,1,2),
                 c(1,1,1,2),
                 c(1,1,1,2),
                 c(3,3,3,3))

    p <- gridExtra::grid.arrange(p + theme(legend.position = 'none'), right.plot, legend, layout_matrix = lay)
    out[[1]] <- p
  }

  if(save == TRUE) {
     ggplot2::ggsave(plot = p, path = save_dir, filename = paste(paste("heatmap_plot",x,y,fill,sep="_"),"png", sep = "."))
  }

  # remove the data tables if made for dual plot but not wanted
  if(list_out == FALSE) out <- out[[1]]

  return(out)

}


## helper functions

g_legend <- function(a_gplot){
  tmp <- ggplot2::ggplot_gtable(ggplot_build(a_gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

