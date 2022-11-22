#' Visualise the range of flow conditions experienced historically
#'
#' @description plot_rngflows generates a scatterplot for two flow variables (such as those produced by calc_flowstats) and overlays two convex hulls: one showing the full range of flow conditions experienced historically, and a second convex hull showing the range of flow conditions with associated biology records. This allows the user to assess to coverage of the biology data with respect to the historical range of flow conditions.
#'
#' @usage plot_rngflows (data, flow_stats, biol_metric, wrap_by = NULL, plotly = FALSE, label = NULL)
#'
#' @param data Name of data frame or tibble containing the data to be analysed, such as that produced by the join_he function.
#' @param flow_stats Vector of two flow statistics to be plotted.
#' @param biol_metric Name of column containing biology data; NAs are interpreted as indicating the absence of a sample.
#' @param wrap_by Grouping variable to facet the plot (e.g. site). Default = NULL.
#' @param plotly Logical value specifying whether or not to render the plot as an interactive plotly plot. Default = FALSE.
#' @param label Optional variable (e.g. date) to label points in plotly plot. Default = NULL.
#'
#' @return A single ggplot2 or plotly object.
#'
#' @export
#'
#' @examples
#' ## Example 1: Produce a single plotly plot, combining data for all sites
#' # plot_rngflows(data = all_data,
#' #               flow_stats = c("Q95z", "Q10z"),
#' #               biol_metric = "LIFE_F_OE",
#' #               wrap_by = NULL,
#' #               label = "Year")
#'
#' ## Example 2: Produce a faceted ggplot, showing data separately for each site
#' # plot_rngflows(data = all_data,
#' #               flow_stats = c("Q95z", "Q10z"),
#' #               biol_metric = "LIFE_F_OE",
#' #               wrap_by = "biol_site_id",
#' #               label = "Year")

plot_rngflows <- function(data, flow_stats, biol_metric, wrap_by = NULL, label = NULL, plotly = FALSE){

  ## Error messages
  # stop if the data input is not defined
      if(missing(data)) {
        stop("The data input needs to be defined")
        }
  # stop if flow statistics are not defined
      if(missing(flow_stats)) {
        stop("'flow_stats' is missing; please specify a vector of two flow statistics from 'data'")
        }
  # stop if the biology metric is not defined
      if(missing(biol_metric)) {
        stop("'biol_metric' is missing; please specify a column containing biology data")
        }
  # stop if more than one biology metric is defined
      if(length(biol_metric) != 1) {
        stop("Only one biology metric can be defined")
        }
  # stop if <2 flow statistics are defined
      if(length(flow_stats) < 2) {
        stop("Two flow statistics must be specified")
      }
  # stop if >2 flow statistics are defined
  if(length(flow_stats) > 2) {
    stop("No more than two flow statistics can be specified")
  }
  # stop if more than one wrap by variables are defined
      if(length(wrap_by) > 1) {
        stop("only one 'wrap_by' variable can be defined")
        }
  # stop if flow statistic 1 is not found in the dataframe
      if(!(flow_stats[1] %in% names(data))) {
        stop("Flow statistic 1 is not found in the data")
        }
  # stop if flow statistic 2 is not found in the dataframe
      if(!(flow_stats[2] %in% names(data))) {
        stop("Flow statistic 2 is not found in the data")
        }
  # stop if biology metric is not found in the dataframe
      if(!(biol_metric %in% names(data))) {
        stop("'biol_metric' is not found in the data")
        }
  # stop if label is not found in the dataframe
      if(!is.null(label)) {
        if(!(label %in% names(data)) & !is.null(label)){
          stop("'label' is not found in the data")
        }
      }

  # stop if wrap by is not found in the dataframe
      if(!is.null(wrap_by) && !(wrap_by %in% names(data))){
        stop("'wrap_by' is not found in the data")
        }
  # stop if plotly is not logical
      if(is.logical(plotly) == FALSE) {
        stop("'plotly' is not logical")
        }

  ## create generic column names for easy manipulation/plotting
    data$x <- dplyr::pull(data, flow_stats[1])
    data$y <- dplyr::pull(data, flow_stats[2])
    data$biol <- dplyr::pull(data, biol_metric)
    if (!is.null(label)) {
      data$z <- dplyr::pull(data, label)
    } else {
      data$z <- NA_integer_
    }
    #data$z <- dplyr::pull(data, label)
    if(!is.null(wrap_by)){data$wrap <- dplyr::pull(data, wrap_by)}

  ## stop if flow statistics are not numeric
    if(!is.numeric(data$x)){stop("Flow statistic 1 is in an invalid format, should be numeric")}
    if(!is.numeric(data$y)){stop("Flow statistic 2 is in an invalid format, should be numeric")}

  ## stop if biology metric is not numeric
    if(!is.numeric(data$biol)){stop("'biol_metric' is in an invalid format, should be numeric")}

  ## create column to flag where biology data is missing
    data$group <- as.factor(ifelse(is.na(data$biol),"All records","Records with samples"))

  ## create a second dataset containing only records with biology data
    data2 <- subset(data,group=="Records with samples")
    data$group <- "All records"

  ## join first and second datasets: this contains data for generating two convex hulls, one for all records (group = "All dates"), and one for records with biology data (group = "Dates with samples")
    data3 <- rbind(data,data2)

    # chull function doesn't allow NAs, so filter data to exclude any
    data3 <- data3[is.na(data3$x) == FALSE, ]
    data3 <- data3[is.na(data3$y) == FALSE, ]

    # stop if not enough data remains
    if(dim(data3)[1] <5 ){stop("Not enough records with flow statistics; function aborted.")}

  ## Generate convex hulls, based on two flow statistics
    find_hull <- function(data3){
      data3[grDevices::chull(data3$x, data3$y), ]
    }
    #hulls <- plyr::ddply(data3, "group", find_hull) %>% dplyr::select(x, y, group)
    if(is.null(wrap_by)){
      hulls <- plyr::ddply(data3, "group", find_hull) %>% dplyr::select(x, y, group)
    } else {
      hulls <- plyr::ddply(data3, .(group, wrap), find_hull) %>% dplyr::select(x, y, group, wrap)
    }

  ## plot data and hulls
  p <- ggplot2::ggplot(data = data3, aes(x = x, y = y, colour = group, fill = group)) +
      geom_polygon(data = hulls, alpha=0.3, aes(x = x, y = y, fill = group)) +
      geom_point(aes(z = z)) +
      scale_color_manual(values = c("black", "#E69F00")) +
      scale_fill_manual(values = c("black", "#E69F00")) +
      xlab(flow_stats[1]) +
      ylab(flow_stats[2]) +
      theme(legend.position = "top") +
      theme(legend.title = element_blank())

  if(!is.null(wrap_by)){
      p <- p + facet_wrap(~wrap)
  }

  ## convert ggplot to plotly
  if (isTRUE(plotly)){
      p <- plotly::ggplotly(p)
  }

  return (p)

}
