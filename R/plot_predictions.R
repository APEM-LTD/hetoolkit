#' Visualise the predictions from a hydro-ecological model
#'
#' @description The plot_predictions function produces a time series plot of predictions from a hydro-ecological model, alongside observed biology and flow data. The plot is faceted by site, as specified by the `site_col` argument. The function generates a ggplot object, that is optionally saved as a .png file named "Predictions_Plot.png".
#'
#' @usage
#' plot_predictions <- function(data, biol_metric, time_col, site_col, flow_stat, pred_col, ncol = 4, save = FALSE, save_dir = getwd())
#'
#' @param data Data to plot, containing observed biology and flow data as well as model predictions.
#' @param time_col Name of column containing time variable for x-axis.
#' @param site_col Name of column containing biology site ID, for faceting. Default = "biol_site_id".
#' @param flow_stat Names of up to two flow statistics of interest.
#' @param biol_metric Name of column containing measured data for the biology metric of interest.
#' @param pred_col Names of columns containing central predictions (required) and lower and upper confidence or prediction intervals (optional). The lower and upper limits are optional but both must be specified (not just a lower or upper limit).
#' @param ncol Number of columns to use in facet plots. Default = 4.
#' @param save Logical value specifying whether or not the output should also be saved as a png file. Default = FALSE.
#' @param save_dir Path to folder where png files are to be saved. Default = Current working directory.
#'
#' @return The function returns a single ggplot object.
#'
#' @export
#'
#' @examples
#' # Generate a plot of predictions from a hydro-ecological model, alongside observed biology and flow data, and save a png file to a sub-folder of the working directory called 'outputs'.
#' # plot_predictions(data = autumn_data,
#' #                  biol_metric = "LIFE_F_OE",
#' #                  time_col = "Year",
#' #                  site_col = "biol_site_id",
#' #                  flow_stat = c("Q95z", "Q10z"),
#' #                  pred_col = c("fit", "lwr", "upr"),
#' #                  ncol = 4)


plot_predictions <- function(data,
                             biol_metric,
                             time_col,
                             site_col,
                             flow_stat,
                             pred_col,
                             ncol = NULL,
                             save = FALSE,
                             save_dir = getwd()){

  # Errors

  if(is.data.frame(data) == FALSE){stop("Data frame 'data' not found")}

  if((time_col %in% colnames(data)) == FALSE)
  {stop("Specified time_col was not identified in 'data'")}

  if((site_col %in% colnames(data)) == FALSE)
  {stop("Specified site_col was not identified in 'data'")}

  if(all(unique(flow_stat) %in% colnames(data)) == FALSE)
  {stop("Specified flow statistics were not identified in 'data'")}

  if(biol_metric %in% colnames(data) == FALSE)
  {stop("Specified biology metric was not identified in 'data'")}

  if(length(pred_col) == 1 && all(unique(pred_col) %in% colnames(data)) == FALSE)
  {stop("Specified central predictions were not identified in 'data'")}

  if(length(pred_col) == 3 && all(unique(pred_col) %in% colnames(data)) == FALSE)
  {stop("Specified central, lower or upper predictions were not identified in 'data'")}

  if(length(pred_col) == 2) {stop("pred_col is of length 2; pred_col must be of length 1 or 3")}

  if(length(pred_col) > 3) {stop("pred_col if of length >3; pred_col must be of length 1 or 3")}

  if(is.logical(save) == FALSE) {stop("Save is not logical")}

  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}

  if(length(flow_stat) > 2){stop("More than 2 flow statistics have been specified")}

  # Read-in data

  data$flow_stat_1 <- dplyr::pull(data, flow_stat[1])

  if(is.numeric(data$flow_stat_1) == FALSE){stop("flow_stat must be numeric")}

  if(is.na(flow_stat[2]) == FALSE){
    data$flow_stat_2 <- dplyr::pull(data, flow_stat[2])

    if(is.numeric(data$flow_stat_2) == FALSE){stop("flow_stat must be numeric")}

    }

  if(is.na(flow_stat[2]) == TRUE){
    data$flow_stat_2 <- NA}

  data$time <- dplyr::pull(data, time_col)

  data$biol_metric <- dplyr::pull(data, biol_metric)

  if(is.numeric(data$biol_metric) == FALSE){stop("biol_metric must be numeric")}

  data$pred_metric <- dplyr::pull(data, pred_col[1])

  if(is.numeric(data$pred_metric) == FALSE){stop("pred_col must be numeric")}

  if(is.na(pred_col[2]) == FALSE){
    data$pred_low <- dplyr::pull(data, pred_col[2])

    if(is.numeric(data$pred_low) == FALSE){stop("pred_col must be numeric")}

    }

  if(is.na(pred_col[3]) == FALSE){
    data$pred_up <- dplyr::pull(data, pred_col[3])

    if(is.numeric(data$pred_up) == FALSE){stop("pred_col must be numeric")}

    T_F <- data$pred_low > data$pred_up
    if("TRUE" %in% T_F == TRUE) {stop("Values in lower prediction interval are greater than upper prediction interval")}

    }

  data$sites <- dplyr::pull(data, site_col)
  data$sites <- as.factor(data$sites)

  # Prep data to allow for y2 axis

    if(is.na(flow_stat[2]) == FALSE){

      y1a_min <- min(data$flow_stat_1, na.rm = TRUE)
      y1b_min <- min(data$flow_stat_2, na.rm = TRUE)

      if(isTRUE(y1a_min <= y1b_min) == TRUE){y1_min <- y1a_min}
      else {y1_min <- y1b_min}

      y1a_max <- max(data$flow_stat_1, na.rm = TRUE)
      y1b_max <- max(data$flow_stat_2, na.rm = TRUE)

      if(isTRUE(y1a_max >= y1b_max) == TRUE){y1_max <- y1a_max}
      else {y1_max <- y1b_max}

      y1_range <- y1_max - y1_min

    }

    if(is.na(flow_stat[2]) == TRUE){

      y1_min <- min(data$flow_stat_1, na.rm = TRUE)
      y1_max <- max(data$flow_stat_1, na.rm = TRUE)
      y1_range <- y1_max - y1_min

    }

    # central predictions only specified

    if(length(pred_col) == 1){

    # Get y2 range

    y2a_min <- min(data$biol_metric, na.rm = TRUE)
    y2b_min <- min(data$pred_metric, na.rm = TRUE)

    if(isTRUE(y2a_min <= y2b_min) == TRUE){y2_min <- y2a_min}
    else {y2_min <- y2b_min}

    y2a_max <- max(data$biol_metric, na.rm = TRUE)
    y2b_max <- max(data$pred_metric, na.rm = TRUE)

    if(isTRUE(y2a_max >= y2b_max) == TRUE){y2_max <- y2a_max}
    else {y2_max <- y2b_max}

    y2_range <- y2_max - y2_min

    # range ratio
    rangeratio_1 <- y1_range/y2_range
    minadj_1 <- y1_min - y2_min*rangeratio_1

    data$y2trans <- data$biol_metric * rangeratio_1 + minadj_1
    data$y3trans <- data$pred_metric * rangeratio_1 + minadj_1

    if(is.na(flow_stat[2]) == FALSE){

    # Plot - 2 flow stats

    plot_1 <- data %>%
      ggplot2::ggplot() +
      geom_line(data, mapping = aes(x = time, y = flow_stat_1, colour = '#56B4E9')) +
      geom_line(data, mapping = aes(x = time, y = flow_stat_2, colour = '#0072B2')) +
      scale_color_identity(name = "Flow Statistics",
                           breaks = c('#56B4E9', '#0072B2'),
                           labels = c(flow_stat[1], flow_stat[2]),
                           guide = "legend") +
      ggnewscale::new_scale_color() +
      geom_point(data, mapping = aes(x = time, y = y2trans, colour = "black")) +
      geom_line(data, mapping = aes(x = time, y = y3trans, colour = "red")) +
      scale_color_identity(name = "Metrics",
                           breaks = c('black', 'red'),
                           labels = c("Observed", "Predicted"),
                           guide = "legend") +
      scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_1)/rangeratio_1, name = biol_metric)) +
      labs(x = time_col, y = "") +
      #ggtitle(biol_metric[3]) +
      theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                      hjust = 0.5),
            legend.title = element_blank(),
            legend.text = element_text(size = 7),
            legend.position = "bottom",
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            axis.title.x = element_text(size = 7),
            axis.title.y.left = element_text(size = 7),
            axis.title.y.right = element_text(size = 7)) +
      facet_wrap(~ sites, ncol = ncol)

    }


    if(is.na(flow_stat[2]) == TRUE){

      # plot - 1 flow stat

      plot_1 <- data %>%
        ggplot2::ggplot() +
        geom_line(data, mapping = aes(x = time, y = flow_stat_1, colour = '#56B4E9')) +
        scale_color_identity(name = "Flow Statistics",
                             breaks = c('#56B4E9'),
                             labels = c(flow_stat[1]),
                             guide = "legend") +
        ggnewscale::new_scale_color() +
        geom_point(data, mapping = aes(x = time, y = y2trans, colour = "black")) +
        geom_line(data, mapping = aes(x = time, y = y3trans, colour = "red")) +
        scale_color_identity(name = "Metrics",
                             breaks = c('black', 'red'),
                             labels = c("Observed", "Predicted"),
                             guide = "legend") +
        scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_1)/rangeratio_1, name = biol_metric)) +
        labs(x = time_col, y = "") +
        #ggtitle(biol_metric[3]) +
        theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                        hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              legend.position = "bottom",
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              axis.title.y.left = element_text(size = 7),
              axis.title.y.right = element_text(size = 7)) +
        facet_wrap(~ sites, ncol = ncol)

    }

    }

    # central, lower and upper predictions specified

    if(length(pred_col) == 3){

      # Get y2 range

      y2a_min <- min(data$biol_metric, na.rm = TRUE)
      y2b_min <- min(data$pred_low, na.rm = TRUE)

      if(isTRUE(y2a_min <= y2b_min) == TRUE){y2_min <- y2a_min}
      else {y2_min <- y2b_min}

      y2a_max <- max(data$biol_metric, na.rm = TRUE)
      y2b_max <- max(data$pred_up, na.rm = TRUE)

      if(isTRUE(y2a_max >= y2b_max) == TRUE){y2_max <- y2a_max}
      else {y2_max <- y2b_max}

      y2_range <- y2_max - y2_min

      # range ratio
      rangeratio_1 <- y1_range/y2_range
      minadj_1 <- y1_min - y2_min*rangeratio_1

      data$y2trans <- data$biol_metric * rangeratio_1 + minadj_1
      data$y3trans <- data$pred_metric * rangeratio_1 + minadj_1
      data$y4trans <- data$pred_low * rangeratio_1 + minadj_1
      data$y5trans <- data$pred_up * rangeratio_1 + minadj_1


      if(is.na(flow_stat[2]) == FALSE){

      # plot - 2 flow stats

      plot_1 <- data %>%
        ggplot2::ggplot() +
        geom_ribbon(data, mapping = aes(x = time, ymin = y4trans, ymax = y5trans), fill = "pink", alpha=.5) +
        geom_line(data, mapping = aes(x = time, y = y3trans, colour = "red")) +
        geom_point(data, mapping = aes(x = time, y = y2trans, colour = "black")) +
        scale_color_identity(name = "Metrics",
                             breaks = c('black', 'red'),
                             labels = c("Observed", "Predicted"),
                             guide = "legend") +
        ggnewscale::new_scale_color() +
        geom_line(data, mapping = aes(x = time, y = flow_stat_1, colour = '#56B4E9')) +
        geom_line(data, mapping = aes(x = time, y = flow_stat_2, colour = '#0072B2')) +
        scale_color_identity(name = "Flow Statistics",
                             breaks = c('#56B4E9', '#0072B2'),
                             labels = c(flow_stat[1], flow_stat[2]),
                             guide = "legend") +
        scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_1)/rangeratio_1, name = biol_metric)) +
        labs(x = time_col, y = "") +
        #ggtitle(biol_metric[3]) +
        theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                        hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              legend.position = "bottom",
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              axis.title.y.left = element_text(size = 7),
              axis.title.y.right = element_text(size = 7)) +
        facet_wrap(~ sites, ncol = ncol)

      }

      if(is.na(flow_stat[2]) == TRUE){

        # plot - 1 flow stat

        plot_1 <- data %>%
          ggplot2::ggplot() +
          geom_ribbon(data, mapping = aes(x = time, ymin = y4trans, ymax = y5trans), fill = "pink", alpha=.5) +
          geom_line(data, mapping = aes(x = time, y = y3trans, colour = "red")) +
          geom_point(data, mapping = aes(x = time, y = y2trans, colour = "black")) +
          scale_color_identity(name = "Metrics",
                               breaks = c('black', 'red'),
                               labels = c("Observed", "Predicted"),
                               guide = "legend") +
          ggnewscale::new_scale_color() +
          geom_line(data, mapping = aes(x = time, y = flow_stat_1, colour = '#56B4E9')) +
          scale_color_identity(name = "Flow Statistics",
                               breaks = c('#56B4E9'),
                               labels = c(flow_stat[1]),
                               guide = "legend") +
          scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_1)/rangeratio_1, name = biol_metric)) +
          labs(x = time_col, y = "") +
          #ggtitle(biol_metric[3]) +
          theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                          hjust = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 7),
                legend.position = "bottom",
                axis.text.x = element_text(size = 6),
                axis.text.y = element_text(size = 6),
                axis.title.x = element_text(size = 7),
                axis.title.y.left = element_text(size = 7),
                axis.title.y.right = element_text(size = 7)) +
          facet_wrap(~ sites, ncol = ncol)

      }

    }

  # option to save

  if(save == TRUE){
    ggsave(paste0(save_dir, sep = "/", "Predictions_Plot.png"), plot = plot_1)}

  return(plot_1)

}
