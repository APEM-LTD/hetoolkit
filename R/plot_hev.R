#' Produces a time series plot of biology and flow data for one site of interest.
#'
#' @description
#' Intended for plotting data for one site at a time; data may need to be subset or filtered
#' by user prior to plotting.
#'
#' The function generates a ggplot object, that is optionally saved as a .png file.
#' If multiplot = TRUE, the png file is named "Multi_Plot.png". If multiplot = FALSE, a separate png file is produced for each biology metric, named "biolmetric_Plot.png".
#'
#' @usage
#' plot_hev <- function(data, date_col, flow_stat, biol_metric, multiplot = TRUE, save = FALSE, save_dir = getwd(), clr_by = NULL)
#'
#' @param data Data to plot.
#' @param date_col Name of column containing time variable for x-axis.
#' @param flow_stat Names of up to two flow statistics of interest.
#' @param biol_metric Names of up to four biology metrics of interest.
#' @param multiplot If TRUE, a multiplot of up to four biology metrics is produced;
#'   if FALSE, a single plot is produced for each biology metric. Default = TRUE.
#' @param save Specifies if outputs should also be saved as png files. Default = FALSE.
#' @param save_dir Path to folder where png files are to be saved;
#'  Default = Current working directory.
#' @param clr_by Selected variable by which to colour biol_metric points. Default = NULL.
#'
#' @return Depending on the multiplot argument, the function returns either a single ggplot
#' object, or a list of ggplot objects.
#'
#' @examples
#' ## Generate hev plots for three biology metrics at one site of interest
#' # plot_hev(data = subset(all_data, biol_site_id = "100582"),
#' #            date_col = "Year",
#' #            flow_stat = c("PRQ75", "PRQ10"),
#' #            biol_metric = "LIFE_F_OE",,
#' #            multiplot = TRUE,
#' #            clr_by = Season))



plot_hev <- function(data,
                     date_col,
                     flow_stat,
                     biol_metric,
                     multiplot = TRUE,
                     save = FALSE,
                     save_dir = getwd(),
                     clr_by = NULL){


  if(is.data.frame(data) == FALSE){stop("Data frame 'data' not found")}

  if((date_col %in% colnames(data)) == FALSE)
  {stop("Specified date column was not identified in 'data'")}

  if(all(unique(flow_stat) %in% colnames(data)) == FALSE)
  {stop("Specified flow statistics were not identified in 'data'")}

  if(all(unique(biol_metric) %in% colnames(data)) == FALSE)
  {stop("Specified biology metrics were not identified in 'data'")}

  if(is.logical(multiplot) == FALSE) {stop("multiplot is not logical")}

  if(is.logical(save) == FALSE) {stop("Save is not logical")}

  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}

  if(length(biol_metric) > 4){stop("More then 4 biology metrics have been selected")}

  if(length(flow_stat) > 2){stop("More then 2 flow statistics have been selected")}

  if(is.null(clr_by) == FALSE && isTRUE(clr_by %in% names(data)) == FALSE)
  {stop("clr_by variable does not exist in data")}

  # Pull-in data

    data$flow_stat_1 <- dplyr::pull(data, flow_stat[1])

      if(is.numeric(data$flow_stat_1) == FALSE)
        {stop("Selected flow_stat is non-numeric")}

    if(is.na(flow_stat[2]) == FALSE){

      data$flow_stat_2 <- dplyr::pull(data, flow_stat[2])

      if(is.numeric(data$flow_stat_2) == FALSE)
      {stop("Second flow_stat is non-numeric")}

      }

    if(is.na(flow_stat[2]) == TRUE){

      data$flow_stat_2 <- NA}

    data$biol_metric_1 <- dplyr::pull(data, biol_metric[1])

    if(is.numeric(data$biol_metric_1) == FALSE)
    {stop("Selected biol_metric is non-numeric")}

    if(is.na(biol_metric[2]) == FALSE){

      data$biol_metric_2 <- dplyr::pull(data, biol_metric[2])

      if(is.numeric(data$biol_metric_2) == FALSE)
      {stop("Second biol_metric is non-numeric")}

      }

    if(is.na(biol_metric[3]) == FALSE){

      data$biol_metric_3 <- dplyr::pull(data, biol_metric[3])

      if(is.numeric(data$biol_metric_3) == FALSE)
      {stop("Third biol_metric is non-numeric")}

      }

    if(is.na(biol_metric[4]) == FALSE){

      data$biol_metric_4 <- dplyr::pull(data, biol_metric[4])

      if(is.numeric(data$biol_metric_4) == FALSE)
      {stop("Fourth biol_metric is non-numeric")}

      }

    data$plot_date <- dplyr::pull(data, date_col)

    if(is.null(clr_by) == FALSE){
    data$clr_by <- dplyr::pull(data, clr_by)}

    # Set-up data transformation

    # get y1 range

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

    # Get y2 range

    y2_min <- min(data$biol_metric_1, na.rm = TRUE)
    y2_max <- max(data$biol_metric_1, na.rm = TRUE)
    y2_range <- y2_max - y2_min

    # range ratio
    rangeratio_1 <- y1_range/y2_range
    minadj_1 <- y1_min - y2_min*rangeratio_1

    data$y2trans <- data$biol_metric_1 * rangeratio_1 + minadj_1

    #Define colours
    #cols <- RColorBrewer::brewer.pal(8, "Dark2")

    if(is.na(flow_stat[2]) == FALSE){

    # First plot

    p1 <- data %>%
          ggplot2::ggplot() +
      geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
      geom_line(data, mapping = aes(x = plot_date, y = flow_stat_2, colour = '#0072B2')) +
      scale_color_identity(name = "Flow Statistics",
                           breaks = c('#56B4E9', '#0072B2'),
                           labels = c(flow_stat[1], flow_stat[2]),
                           guide = "legend") +
      ggnewscale::new_scale_color() +
      geom_point(data, mapping = aes(x = plot_date, y = y2trans, colour = clr_by)) +
      scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_1)/rangeratio_1,
                                                            name = biol_metric[1])) +
      labs(x = date_col, y = "") +
      #ggtitle(biol_metric[1]) +
      theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                      hjust = 0.5),
            legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            axis.title.x = element_text(size = 7),
            axis.title.y.left = element_text(size = 7),
            axis.title.y.right = element_text(size = 7))

    if(is.na(biol_metric[2]) == FALSE){

      # Get y3 range

      y3_min <- min(data$biol_metric_2, na.rm = TRUE)
      y3_max <- max(data$biol_metric_2, na.rm = TRUE)
      y3_range <- y3_max - y3_min

      # range ratio
      rangeratio_2 <- y1_range/y3_range
      minadj_2 <- y1_min - y3_min*rangeratio_2

      data$y3trans <- data$biol_metric_2 * rangeratio_2 + minadj_2

      p2 <- data %>%
        ggplot2::ggplot() +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_2, colour = '#0072B2')) +
        scale_color_identity(name = "Flow Statistics",
                             breaks = c('#56B4E9', '#0072B2'),
                             labels = c(flow_stat[1], flow_stat[2]),
                             guide = "legend") +
        ggnewscale::new_scale_color() +
        geom_point(data, mapping = aes(x = plot_date, y = y3trans, colour = clr_by)) +
        scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_2)/rangeratio_2, name = biol_metric[2])) +
        labs(x = date_col, y = "") +
        #ggtitle(biol_metric[2]) +
        theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                        hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              axis.title.y.left = element_text(size = 7),
              axis.title.y.right = element_text(size = 7))

    }

    if(is.na(biol_metric[3]) == FALSE){

      # Get y4 range

      y4_min <- min(data$biol_metric_3, na.rm = TRUE)
      y4_max <- max(data$biol_metric_3, na.rm = TRUE)
      y4_range <- y4_max - y4_min

      # range ratio
      rangeratio_3 <- y1_range/y4_range
      minadj_3 <- y1_min - y4_min*rangeratio_3

      data$y4trans <- data$biol_metric_3 * rangeratio_3 + minadj_3


      p3 <- data %>%
        ggplot2::ggplot() +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_2, colour = '#0072B2')) +
        scale_color_identity(name = "Flow Statistics",
                             breaks = c('#56B4E9', '#0072B2'),
                             labels = c(flow_stat[1], flow_stat[2]),
                             guide = "legend") +
        ggnewscale::new_scale_color() +
        geom_point(data, mapping = aes(x = plot_date, y = y4trans, colour = clr_by)) +
        scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_3)/rangeratio_3, name = biol_metric[3])) +
        labs(x = date_col, y = "") +
        #ggtitle(biol_metric[3]) +
        theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                        hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              axis.title.y.left = element_text(size = 7),
              axis.title.y.right = element_text(size = 7))

    }

    if(is.na(biol_metric[4]) == FALSE){

      # Get y5 range

      y5_min <- min(data$biol_metric_4, na.rm = TRUE)
      y5_max <- max(data$biol_metric_4, na.rm = TRUE)
      y5_range <- y5_max - y5_min

      # range ratio
      rangeratio_4 <- y1_range/y5_range
      minadj_4 <- y1_min - y5_min*rangeratio_4

      data$y5trans <- data$biol_metric_4 * rangeratio_4 + minadj_4

      p4 <- data %>%
        ggplot2::ggplot() +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_2, colour = '#0072B2')) +
        scale_color_identity(name = "Flow Statistics",
                             breaks = c('#56B4E9', '#0072B2'),
                             labels = c(flow_stat[1], flow_stat[2]),
                             guide = "legend") +
        ggnewscale::new_scale_color() +
        geom_point(data, mapping = aes(x = plot_date, y = y5trans, colour = clr_by)) +
        scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_3)/rangeratio_3, name = biol_metric[4])) +
        labs(x = date_col, y = "") +
        #ggtitle(biol_metric[3]) +
        theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                        hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              axis.title.y.left = element_text(size = 7),
              axis.title.y.right = element_text(size = 7))

    }

    }

    if(is.na(flow_stat[2]) == TRUE){


      # First plot

      p1 <- data %>%
        ggplot2::ggplot() +
        geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
        scale_color_identity(name = "Flow Statistics",
                             breaks = c('#56B4E9'),
                             labels = c(flow_stat[1]),
                             guide = "legend") +
        ggnewscale::new_scale_color() +
        geom_point(data, mapping = aes(x = plot_date, y = y2trans, colour = clr_by)) +
        scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_1)/rangeratio_1,
                                                              name = biol_metric[1])) +
        labs(x = date_col, y = "") +
        #ggtitle(biol_metric[1]) +
        theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                        hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              axis.title.y.left = element_text(size = 7),
              axis.title.y.right = element_text(size = 7))

      if(is.na(biol_metric[2]) == FALSE){

        # Get y3 range

        y3_min <- min(data$biol_metric_2, na.rm = TRUE)
        y3_max <- max(data$biol_metric_2, na.rm = TRUE)
        y3_range <- y3_max - y3_min

        # range ratio
        rangeratio_2 <- y1_range/y3_range
        minadj_2 <- y1_min - y3_min*rangeratio_2

        data$y3trans <- data$biol_metric_2 * rangeratio_2 + minadj_2

        p2 <- data %>%
          ggplot2::ggplot() +
          geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
          scale_color_identity(name = "Flow Statistics",
                               breaks = c('#56B4E9'),
                               labels = c(flow_stat[1]),
                               guide = "legend") +
          ggnewscale::new_scale_color() +
          geom_point(data, mapping = aes(x = plot_date, y = y3trans, colour = clr_by)) +
          scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_2)/rangeratio_2, name = biol_metric[2])) +
          labs(x = date_col, y = "") +
          #ggtitle(biol_metric[2]) +
          theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                          hjust = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 7),
                axis.text.x = element_text(size = 6),
                axis.text.y = element_text(size = 6),
                axis.title.x = element_text(size = 7),
                axis.title.y.left = element_text(size = 7),
                axis.title.y.right = element_text(size = 7))

      }

      if(is.na(biol_metric[3]) == FALSE){

        # Get y4 range

        y4_min <- min(data$biol_metric_3, na.rm = TRUE)
        y4_max <- max(data$biol_metric_3, na.rm = TRUE)
        y4_range <- y4_max - y4_min

        # range ratio
        rangeratio_3 <- y1_range/y4_range
        minadj_3 <- y1_min - y4_min*rangeratio_3

        data$y4trans <- data$biol_metric_3 * rangeratio_3 + minadj_3


        p3 <- data %>%
          ggplot2::ggplot() +
          geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
          scale_color_identity(name = "Flow Statistics",
                               breaks = c('#56B4E9'),
                               labels = c(flow_stat[1]),
                               guide = "legend") +
          ggnewscale::new_scale_color() +
          geom_point(data, mapping = aes(x = plot_date, y = y4trans, colour = clr_by)) +

          scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_3)/rangeratio_3, name = biol_metric[3])) +
          labs(x = date_col, y = "") +
          #ggtitle(biol_metric[3]) +
          theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                          hjust = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 7),
                axis.text.x = element_text(size = 6),
                axis.text.y = element_text(size = 6),
                axis.title.x = element_text(size = 7),
                axis.title.y.left = element_text(size = 7),
                axis.title.y.right = element_text(size = 7))

      }

      if(is.na(biol_metric[4]) == FALSE){

        # Get y5 range

        y5_min <- min(data$biol_metric_4, na.rm = TRUE)
        y5_max <- max(data$biol_metric_4, na.rm = TRUE)
        y5_range <- y5_max - y5_min

        # range ratio
        rangeratio_4 <- y1_range/y5_range
        minadj_4 <- y1_min - y5_min*rangeratio_4

        data$y5trans <- data$biol_metric_4 * rangeratio_4 + minadj_4

        p4 <- data %>%
          ggplot2::ggplot() +
          geom_line(data, mapping = aes(x = plot_date, y = flow_stat_1, colour = '#56B4E9')) +
          scale_color_identity(name = "Flow Statistics",
                               breaks = c('#56B4E9'),
                               labels = c(flow_stat[1]),
                               guide = "legend") +
          ggnewscale::new_scale_color() +
          geom_point(data, mapping = aes(x = plot_date, y = y5trans, colour = clr_by)) +
          scale_y_continuous(name = "Flow", sec.axis = sec_axis(~ (. - minadj_3)/rangeratio_3, name = biol_metric[4])) +
          labs(x = date_col, y = "") +
          #ggtitle(biol_metric[3]) +
          theme(plot.title = element_text(color = "black", size = 10, face = "bold",
                                          hjust = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 7),
                axis.text.x = element_text(size = 6),
                axis.text.y = element_text(size = 6),
                axis.title.x = element_text(size = 7),
                axis.title.y.left = element_text(size = 7),
                axis.title.y.right = element_text(size = 7))
      }

    }

    if(length(biol_metric) == 1){

      print(p1)

      if(save == TRUE){
        ggsave(paste0(save_dir, sep = "/", biol_metric[1], "_Plot.png"), plot = p1)}

      hevPlot <- p1

      return(hevPlot)

    }

    if(length(biol_metric) == 2 && multiplot == FALSE){

      print(p1)
      print(p2)

      if(save == TRUE){
        ggsave(paste0(save_dir, sep = "/", biol_metric[1], "_Plot.png"), plot = p1)
        ggsave(paste0(save_dir, sep = "/", biol_metric[2], "_Plot.png"), plot = p2)}

      hevPlot <- list(p1, p2)
      return(hevPlot)

      }

      if(length(biol_metric) == 2 && multiplot == TRUE){

        plot_a <- ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")

        if(save == TRUE){
        ggsave(paste0(save_dir, sep = "/", "Multi_Plot.png"), plot = plot_a)}

        return(plot_a)

      }


    if(length(biol_metric) == 3 && multiplot == FALSE){

      print(p1)
      print(p2)
      print(p3)

      if(save == TRUE){
        ggsave(paste0(save_dir, sep = "/", biol_metric[1], "_Plot.png"), plot = p1)
        ggsave(paste0(save_dir, sep = "/", biol_metric[2], "_Plot.png"), plot = p2)
        ggsave(paste0(save_dir, sep = "/", biol_metric[3], "_Plot.png"), plot = p3)}

      hevPlot <- list(p1, p2, p3)
      return(hevPlot)

      }

      if(length(biol_metric) == 3 && multiplot == TRUE){

        plot_a <- ggpubr::ggarrange(p1, p2, p3, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

      if(save == TRUE){
        ggsave(paste0(save_dir, sep = "/", "Multi_Plot.png"), plot = plot_a)}

      return(plot_a)

    }

    if(length(biol_metric) == 4 && multiplot == FALSE){

      print(p1)
      print(p2)
      print(p3)
      print(p4)

      if(save == TRUE){
      ggsave(paste0(save_dir, sep = "/", biol_metric[1], "_Plot.png"), plot = p1)
      ggsave(paste0(save_dir, sep = "/", biol_metric[2], "_Plot.png"), plot = p2)
      ggsave(paste0(save_dir, sep = "/", biol_metric[3], "_Plot.png"), plot = p3)
      ggsave(paste0(save_dir, sep = "/", biol_metric[4], "_Plot.png"), plot = p4)}

      hevPlot <- list(p1, p2, p3, p4)
      return(hevPlot)

      }

      if(length(biol_metric) == 4 && multiplot == TRUE){

        plot_a <- ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

        if(save == TRUE){
          ggsave(paste0(save_dir, sep = "/", "Multi_Plot.png"), plot = plot_a)}

        return(plot_a)

      }

}
