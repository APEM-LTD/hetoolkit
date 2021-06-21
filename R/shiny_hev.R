#' Produces an interactive time series plot of biology and flow data for one site of interest.
#'
#' @description
#' The shiny_hev function launches a shiny app of:
#'  - If multiplot = TRUE, a multiplot plotly plot with:
#'    (i)	a drop-down list to select the site of interest.
#'
#' - If multiplot = FALSE, a plotly plot with:
#'    (i)	a drop-down list to select the site of interest, and
#'    (ii)	a drop-down list to select which biol_metric to plot.
#'
#' @usage
#' shiny_hev(data, sites_col, date_col, flow_stat, biol_metric, multiplot = TRUE)
#'
#' @param data Data frame, containing the data to plot.
#' @param sites_col Name of variable defining site ID.
#' @param date_col Name of column containing time variable.
#' @param flow_stat Flow statistics of interest; two should be chosen.
#' @param biol_metric Biology metrics of interest; up to four can be chosen.
#' @param multiplot If TRUE: a multiplot of up to four biology metrics is displayed;
#'   If FALSE: a single  plot is displayed.  Default = TRUE.
#'
#' @return Launches a shiny app containing an interactive time series plot of biology and flow data.
#'
#' @export
#'
#' @examples
#' ## Multi-panel plot
#'
#' # shiny_hev(data = all_data,
#' #            sites_col = "biol_site_id",
#' #            date_col = "Year",
#' #            flow_stat = c("Q10z", "Q95z"),
#' #            biol_metric = c("WHPT_ASPT_OE", "LIFE_F_OE"),
#' #            multiplot = TRUE)



shiny_hev <- function(data,
                      sites_col,
                      date_col,
                      flow_stat,
                      biol_metric,
                      multiplot = TRUE){

  if(is.data.frame(data) == FALSE){stop("Data frame 'data' not found")}
  if((date_col %in% colnames(data)) == FALSE)
    {stop("Specified date column was not identified in 'data'")}
  if((sites_col %in% colnames(data)) == FALSE)
    {stop("Specified sites column was not identified in 'data'")}
  if(all(unique(flow_stat) %in% colnames(data)) == FALSE)
    {stop("Specified flow statistics were not identified in 'data'")}
  if(all(unique(biol_metric) %in% colnames(data)) == FALSE)
    {stop("Specified biology metrics were not identified in 'data'")}

  # Pull in the correct data columns

  data$flow_stat_1 <- dplyr::pull(data, flow_stat[1])

  if(is.na(flow_stat[2]) == FALSE){
  data$flow_stat_2 <- dplyr::pull(data, flow_stat[2])}

  if(is.na(flow_stat[2]) == TRUE){
    data$flow_stat_2 <- NA}

  data$plot_date <- dplyr::pull(data, date_col)
  data$sites <- dplyr::pull(data, sites_col)
  data$sites <- as.factor(data$sites)

  data$biol_metric_1 <- dplyr::pull(data, biol_metric[1])

  if(is.na(biol_metric[2]) == FALSE){

    data$biol_metric_2 <- dplyr::pull(data, biol_metric[2])}

  if(is.na(biol_metric[3]) == FALSE){

    data$biol_metric_3 <- dplyr::pull(data, biol_metric[3])}

  if(is.na(biol_metric[4]) == FALSE){

    data$biol_metric_4 <- dplyr::pull(data, biol_metric[4])}


  if(multiplot == TRUE){

  # Set-up UI

  ui <- shiny::fluidPage(
    plotly::plotlyOutput("plot1", height = "75vh"),

    # Select Site Input
    shiny::selectInput("select", "Select Site",
                       choices = (unique(data$sites))
    ))

  # Server

  server <- function(input, output){

    dataInput <- shiny::reactive({

      filter(data, sites == input$select)

    })

    output$plot1 <- plotly::renderPlotly({

      if(is.na(biol_metric[1]) == FALSE){

       p1 <- plot_ly() %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), name = flow_stat[1]) %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), name = flow_stat[2]) %>%
          add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_1, marker = list(color = "black"), yaxis = "y2",
                    name = biol_metric[1], type = "scatter") %>%
          layout(
            xaxis = NULL,
            yaxis = list(side="left", title = 'Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
            yaxis2 = list(side="right", title = biol_metric[1], overlaying = "y", zeroline = F, hoverformat = '.3f', showgrid=F),
            legend = list(orientation = 'h'),
            margin = list(r=50))

      }

      if(is.na(biol_metric[2]) == FALSE){

    p2  <- plot_ly() %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), showlegend = FALSE) %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), showlegend = FALSE) %>%
          add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_2, marker = list(color = "black"), yaxis = "y2",
                    name = biol_metric[2], type = "scatter") %>%
          layout(
            xaxis = NULL,
            yaxis = list(side="left", title = 'Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
            yaxis2 = list(side="right", title = biol_metric[2], overlaying = "y3", zeroline = F, hoverformat = '.3f', showgrid=F),
            legend = list(orientation = 'h'),
            margin = list(r=50))
      }

      if(is.na(biol_metric[3]) == FALSE){

    p3 <- plot_ly() %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), showlegend = FALSE) %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), showlegend = FALSE) %>%
          add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_3, marker = list(color = "black"), yaxis = "y2",
                    name = biol_metric[3], type = "scatter") %>%
          layout(
            xaxis = NULL,
            yaxis = list(side="left", title = 'Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
            yaxis2 = list(side="right", title = biol_metric[3], overlaying = "y5", zeroline = F, hoverformat = '.3f', showgrid=F),
            legend = list(orientation = 'h'),
            margin = list(r=50))
      }

      if(is.na(biol_metric[4]) == FALSE){

        p4 <- plot_ly() %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), name = flow_stat[1]) %>%
          add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), name = flow_stat[2]) %>%
          add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_4, marker = list(color = "black"), yaxis = "y2",
                    name = biol_metric[4], type = "scatter") %>%
          layout(
            xaxis = NULL,
            yaxis = list(side="left", title = 'Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
            yaxis2 = list(side="right", title = biol_metric[4], overlaying = "y7", zeroline = F, hoverformat = '.3f', showgrid=F),
            legend = list(orientation = 'h'),
            margin = list(r=50))
      }


      if(length(biol_metric) == 1){
        a <- p1 }

      if(length(biol_metric) == 2){
        a <- subplot(p1, p2, titleY = TRUE, margin = 0.1)}

      if(length(biol_metric) == 3){
        a <- subplot(p1, p2, p3, nrows = 2, titleY = TRUE, margin = 0.1)}

      if(length(biol_metric) == 4){
        a <- subplot(p1, p2, p3, p4, nrows = 2, titleY = TRUE, margin = 0.1)}

      print(a)

    })

  }

  shiny::shinyApp(ui, server)

  }

  else {

      # Set-up UI

      ui <- shiny::fluidPage(
        plotly::plotlyOutput("plot1", height = "75vh"),

        # Select Site Input
        shiny::selectInput("select", "Select Site",
                           choices = (unique(data$sites))),

        # Select Metric Input
        shiny::selectInput("metric", "Select Metric",
                            c(biol_metric[1],
                              if(is.na(biol_metric[2]) != TRUE){(biol_metric[2])},
                              if(is.na(biol_metric[3]) != TRUE){(biol_metric[3])},
                              if(is.na(biol_metric[4]) != TRUE){(biol_metric[4])}))
              )

      # Server

      server <- function(input, output){

        dataInput <- shiny::reactive({

          filter(data, sites == input$select)

        })


        output$plot1 <- plotly::renderPlotly({

          if (isTRUE(input$metric == biol_metric[2]) == TRUE){

            plot_ly() %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), name = flow_stat[1]) %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), name = flow_stat[2]) %>%
              add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_2, marker = list(color = "black"), yaxis = "y2",
                        name = biol_metric[2], type = "scatter") %>%
              layout(
                title = biol_metric[2],
                xaxis = NULL,
                yaxis = list(side="left", title = 'Standardised Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
                yaxis2 = list(side="right", title = 'OE Ratio', overlaying = "y", zeroline = F, hoverformat = '.3f', showgrid=F),
                legend = list(orientation = 'h'),
                margin = list(r=50))

          }

          else if (isTRUE(input$metric == biol_metric[3]) == TRUE){

            plot_ly() %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), name = flow_stat[1]) %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), name = flow_stat[2]) %>%
              add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_3, marker = list(color = "black"), yaxis = "y2",
                        name = biol_metric[3], type = "scatter") %>%
              layout(
                title = biol_metric[3],
                xaxis = NULL,
                yaxis = list(side="left", title = 'Standardised Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
                yaxis2 = list(side="right", title = 'OE Ratio', overlaying = "y", zeroline = F, hoverformat = '.3f', showgrid=F),
                legend = list(orientation = 'h'),
                margin = list(r=50))
          }

          else if (isTRUE(input$metric == biol_metric[4]) == TRUE){

            plot_ly() %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), name = flow_stat[1]) %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), name = flow_stat[2]) %>%
              add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_4, marker = list(color = "black"), yaxis = "y2",
                        name = biol_metric[4], type = "scatter") %>%
              layout(
                title = biol_metric[4],
                xaxis = NULL,
                yaxis = list(side="left", title = 'Standardised Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
                yaxis2 = list(side="right", title = 'OE Ratio', overlaying = "y", zeroline = F, hoverformat = '.3f', showgrid=F),
                legend = list(orientation = 'h'),
                margin = list(r=50))
          }

          else {

            plot_ly() %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_1, line = list(color = '#56B4E9'), name = flow_stat[1]) %>%
              add_lines(x = dataInput()$plot_date, y = dataInput()$flow_stat_2, line = list(color = '#0072B2'), name = flow_stat[2]) %>%
              add_trace(x = dataInput()$plot_date, y = dataInput()$biol_metric_1, marker = list(color = "black"), yaxis = "y2",
                        name = biol_metric[1], type = "scatter") %>%
              layout(
                title = biol_metric[1],
                xaxis = NULL,
                yaxis = list(side="left", title = 'Standardised Flow', zeroline = F, hoverformat = '.2f', showgrid=F),
                yaxis2 = list(side="right", title = 'OE Ratio', overlaying = "y", zeroline = F, hoverformat = '.3f', showgrid=F),
                legend = list(orientation = 'h'),
                margin = list(r=50))
          }

        })

      }

      shiny::shinyApp(ui, server)

  }

}


