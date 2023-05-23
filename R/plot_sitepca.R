#' Summarising environmental characteristics of biological sampling sites
#'
#' @description Performs PCA to summarise environmental characteristics of biology sampling sites.
#'
#' @usage plot_sitepca(data, vars, eigenvectors = FALSE, label_by, colour_by, plotly = FALSE, save = FALSE, save_dir = getwd(), ...)
#'
#' @param data A data frame of site-level environmental characteristics such as that produced by import_env.
#' @param vars A list of at least three continuous environmental variables from 'data'.
#' @param eigenvectors Logical option to add eigenvectors to PCA plot. Default = FALSE.
#' @param label_by Optional variable to label points (e.g. by site ID). Default = NULL.
#' @param colour_by Optional variable to colour points (e.g. by catchment). Default = NULL.
#' @param plotly Logical value specifying whether or not to render the plot as an interactive plotly plot. Default = FALSE.
#' @param save Logical value specifying whether or not output plot should be saved as a png file (called PCA_plot.png). Default = FALSE.
#' @param save_dir Path to folder where plot should be saved. Default = Current working directory.
#' @param ... Provision to include additional ggplot plotting and saving arguments, including for example: theme, file type, width and size. See ?theme and ?ggsave for details.
#'
#' @details The environmental variables listed in 'vars' must be numeric, and complete; sites with missing data will be excluded from the analysis. All variables are automatically centered to zero and scaled to have unit variance prior to analysis (see ?stats::prcomp for further details).
#'
#' The plot_sitepca function performs a PCA using stats::prcomp() and plots the z-scores of the first two principal components. Using label_by exchanges a point for a defining variable, such as site ID, which can help in identifying outliers.
#
#' Setting eigenvectors = TRUE will add these as arrows to the plot to indicate the direction and strength of correlation between the environmental variables and the principal components. Longer arrows indicate stronger correlations. These eigenvectors allow the axes to be interpreted as environmental gradients.
#'
#' @return Depending on the 'plotly' argument, either a ggplot or plotly plot displaying the   sites in 2D space to show site similarity and identify potential outliers.
#'
#' @export
#'
#' @export
#'
#' @examples
#' # df = read.csv("INV_OPEN_DATA_SITE.csv")
#'
#' # Produce and save PCA plot
#' # plot_sitepca(data = env_data, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), save = TRUE)
#'
#' # PCA plot with eigenvectors
#' # plot_sitepca(data = env_data, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), eigenvectors = TRUE)
#'
#' # PCA plot with points labeled
#' # plot_sitepca(data = env_data, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), label_by = "biol_site_id")



plot_sitepca<- function(data = NULL,
                        vars = NULL,
                        eigenvectors = FALSE,
                        label_by = NULL,
                        colour_by  = NULL,
                        plotly = FALSE,
                        save = FALSE,
                        save_dir = getwd(),
                        ...){

  # Errors:
  # make sure the right data had been input:
  if(is.null(data)) {stop("data missing, with no default")}
  if(!is.data.frame(data)) {stop("data input must be a dataframe")}

  # vars is not specified, missing from data, invalid format
  if(is.null(vars)) {stop("vars is missing, please specify")}
  if(is.character(vars)==FALSE) {stop("vars must be vector of character strings")}
  if(anyNA(vars)) {stop("vars contains NAs, not valid in list of vars")}
  if(length(vars %in% colnames(data))!=length(vars)) {stop("missmatch between vars names and names in input dataframe")}

  # label_by
  if(!is.null(label_by) && is.character(label_by)==FALSE) {stop("label_by must be character string")}
  if(!is.null(label_by) && length(label_by)>1) {stop("use only one variable as a label name")}
  if(!is.null(label_by) && !label_by %in% colnames(data)) {stop("missmatch between label_by name and names in input dataframe")}

  # colour_by
  if(!is.null(colour_by) && is.character(colour_by)==FALSE) {stop("colour_by must be character string or vector of character strings")}
  if(!is.null(colour_by) && length(colour_by %in% colnames(data))!=length(colour_by)) {stop("missmatch between colour_by names and names in input dataframe")}

  # check logical value for logical inputs
  if(is.logical(eigenvectors)==FALSE) {stop("eigenvectors must be a logical statement")}
  if(is.logical(plotly)==FALSE) {stop("plotly must be a logical statement")}

  # Check save settings are valid
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}

  # Format input data
  data <- as_tibble(data)

  # want to keep label_by and colour_by for site ID and water body for labels and groupings (--> z and colours in ggplot)...
  data <- subset(data, select = c(vars, label_by, colour_by))

  # Drop sites with NA for one or more variables
  if(nrow(data[!complete.cases(data), ])>0){
    warning(paste(nrow(data[!complete.cases(data), ]),"sites omitted due to incomplete data"))
  }
  data <- data[complete.cases(data), ]

  # Run PCA (dropping column with site ID)
  sitepca <- prcomp(data %>% dplyr::select(vars), center = TRUE, scale. = TRUE)

  # set label_id to rownames of sitepca output to use in plotting
  if(!is.null(label_by)){
    temp <- tibble::column_to_rownames(data, var = label_by)
    row.names(sitepca$x) <- row.names(temp)
  }

  # set plot characteristics according to parameters
  shape = 19 ; label.size = NULL; colour = NULL
  loadings = FALSE; loadings.label = FALSE; loadings.label.size = NULL; loadings.colour = NULL; loadings.label.colour = NULL

  # settings for using a label instead of a point
  if(!is.null(label_by)) {shape = FALSE ; label.size = 3}
  #if(plotly == TRUE) {z = label_by}

  # settings for adding eigenvector arrows
  if(eigenvectors==TRUE) {loadings = TRUE; loadings.label = TRUE; loadings.label.size = 3; loadings.colour = 'blue'; loadings.label.colour = "blue"}

  p <- ggplot2::autoplot(sitepca, shape = shape, label.size = label.size, loadings = loadings, loadings.label = loadings.label, loadings.label.size = loadings.label.size, loadings.colour = loadings.colour, loadings.label.colour = loadings.label.colour )

  # settings for adding grouping via colour
  if(!is.null(colour_by)){

    data<-data %>% dplyr::mutate_at(colour_by, as.character)

    cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    loadings.colour = "black"; loadings.label.colour = "black"
    p <- ggplot2::autoplot(sitepca, data=data, shape = shape, label.size = label.size, loadings = loadings, loadings.label = loadings.label, loadings.label.size = loadings.label.size, loadings.colour = loadings.colour, loadings.label.colour = loadings.label.colour, colour = colour_by) +
        labs(colour = stringr::str_to_title(sub("_"," ",colour_by))) +
        theme(legend.key = element_rect(fill = NA, color = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-10,-10,0,-10)) +
      scale_colour_manual(values=cbbPalette)
  }

  # p <- p + ggplot2::ggtitle("Bi-plot of principal components 1 and 2")

  # save plot
  if(save == TRUE){
    ggplot2::ggsave(plot = p, path = save_dir, filename = paste("PCA_plot.png", sep = "."))
  }

  # convert ggplot to plotly
  if (isTRUE(plotly)){
    p <- p + ggplot2::theme(legend.position = "right", legend.direction = "vertical")
    p <- plotly::ggplotly(p)
  }

  return(p)

}
