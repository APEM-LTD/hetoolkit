#' Generating a variety of diagnostic plots for a mixed-effects regression (lmer) model
#'
#' @description Generates a variety of diagnostic plots for a mixed-effects regression (lmer) model.
#'
#' @usage
#' diag_lmer(model, data, facet_by = NULL, colour_by = NULL, order_by = "mean", order = "ascending", ncol = 4, scales = "fixed", save = FALSE, save_dir = getwd(), ...)
#'
#' @param model A fitted linear mixed-effects lmer model of class `lmerMod`.
#' @param data A data frame or tibble containing the model calibration dataset.
#' @param facet_by A factor by which to facet wrap the plots (e.g. site). Default = `NULL`.
#' @param colour_by A factor by which to colour-code data points (e.g. season). Default = `NULL`.
#' @param order_by Method specifying how to order the levels of the faceting factor. Two options: `mean` of the fitted values, or `variance` of the residuals. Default = `NULL`. (levels ordered alphabetically). Ignored if `facet_by` = `NULL`.
#' @param order Command to arrange plots in `ascending` (default) or `descending` order of facet_by. Ignored if `facet_by` = `NULL`.
#' @param ncol Number of columns to use in facet plots. Default = 4. Ignored if `facet_by` = `NULL`.
#' @param scales When faceting, Specifies whether plotting scales should be fixed (`fixed`), free (`free`), or free in one dimension (`free_x`, `free_y`). Default = `fixed`. Ignored if `facet_by` = `NULL`.
#' @param save Specifies if output plot should be saved as a png file. Default = `FALSE`.
#' @param save_dir Path to folder where png file should be saved. Default = Current working directory.
#' @param ... Provision to include additional arguments.
#'
#' @details The following diagnostic plots are produced:
#' 1.	Fitted vs observed values (all records, or faceted).
#' 2.	Normal probability plot (all records, or faceted).
#' 3.	Residuals vs fitted (all records or faceted).
#' 4.	Histogram of residuals (all records) or boxplot of residuals (faceted).
#' 5.	Residuals vs model fixed predictor(s) (all records only). This displays a linear model (red) for each fixed predictor vs the model's residuals. The blue line is a loess-smoothed line. The main purpose of this plot is to check whether or not the relationship between residuals and a predictor is linear.
#'
#' @return A list of five ggplot objects. These objects can optionally be saved as five separate png files.
#'
#' @export
#'
#' @examples
#' ## Example 1
#' # attach(lme4)
#' # sleepstudy$type <- rep(c("A","B","C"),60)
#'
#' # run model
#' # mod <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # run diagnostic plots - with facet option
#' # diag_lmer(model = mod, data = sleepstudy, facet_by = "Subject", order_by = "mean", order = "ascending", ncol = 4, scales = "fixed", colour_by = "type")
#'
#' ## Example 2 - without facet option, and saving to png file
#' # diag_lmer(model = mod, data = sleepstudy, facet_by = NULL, order_by = "mean", order = "ascending", ncol = 4, scales = "fixed", colour_by = "type", save = TRUE)



diag_lmer <- function(model,
          data,
          facet_by = NULL,
          colour_by = NULL,
          order_by = "mean",
          order = "ascending",
          ncol = 4,
          scales = "fixed",
          save = FALSE,
          save_dir = getwd(),
          ...) {


  ## errors:

  # make sure model object of the correct class has been input:
  if(is.null(model)|| !class(model)[1]=="lmerMod") stop("please specify a lmer class of model")

  # make sure the correct data and settings have been input:
  if(is.null(data)) {stop("data missing, with no default")}
  if(!is.data.frame(data)) {stop("data input must be a dataframe or tibble")}
  if(!is.null(facet_by) && facet_by %in% colnames(data) == FALSE) {
    stop("facet_by name not within supplied dataframe, revise facet_by name or add to datafame")}
  if(!is.null(colour_by) && colour_by %in% colnames(data) == FALSE) {
    stop("colour_by name not within supplied dataframe, revise colour_by name or add to datafame")}
  if(!is.null(facet_by) && order_by %in% c("mean", "variance") == FALSE) {stop("'order_by' must be either 'mean' or 'variance'")}
  if(!is.null(facet_by) && order %in% c("ascending", "descending") == FALSE) {stop("'order' must be either 'ascending' or 'descending'")}
  if(!is.null(facet_by) && scales %in% c("fixed", "free_x", "free_y", "free") == FALSE) {stop("'scales' must be one of 'fixed', 'free_x', 'free_y', or 'free'")}

  # check save settings are valid
  if(file.exists(save_dir) == FALSE) {stop("Specified save directory does not exist")}
  if(is.logical(save) == FALSE) {stop("Save is not logical")}


  ## prepare data for plotting

  # extract fitted values and residuals from model
  data$fitted <- fitted(model)
  data$resid <- residuals(model)

  # create columns with generic names for easy plotting
  data$y <- model@frame[,1]

  data$colour_by <- data[,colour_by]

  # create a facet_by factor
  if(!is.null(facet_by)){
    data$facet_by <- dplyr::pull(data, facet_by)
    data$facet_by <- as.factor(data$facet_by)
    # re-order by the mean of the fitted value
    if(order_by=="mean"){
      data <- data %>% dplyr::mutate(facet_by = forcats::fct_reorder(
        facet_by, fitted, .fun='mean', .desc = ifelse(order == "ascending",FALSE,TRUE)))
    } else {
         # re-order by the variance of the residuals
      if(order_by=="variance"){
        data %>% dplyr::mutate(facet_by = forcats::fct_reorder(
            facet_by, resid, .fun='var', .desc = ifelse(order == "ascending",FALSE,TRUE)))
      } else {
        # original (alphabetical/numerical) order
        data$facet_by <- factor(data$facet_by)
      }
    }
  }

  ## create empty list for plots
  p_out <- list()

  # 1A. fitted vs observed (all data)
  p <- ggplot2::ggplot(data, aes(x = y, y = fitted, if(!is.null(colour_by)) {colour = colour_by})) +
            geom_point() +
            xlab("Observed") + ylab("Fitted") +
            labs(title = "Fitted vs observed values",
                 subtitle = "Matching values would be on the dotted 1:1 line",
                 colour = stringr::str_to_title(colour_by)) +
            geom_abline(intercept = 0, slope=1, size = 0.5, colour = "black", linetype="dashed") +
            geom_smooth(method='lm', formula= y~x, se=FALSE) +
            theme(legend.key = element_rect(fill = NA, color = NA))

  if(is.null(facet_by)) p_out[[1]]<-p

  # 1B. fitted vs observed (faceted)
  if(!is.null(facet_by)){
    p <- p + facet_wrap(~facet_by, scales = scales, ncol = ncol) +
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.margin=margin(0,0,0,0), legend.box.margin=margin(-10,-10,0,-10))

    p_out[[1]]<-p

  }

  # 2A. Normal probability plot (all data)
  p <- if(is.null(colour_by)) {ggplot2::ggplot(data, aes(sample = resid))
        } else {ggplot2::ggplot(data, aes(sample = resid, colour = colour_by))}
  p <-  p + stat_qq() +
            stat_qq_line() +
            xlab("Theoretical values") + ylab("Standardised residuals") +
            labs(title = "Non-normality of residuals, and outliers",
                 subtitle = "Points should fall along a straight diagonal line",
                 colour=stringr::str_to_title(colour_by)) +
            theme(legend.key = element_rect(fill = NA, color = NA))

  if(is.null(facet_by)) p_out[[2]]<-p

  # 2B. Normal probability plot (faceted)
  if(!is.null(facet_by)){
    p<- p + facet_wrap(.~facet_by, scales = scales, ncol = ncol) +
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.margin=margin(0,0,0,0), legend.box.margin=margin(-10,-10,0,-10))
    p_out[[2]]<-p

  }

  # 3A. Residuals vs Fitted (all data)
  p <- ggplot2::ggplot(data, aes(x = fitted, y = resid, if(!is.null(colour_by)){colour=colour_by})) +
              geom_point() +
              xlab("Fitted") + ylab("Residuals") +
              labs(title = "Variance of residuals",
                   subtitle = word_wrap("Residuals should be centred around 0 and have a similar amount of vertical scatter at all fitted values",50),
                   colour=stringr::str_to_title(colour_by)) +
              geom_hline(yintercept=0, linetype="dashed", colour="black") +
              theme(legend.key = element_rect(fill = NA, color = NA))

  if(is.null(facet_by)) p_out[[3]]<-p

  # 3B. Residuals vs Fitted (faceted)
  if(!is.null(facet_by)){
    p<- p + facet_wrap(~facet_by, scales = scales, ncol = ncol) +
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.margin=margin(0,0,0,0), legend.box.margin=margin(-10,-10,0,-10))
    p_out[[3]]<-p

  }

  # 4A. Histogram of residuals (all data) - I prefer this to a single boxplot.
  p <- sjPlot::plot_model(model, type = "diag")[[3]]
  if(is.null(facet_by)) p_out[[4]]<-p

  # 4B. Boxplot of residuals by Group, to check for equal variances
  if(!is.null(facet_by)){
    p <- ggplot2::ggplot(data, aes(facet_by, resid)) +
                geom_boxplot() + xlab(stringr::str_to_title(facet_by)) + ylab("Residuals") +
                labs(title = "Boxplot of residuals by group",
                     subtitle = "Distribution should have equal variance centred on zero") +
                geom_hline(yintercept=0, linetype="dashed", colour="black") +
                coord_flip()
    p_out[[4]]<-p

  }

  # 5. Plot of residuals vs each fixed predictor
  p <- sjPlot::plot_model(model, type = 'resid', grid = TRUE, show.data = TRUE)
  p <- p + labs(title = paste0("Residuals vs fixed predictor",ifelse(dim(model@pp$X)[2]>2,"s","")), y = "Residuals")
  p_out[[5]] <- p

  # save plots as png files
  if(save==TRUE){
      for(i in 1:length(p_out)){
          ggplot2::ggsave(plot = p_out[[i]], path = save_dir, filename = paste0("Diagnostic_plot_",ifelse(!is.null(facet_by),"faceted_",""),i,".png"))
      }
  }

  return(p_out)

}
