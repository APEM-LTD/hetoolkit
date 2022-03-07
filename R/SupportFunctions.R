library(ggplot2)
############################# strinterp ###################################

"stinterp" <-
  function (x, y, xout, yp, method = c("scaledstineman", "stineman",
                                       "parabola"))
  {
    if (missing(x) || missing(y) || missing(xout))
      stop("Wrong number of input arguments, x, y and xout must be specified")
    if (!is.vector(x) || !is.vector(y) || !is.vector(xout) ||
        !is.numeric(x) || !is.numeric(y) || !is.numeric(xout))
      stop("x, y and xout must be numeric vectors")
    if (length(x) < 2)
      stop("x must have 2 or more elements")
    if (length(x) != length(y))
      stop("x must have the same number of elements as y")
    if (any(is.na(x)) || any(is.na(x)) || any(is.na(xout)))
      stop("NAs in x, y or xout are not allowed")
    if (!missing(yp)) {
      if (!is.vector(yp) || !is.numeric(yp))
        stop("yp must be a numeric vector")
      if (length(y) != length(yp))
        stop("When specified, yp must have the same number of elements as y")
      if (any(is.na(yp)))
        stop("NAs in yp are not allowed")
      if (!missing(method))
        stop("Method should not be specified if yp is given")
    }
    dx <- diff(x)
    dy <- diff(y)
    if (any(dx <= 0))
      stop("The values of x must strictly increasing")

    #calculation of slopes if needed
    if (missing(yp)) {
      yp <- switch(match.arg(method), # this allows for partial argument matching
                   scaledstineman = stinemanSlopes(x,y, scale = TRUE),
                   stineman = stinemanSlopes(x, y, scale = FALSE),
                   parabola = parabolaSlopes(x, y))
    }

    # preparations
    m <- length(x)
    m1 <- m - 1
    s <- dy/dx
    k <- length(xout)

    ix <- findInterval(xout, x, rightmost.closed = TRUE)

    # For edgepoints allow extrapolation
    # within a tiny range (set by machine precision).

    epx <- 5 * (.Machine$double.eps) * diff(range(x))
    ix[min(x) - epx <= xout & xout <= min(x)] <- 1
    ix[max(x) <= xout & xout <= max(x) + epx] <- m1
    idx <- 1 <= ix & ix <= m1
    ix1 <- ix[idx]
    ix2 <- ix1 + 1

    # computation of the interpolant for the three cases dyo1dyo2 ==, > and < 0

    dxo1 <- xout[idx] - x[ix1]
    dxo2 <- xout[idx] - x[ix2]
    y0o <- y[ix1] + s[ix1] * dxo1
    dyo1 <- (yp[ix1] - s[ix1]) * dxo1
    dyo2 <- (yp[ix2] - s[ix1]) * dxo2
    dyo1dyo2 <- dyo1 * dyo2
    yo <- y0o
    if (m > 2 || !missing(yp)) { # linear interpolation is sufficient for m=2 unless slopes are given, then nothing more is done
      id <- dyo1dyo2 > 0
      yo[id] <- y0o[id] + dyo1dyo2[id]/(dyo1[id] + dyo2[id])
      id <- dyo1dyo2 < 0
      yo[id] <- y0o[id] + dyo1dyo2[id] * (dxo1[id] + dxo2[id])/(dyo1[id] -
                                                                  dyo2[id])/((dx[ix1])[id])
    }

    # return the results

    yout <- rep(NA, k)
    yout[idx] <- yo
    list(x = xout, y = yout)
  }

############################# na_interpolation ###################################


na_interpolation <- function(x, option = "linear", maxgap = Inf, ...) {

  # Variable 'data' is used for all transformations to the time series
  # 'x' needs to stay unchanged to be able to return the same ts class in the end
  data <- x


  #----------------------------------------------------------
  # Mulivariate Input
  # The next 20 lines are just for checking and handling multivariate input.
  #----------------------------------------------------------

  # Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    # Go through columns and impute them by calling this function with univariate input
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      # if imputing a column does not work - mostly because it is not numeric - the column is left unchanged
      tryCatch(data[, i] <- na_interpolation(data[, i], option, maxgap), error = function(cond) {
        warning(paste("imputeTS: No imputation performed for column", i, "because of this", cond), call. = FALSE)
      })
    }
    return(data)
  }


  #----------------------------------------------------------
  # Univariate Input
  # All relveant imputation / pre- postprocessing  code is within this part
  #----------------------------------------------------------

  else {
    missindx <- is.na(data)

    ##
    ## 1. Input Check and Transformation
    ##


    # 1.1 Check if NAs are present
    if (!anyNA(data)) {
      return(x)
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }

    # 1.3 Check for algorithm specific minimum amount of non-NA values
    if (sum(!missindx) < 2) {
      warning("No imputation performed: Input data needs at least 2 non-NA data points for applying na_interpolation")
      return(x)
    }

    # 1.4 Checks and corrections for wrong data dimension

    # Check if input dimensionality is not as expected
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      warning("No imputation performed: Wrong input type for parameter x")
      return(x)
    }

    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    # 1.5 Check if input is numeric
    if (!is.numeric(data)) {
      warning("No imputation performed: Input x is not numeric")
      return(x)
    }

    ##
    ## End Input Check
    ##


    ##
    ## 2. Imputation Code
    ##

    n <- length(data)

    allindx <- 1:n
    indx <- allindx[!missindx]

    data_vec <- as.vector(data)

    if (option == "linear") {
      interp <- stats::approx(indx, data_vec[indx], 1:n, rule = 2, ...)$y
    }
    else if (option == "spline") {
      interp <- stats::spline(indx, data_vec[indx], n = n, ...)$y
    }
    else if (option == "stine") {
      interp <- stinepack::stinterp(indx, data_vec[indx], 1:n, ...)$y
      # avoid NAs at the beginning and end of series // same behavior like
      # for approx with rule = 2.
      if (any(is.na(interp))) {
        interp <- na_locf(interp, na_remaining = "rev")
      }
    }
    else {
      stop("No imputation performed: Wrong parameter 'option' given. Value must be either 'linear', 'spline' or 'stine'.")
    }

    # Merge interpolated values back into original time series
    data[missindx] <- interp[missindx]

    ##
    ## End Imputation Code
    ##


    ##
    ## 3. Post Processing
    ##

    # 3.1 Check for Maxgap option

    # If maxgap = Inf then do nothing and when maxgap is lower than 0
    if (is.finite(maxgap) && maxgap >= 0) {

      # Get logical vector of the time series via is.na() and then get the
      # run-length encoding of it. The run-length encoding describes how long
      # the runs of FALSE and TRUE are
      rlencoding <- rle(is.na(x))

      # Runs smaller than maxgap (which shall still be imputed) are set FALSE
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE

      # The original vector is being reconstructed by reverse.rls, only now the
      # longer runs are replaced now in the logical vector derived from is.na()
      # in the beginning all former NAs that are > maxgap are also FALSE
      en <- inverse.rle(rlencoding)

      # Set all positions in the imputed series with gaps > maxgap to NA
      # (info from en vector)
      data[en == TRUE] <- NA
    }

    ##
    ## End Post Processing
    ##


    ##
    ## 4. Final Output Formatting
    ##

    # Give back the object originally supplied to the function
    # (necessary for multivariate input with only 1 column)
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }

    ##
    ## End Final Output Formatting
    ##

    return(data)
  }
}

############################# ggplot_na_imputations ###################################

ggplot_na_imputations <- function(x_with_na,
                                  x_with_imputations,
                                  x_with_truth = NULL,
                                  x_axis_labels = NULL,
                                  title = "Imputed Values",
                                  subtitle = "Visualization of missing value replacements",
                                  xlab = "Time",
                                  ylab = "Value",
                                  color_points = "steelblue",
                                  color_imputations = "indianred",
                                  color_truth = "seagreen3",
                                  color_lines = "lightslategray",
                                  shape_points = 16,
                                  shape_imputations = 18,
                                  shape_truth = 16,
                                  size_points = 1.5,
                                  size_imputations = 2.5,
                                  size_truth = 1.5,
                                  size_lines = 0.5,
                                  linetype = "solid",
                                  connect_na = TRUE,
                                  legend = TRUE,
                                  legend_size = 5,
                                  label_known = "known values",
                                  label_imputations = "imputed values",
                                  label_truth = "ground truth",
                                  theme = ggplot2::theme_linedraw()) {


  ##
  ## 1. Input Check and Transformation
  ##

  # 1.1 special handling data types
  # x_with_na
  if (any(class(x_with_na) == "tbl_ts")) {
    x_with_na <- as.vector(as.data.frame(x_with_na)[, 2])
  }
  else if (any(class(x_with_na) == "tbl")) {
    x_with_na <- as.vector(as.data.frame(x_with_na)[, 1])
  }
  # x_with_imputations
  if (any(class(x_with_imputations) == "tbl_ts")) {
    x_with_imputations <- as.vector(as.data.frame(x_with_imputations)[, 2])
  }
  else if (any(class(x_with_imputations) == "tbl")) {
    x_with_imputations <- as.vector(as.data.frame(x_with_imputations)[, 1])
  }
  # x_with_truth
  if (any(class(x_with_truth) == "tbl_ts")) {
    x_with_truth <- as.vector(as.data.frame(x_with_truth)[, 2])
  }
  else if (any(class(x_with_truth) == "tbl")) {
    x_with_truth <- as.vector(as.data.frame(x_with_truth)[, 1])
  }

  # 1.2 Check if the input is multivariate

  if (!is.null(dim(x_with_na)[2]) && dim(x_with_na)[2] > 1) {
    stop("x_with_na is not univariate.
    The function only works with univariate input for x_with_na.
    For data types with multiple variables/columns only input the
         column you want to plot as parameter x_with_na.")
  }

  if (!is.null(dim(x_with_imputations)[2]) && dim(x_with_imputations)[2] > 1) {
    stop("x_with_imputations is not univariate.
    The function only works with univariate input for x_with_imputations.
    For data types with multiple variables/columns only input the column
         you want to plot as parameter x_with_imputations")
  }

  if (!is.null(dim(x_with_truth)[2]) && dim(x_with_truth)[2] > 1) {
    stop("x_with_na is not univariate.
    The function only works with univariate input for x_with_truth.
    For data types with multiple variables/columns only input the
         column you want to plot as parameter x_with_truth")
  }


  # 1.3 Checks and corrections for wrong data dimension

  # Altering multivariate objects with 1 column (which are essentially
  # univariate) to be dim = NULL
  if (!is.null(dim(x_with_na)[2])) {
    x_with_na <- x_with_na[, 1]
  }
  if (!is.null(dim(x_with_imputations)[2])) {
    x_with_imputations <- x_with_imputations[, 1]
  }
  if (!is.null(dim(x_with_truth)[2])) {
    x_with_truth <- x_with_truth[, 1]
  }


  # 1.4 Input as vector

  x_with_na <- as.vector(x_with_na)
  x_with_imputations <- as.vector(x_with_imputations)
  x_with_truth <- as.vector(x_with_truth)


  # 1.5 Check if input is numeric

  if (!is.numeric(x_with_na)) {
    stop("Input x_with_na is not numeric")
  }
  if (!is.numeric(x_with_imputations)) {
    stop("Input x_with_imputations is not numeric")
  }
  if (!is.numeric(x_with_truth) && !is.null(x_with_truth)) {
    stop("Input x_with_truth is not numeric")
  }


  # 1.6 Same length of the series

  # x_with_na and x_with_imputations need same length
  if (length(x_with_na) != length(x_with_imputations)) {
    stop("Input x_with_na and x_with_imputations need to have the same length.
         x_with_na is the time series with NAs before imputation.
         x_with_imputations is the time series with filled NAs after applying imputation.")
  }

  # if x_with_truth available it needs also same length
  if (!is.null(x_with_truth) && (length(x_with_na) != length(x_with_truth))) {
    stop("Input x_with_na, x_with_imputations and x_with_truth need to have the same length.
         x_with_na is the time series with NAs before imputation.
         x_with_imputations is the time series with filled NAs after applying imputation.
         x_with_truth (optional) is the series with the ground truth for the imputed values")
  }



  # 1.7 Check preconditions about amount of NAs

  # Unwanted all NA inputs
  missindx_x_with_na <- is.na(x_with_na)
  if (all(missindx_x_with_na)) {
    stop("Input x_with_na consists only of NAs.
     Something with the input likely went wrong.
     Creating a ggplot_na_imputations plot does not make sense with an all NA input.
     This are the required inputs:
     x_with_na (time series before imputation that still has NAs),
     x_with_imputations (time series after imputation, where NAs were replaced by imputation")
  }

  missindx_x_with_imputations <- is.na(x_with_imputations)
  if (all(missindx_x_with_imputations)) {
    stop("Input x_with_imputations consists only of NAs.
     Something with the input likely went wrong.
     Creating a ggplot_na_imputations plot does not make sense with an all NA input.
     This are the required inputs:
     x_with_na (time series before imputation that still has NAs),
     x_with_imputations (time series after imputation, where NAs were replaced by imputation")
  }


  # Unwanted no NA inputs
  if (!anyNA(x_with_na)) {
    stop("Input x_with_na contains no NAs. At least one missing value is needed
     to create a meaningful ggplot_na_imputations plot)
     This are the required inputs:
     x_with_na (time series before imputation that still has NAs),
     x_with_imputations (time series after imputation, where NAs were replaced by imputation")
  }


  ##
  ## End Input Check and Transformation
  ##



  ##
  ## 2. Preparations
  ##

  # 2.1 Create dataframe for ggplot2

  # Define x-axis label data
  # if Date or POSIXct given for x_axis_labels time information can be plotted
  if (any(class(x_axis_labels) == "Date")) {
    time <- x_axis_labels
  }
  else if (any(class(x_axis_labels) == "POSIXct")) {
    time <- x_axis_labels
  }
  else if (is.null(x_axis_labels)) {
    time <- seq_along(x_with_na)
  }
  else {
    stop("Input for x_axis_labels is not in a supported format, must be a
           vector of Date or a POSIXct objects with the same length as
           x_with_na and x_with_imputations")
  }

  if (!is.null(x_with_truth)) {
    df <- data.frame(time, x_with_imputations, x_with_na, x_with_truth)
  }
  else {
    df <- data.frame(time, x_with_imputations, x_with_na)
  }

  ##
  ## End Preparations
  ##



  ##
  ## 3. Create the ggplot2 plot
  ##

  # Create the plot
  gg <- ggplot2::ggplot(data = df)

  ## Add Lines
  # Don't connect the lines in the missing areas
  if (connect_na == FALSE) {
    gg <- gg + ggplot2::geom_line(
      data = df, ggplot2::aes(x = time, y = x_with_na),
      na.rm = TRUE, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  # If truth available connect the true values in the missing areas
  else if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::geom_line(
      data = df, ggplot2::aes(x = time, y = x_with_truth),
      na.rm = TRUE, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  # If no truth available connect the imputed values in the missing areas
  else {
    gg <- gg + ggplot2::geom_line(
      data = df, ggplot2::aes(x = time, y = x_with_imputations),
      na.rm = TRUE, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }


  # Remove known values from imputations - to avoid overplotting
  df$x_with_imputations[!is.na(x_with_na)] <- NA
  if (!is.null(x_with_truth)) {
    df$x_with_truth[!is.na(x_with_na)] <- NA
  }

  ## Add points

  # Points for regular, known values
  gg <- gg + ggplot2::geom_point(
    data = df, ggplot2::aes(x = time, y = x_with_na, color = "1"),
    na.rm = TRUE, shape = shape_points, size = size_points
  )


  # Points for Imputations
  gg <- gg + ggplot2::geom_point(
    data = df, ggplot2::aes(x = time, y = x_with_imputations, color = "2"),
    na.rm = TRUE, size = size_imputations, shape = shape_imputations
  )

  # Points for truth
  if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::geom_point(
      data = df, ggplot2::aes(x = time, y = x_with_truth, color = "3"),
      na.rm = TRUE, shape = shape_truth, size = size_truth
    )
  }


  if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::scale_color_manual(
      name = ggplot2::element_blank(),
      breaks = c("1", "2", "3"),
      labels = c(label_known, label_imputations, label_truth),
      values = c(color_points, color_imputations, color_truth)
    )
  }
  else {
    gg <- gg + ggplot2::scale_color_manual(
      name = ggplot2::element_blank(),
      breaks = c("1", "2"),
      labels = c(label_known, label_imputations),
      values = c(color_points, color_imputations)
    )
  }

  gg <- gg + ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +

    ggplot2::ggtitle(label = title, subtitle = subtitle) + theme

  if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(size = legend_size,
                          shape = c(shape_points, shape_imputations, shape_truth))
    ))
  }
  else {
    gg <- gg + ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(size = legend_size,
                          shape = c(shape_points, shape_imputations))
    ))
  }

  gg <- gg + ggplot2::theme(
    legend.position = base::ifelse(legend == TRUE, "bottom", "none"),
    legend.title = ggplot2::element_blank()
  )

  ##
  ##  End creating the ggplot2 plot
  ##

  return(gg)
}

############################# is_empty (sjmisc) ###################################

is_empty <- function(x, first.only = TRUE, all.na.empty = TRUE) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      # characters may also be of length 0
      if (length(x) == 0) return(TRUE)
      # else, check all elements of x
      zero_len <- nchar(x) == 0
      # return result for multiple elements of character vector
      if (first.only) {
        zero_len <- .is_true(zero_len[1])
        if (length(x) > 0) x <- x[1]
      } else {
        return(unname(zero_len))
      }
      # we have a non-character vector here. check for length
    } else if (is.list(x)) {
      x <- purrr::compact(x)
      zero_len <- length(x) == 0
    } else {
      zero_len <- length(x) == 0
    }
  }

  any(is.null(x) || zero_len || (all.na.empty && all(is.na(x))))
}


.is_true <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

############################# word_wrap (sjmisc) ###################################

word_wrap <- function(labels, wrap, linesep = NULL) {
  # infinite wrap? then return labels
  if (is.infinite(wrap) | wrap == 0) return(labels)
  # expressions can't be wrapped
  if (is.expression(labels)) {
    warning("Word wrap is not available for expressions.")
    return(labels)
  }
  # check if labels have NA values and remove them
  if (anyNA(labels)) labels <- as.character(stats::na.omit(labels))
  # check for valid value
  if (is.null(labels) || length(labels) == 0) return(NULL)
  # coerce to character, if factor
  if (!is.character(labels)) labels <- as.character(labels)
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
    lsub <- 0
    ori.linesep <- '\n'
  } else {
    # however, for html-function we can use "<br>"
    # as argument
    lsub <- nchar(linesep) - 1
    ori.linesep <- linesep
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- paste('(.{1,', wrap, '})(\\s|$)', sep = "")
  # iterate all labels
  for (n in seq_len(length(labels))) {
    # check if wrap exceeds lengths of labels
    if (wrap > 0 && nchar(labels[n]) > wrap) {
      # insert line breaks
      labels[n] <- gsub(pattern, linesep, labels[n])

      # in case label was short enough, we still have a line break
      # at the end of the label. here we remove any trailing line breaks
      l <- nchar(labels[n])
      # get last char
      lc <- substr(labels[n], l - lsub, l)
      # check if line break
      if (lc == ori.linesep) {
        # if yes, remove it
        labels[n] <- substr(labels[n], 0, l - (lsub + 1))
      }
    }
  }

  labels
}

############################# str_contains (sjmisc) ###################################

str_contains <- function(x, pattern, ignore.case = FALSE, logic = NULL, switch = FALSE) {
  # check if correct length when switching
  if (switch && length(x) > 1) {
    warning("`x` must be of length 1 when `switch = TRUE`. First element will be used.", call. = FALSE)
    x <- x[1]
  }
  # counter for matches
  cnt <- c()
  # ignore case for x and pattern
  if (ignore.case) {
    x <- tolower(x)
    pattern <- tolower(pattern)
  }
  # iterate patterns
  for (k in pattern) {
    # append result
    if (switch)
      cnt <- c(cnt, !is_empty(grep(x, k, fixed = TRUE)))
    else
      cnt <- c(cnt, !is_empty(grep(k, x, fixed = TRUE)))
  }
  # which logical combination?
  if (is.null(logic))
    return(cnt)
  else if (logic %in% c("or", "OR", "|"))
    return(any(cnt))
  else if (logic %in% c("and", "AND", "&"))
    return(all(cnt))
  else if (logic %in% c("not", "NOT", "!"))
    return(!any(cnt))
  return(cnt)
}


############################# miss_var_run (naniar) ###################################


miss_var_run <- function(data, var){

  test_if_null(data)

  test_if_missing(var)

  test_if_dataframe(data)

  UseMethod("miss_var_run")

}

#' @export


miss_var_run.default <- function(data, var){

  var <- rlang::enquo(var)

  data_pull <-  data %>% dplyr::pull(!!var)

  tibble::as_tibble(c(rle(is.na(data_pull)))) %>%
    dplyr::rename(run_length = lengths,
                  is_na = values) %>%
    dplyr::mutate(is_na = dplyr::if_else(is_na == TRUE,
                                         true = "missing",
                                         false = "complete"))
  # also look into `label_na`
  # naniar::is_na(TRUE)

}

#' @export
miss_var_run.grouped_df <- function(data,var){

  var <- rlang::enquo(var)

  tidyr::nest(data) %>%
    dplyr::mutate(data = purrr::map(data,
                                    var = !!var,
                                    .f = miss_var_run)) %>%
    tidyr::unnest(cols = c(data))

}


############################# roll_meab (RcppRoll) ###################################


roll_mean <- function(x,
                      n = 1L,
                      weights = NULL,
                      by = 1L,
                      fill = numeric(0),
                      partial = FALSE,
                      align = c("center", "left", "right"),
                      normalize = TRUE,
                      na.rm = FALSE)
{
  if (!identical(partial, FALSE)) {
    warning("'partial' argument is currently unimplemented; using 'partial = FALSE'")
    partial <- FALSE
  }

  result <- roll_mean_impl(
    x,
    as.integer(n),
    as.numeric(weights),
    as.integer(by),
    as.numeric(fill),
    as.logical(partial),
    as.character(match.arg(align)),
    as.logical(normalize),
    as.logical(na.rm)
  )
  colnames(result) <- colnames(x)
  result
}

##############################################################
## test if integer

testInteger <- function(x){
  test <- all.equal(x, as.integer(x), check.attributes = FALSE)
  if(test == TRUE){ return(TRUE) }
  else { return(FALSE) }
}





############################# test_if_null etc (naniar) ###################################

test_if_null <- function(x){

  # test for null
  if (is.null(x)) {
    stop("Input must not be NULL", call. = FALSE)
  }
}

test_if_missing <- function(x){

  # test for null
  if (missing(x)) {
    stop("argument must be specified", call. = FALSE)
  }
}

#' Test if input is a data.frame
#'
#' @param x object
#'
#' @return an error if input (x) is a data.frame
#'
#' @examples
#' \dontrun{
#' # success
#' test_if_dataframe(airquality)
#' #fail
#' my_test <- matrix(10)
#' test_if_dataframe(my_test)
#' }
#'
test_if_dataframe <- function(x){
  # test for dataframe
  if (!inherits(x, "data.frame")) {
    stop("Input must inherit from data.frame", call. = FALSE)
  }
}

