#' @export
mid_date <- function(startdate, enddate) {
  stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
  stopifnot(length(startdate) == length(enddate))
  idx <- enddate < startdate
  if (any(idx, na.rm = TRUE)) {
    enddate[idx] <- NULL
    startdate[idx] <- NULL
    cat(paste0("\nWARNING: NAs assigned to ", sum(idx, na.rm = TRUE),
               " inconsistent date pairs\n"))
  }
  res <- NULL
  if (length(startdate > 0)) {
    intobj <- lubridate::interval(startdate, enddate)
    res <- lubridate::as_date(lubridate::int_start(intobj) +
                                ((lubridate::int_end(intobj) -
                                    lubridate::int_start(intobj)) / 2))
  }
  res
}
