IsDate <- function(x, date.format = NULL) {
  # Check if field is a date using as.Date that looks for unambiguous dates
  #   Assumes date format so NA returned not Character error.
  #   Why? with no date format, R tries two defaults then gives error.
  #   BUT With a dateformat R returns NA
  # Args
  #   Suspected date and optional date format string
  # Returns
  #   TRUE if thinbks it is a date
    formatted = try(as.Date(x, date.format), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
    is_date[is.na(x)] = NA  # Insert NA for NA in x
    return(is_date)
  }
