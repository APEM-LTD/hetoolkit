IsWholeNumber <- function(x, tol = .Machine$double.eps^0.5) {
  # Check if field is a whole number
  abs(x - round(x)) < tol
  }
