
valid_source <- function(x) {
  valid <- is.data.frame(x) |
    is.tbl(x)
  if (!valid) {
    stop("x is not of a valid source")
  }
}
