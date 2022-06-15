se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  sqrt(var(x) / length(x))
}
