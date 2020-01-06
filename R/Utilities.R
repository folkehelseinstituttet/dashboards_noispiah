FormatNorwegian_int <- function(x) {
  if (x < 10000) {
    return(formatC(x, big.mark = "", format = "f", drop0trailing = T))
  } else {
    formatC(x, big.mark = " ", format = "f", drop0trailing = T)
  }
}

#' FormatNorwegian
#' @param x a
#' @export FormatNorwegian
FormatNorwegian <- Vectorize(FormatNorwegian_int)
