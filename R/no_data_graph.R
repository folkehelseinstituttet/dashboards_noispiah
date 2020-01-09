#' no data graph
#' @export
no_data_graph <- function() {
  pd <- data.frame(x = 0, y = 0, lab = "Ingen data")
  q <- ggplot(pd, aes(x = x, y = y, label = lab))
  q <- q + geom_text()
  q <- q + scale_x_continuous("", breaks = NULL)
  q <- q + scale_y_continuous("", breaks = NULL)
  q
}

#' no data graph
#' @export
no_data_table <- function() {
  xtab <- xtable::xtable(data.frame("Ingen data"))
  xtable::print.xtable(xtab,
    include.rownames = FALSE,
    include.colnames = FALSE,
    caption.placement = "top",
    comment = F
  )
}
