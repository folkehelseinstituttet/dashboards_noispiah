#' Figure_ForskrivningerAvAntibiotikaTilForebyggingOgBehandlingPerIndikasjon
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @export
Figure_ForskrivningerAvAntibiotikaTilForebyggingOgBehandlingPerIndikasjon <- function(
  data,
  arg
  ) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehjem_fig2")$arg

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(
    data = data,
    arg = arg
  )

  if(nrow(tab)==0) return(no_data_graph())
  tab[, denom := sum(n)]
  tab[, catFill := forebyggVsBehandOgMethVsAndre]
  tab[, nFill := sum(n), by=catFill]
  tab[, labFill := sprintf("%s (n=%s)", catFill, nFill)]

  skeleton <- data.table(
    catFill=c(
      "Forebygging metenamin",
      "Forebygging andre antibiotika",
      "Behandling metenamin",
      "Behandling andre antibiotika"
    )
  )
  ordering <- unique(tab[,c("labFill","catFill")])
  ordering <- merge(skeleton,ordering,by="catFill",all=T)
  ordering[is.na(labFill),labFill:=glue::glue("{catFill} (n=0)",catFill=catFill)]
  ordering[,catFill:=factor(catFill,levels=c(
    "Forebygging metenamin",
    "Forebygging andre antibiotika",
    "Behandling metenamin",
    "Behandling andre antibiotika"
  ))]
  setorder(ordering,-catFill)
  tab[,labFill:=factor(labFill,levels=ordering$labFill)]

  tab[, catX := IndikasjonCategory]
  tab[, nX := sum(n), by=catX]
  tab[, labX := sprintf("%s (n=%s)", catX, nX)]

  tab[, total_height := sum(n/denom), by=.(labX)]

  ordering <- unique(tab[,c("labX","catX","nX")])
  setorder(ordering,nX,catX)
  tab[,labX:=factor(labX,levels=ordering$labX)]

  q <- ggplot(tab, aes(x = labX, y = n / denom, fill = labFill))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                             values=c("blue",
                                      "darkgreen",
                                      "yellow",
                                      "purple"),
                             drop=F, guide = guide_legend(ncol = 2, byrow = T, reverse = T))
  q <- q + scale_x_discrete("Indikasjon (n=antall forskrivninger)")
  q <- q + scale_y_continuous(sprintf("Andel (%%) av forskrivninger til\nforebygging og behandling (n=%s)", sum(tab$n)),
                              labels = scales::percent,
                              expand=c(0,0),
                              limits=c(0,max(tab$total_height)*1.1))
  q <- q + coord_flip()
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(legend.position = "bottom")
  lemon::grid_arrange_shared_legend(q, plot=F)
}
