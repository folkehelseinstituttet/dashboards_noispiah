#' Data_ForskrivningerAvAntibiotikaPerIndikasjon
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForskrivningerAvAntibiotikaPerIndikasjon
Data_ForskrivningerAvAntibiotikaPerIndikasjon <- function(di, da, DATE_USE) {
  . <- NULL

  temp <- da[PrevalensDato == DATE_USE & !is.na(forebyggingVsBehandling)]

  tab <- temp[forebyggingVsBehandling == "Behandling", .(n = .N), by = .(IndikasjonCategory,
                                                                         forebyggVsBehandOgMethVsAndre,
                                                                         ATCSubstans,
                                                                         ABBredMethAndre,
                                                                         forebyggingVsBehandling)]
  tab[, denom := sum(n), by = IndikasjonCategory]

  return(tab)
}


#' Figure_ForskrivningerAvAntibiotikaPerIndikasjon
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @param indikasjon a
#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
#' @export Figure_ForskrivningerAvAntibiotikaPerIndikasjon
Figure_ForskrivningerAvAntibiotikaPerIndikasjon <- function(di, da, DATE_USE, indikasjon = "Nedre urinveisinfeksjon") {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_ForskrivningerAvAntibiotikaPerIndikasjon(di = di, da = da, DATE_USE = DATE_USE)[IndikasjonCategory == indikasjon]
  if(nrow(tab)==0) return(no_data_graph())
  tab[, xLab := sprintf("%s (%s)", ATCSubstans, n)]
  tab[, denom := sum(n), by = IndikasjonCategory]
  tab[, sorting := sum(n), by = xLab]

  tab[, ABBredMethAndre:=factor(
    ABBredMethAndre,
    levels=c(
      "Methenamine",
      "bredspektrede antibiotika",
      "andre antibiotika"
    ),
    labels=c(
      "Methenamine",
      "Bredspektrede antibiotika",
      "Andre antibiotika"
    ))]

  q <- ggplot(tab, aes(x = reorder(xLab, sorting), y = n / denom * 100, fill = ABBredMethAndre))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_fill_manual("",
                             values=c("green",
                                      "orange",
                                      "blue"),
                             drop=F, guide = guide_legend(ncol = 3, byrow = T, reverse = F))
  q <- q + scale_x_discrete("Antibiotika (virkestoff (antall forskrivninger))")
  q <- q + scale_y_continuous(sprintf(
    "Andel (%%) av forskrivninger til behandling\nav %s (n=%s)",
    tolower(indikasjon),
    sum(tab$n)
  ))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}
