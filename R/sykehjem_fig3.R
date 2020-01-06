#' Data_ForskrivningerAvAntibiotikaPerIndikasjon
#' @param data a
#' @param arg a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForskrivningerAvAntibiotikaPerIndikasjon
Data_ForskrivningerAvAntibiotikaPerIndikasjon <- function(
  data,
  arg
) {
  . <- NULL

  temp <- data$da[PrevalensDato == arg$DATE_USE & !is.na(forebyggingVsBehandling)]
  if(arg$with_metenamin){
    temp[,antibiotics:=ABBredMethAndre]
  } else {
    temp[,antibiotics:=ABBredAndre]
  }

  tab <- temp[forebyggingVsBehandling == "Behandling", .(n = .N), by = .(IndikasjonCategory,
                                                                         forebyggVsBehandOgMethVsAndre,
                                                                         ATCSubstans,
                                                                         antibiotics,
                                                                         forebyggingVsBehandling)]
  tab[, denom := sum(n), by = IndikasjonCategory]

  return(tab)
}


#' Figure_ForskrivningerAvAntibiotikaPerIndikasjon
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
#' @export
Figure_ForskrivningerAvAntibiotikaPerIndikasjon <- function(
  data,
  arg
  ) {


  tab <- Data_ForskrivningerAvAntibiotikaPerIndikasjon(
    data = data,
    arg = arg
    )[IndikasjonCategory == arg$indikasjon]

  if(nrow(tab)==0) return(no_data_graph())
  tab[, xLab := sprintf("%s (n=%s)", ATCSubstans, n)]
  tab[, denom := sum(n), by = IndikasjonCategory]
  tab[, sorting := sum(n), by = xLab]

  if(arg$with_metenamin){
    tab[, antibiotics:=factor(
      antibiotics,
      levels=c(
        "Metenamin",
        "bredspektrede antibiotika",
        "andre antibiotika"
      ),
      labels=c(
        "Metenamin",
        "Bredspektrede antibiotika",
        "Andre antibiotika"
      ))]

    fill_vals <- c("green","orange","blue")
  } else {
    tab[, antibiotics:=factor(
      antibiotics,
      levels=c(
        "bredspektrede antibiotika",
        "andre antibiotika"
      ),
      labels=c(
        "Bredspektrede antibiotika",
        "Andre antibiotika"
      ))]

    fill_vals <- c("orange","blue")
  }

  tab[, total_height := sum(n/denom), by=.(xLab)]

  q <- ggplot(tab, aes(x = reorder(xLab, sorting), y = n / denom, fill = antibiotics))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                             values=fill_vals,
                             drop=F, guide = guide_legend(ncol = length(fill_vals), byrow = T, reverse = F))
  q <- q + scale_x_discrete("Antibiotika (virkestoff (n=antall forskrivninger))")
  q <- q + scale_y_continuous(
    sprintf(
      "Andel (%%) av forskrivninger til behandling\nav %ser (n=%s)",
      tolower(arg$display_name),
      sum(tab$n)
    ),
    labels = scales::percent,
    expand=c(0,0),
    limits=c(0,max(tab$total_height)*1.1))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}
