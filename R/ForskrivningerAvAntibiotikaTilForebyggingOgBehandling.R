#' Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @param group1 a
#' @param group2 a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling
Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling <- function(di,
                                                                       da,
                                                                       DATE_USE,
                                                                       ab="ABBredMethAndre",
                                                                       group1="IndikasjonCategory",
                                                                       group2="forebyggVsBehandOgMethVsAndre") {
  . <- NULL

  temp <- da[PrevalensDato == DATE_USE & !is.na(forebyggingVsBehandling)]

  tab <- temp[, .(n = .N), by = .(
    forebyggingVsBehandling,
    "ab"=get(ab),
    "group1"=get(group1),
    "group2"=get(group2))]

  if(!ab %in% names(tab)) setnames(tab,"ab",ab)
  if(!group1 %in% names(tab)) setnames(tab,"group1",group1)
  if(!group2 %in% names(tab)) setnames(tab,"group2",group2)

  return(tab)
}


#' Figure_ForskrivningerAvAntibiotikaTilForebyggingOgBehandlingPerIndikasjon
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_ForskrivningerAvAntibiotikaTilForebyggingOgBehandlingPerIndikasjon
Figure_ForskrivningerAvAntibiotikaTilForebyggingOgBehandlingPerIndikasjon <- function(di, da, DATE_USE) {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(di = di,
                                                                    da = da,
                                                                    DATE_USE = DATE_USE,
                                                                    ab="forebyggVsBehandOgMethVsAndre",
                                                                    group1="IndikasjonCategory",
                                                                    group2="forebyggVsBehandOgMethVsAndre")
  tab[, denom := sum(n)]
  tab[, catFill := forebyggVsBehandOgMethVsAndre]
  tab[, nFill := sum(n), by=catFill]
  tab[, labFill := sprintf("%s (n=%s)", catFill, nFill)]

  ordering <- unique(tab[,c("labFill","catFill")])
  ordering[,catFill:=factor(catFill,levels=c(
    "Forebygging Methenamine",
    "Forebygging andre antibiotika",
    "Behandling Methenamine",
    "Behandling andre antibiotika"
  ))]
  setorder(ordering,-catFill)
  tab[,labFill:=factor(labFill,levels=ordering$labFill)]

  tab[, catX := IndikasjonCategory]
  tab[, nX := sum(n), by=catX]
  tab[, labX := sprintf("%s (n=%s)", catX, nX)]

  ordering <- unique(tab[,c("labX","catX","nX")])
  setorder(ordering,nX,catX)
  tab[,labX:=factor(labX,levels=ordering$labX)]

  q <- ggplot(tab, aes(x = labX, y = n / denom, fill = labFill))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(ncol = 2, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Indikasjon")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til forebygging og behandling\n(n=%s)", sum(tab$n)),
                              labels = scales::percent)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position = "bottom")
  q <- q + labs(caption = "\n\n\n\n\n\n")
  q <- q + theme(
    legend.position = c(0.0, -0.13),
    legend.justification = c(0.5, 1),
    legend.box.margin = margin(c(0, 0, 0, 00)),
    legend.direction = "horizontal"
  )
  q
}

#' Figure_ForskrivningerAvAntibiotikaTilBehandlingPerIndikasjon
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_ForskrivningerAvAntibiotikaTilBehandlingPerIndikasjon
Figure_ForskrivningerAvAntibiotikaTilBehandlingPerIndikasjon <- function(di, da, DATE_USE) {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(di = di,
                                                                    da = da,
                                                                    DATE_USE = DATE_USE,
                                                                    ab="forebyggVsBehandOgMethVsAndre",
                                                                    group1="IndikasjonCategorySykehus",
                                                                    group2="forebyggVsBehandOgMethVsAndre")
  tab <- tab[forebyggingVsBehandling=="Behandling"]
  tab[, denom := sum(n)]
  tab[, catFill := forebyggVsBehandOgMethVsAndre]
  tab[, nFill := sum(n), by=catFill]
  tab[, labFill := sprintf("%s (n=%s)", catFill, nFill)]

  ordering <- unique(tab[,c("labFill","catFill")])
  ordering[,catFill:=factor(catFill,levels=c(
    "Forebygging Methenamine",
    "Forebygging andre antibiotika",
    "Behandling Methenamine",
    "Behandling andre antibiotika"
  ))]
  setorder(ordering,-catFill)
  tab[,labFill:=factor(labFill,levels=ordering$labFill)]

  tab[, catX := IndikasjonCategorySykehus]
  tab[, nX := sum(n), by=catX]
  tab[, labX := sprintf("%s (n=%s)", catX, nX)]

  ordering <- unique(tab[,c("labX","catX","nX")])
  setorder(ordering,nX,catX)
  tab[,labX:=factor(labX,levels=ordering$labX)]

  q <- ggplot(tab, aes(x = labX, y = n / denom, fill = labFill))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(ncol = 2, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Indikasjon")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til behandling (n=%s)", sum(tab$n)),
                              labels=scales::percent)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position = "bottom")
  q <- q + labs(caption = "\n\n\n\n\n\n")
  q <- q + theme(
    legend.position = c(0.0, -0.13),
    legend.justification = c(0.5, 1),
    legend.box.margin = margin(c(0, 0, 0, 00)),
    legend.direction = "horizontal"
  )
  q
}


#' Figure_ForskrivningerAvAntibiotikaTilBehandlingPerSpesialitet
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_ForskrivningerAvAntibiotikaTilBehandlingPerSpesialitet
Figure_ForskrivningerAvAntibiotikaTilBehandlingPerSpesialitet <- function(di, da, DATE_USE) {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(di = di,
                                                                    da = da,
                                                                    DATE_USE = DATE_USE,
                                                                    ab="ABBredMethAndre",
                                                                    group1="c_spesialitet",
                                                                    group2="c_spesialitet")
  tab <- tab[forebyggingVsBehandling=="Behandling"]

  tab[, denom := sum(n)]
  tab[, catFill := ABBredMethAndre]
  tab[, nFill := sum(n), by=catFill]
  tab[, labFill := sprintf("%s (n=%s)", catFill, nFill)]

  ordering <- unique(tab[,c("labFill","catFill")])
  ordering[,catFill:=factor(catFill,levels=c(
    "Methenamine",
    "andre antibiotika",
    "bredspektrde antibiotika"
  ))]
  setorder(ordering,-catFill)
  tab[,labFill:=factor(labFill,levels=ordering$labFill)]

  tab[, catX := c_spesialitet]
  tab[, nX := sum(n), by=catX]
  tab[, labX := sprintf("%s (n=%s)", catX, nX)]

  ordering <- unique(tab[,c("labX","catX","nX")])
  setorder(ordering,-catX)
  tab[,labX:=factor(labX,levels=ordering$labX)]

  q <- ggplot(tab, aes(x = labX, y = n / denom, fill = labFill))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(ncol = 2, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Indikasjon")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til behandling (n=%s)", sum(tab$n)),
                              labels=scales::percent)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position = "bottom")
  q <- q + labs(caption = "\n\n\n\n\n\n")
  q <- q + theme(
    legend.position = c(0.0, -0.13),
    legend.justification = c(0.5, 1),
    legend.box.margin = margin(c(0, 0, 0, 00)),
    legend.direction = "horizontal"
  )
  q

}
