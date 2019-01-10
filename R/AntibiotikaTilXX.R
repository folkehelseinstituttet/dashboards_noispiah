#' Data_AntibiotikaTilXX
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @param indikasjon a
#' @param xforebyggingVsBehandling a
#' @param ab a
#' @param leftVsRightVar a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_AntibiotikaTilXX
Data_AntibiotikaTilXX <- function(di, da, DATE_USE,
                                  indikasjon=NULL,
                                  xforebyggingVsBehandling="Forebygging",
                                  ab="ABBredMethAndre",
                                  leftVsRightVar="SykehusKlassifisering") {
  . <- NULL

  temp <- da[forebyggingVsBehandling == xforebyggingVsBehandling & PrevalensDato==DATE_USE]
  if(!is.null(indikasjon)) temp <- temp[IndikasjonCategorySykehus %in% indikasjon]
  tab <- temp[, .(n = .N),
              keyby = .(
                "lr"=get(leftVsRightVar),
                "ab"=get(ab))]

  tab[,side:="Left"]
  tab[lr %in% c(
    "Kirurgisk profylakse",
    "Samfunnservervet infeksjon",
    "Øvre urinveisinfeksjon"
    ),side:="Right"]

  tab <- dcast.data.table(tab,ab~side,value.var = "n")
  if(!"Left" %in%names(tab)) tab[,Left:=0]
  if(!"Right" %in% names(tab)) tab[,Right:=0]

  tab[is.na(Left),Left:=0]
  tab[is.na(Right),Right:=0]

  return(tab)
}

#' Figure_AntibiotikaTilForebygging
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_AntibiotikaTilForebygging
Figure_AntibiotikaTilForebygging <- function(di, da, DATE_USE) {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_AntibiotikaTilXX(di = di, da = da, DATE_USE=DATE_USE,
                               indikasjon = NULL,
                               xforebyggingVsBehandling = "Forebygging",
                               ab="ABBredMethAndre")
  tab[,denom:=sum(Left,na.rm=T)+sum(Right,na.rm=T)]

  maxVal <- max(c(tab$Left,tab$Right),na.rm=T)/tab$denom[1]

  q <- ggplot(tab, aes(x = ab))
  q <- q + geom_col(mapping=aes(y=-Left/denom),alpha=0.75)
  q <- q + geom_col(mapping=aes(y=Right/denom),alpha=0.75)
  q <- q + geom_hline(yintercept=0)
  q <- q + coord_flip()
  #q <- q + scale_colour_brewer("", palette = "Set1", guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  #q <- q + scale_x_continuous("Undersøkelsestidpunkt",
  #                            breaks=ordering$xVal,
  #                            labels=ordering$PrevalensTittel)
  q <- q + scale_y_continuous("Andel av forskrivninger til behandling per undersøkelsestidspunkt",
                              labels=scales::percent,
                              lim=c(-maxVal-0.05,maxVal+0.05))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}


#' Figure_AntibiotikaTilBehandling
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @param indikasjon a
#' @param ab a
#' @param leftVsRightVar a
#' @import data.table
#' @import ggplot2
#' @export Figure_AntibiotikaTilBehandling
Figure_AntibiotikaTilBehandling <- function(di, da, DATE_USE, indikasjon=NULL,
                                            ab="ABBredMethAndre",
                                            leftVsRightVar="SykehusKlassifisering") {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_AntibiotikaTilXX(di = di, da = da, DATE_USE=DATE_USE,
                               indikasjon = indikasjon,
                               xforebyggingVsBehandling = "Behandling",
                               ab=ab,
                               leftVsRightVar=leftVsRightVar)
  tab[,denom:=sum(Left,na.rm=T)+sum(Right,na.rm=T)]

  maxVal <- max(c(tab$Left,tab$Right),na.rm=T)/tab$denom[1]

  q <- ggplot(tab, aes(x = ab))
  q <- q + geom_col(mapping=aes(y=-Left/denom),alpha=0.75)
  q <- q + geom_col(mapping=aes(y=Right/denom),alpha=0.75)
  q <- q + geom_hline(yintercept=0)
  q <- q + coord_flip()
  #q <- q + scale_colour_brewer("", palette = "Set1", guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  #q <- q + scale_x_continuous("Undersøkelsestidpunkt",
  #                            breaks=ordering$xVal,
  #                            labels=ordering$PrevalensTittel)
  q <- q + scale_y_continuous("Andel av forskrivninger til behandling per undersøkelsestidspunkt",
                              labels=scales::percent,
                              lim=c(-maxVal-0.05,maxVal+0.05))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}


