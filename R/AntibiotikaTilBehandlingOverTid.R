#' Data_AntibiotikaTilBehandlingOverTid
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @param indikasjon a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_AntibiotikaTilBehandlingOverTid
Data_AntibiotikaTilBehandlingOverTid <- function(di, da, indikasjon=NULL) {
  . <- NULL

  temp <- da[forebyggingVsBehandling == "Behandling"]
  if(!is.null(indikasjon)) temp <- temp[Indikasjon %in% indikasjon]
  tab <- temp[forebyggingVsBehandling == "Behandling", .(n = .N),
            keyby = .(
              PrevalensDato,
              ABBredMethAndre)]

  skeleton <- data.table(expand.grid(unique(tab$PrevalensDato),unique(tab$ABBredMethAndre)))
  setnames(skeleton,c("PrevalensDato","ABBredMethAndre"))

  skeleton <- merge(skeleton,unique(da[,c("PrevalensDato","PrevalensTittel")]),by="PrevalensDato")
  tab <- merge(skeleton, tab, by=c("PrevalensDato","ABBredMethAndre"),all.x=T)

  tab[is.na(n),n:=0]
  tab[, denom := sum(n), by = PrevalensDato]

  return(tab)
}

#' Figure_AntibiotikaTilBehandlingOverTid1
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_AntibiotikaTilBehandlingOverTid1
Figure_AntibiotikaTilBehandlingOverTid1 <- function(di, da, DATE_USE) {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_AntibiotikaTilBehandlingOverTid(di = di, da = da)

  ordering <- tab[,.(n=sum(n)),
                  keyby=.(
                    PrevalensDato,
                    PrevalensTittel
                  )]
  ordering[,xLab:=sprintf("%s\n(n=%s)",PrevalensTittel,n)]
  ordering[,xVal:=1:.N]

  tab <- merge(tab,ordering[,c("PrevalensDato","xVal")],by="PrevalensDato")

  q <- ggplot(tab, aes(x = xVal, y = n / denom, colour = ABBredMethAndre))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_colour_brewer("", palette = "Set1", guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  q <- q + scale_x_continuous("Undersøkelsestidpunkt",
                              breaks=ordering$xVal,
                              labels=ordering$PrevalensTittel)
  q
  q <- q + scale_y_continuous("Andel av forskrivninger til behandling per undersøkelsestidspunkt",
                              labels=scales::percent)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}


#' Figure_AntibiotikaTilBehandlingOverTid2
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_AntibiotikaTilBehandlingOverTid2
Figure_AntibiotikaTilBehandlingOverTid2 <- function(di, da, DATE_USE) {
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_AntibiotikaTilBehandlingOverTid(di = di, da = da, indikasjon=c("Nedre luftveisinfeksjon"))

  ordering <- tab[,.(n=sum(n)),
                  keyby=.(
                    PrevalensDato,
                    PrevalensTittel
                  )]
  ordering[,xLab:=sprintf("%s\n(n=%s)",PrevalensTittel,n)]
  ordering[,xVal:=1:.N]

  tab <- merge(tab,ordering[,c("PrevalensDato","xVal")],by="PrevalensDato")

  q <- ggplot(tab, aes(x = xVal, y = n / denom, colour = ABBredMethAndre))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_colour_brewer("", palette = "Set1", guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  q <- q + scale_x_continuous("Undersøkelsestidpunkt",
                              breaks=ordering$xVal,
                              labels=ordering$PrevalensTittel)
  q
  q <- q + scale_y_continuous("Andel av forskrivninger til behandling\nav samfunnservervede nedre\nluftveisinfeksjoner per undersøkelsestidspunkt",
                              labels=scales::percent)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}
