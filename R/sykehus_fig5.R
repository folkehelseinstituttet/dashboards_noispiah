#' Data_AntibiotikaTilBehandlingOverTid
#' @param di a
#' @param da a
#' @param indikasjon a
#' @param ab a
#' @param klassifisering a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_AntibiotikaTilBehandlingOverTid
Data_AntibiotikaTilBehandlingOverTid <- function(
  di,
  da,
  indikasjon=NULL,
  ab="sykehusAB2",
  klassifisering=NULL
  ) {

  temp <- da[forebyggingVsBehandling == "Behandling"]
  if(!is.null(indikasjon)) temp <- temp[Indikasjon %in% indikasjon]
  if(!is.null(klassifisering)) temp <- temp[Klassifisering %in% klassifisering]
  temp[,ab:=get(ab)]
  tab <- temp[forebyggingVsBehandling == "Behandling", .(n = .N),
            keyby = .(
              PrevalensDato,
              ab)]

  skeleton <- data.table(expand.grid(unique(tab$PrevalensDato),unique(tab$ab)))
  setnames(skeleton,c("PrevalensDato","ab"))

  skeleton <- merge(skeleton,unique(da[,c("PrevalensDato","PrevalensTittel")]),by="PrevalensDato")
  tab <- merge(skeleton, tab, by=c("PrevalensDato","ab"),all.x=T)

  tab[is.na(n),n:=0]
  tab[, denom := sum(n), by = PrevalensDato]

  return(tab)
}

#' Figure_AntibiotikaTilBehandlingOverTid1
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @export
Figure_AntibiotikaTilBehandlingOverTid1 <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig5")$arg

  skeleton <- Data_AntibiotikaTilBehandlingOverTid(
    di = data$di,
    da = data$da_all,
    indikasjon = arg$indikasjon,
    ab=arg$ab,
    klassifisering = arg$klassifisering
    )
  skeleton[,n:=NULL]
  skeleton[,denom:=NULL]

  tab <- Data_AntibiotikaTilBehandlingOverTid(
    di = data$di,
    da = data$da,
    indikasjon = arg$indikasjon,
    ab=arg$ab,
    klassifisering = arg$klassifisering
    )

  tab[, PrevalensTittel:=NULL]
  tab <- merge(skeleton, tab, all.x=T, by=c("PrevalensDato","ab"))
  tab[is.na(n), n:=0]
  tab[is.na(denom), denom:=0]
  if(nrow(tab)==0) return(no_data_graph())
  tab <- tab[stringr::str_detect(PrevalensTittel,"^[24]")]

  ordering <- tab[,.(n=sum(n)),
                  keyby=.(
                    PrevalensDato,
                    PrevalensTittel
                  )]
  ordering[,xLab:=sprintf("%s\n(n=%s)",PrevalensTittel,n)]
  ordering[,xVal:=1:.N]

  tab <- merge(tab,ordering[,c("PrevalensDato","xVal")],by="PrevalensDato")
  tab[,ab:=factor(ab, levels=rev(levels(ab)))]

  tab[, prop := n / denom]
  tab[is.nan(prop), prop:=0]

  q <- ggplot(tab, aes(x = xVal, y = prop, fill = ab))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                               values=c("blue",
                                        "purple",
                                        "orange"),
                               drop=F, guide = guide_legend(ncol = 3, byrow = T, reverse = T))
  q <- q + scale_x_continuous("Undersøkelsestidspunkt (n=antall forskrivninger)",
                              breaks=ordering$xVal,
                              labels=ordering$xLab)
  q <- q + scale_y_continuous("Fordeling (%) av forskrivninger til behandling\nper undersøkelsestidspunkt",
                              labels=scales::percent,
                              expand=c(0,0))
  q <- q + expand_limits(y=0)
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}


#' Figure_AntibiotikaTilBehandlingOverTid2
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @export
Figure_AntibiotikaTilBehandlingOverTid2 <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig6")$arg

  skeleton <- Data_AntibiotikaTilBehandlingOverTid(
    di = data$di,
    da = data$da_all,
    indikasjon = arg$indikasjon,
    ab=arg$ab,
    klassifisering = arg$klassifisering
  )
  skeleton[,n:=NULL]
  skeleton[,denom:=NULL]

  tab <- Data_AntibiotikaTilBehandlingOverTid(
    di = data$di,
    da = data$da,
    indikasjon=arg$indikasjon,
    ab=arg$ab,
    klassifisering=arg$klassifisering
  )
  tab[, PrevalensTittel:=NULL]
  tab <- merge(skeleton, tab, all.x=T, by=c("PrevalensDato","ab"))
  tab[is.na(n), n:=0]
  tab[is.na(denom), denom:=0]

  if(nrow(tab)==0) return(no_data_graph())
  tab <- tab[stringr::str_detect(PrevalensTittel,"^[24]")]

  ordering <- tab[,.(n=sum(n)),
                  keyby=.(
                    PrevalensDato,
                    PrevalensTittel
                  )]
  ordering[,xLab:=sprintf("%s\n(n=%s)",PrevalensTittel,n)]
  ordering[,xVal:=1:.N]

  tab <- merge(tab,ordering[,c("PrevalensDato","xVal")],by="PrevalensDato")
  tab[,ab:=factor(ab, levels=rev(levels(ab)))]

  q <- ggplot(tab, aes(x = xVal, y = n / denom, fill = ab))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                               values=c(
                                 "blue",
                                 "purple",
                                 "orange",
                                 "green"),
                               drop=F, guide = guide_legend(ncol = 2, byrow = T, reverse = T))
  q <- q + scale_x_continuous("Undersøkelsestidspunkt (n=antall forskrivninger)",
                              breaks=ordering$xVal,
                              labels=ordering$xLab)
  q <- q + scale_y_continuous("Fordeling (%) av forskrivninger til behandling av\nsamfunnservervede nedre luftveisinfeksjoner\nper undersøkelsestidspunkt",
                              labels=scales::percent,
                              expand=c(0,0))
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  q <- q + theme(legend.position = "bottom")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}





