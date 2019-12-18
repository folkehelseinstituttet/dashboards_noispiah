#' Data_AntibiotikaTilXX
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @param indikasjon a
#' @param xforebyggingVsBehandling a
#' @param ab a
#' @param leftVsRightVar a
#' @param extraGrouping a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_AntibiotikaTilXX
Data_AntibiotikaTilXX <- function(di, da, DATE_USE,
                                  indikasjon=NULL,
                                  xforebyggingVsBehandling="Forebygging",
                                  ab="ABBredMethAndre",
                                  leftVsRightVar="SykehusKlassifisering",
                                  extraGrouping=ab) {
  . <- NULL

  temp <- da[forebyggingVsBehandling == xforebyggingVsBehandling & PrevalensDato==DATE_USE]
  if(!is.null(indikasjon)) temp <- temp[IndikasjonCategorySykehus %in% indikasjon]
  tab <- temp[, .(n = .N),
              keyby = .(
                "lr"=get(leftVsRightVar),
                "ab"=get(ab),
                "extraGrouping"=get(extraGrouping))]

  tab[,side:="Left"]
  tab[lr %in% c(
    "Kirurgisk profylakse",
    "Samfunnservervet infeksjon",
    "Øvre urinveisinfeksjon"
    ),side:="Right"]
  if(nrow(tab)==0) return(tab)

  tab <- dcast.data.table(tab,ab+extraGrouping~side,value.var = "n")
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
                               ab="sykehusAB4",
                               extraGrouping="sykehusAB5")
  tab[,ordering:=Left+Right]
  setorder(tab,ordering)
  tab[,ab:=factor(ab,levels=ab)]
  tab[,denom:=sum(Left,na.rm=T)+sum(Right,na.rm=T)]

  maxVal <- max(c(tab$Left,tab$Right),na.rm=T)/tab$denom[1]
  seqBy <- 0.05
  if(maxVal>0.3) seqBy <- 0.1

  q <- ggplot(tab, aes(x = ab, fill=extraGrouping))
  q <- q + geom_col(mapping=aes(y=-Left/denom),alpha=0.75,width=0.75)
  q <- q + geom_col(mapping=aes(y=Right/denom),alpha=0.75,width=0.75)
  q <- q + geom_hline(yintercept=0)
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",
                               values=c("orange",
                                        "blue"),
                               drop=F, guide = guide_legend(ncol = 2, byrow = T, reverse = F))
  #q <- q + scale_colour_brewer("", palette = "Set1", guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Antibiotika")
  q <- q + scale_y_continuous(sprintf("Prosentandel av total antall terapeutiske foreskrivninger\n(n=%s)",tab$denom[1]),
                              lim=c(-maxVal-0.025,maxVal+0.025),
                              breaks=seq(-1,1,seqBy),
                              labels=paste0(round(abs(seq(-1,1,seqBy))*100),"%"))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + labs(caption="Helsetjenesteassosiert infeksjoner til venstre, samfunnservervede infeksjoner til høyre")
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
#' @param extraGrouping a
#' @param captionLeft a
#' @param captionRight a
#' @param xLab a
#' @param colours a
#' @param xforebyggingVsBehandling a
#' @import data.table
#' @import ggplot2
#' @export Figure_AntibiotikaTilBehandling
Figure_AntibiotikaTilBehandling <- function(di, da, DATE_USE, indikasjon=NULL,
                                            ab="sykehusAB4",
                                            leftVsRightVar="SykehusKlassifisering",
                                            extraGrouping="sykehusAB5",
                                            captionLeft="Helsetjenesteassosierte\ninfeksjoner (n={n})",
                                            captionRight="Samfunnservervede\ninfeksjoner (n={n})",
                                            xLab="Andel (%) av forskrivinger til behandling av nedre luftveisinfeksjoner",
                                            colours=c("orange","blue"),
                                            xforebyggingVsBehandling="Behandling") {

  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_AntibiotikaTilXX(di = di, da = da, DATE_USE=DATE_USE,
                               indikasjon = indikasjon,
                               xforebyggingVsBehandling = xforebyggingVsBehandling,
                               ab=ab,
                               leftVsRightVar=leftVsRightVar,
                               extraGrouping=extraGrouping)
  if(nrow(tab)==0) return(no_data_graph())
  num_ab <- length(unique(tab$ab))
  if(num_ab<5){
    tab_empty <- tab[1]
    lab_pos <- 1
  } else if(num_ab<10){
    tab_empty <- tab[1:2]
    lab_pos <- 2
  } else if(num_ab<20) {
    tab_empty <- tab[1:3]
    lab_pos <- 3
  } else {
    tab_empty <- tab[1:4]
    lab_pos <- 4
  }
  tab_empty[,ab:=unlist(lapply(1:.N, function(x)paste0(rep(" ",x),collapse="")))]
  tab_empty[,Left:=0]
  tab_empty[,Right:=0]
  tab <- rbind(tab,tab_empty)
  ordering <- tab[,.(n=sum(Left,na.rm=T)+sum(Right,na.rm=T)),by=.(ab)]
  setorder(ordering,n)
  tab[,ab:=factor(ab,levels=ordering$ab)]
  tab[,denomLeft:=sum(Left,na.rm=T)]
  tab[,denomRight:=sum(Right,na.rm=T)]

  xLab <- glue::glue("{xLab} (n={n})",
                     xLab=xLab,
                     n=tab$denomLeft[1]+tab$denomRight[1])

  tab[,numLeft:=sum(Left,na.rm=T),by=.(ab)]
  tab[,numRight:=sum(Right,na.rm=T),by=.(ab)]
  maxVal <- max(c(tab$numLeft/tab$denomLeft[1],tab$numRight/tab$denomRight[1]),na.rm=T)+0.05
  tab[,numLeft:=NULL]
  tab[,numRight:=NULL]

  if(maxVal < 0.35){
    xBreaks <- seq(-1,1,0.05)
  } else if(maxVal < 0.5) {
    xBreaks <- seq(-1,1,0.1)
  } else {
    xBreaks <- seq(-1,1,0.25)
  }
  xLabs <- paste0(round(abs(xBreaks)*100),"%")

  q <- ggplot(tab, aes(x = ab, fill=extraGrouping))
  q <- q + geom_col(mapping=aes(y=-Left/denomLeft),colour = "black", alpha = 1,width=0.75)
  q <- q + geom_col(mapping=aes(y=Right/denomRight),colour = "black", alpha = 1,width=0.75)
  q <- q + geom_hline(yintercept=0)
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",
                             values=colours,
                             drop=F, guide = guide_legend(ncol = 2, byrow = T, reverse = F))
  #q <- q + scale_colour_brewer("", palette = "Set1", guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Antibiotika (virkestoff)", expand = expand_scale(add = c(1, 0)))
  q <- q + scale_y_continuous(xLab,
                              lim=c(-maxVal,maxVal),
                              breaks=xBreaks,
                              labels=xLabs,
                              expand=c(0,0))
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(axis.line.y=element_blank())
  q <- q + theme(legend.position = "bottom")
  q <- q + geom_label(mapping=aes(
    x = ordering$ab[lab_pos],
    y = -maxVal,
    label = glue::glue(captionLeft,n=tab$denomLeft[1])),
    hjust=0,
    vjust=1,
    fill="white")
  q <- q + geom_label(mapping=aes(
    x = ordering$ab[lab_pos],
    y = maxVal,
    label = glue::glue(captionRight,n=tab$denomRight[1])),
    hjust=1,
    vjust=1,
    fill="white")
  # q <- q + labs(caption="\n\n\n\n\n\n")
  # q <- q + theme(legend.position=c(0.0,-0.13),
  #          legend.justification=c(0.5, 1),
  #          legend.box.margin=margin(c(0,0,0,00)),
  #          legend.direction="horizontal")
  q

}


