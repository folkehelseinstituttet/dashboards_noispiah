#' Data_AndelAntibiotikaTilForebyggingOgBehandling
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @export Data_AndelAntibiotikaTilForebyggingOgBehandling
Data_AndelAntibiotikaTilForebyggingOgBehandling <- function(di, da, DATE_USE) {
  temp <- da[PrevalensDato == DATE_USE & !is.na(Klassifisering)]

  tab <- temp[, .(n = .N), by = .(Klassifisering)]
  tab[, denom := sum(n)]
  tab[,prop:=n/denom]

  tab[, grouping:="Annet/ukjent"]
  tab[ Klassifisering %in% c(
    "Samfunnservervet infeksjon",
    "Helsetjenesteassosiert infeksjon"
  ), grouping:="Behandling"]
  tab[ Klassifisering %in% c(
    "Kirurgisk profylakse 1",
    "Kirurgisk profylakse 2",
    "Kirurgisk profylakse 3",
    "Medisinsk profylakse"
  ), grouping:="Forebygging"]

  return(tab)
}

#' Figure_AndelAntibiotikaTilForebyggingOgBehandling
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @export Figure_AndelAntibiotikaTilForebyggingOgBehandling
Figure_AndelAntibiotikaTilForebyggingOgBehandling <- function(di, da, DATE_USE) {
  tab <- Data_AndelAntibiotikaTilForebyggingOgBehandling(di=di,da=da,DATE_USE=DATE_USE)
  if(nrow(tab)==0) return(no_data_graph())
  tab[, groupingLab:= glue::glue("{grouping} (n={n})",grouping=grouping,n=sum(n)),by=grouping]

  tab[,xLab:=sprintf("%s (n=%s)",Klassifisering, n)]
  setorder(tab,prop)
  tab[,xLab:=factor(xLab,levels=xLab)]

  fillVals <- unique(tab[,c("grouping","groupingLab")])
  skeleton <- data.table(grouping=c("Annet/ukjent","Behandling","Forebygging"),
                         colours=c("gray","green","yellow"))
  fillVals <- merge(fillVals,skeleton,by="grouping",all.y=T)
  fillVals[is.na(groupingLab),groupingLab:=glue::glue("{grouping} (n=0)",grouping=grouping)]
  fillValsVector <- fillVals$colours
  names(fillValsVector) <- fillVals$groupingLab

  tab[,groupingLab:=factor(groupingLab,levels=names(fillValsVector))]

  q <- ggplot(tab, aes(x = xLab, y = prop, fill= groupingLab))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_x_discrete("Klassifisering (n=antall forskrivninger)")
  q <- q + scale_y_continuous("Andel (%) forskrivninger antibiotika\ntil forebygging og behandling",
                              labels=scales::percent,
                              expand=c(0,0),
                              lim=c(0,max(tab$prop)*1.1))
  #q <- q + labs(main = "Andel antibiotika til forebygging og behandling")
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",values=fillValsVector, drop=F)
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(legend.position = "bottom")
  #q <- q + theme(axis.title.y = element_text(hjust=0.5))
  lemon::grid_arrange_shared_legend(q)
}
