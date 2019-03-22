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
  fillVals[,colours:=""]
  fillVals[grouping %in% c("Behandling"), colours:="green"]
  fillVals[grouping %in% c("Forebygging"), colours:="yellow"]
  fillVals[grouping %in% c("Annet/ukjent"), colours:="gray"]
  fillValsVector <- fillVals$colours
  names(fillValsVector) <- fillVals$groupingLab

  q <- ggplot(tab, aes(x = xLab, y = prop, fill= groupingLab))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_x_discrete("Klassifisering (antall forskrivninger)")
  q <- q + scale_y_continuous("Andel (%) forskrivninger antibiotika\ntil forebygging og behandling",
                              labels=scales::percent)
  #q <- q + labs(main = "Andel antibiotika til forebygging og behandling")
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",values=fillValsVector)
  q <- q + theme(legend.position = "bottom")
  #q <- q + theme(axis.title.y = element_text(hjust=0.5))
  lemon::grid_arrange_shared_legend(q)
}
