#' Data_AndelAntibiotikaTilForebyggingOgBehandling
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @export Data_AndelAntibiotikaTilForebyggingOgBehandling
Data_AndelAntibiotikaTilForebyggingOgBehandling <- function(di, da, DATE_USE) {
  temp <- da[PrevalensDato == DATE_USE &
               Klassifisering %in% c(
                 "Helsetjenesteassosiert infeksjon",
                 "Samfunnservervet infeksjon",
                 "Kirpro1",
                 "Kirpro2",
                 "Kirpro3",
                 "Medpro",
                 "Annet",
                 "Ukjent"
               )]

  RAWmisc::RecodeDT(temp,
                    c(
                      "Helsetjenesteassosiert infeksjon"="Helsetjenesteassosiert infeksjon",
                      "Samfunnservervet infeksjon"="Samfunnservervet infeksjon",
                      "Kirpro1"="Kirurgisk profylakse 1",
                      "Kirpro2"="Kirurgisk profylakse 2",
                      "Kirpro3"="Kirurgisk profylakse 3",
                      "Medpro"="Medisinsk profylakse",
                      "Annet"="Annet/ukjent",
                      "Ukjent"="Annet/ukjent"
                    ),var="Klassifisering")

  tab <- temp[, .(n = .N), by = .(Klassifisering)]
  tab[, denom := sum(n)]
  tab[,prop:=n/denom]

  return(tab)
}

#' Figure_AndelAntibiotikaTilForebyggingOgBehandling
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @export Figure_AndelAntibiotikaTilForebyggingOgBehandling
Figure_AndelAntibiotikaTilForebyggingOgBehandling <- function(di, da, DATE_USE) {
  tab <- Data_AndelAntibiotikaTilForebyggingOgBehandling(di=di,da=da,DATE_USE=DATE_USE)
  tab[,xLab:=sprintf("%s (n=%s)",Klassifisering, n)]
  setorder(tab,prop)
  tab[,xLab:=factor(xLab,levels=xLab)]

  q <- ggplot(tab, aes(x = xLab, y = prop))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_x_discrete("Indikasjon")
  q <- q + scale_y_continuous("Andel (%) forskrivninger antibiotika til forebygging og behandling",
                              labels=scales::percent)
  #q <- q + labs(main = "Andel antibiotika til forebygging og behandling")
  q <- q + coord_flip()
  q <- q + theme(legend.position = "bottom")
  q
}
