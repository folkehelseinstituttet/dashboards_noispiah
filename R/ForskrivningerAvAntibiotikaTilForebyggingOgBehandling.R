#' Data_ForekomstHAIiSykehjemPerAvdelingstype
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForekomstHAIiSykehjemPerAvdelingstype
Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling <- function(di,da,DATE_USE){
  . <- NULL

  ab <- readxl::read_excel(file.path(system.file("extdata",package="noispiah"),"2018-08-30_Antibiotikagrupper.xlsx"))
  setDT(ab)

  temp <- da[InstitusjonType=="Sykehjem" & PrevalensDato==DATE_USE &
    Klassifisering %in% c(
    "Helsetjenesteassosiert infeksjon",
    "Samfunnservervet infeksjon",
    "Kirpro1",
    "Kirpro2",
    "Kirpro3",
    "Medpro"
  )]

  temp[,forebyggingVsBehandling:=""]
  temp[Klassifisering %in% c(
    "Helsetjenesteassosiert infeksjon",
    "Samfunnservervet infeksjon"
  ),forebyggingVsBehandling:="Behandling"]
  temp[Klassifisering %in% c(
    "Kirpro1",
    "Kirpro2",
    "Kirpro3",
    "Medpro"
  ),forebyggingVsBehandling:="Forebygging"]
  xtabs(~temp$forebyggingVsBehandling)

  temp[,AB:="andre antibiotika"]
  temp[ATCKode %in% ab[`ATCSubstans (virkestoff)`=="Methenamine"]$ATCKode,AB:="Methenamine"]
  temp[ATCKode %in% ab[Gruppe=="Bredspektrede"]$ATCKode,AB:="bredspektrede antibiotika"]
  xtabs(~temp$AB)

  xtabs(~temp$forebyggingVsBehandling+temp$AB)
  temp[,category:=sprintf("%s %s",forebyggingVsBehandling,AB)]
  xtabs(~temp$category)


  temp[,IndikasjonCategory:=Indikasjon]
  temp[Indikasjon %in% c(
    "Klinisk sepsis med antatt utgangspunkt luftveier",
    "Klinisk sepsis med antatt utgangspunkt urinveier",
    "Klinisk sepsis med antatt utgangspunkt abdomen",
    "Klinisk sepsis med annet antatt utgangspunkt",
    "Klinisk sepsis med usikkert utgangspunkt",
    "Laboratoriebekreftet blodbaneinfeksjon",
    "Nøytropen feber"
  ),IndikasjonCategory:="Klinisk sepsis"]

  tab <- temp[,.(n=.N),by=.(IndikasjonCategory,category)]
  tab[,denom:=sum(n),by=IndikasjonCategory]

  return(tab)
}


#' Figure_ForekomstHAIiSykehjemPerAvdelingstype
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_ForekomstHAIiSykehjemPerAvdelingstype
Figure_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling <- function(di,da,DATE_USE){
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(di=di,da=da,DATE_USE=DATE_USE)
  tab[,xLab := sprintf("%s (%s)",Avdelingstype,AntallBeboereKl8)]

  q <- ggplot(tab,aes(x=IndikasjonCategory,y=n/denom*100,fill=category))
  q <- q + geom_col(colour="black")
  q <- q + scale_fill_brewer("",palette="Set1",guide=guide_legend(ncol=2,byrow=T,reverse = TRUE))
  q <- q + scale_x_discrete("Indikasjon")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til forebygging og behandling (n=%s)",sum(tab$n)))
  q <- q + labs(main="Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position="bottom")
  q
}
