#' Data_ForekomstHAIiSykehjemPerAvdelingstype
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForekomstHAIiSykehjemPerAvdelingstype
Data_ForekomstHAIiSykehjemPerAvdelingstype <- function(di,da,DATE_USE){
  . <- NULL
  InstitusjonType <- NULL
  PrevalensDato <- NULL
  InstitusjonId <- NULL
  AntallBeboereKl8 <- NULL
  AntallBeboereMedInfeksjon <- NULL
  AntallUrinveisInfeksjonerUtenUrinveiskateter_EgenInstitusjon <- NULL
  AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehus <- NULL
  AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehjem <- NULL
  AntallUrinveisInfeksjonerMedUrinveiskateter_EgenInstitusjon <- NULL
  AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehus <- NULL
  AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehjem <- NULL
  AntallNedreLuftveisInfeksjoner_EgenInstitusjon <- NULL
  AntallNedreLuftveisInfeksjoner_AnnetSykehus <- NULL
  AntallNedreLuftveisInfeksjoner_AnnetSykehjem <- NULL
  AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon <- NULL
  AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus <- NULL
  AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem <- NULL
  AntallDypePostOpSarinfeksjoner_EgenInstitusjon <- NULL
  AntallDypePostOpSarinfeksjoner_AnnetSykehus <- NULL
  AntallDypePostOpSarinfeksjoner_AnnetSykehjem <- NULL
  AntallHudInfeksjoner_EgenInstitusjon <- NULL
  AntallHudInfeksjoner_AnnetSykehus <- NULL
  AntallHudInfeksjoner_AnnetSykehjem <- NULL
  andelBeboereMedInfeksjonHAI <- NULL
  antallBeboereMedInfeksjonHAI <- NULL
  antallBeboereHAI <- NULL
  prevalensAvInfeksjonerHAI <- NULL
  antallInfeksjonerHAI <- NULL
  AntallBeboereSomGisAntibiotika <- NULL
  ATCSubstans <- NULL
  AndelBeboereSomGisAB <- NULL
  antallBeboerePaAB <- NULL
  antallBeboereAB <- NULL
  PrevalensAvABbruk <- NULL
  AntallForskrivningerAB <- NULL

  Avdelingstype <- NULL
  perc <- NULL
  value <- NULL
  variable <- NULL
  xLab <- NULL

  tab <- di[InstitusjonType=="Sykehjem" & PrevalensDato==DATE_USE,
           .(
             AntallBeboereKl8=sum(AntallBeboereKl8),
             antallInfeksjonerUrine=sum(
               AntallUrinveisInfeksjonerUtenUrinveiskateter_EgenInstitusjon+
               AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehus+
               AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehjem+
               AntallUrinveisInfeksjonerMedUrinveiskateter_EgenInstitusjon+
               AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehus+
               AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehjem),
             antallInfeksjonerNedreLuftveis=sum(
               AntallNedreLuftveisInfeksjoner_EgenInstitusjon+
               AntallNedreLuftveisInfeksjoner_AnnetSykehus+
               AntallNedreLuftveisInfeksjoner_AnnetSykehjem),
             antallInfeksjonerOperasjonsOmrade=sum(
               AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon+
               AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus+
               AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem+
               AntallDypePostOpSarinfeksjoner_EgenInstitusjon+
               AntallDypePostOpSarinfeksjoner_AnnetSykehus+
               AntallDypePostOpSarinfeksjoner_AnnetSykehjem),
             antallInfeksjonerHud=sum(
               AntallHudInfeksjoner_EgenInstitusjon+
               AntallHudInfeksjoner_AnnetSykehus+
               AntallHudInfeksjoner_AnnetSykehjem)
           ),
           by=.(Avdelingstype)]
  tab <- melt.data.table(tab, id.vars=c("Avdelingstype","AntallBeboereKl8"))
  tab[,perc:=value/AntallBeboereKl8*100]
  RAWmisc::RecodeDT(
    tab,
    switch=c(
      "antallInfeksjonerUrine"="Urinveisinfeksjoner",
      "antallInfeksjonerNedreLuftveis"="Nedre luftveisinfeksjoner",
      "antallInfeksjonerOperasjonsOmrade"="Infeksjoner i operasjonsomr\u00E5de",
      "antallInfeksjonerHud"="Hudinfeksjoner"
    ),
    var="variable")

  tab[,variable:=factor(variable,levels=c(
    "Hudinfeksjoner",
    "Infeksjoner i operasjonsomr\u00E5de",
    "Nedre luftveisinfeksjoner",
    "Urinveisinfeksjoner"
  ))]

  return(tab)
}


#' Figure_ForekomstHAIiSykehjemPerAvdelingstype
#' @param di a
#' @param da a
#' @param DATE_USE a
#' @import data.table
#' @import ggplot2
#' @export Figure_ForekomstHAIiSykehjemPerAvdelingstype
Figure_ForekomstHAIiSykehjemPerAvdelingstype <- function(di,da,DATE_USE){
  xLab <- NULL
  Avdelingstype <- NULL
  AntallBeboereKl8 <- NULL
  perc <- NULL
  variable <- NULL

  tab <- Data_ForekomstHAIiSykehjemPerAvdelingstype(di=di,da=da,DATE_USE=DATE_USE)
  tab[,xLab := sprintf("%s (%s)",Avdelingstype,AntallBeboereKl8)]

  q <- ggplot(tab,aes(x=xLab,y=perc,fill=variable))
  q <- q + geom_bar(stat="identity",colour="black")
  q <- q + scale_fill_brewer("",palette="Set1",guide=guide_legend(ncol=2,byrow=T,reverse = TRUE))
  q <- q + scale_x_discrete("Avdelingstype (antall beboere)")
  q <- q + scale_y_continuous("Prevalens av helsetjenesteassosierte infeksjoner")
  q <- q + labs(main="Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position="bottom")
  q
}
