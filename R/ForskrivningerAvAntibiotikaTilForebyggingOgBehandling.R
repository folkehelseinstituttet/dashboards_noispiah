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
                                                                       group1="IndikasjonCategory",
                                                                       group2="category") {
  . <- NULL

  ab <- readxl::read_excel(file.path(system.file("extdata", package = "noispiah"), "2018-08-30_Antibiotikagrupper.xlsx"))
  setDT(ab)

  temp <- da[PrevalensDato == DATE_USE &
    Klassifisering %in% c(
      "Helsetjenesteassosiert infeksjon",
      "Samfunnservervet infeksjon",
      "Kirpro1",
      "Kirpro2",
      "Kirpro3",
      "Medpro"
    )]

  # speciality start
  temp[,c_spesialitet:=Spesialitet]
  RAWmisc::RecodeDT(temp,
    c(
      "Ortopedisk kirurgi"="Ortopedisk kirurgi",

      "Fødselshjelp og kvinnesykdommer"="Gynekologi",
      "Generell gynekologi"="Gynekologi",
      "Gynekologisk onkologi"="Gynekologi",
      "Obstetrikk"="Gynekologi",

      "Endokrin kirurgi"="Kirurgi",
      "Gastroenterologisk kirurgi"="Kirurgi",
      "Generell kirurgi"="Kirurgi",
      "Karkirurgi"="Kirurgi",
      "Kirurgi"="Kirurgi",
      "Kjeve- og ansiktskirurgi"="Kirurgi",
      "Nevrokirurgi"="Kirurgi",
      "Plastikkirurgi"="Kirurgi",
      "Thoraxkirurgi"="Kirurgi",
      "Urologi"="Kirurgi",

      "Cerebrovaskulære sykdommer"="Indremedisin",
      "Endokrinologi og metabolisme"="Indremedisin",
      "Fordøyelsessykdommer"="Indremedisin",
      "Geriatri"="Indremedisin",
      "Hematologi (blodsykdommer)"="Indremedisin",
      "Hjertesykdommer"="Indremedisin",
      "Indremedisin"="Indremedisin",
      "Infeksjonsmedisin"="Indremedisin",
      "Lungesykdommer"="Indremedisin",
      "Nyresykdommer"="Indremedisin",
      "Observasjon"="Indremedisin",

      "Nyfødtmedisin"="Barnemedisin",
      "Barneintensiv"="Barnemedisin",
      "Barnekirurgi"="Barnemedisin",
      "Barnesykdommer"="Barnemedisin",

      "Kirurgisk intensiv / overvåking"="Kirurgisk intensiv / overvåking",

      "Medisinsk intensiv / overvåking"="Medisinsk intensiv / overvåking",

      "Onkologi"="Onkologi",

      "Generell nevrologi"="Nevrologi",
      "Nevrologi"="Nevrologi",

      "Fysikalsk medisin/rehabilitering"="Andre spesialiteter",
      "Hud- og veneriske sykdommer"="Andre spesialiteter",
      "Revmatologi"="Andre spesialiteter",
      "Øre-nese-hals"="Andre spesialiteter",
      "Øyesykdommer"="Andre spesialiteter",

      "Annet/ukjent"="Annet/ukjent"),
    "c_spesialitet"
    )

  temp[,c_spesialitet:=factor(c_spesialitet,levels = c(
    "Kirurgisk intensiv / overvåking",
    "Medisinsk intensiv / overvåking",
    "Ortopedisk kirurgi",
    "Kirurgi",
    "Indremedisin",
    "Onkologi",
    "Nevrologi",
    "Barnemedisin",
    "Gynekologi",
    "Andre spesialiteter",
    "Annet/ukjent"
  ))]

  # speciality end

  temp[, forebyggingVsBehandling := ""]
  temp[Klassifisering %in% c(
    "Helsetjenesteassosiert infeksjon",
    "Samfunnservervet infeksjon"
  ), forebyggingVsBehandling := "Behandling"]
  temp[Klassifisering %in% c(
    "Kirpro1",
    "Kirpro2",
    "Kirpro3",
    "Medpro"
  ), forebyggingVsBehandling := "Forebygging"]
  # xtabs(~temp$forebyggingVsBehandling)

  temp[, AB := "andre antibiotika"]
  temp[ATCKode %in% ab[`ATCSubstans (virkestoff)` == "Methenamine"]$ATCKode, AB := "Methenamine"]
  temp[ATCKode %in% ab[Gruppe == "Bredspektrede"]$ATCKode, AB := "bredspektrede antibiotika"]
  # xtabs(~temp$AB)

  # xtabs(~temp$forebyggingVsBehandling+temp$AB)
  temp[, category := sprintf("%s %s", forebyggingVsBehandling, AB)]
  RAWmisc::RecodeDT(temp,
                    switch=c(
                      "Forebygging Methenamine"="Forebygging Methenamine",
                      "Forebygging andre antibiotika"="Forebygging andre antibiotika",
                      "Forebygging bredspektrede antibiotika"="Forebygging andre antibiotika",
                      "Behandling Methenamine"="Behandling Methenamine",
                      "Behandling andre antibiotika"="Behandling andre antibiotika",
                      "Behandling bredspektrede antibiotika"="Behandling andre antibiotika"
                    ),
                    "category")
  # xtabs(~temp$category)
  # xtabs(~temp$Indikasjon)


  temp[, IndikasjonCategory := Indikasjon]
  temp[Indikasjon %in% c(
    "Klinisk sepsis med antatt utgangspunkt luftveier",
    "Klinisk sepsis med antatt utgangspunkt urinveier",
    "Klinisk sepsis med antatt utgangspunkt abdomen",
    "Klinisk sepsis med annet antatt utgangspunkt",
    "Klinisk sepsis med usikkert utgangspunkt",
    "Laboratoriebekreftet blodbaneinfeksjon",
    "N\u00F8ytropen feber"
  ), IndikasjonCategory := "Klinisk sepsis"]

  tab <- temp[, .(n = .N), by = .(
    forebyggingVsBehandling,
    AB,
    "group1"=get(group1),
    "group2"=get(group2))]
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
                                                                    group1="IndikasjonCategory",
                                                                    group2="category")
  tab[, denom := sum(n)]
  tab[, nCategory := sum(n), by=category]
  tab[, labCategory := sprintf("%s (n=%s)", category, nCategory)]

  ordering <- unique(tab[,c("labCategory","category")])
  ordering[,category:=factor(category,levels=c(
    "Forebygging Methenamine",
    "Forebygging andre antibiotika",
    "Behandling Methenamine",
    "Behandling andre antibiotika"
  ))]
  setorder(ordering,-category)
  tab[,labCategory:=factor(labCategory,levels=ordering$labCategory)]

  tab[, nIndikasjonCategory := sum(n), by=IndikasjonCategory]
  tab[, labIndikasjonCategory := sprintf("%s (n=%s)", IndikasjonCategory, nIndikasjonCategory)]

  ordering <- unique(tab[,c("labIndikasjonCategory","IndikasjonCategory","nIndikasjonCategory")])
  setorder(ordering,nIndikasjonCategory,IndikasjonCategory)
  tab[,labIndikasjonCategory:=factor(labIndikasjonCategory,levels=ordering$labIndikasjonCategory)]

  q <- ggplot(tab, aes(x = labIndikasjonCategory, y = n / denom * 100, fill = labCategory))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(ncol = 2, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Indikasjon")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til forebygging og behandling\n(n=%s)", sum(tab$n)))
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
                                                                    group1="IndikasjonCategory",
                                                                    group2="category")
  tab <- tab[forebyggingVsBehandling=="Behandling"]
  tab[, denom := sum(n)]
  tab[, nCategory := sum(n), by=category]
  tab[, labCategory := sprintf("%s (n=%s)", category, nCategory)]

  ordering <- unique(tab[,c("labCategory","category")])
  ordering[,category:=factor(category,levels=c(
    "Forebygging Methenamine",
    "Forebygging andre antibiotika",
    "Behandling Methenamine",
    "Behandling andre antibiotika"
  ))]
  setorder(ordering,-category)
  tab[,labCategory:=factor(labCategory,levels=ordering$labCategory)]

  tab[, nIndikasjonCategory := sum(n), by=IndikasjonCategory]
  tab[, labIndikasjonCategory := sprintf("%s (n=%s)", IndikasjonCategory, nIndikasjonCategory)]

  ordering <- unique(tab[,c("labIndikasjonCategory","IndikasjonCategory","nIndikasjonCategory")])
  setorder(ordering,nIndikasjonCategory,IndikasjonCategory)
  tab[,labIndikasjonCategory:=factor(labIndikasjonCategory,levels=ordering$labIndikasjonCategory)]

  q <- ggplot(tab, aes(x = labIndikasjonCategory, y = n / denom, fill = labCategory))
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
                                                                    group1="c_spesialitet",
                                                                    group2="c_spesialitet")
  tab <- tab[forebyggingVsBehandling=="Behandling"]
  tab[, denom := sum(n)]
  tab[, nSpesialitet := sum(n), by=c_spesialitet]
  tab[, labSpesialitet := sprintf("%s (n=%s)", c_spesialitet, nSpesialitet)]

  ordering <- unique(tab[,c("labSpesialitet","c_spesialitet")])
  setorder(ordering,-c_spesialitet,labSpesialitet)
  tab[,labSpesialitet:=factor(labSpesialitet,levels=ordering$labSpesialitet)]

  q <- ggplot(tab, aes(x = labSpesialitet, y = n / denom, fill = AB))
  q <- q + geom_col(colour = "black", alpha = 0.5)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(ncol = 2, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Spesialitet")
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
