#' CleanDA
#' @param da a
#' @param type a
#' @export CleanDA
CleanDA <- function(da, type="sykehjem"){
  ab <- readxl::read_excel(file.path(system.file("extdata", package = "noispiah"), "2018-08-30_Antibiotikagrupper.xlsx"))
  setDT(ab)

  if(type=="sykehjem"){
    da[,NumberPeople:=AntallBeboereKl8]
    da[,NumberPeopleSomGisAntibiotika:=AntallBeboereSomGisAntibiotika]
  } else {
    da[,NumberPeople:=AntallPasienterKl8]
    da[,NumberPeopleSomGisAntibiotika:=AntallPasienterSomGisAntibiotika]

    CleanSpesialitet(da)
  }

  da[, forebyggingVsBehandling := as.character(NA)]
  da[Klassifisering %in% c(
    "Helsetjenesteassosiert infeksjon",
    "Samfunnservervet infeksjon"
  ), forebyggingVsBehandling := "Behandling"]
  da[Klassifisering %in% c(
    "Kirpro1",
    "Kirpro2",
    "Kirpro3",
    "Medpro"
  ), forebyggingVsBehandling := "Forebygging"]



  # AB, Bred, Meth, Andre
  da[, ABBredMethAndre := "andre antibiotika"]
  da[ATCKode %in% ab[`ATCSubstans (virkestoff)` == "Methenamine"]$ATCKode, ABBredMethAndre := "Methenamine"]
  da[ATCKode %in% ab[Gruppe == "Bredspektrede"]$ATCKode, ABBredMethAndre := "bredspektrede antibiotika"]
  # xtabs(~temp$AB)

  # xtabs(~temp$forebyggingVsBehandling+temp$AB)
  # ForebyggBehandMethAndre
  da[, forebyggVsBehandOgMethVsAndre := sprintf("%s %s", forebyggingVsBehandling, ABBredMethAndre)]
  RAWmisc::RecodeDT(da,
                    switch=c(
                      "Forebygging Methenamine"="Forebygging Methenamine",
                      "Forebygging andre antibiotika"="Forebygging andre antibiotika",
                      "Forebygging bredspektrede antibiotika"="Forebygging andre antibiotika",
                      "Behandling Methenamine"="Behandling Methenamine",
                      "Behandling andre antibiotika"="Behandling andre antibiotika",
                      "Behandling bredspektrede antibiotika"="Behandling andre antibiotika"
                    ),
                    "forebyggVsBehandOgMethVsAndre")

  da[, IndikasjonCategory := Indikasjon]
  da[Indikasjon %in% c(
    "Klinisk sepsis med antatt utgangspunkt luftveier",
    "Klinisk sepsis med antatt utgangspunkt urinveier",
    "Klinisk sepsis med antatt utgangspunkt abdomen",
    "Klinisk sepsis med annet antatt utgangspunkt",
    "Klinisk sepsis med usikkert utgangspunkt",
    "Laboratoriebekreftet blodbaneinfeksjon",
    "N\u00F8ytropen feber"
  ), IndikasjonCategory := "Klinisk sepsis"]

  da[, IndikasjonCategorySykehus := IndikasjonCategory]
  da[IndikasjonCategory %in% c("Klinisk sepsis"), IndikasjonCategorySykehus := "Klinisk sepsis, laboratoriebekreftet blodbaneinfeksjon og nøytropen feber"]

  RAWmisc::RecodeDT(da,
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

  da[,SykehusKlassifisering:=Klassifisering]
  RAWmisc::RecodeDT(da,
                    c(
                      "Kirurgisk profylakse 1"="Kirurgisk profylakse",
                      "Kirurgisk profylakse 2"="Kirurgisk profylakse",
                      "Kirurgisk profylakse 3"="Kirurgisk profylakse"
                    ),var="SykehusKlassifisering")


}
