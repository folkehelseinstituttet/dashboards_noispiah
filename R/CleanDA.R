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

    da[!is.na(ATCKode), sykehusAB1:="Andre antibiotika"]
    da[ATCKode %in% c(
      "J01CR05"
    ), sykehusAB1:="Piperacillin med enzymhemmer"]
    da[ATCKode %in% c(
      "J01DC02"
    ), sykehusAB1:="2. generasjons cefalosporiner"]
    da[ATCKode %in% c(
      "J01DD01",
      "J01DD02",
      "J01DD04",
      "J01DD52"
    ), sykehusAB1:="3. generasjons cefalosporiner"]
    da[ATCKode %in% c(
      "J01MA01",
      "J01MA02",
      "J01MA12",
      "JO1MA14"
    ), sykehusAB1:="Kinoloner"]
    da[ATCKode %in% c(
      "J01DH02",
      "J01DH03",
      "J01DH51"
    ), sykehusAB1:="Karbapenemer"]

    da[,sykehusAB1:=factor(sykehusAB1,levels=c(
      "Piperacillin med enzymhemmer",
      "2. generasjons cefalosporiner",
      "3. generasjons cefalosporiner",
      "Kinoloner",
      "Karbapenemer",
      "Andre antibiotika"
    ))]

    #
    da[!is.na(ATCKode), sykehusAB2:="Andre antibiotika"]
    da[ATCKode %in% ab[Gruppe == "Bredspektrede"]$ATCKode, sykehusAB2 := "Bredspektrede antibiotika"]
    da[ATCKode %in% c(
      "J01CE01",
      "J01CE02"
    ), sykehusAB2:="Fenoksymetyl- og benzylpenicillin"]

    da[,sykehusAB2:=factor(sykehusAB2,levels=c(
      "Bredspektrede antibiotika",
      "Fenoksymetyl- og benzylpenicillin",
      "Andre antibiotika"
    ))]

    #
    da[!is.na(ATCKode), sykehusAB3:="Andre antibiotika"]
    da[ATCKode %in% c(
      "J01DD01",
      "J01DD02",
      "J01DD04",
      "J01DD52"
    ), sykehusAB3:="3. generasjons cefalosporiner"]
    da[ATCKode %in% c(
      "J01CR05"
    ), sykehusAB3:="Bredspektrede penicilliner"]
    da[ATCKode %in% c(
      "J01CE01",
      "J01CE02"
    ), sykehusAB3:="Fenoksymetyl- og benzylpenicillin"]

    da[,sykehusAB3:=factor(sykehusAB3,levels=c(
      "3. generasjons cefalosporiner",
      "Bredspektrede penicilliner",
      "Fenoksymetyl- og benzylpenicillin",
      "Andre antibiotika"
    ))]

    #
    toMerge <- ab[,c("ATCKode","Tekst på y-akse")]
    setnames(toMerge,c("ATCKode","sykehusAB4"))

    nrow(da)
    da <- merge(da,toMerge,by="ATCKode",all.x=T)
    nrow(da)

    #
    da[!is.na(ATCKode), sykehusAB5:="Andre antibiotika"]
    da[ATCKode %in% ab[Gruppe == "Bredspektrede"]$ATCKode, sykehusAB5 := "Bredspektrede antibiotika"]

    da[,sykehusAB5:=factor(sykehusAB5,levels=c(
      "Bredspektrede antibiotika",
      "Andre antibiotika"
    ))]
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

  da[Indikasjon %in% c(
    "Klinisk sepsis med antatt utgangspunkt luftveier",
    "Klinisk sepsis med antatt utgangspunkt urinveier",
    "Klinisk sepsis med antatt utgangspunkt abdomen",
    "Klinisk sepsis med annet antatt utgangspunkt",
    "Klinisk sepsis med usikkert utgangspunkt",
    "Laboratoriebekreftet blodbaneinfeksjon",
    "N\u00F8ytropen feber"
  ), IndikasjonCategory := "Klinisk sepsis"]

  da[, IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:=IndikasjonCategorySykehus]
  da[Klassifisering=="Helsetjenesteassosiert infeksjon" & IndikasjonCategorySykehus=="Nedre luftveisinfeksjon",
     IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:="Helsetjenesteassosiert nedre luftveisinfeksjon"]
  da[Klassifisering=="Samfunnservervet infeksjon" & IndikasjonCategorySykehus=="Nedre luftveisinfeksjon",
     IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:="Samfunnservervet nedre luftveisinfeksjon"]
  da[Klassifisering=="Medisinsk profylakse" & IndikasjonCategorySykehus=="Nedre luftveisinfeksjon",
     IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:="Profylaksis nedre luftveisinfeksjon"]

  da[, IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:=IndikasjonCategorySykehus]
  da[Klassifisering=="Helsetjenesteassosiert infeksjon" & IndikasjonCategorySykehus=="Nedre luftveisinfeksjon",
     IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:="Helsetjenesteassosiert nedre luftveisinfeksjon"]
  da[Klassifisering=="Samfunnservervet infeksjon" & IndikasjonCategorySykehus=="Nedre luftveisinfeksjon",
     IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:="Samfunnservervet nedre luftveisinfeksjon"]
  da[Klassifisering=="Medisinsk profylakse" & IndikasjonCategorySykehus=="Nedre luftveisinfeksjon",
     IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon:="Profylaksis nedre luftveisinfeksjon"]


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

  return(da)
}
