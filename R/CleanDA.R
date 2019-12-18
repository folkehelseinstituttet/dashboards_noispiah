#' CleanDA
#' @param da a
#' @param type a
#' @export CleanDA
CleanDA <- function(da, type="sykehjem"){
  ab <- readxl::read_excel(file.path(system.file("extdata", package = "noispiah"), "2019-05-06_Antibiotikagrupper.xlsx"))
  setDT(ab)

  if(type=="sykehjem"){
    da[,NumberPeople:=AntallBeboereKl8]
    da[,NumberPeopleSomGisAntibiotika:=AntallBeboereSomGisAntibiotika]
  } else {
    da[,NumberPeople:=AntallPasienterKl8]
    da[,NumberPeopleSomGisAntibiotika:=AntallPasienterSomGisAntibiotika]

    CleanSpesialitet(da)

    ab_1 <- ab[!is.na(sykehusAB1),c("ATCKode","sykehusAB1","sykehusAB1_order")]
    ab_1_order <- unique(ab_1[,c("sykehusAB1","sykehusAB1_order")])
    setorder(ab_1_order,sykehusAB1_order)
    da[ab_1,on="ATCKode",sykehusAB1:=sykehusAB1]
    da[is.na(sykehusAB1),sykehusAB1:="Andre antibiotika"]
    da[,sykehusAB1:=factor(sykehusAB1,levels=c(
      ab_1_order$sykehusAB1,
      "Andre antibiotika"
    ))]

    ab_2 <- ab[!is.na(sykehusAB2),c("ATCKode","sykehusAB2","sykehusAB2_order")]
    ab_2_order <- unique(ab_2[,c("sykehusAB2","sykehusAB2_order")])
    setorder(ab_2_order,sykehusAB2_order)
    da[ab_2,on="ATCKode",sykehusAB2:=sykehusAB2]
    da[is.na(sykehusAB2),sykehusAB2:="Andre antibiotika"]
    da[,sykehusAB2:=factor(sykehusAB2,levels=c(
      ab_2_order$sykehusAB2,
      "Andre antibiotika"
    ))]

    ab_3 <- ab[!is.na(sykehusAB3),c("ATCKode","sykehusAB3","sykehusAB3_order")]
    ab_3_order <- unique(ab_3[,c("sykehusAB3","sykehusAB3_order")])
    setorder(ab_3_order,sykehusAB3_order)
    da[ab_3,on="ATCKode",sykehusAB3:=sykehusAB3]
    da[is.na(sykehusAB3),sykehusAB3:="Andre antibiotika"]
    da[,sykehusAB3:=factor(sykehusAB3,levels=c(
      ab_3_order$sykehusAB3,
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
  da[ATCKode %in% ab[`ATCSubstans (virkestoff)` %in% c("Methenamine","Metenamin")]$ATCKode, ABBredMethAndre := "Metenamin"]
  da[ATCKode %in% ab[Gruppe == "Bredspektrede"]$ATCKode, ABBredMethAndre := "bredspektrede antibiotika"]
  # xtabs(~temp$AB)

  da[, ABBredAndre := ABBredMethAndre]
  da[ABBredMethAndre == "Metenamin", ABBredAndre := "bredspektrede antibiotika"]
  # xtabs(~temp$AB)

  # xtabs(~temp$forebyggingVsBehandling+temp$AB)
  # ForebyggBehandMethAndre
  da[, forebyggVsBehandOgMethVsAndre := sprintf("%s %s", forebyggingVsBehandling, ABBredMethAndre)]
  RAWmisc::RecodeDT(da,
                    switch=c(
                      "Forebygging Metenamin"="Forebygging metenamin",
                      "Forebygging andre antibiotika"="Forebygging andre antibiotika",
                      "Forebygging bredspektrede antibiotika"="Forebygging andre antibiotika",
                      "Behandling Metenamin"="Behandling metenamin",
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
    "N\u00F8ytropen feber",
    "Neutropen feber"
  ), IndikasjonCategory := "Klinisk sepsis"]

  da[, IndikasjonCategorySykehus := IndikasjonCategory]
  da[IndikasjonCategory %in% c("Klinisk sepsis"), IndikasjonCategorySykehus := "Klinisk sepsis, laboratoriebekreftet blodbaneinfeksjon og nøytropen feber"]

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
