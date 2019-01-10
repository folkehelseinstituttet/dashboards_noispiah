#' CleanDA
#' @param da a
#' @export CleanDA
CleanDA <- function(da){
  ab <- readxl::read_excel(file.path(system.file("extdata", package = "noispiah"), "2018-08-30_Antibiotikagrupper.xlsx"))
  setDT(ab)

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

  if("Spesialitet" %in% names(da)){
    # speciality start
    da[,c_spesialitet:=Spesialitet]
    RAWmisc::RecodeDT(da,
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

    da[,c_spesialitet:=factor(c_spesialitet,levels = c(
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
  }

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
}
