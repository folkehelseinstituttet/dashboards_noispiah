#' CleanSpesialitet
#' @param d a
#' @export CleanSpesialitet
CleanSpesialitet <- function(d){
  if("Spesialitet" %in% names(da)){
    # speciality start
    d[,c_spesialitet:=Spesialitet]
    RAWmisc::RecodeDT(d,
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

    d[,c_spesialitet:=factor(c_spesialitet,levels = c(
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
}
