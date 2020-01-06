#' CleanSpesialitet
#' @param d a
#' @export CleanSpesialitet
CleanSpesialitet <- function(d) {
  if ("Spesialitet" %in% names(d)) {
    # speciality start
    d[, c_spesialitet := SpesialitetKode]
    RAWmisc::RecodeDT(
      d,
      c(
        "S02" = "Kirurgi", # "Kirurgi"
        "S0201" = "Kirurgi", # "Generell kirurgi"
        "S0202" = "Kirurgi", # "Barnekirurgi"
        "S0203" = "Kirurgi", # "Endokrin kirurgi"
        "S0204" = "Kirurgi", # "Gastroenterologisk kirurgi"
        "S0205" = "Kirurgi", # "Karkirurgi"
        "S0206" = "Kirurgi", # "Kjeve- og ansiktskirurgi"
        "S0207" = "Kirurgi", # "Nevrokirurgi"
        "S0209" = "Kirurgi", # "Plastikkirurgi"
        "S0210" = "Kirurgi", # "Thoraxkirurgi"
        "S0211" = "Kirurgi", # "Urologi"

        "S0208" = "Ortopedisk kirurgi", # "Ortopedisk kirurgi"

        "S03" = "Indremedisin", # "Indremedisin"
        "S0301" = "Indremedisin", # "Endokrinologi og metabolisme"
        "S0302" = "Indremedisin", # "Fordøyelsessykdommer"
        "S0303" = "Indremedisin", # "Geriatri"
        "S0304" = "Indremedisin", # "Hematologi (blodsykdommer)"
        "S0305" = "Indremedisin", # "Infeksjonsmedisin"
        "S0306" = "Indremedisin", # "Hjertesykdommer"
        "S0307" = "Indremedisin", # "Lungesykdommer"
        "S0308" = "Indremedisin", # "Nyresykdommer"

        "S04" = glue::glue("F{fhi::nb$oe}dselshjelp og kvinnesykdommer"), # "Fødselshjelp og kvinnesykdommer"
        "S0401" = glue::glue("F{fhi::nb$oe}dselshjelp og kvinnesykdommer"), # "Generell gynekologi"
        "S0402" = glue::glue("F{fhi::nb$oe}dselshjelp og kvinnesykdommer"), # "Gynekologisk onkologi"
        "S0403" = glue::glue("F{fhi::nb$oe}dselshjelp og kvinnesykdommer"), # "Obstetrikk"

        "S05" = "Hud- og veneriske sykdommer", # "Hud- og veneriske sykdommer"

        "S06" = "Barnesykdommer", # "Barnesykdommer"
        "S0601" = "Barnesykdommer", # "Nyfødtmedisin"
        "S0602" = "Barnesykdommer", # "Barneintensiv"

        "S07" = "Nevrologi", # "Nevrologi"
        "S0701" = "Nevrologi", # "Generell nevrologi"
        "S0702" = "Nevrologi", # "Cerebrovaskulære sykdommer"

        "S09" = glue::glue("{fhi::nb$OE}re-nese-halssykdommer"), # "Øre-nese-hals"

        "S10" = glue::glue("{fhi::nb$OE}yesykdommer"), # "Øyesykdommer"

        "S11" = "Onkologi", # "Onkologi"

        "S12" = "Revmatologi", # "Revmatologi"

        "S16" = "Fysikalsk medisin og rehabilitering", # "Fysikalsk medisin/rehabilitering"

        "A02" = "Observasjon", # "Observasjon"

        "K A03/A04" = glue::glue("Kirurgisk intensiv/overv{fhi::nb$aa}king"), # "Kirurgisk intensiv / overvåking"

        "M A03/A04" = glue::glue("Medisinsk intensiv/overv{fhi::nb$aa}king"), # "Medisinsk intensiv / overvåking"

        "100" = "Annet/ukjent" # "Annet/ukjent"
      ),
      "c_spesialitet"
    )

    d[, c_spesialitet := factor(c_spesialitet, levels = c(
      "Kirurgi",
      "Ortopedisk kirurgi",
      "Indremedisin",
      glue::glue("F{fhi::nb$oe}dselshjelp og kvinnesykdommer"),
      "Hud- og veneriske sykdommer",
      "Barnesykdommer",
      "Nevrologi",
      glue::glue("{fhi::nb$OE}re-nese-halssykdommer"),
      glue::glue("{fhi::nb$OE}yesykdommer"),
      "Onkologi",
      "Revmatologi",
      "Fysikalsk medisin og rehabilitering",
      "Observasjon",
      glue::glue("Kirurgisk intensiv/overv{fhi::nb$aa}king"),
      glue::glue("Medisinsk intensiv/overv{fhi::nb$aa}king"),
      "Annet/ukjent"
    ))]
  }
}
