#' fn_email
#' @param data a
#' @param argset_email a
#' @export
fn_email <- function(data, argset_email) {
  # data <- plan_email$data_get()
  # argset_email <- plan_email$analysis_get(1)$argset_email

  email_subject_sykehjem <- glue::glue(
    "Resultatrapporter fra prevalensunders{fhi::nb$oe}kelsen av ",
    "helsetjenesteassosierte infeksjoner og antibiotikabruk i sykehjem ",
    "{convert_date_to_season(argset_email$DATE_USE)}"
  )

  email_text_sykehjem <- glue::glue(
    "Hei<br><br>",

    "Vedlagt finner du Folkehelseinstituttets rapporter ",
    "med resultater fra prevalensunders{fhi::nb$oe}kelsen av helsetjenesteassosierte ",
    "infeksjoner og antibiotikabruk i sykehjem {convert_date_to_season(argset_email$DATE_USE)}. ",
    "Rapportene lages automatisk basert p{fhi::nb$aa} rapportert datagrunnlag. ",
    "Hvis rapporter mangler informasjon fra et eller flere sykehjem, ",
    "skyldes det sannsynligvis at data ikke er levert.<br><br>",

    "Vi h{fhi::nb$aa}per rapportene vil v{fhi::nb$ae}re nyttige i deres arbeid for god kvalitet p{fhi::nb$aa} helsetjenestene.<br><br>",

    "Sp{fhi::nb$oe}rsm{fhi::nb$aa}l og tilbakemeldinger p{fhi::nb$aa} rapportene kan sendes til prevalens@fhi.no.<br><br>",

    "<i>Vennlig hilsen</i><br>",
    "<i>Avdeling for smittevernregistre og </i><br>",
    "<i>Seksjon for resistens- og infeksjonsforebygging</i><br>",
    "<i>Folkehelseinstituttet</i><br><br>",
    "<img src='cid:logo_kort.png' width='75'>"
  )

  email_subject_sykehus <- glue::glue(
    "Resultatrapporter fra prevalensunders{fhi::nb$oe}kelsen av ",
    "helsetjenesteassosierte infeksjoner og antibiotikabruk i sykehus ",
    "{convert_date_to_kvartal(argset_email$DATE_USE)}"
  )

  email_text_sykehus <- glue::glue(
    "Hei<br><br>",

    "Vedlagt finner du Folkehelseinstituttets rapporter ",
    "med resultater fra prevalensunders{fhi::nb$oe}kelsen av helsetjenesteassosierte ",
    "infeksjoner og antibiotikabruk i sykehus {convert_date_to_kvartal(argset_email$DATE_USE)}.<br><br>",

    "Vi h{fhi::nb$aa}per rapportene vil v{fhi::nb$ae}re nyttige i deres arbeid for god kvalitet p{fhi::nb$aa} helsetjenestene.<br><br>",

    "Sp{fhi::nb$oe}rsm{fhi::nb$aa}l og tilbakemeldinger p{fhi::nb$aa} rapportene kan sendes til prevalens@fhi.no.<br><br>",

    "<i>Vennlig hilsen</i><br>",
    "<i>Avdeling for smittevernregistre og </i><br>",
    "<i>Seksjon for resistens- og infeksjonsforebygging</i><br>",
    "<i>Folkehelseinstituttet</i><br><br>",
    "<img src='cid:logo_kort.png' width='75'>"
  )

  if (argset_email$type == "sykehjem") {
    email_subject <- email_subject_sykehjem
    email_text <- email_text_sykehjem
  } else {
    email_subject <- email_subject_sykehus
    email_text <- email_text_sykehus
  }

  fd::mailgun(
    subject = email_subject,
    html = email_text,
    to = argset_email$email,
    bcc = c("Torunn.Alberg@fhi.no", "RichardAubrey.White@fhi.no"),
    inlines = system.file("extdata", "logo_kort.png", package = "noispiah"),
    attachments = argset_email$files$file_absolute,
    include_footer = F
  )
}
