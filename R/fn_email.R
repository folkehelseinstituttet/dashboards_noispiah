#' fn_email
#' @param data a
#' @param arg_pdf a
#' @export
fn_email <- function(data, arg_pdf){
  # data <- plan_email$data_get()
  # arg_email <- plan_email$analysis_get(1)$arg_email

  email_subject_sykehjem <- glue::glue(
    "Resultatrapporter fra prevalensundersøkelsen av ",
    "helsetjenesteassosierte infeksjoner og antibiotikabruk i sykehjem ",
    "våren 2019"
    )

  email_text_sykehjem <- glue::glue(
    "Hei<br><br>",

    "Vedlagt finner du Folkehelseinstituttets rapporter ",
    "med resultater fra prevalensundersøkelsen av helsetjenesteassosierte ",
    "infeksjoner og antibiotikabruk i sykehjem våren 2019. ",
    "Rapportene lages automatisk basert på rapportert datagrunnlag. ",
    "Hvis rapporter mangler informasjon fra et eller flere sykehjem, ",
    "skyldes det sannsynligvis at data ikke er levert.<br><br>",

    "Vi håper rapportene vil være nyttige i deres arbeid for god kvalitet på helsetjenestene.<br><br>",

    "Spørsmål og tilbakemeldinger på rapportene kan sendes til prevalens@fhi.no.<br><br>",

    "<i>Vennlig hilsen</i><br>",
    "<i>Avdeling for smittevernregistre og </i><br>",
    "<i>Seksjon for resistens- og infeksjonsforebygging</i><br>",
    "<i>Folkehelseinstituttet</i>"
  )

  email_subject_sykehus <- glue::glue(
    "Resultatrapporter fra prevalensundersøkelsen av ",
    "helsetjenesteassosierte infeksjoner og antibiotikabruk i sykehus ",
    "2. kvartal 2019"
  )

  email_text_sykehus <- glue::glue(
    "Hei<br><br>",

    "Vedlagt finner du Folkehelseinstituttets rapporter ",
    "med resultater fra prevalensundersøkelsen av helsetjenesteassosierte ",
    "infeksjoner og antibiotikabruk i sykehus 2. kvartal 2019.<br><br>",

    "Vi håper rapportene vil være nyttige i deres arbeid for god kvalitet på helsetjenestene.<br><br>",

    "Spørsmål og tilbakemeldinger på rapportene kan sendes til prevalens@fhi.no.<br><br>",

    "<i>Vennlig hilsen</i><br>",
    "<i>Avdeling for smittevernregistre og </i><br>",
    "<i>Seksjon for resistens- og infeksjonsforebygging</i><br>",
    "<i>Folkehelseinstituttet</i>"
  )

  if(arg_email$type=="sykehjem"){
    email_subject <- email_subject_sykehjem
    email_text <- email_text_sykehjem
  } else {
    email_subject <- email_subject_sykehus
    email_text <- email_text_sykehus
  }

  fd::mailgun(
    subject = email_subject,
    html = email_text,
    to = arg_email$email,
    attachments = arg_email$files$file_absolute
  )
}
