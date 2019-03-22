#' Data_DeltagelseForekomstHAIogABiSykehjem
#' @param di a
#' @param da a
#' @param level a
#' @param DATE_USE a
#' @param type sykehjem
#' @import data.table
#' @export Data_DeltagelseForekomstHAIogABiSykehjem
Data_DeltagelseForekomstHAIogABiSykehjem <- function(di, da, level, DATE_USE, type="sykehjem") {
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

  if(type=="sykehjem"){
    if (!level %in% c("landsdekkende", "fylke", "kommune")) stop("Bad level")

    if (level == "landsdekkende") {
      varGrouping <- "Fylke"
    } else if (level == "fylke") {
      varGrouping <- "Kommune"
    } else if (level == "kommune") {
      varGrouping <- "Institusjon"
    }
  } else {
    if (!level %in% c("landsdekkende", "helseforetak", "institusjon")) stop("Bad level")

    if (level == "landsdekkende") {
      varGrouping <- "HelseForetak"
    } else if (level == "helseforetak") {
      varGrouping <- "Institusjon"
    } else if (level == "institusjon") {
      varGrouping <- "Institusjon"
    }
  }

  t1a <- di[PrevalensDato == DATE_USE,
    .(
      antallSykehjemHAI = length(unique(InstitusjonId)),
      antallBeboereHAI = sum(NumberPeople),
      antallBeboereMedInfeksjonHAI = sum(NumberPeopleMedInfeksjon),
      antallInfeksjonerHAI = sum(antallInfeksjonerHAI)
    ),
    by = .(get(varGrouping))
  ]
  t1b <- di[PrevalensDato == DATE_USE,
            .(
              antallSykehjemHAI = length(unique(InstitusjonId)),
              antallBeboereHAI = sum(NumberPeople),
              antallBeboereMedInfeksjonHAI = sum(NumberPeopleMedInfeksjon),
              antallInfeksjonerHAI = sum(antallInfeksjonerHAI)
            )
            ]
  t1b[,get:="ØØØ"]
  t1 <- rbind(t1a,t1b)

  t1[, andelBeboereMedInfeksjonHAI := RAWmisc::Format(100 * antallBeboereMedInfeksjonHAI / antallBeboereHAI, 1)]
  t1[, prevalensAvInfeksjonerHAI := RAWmisc::Format(100 * antallInfeksjonerHAI / antallBeboereHAI, 1)]
  setnames(t1, "get", varGrouping)

  # creating T2
  daTemp1 <- unique(da[PrevalensDato == DATE_USE,
                       c(
                         varGrouping,
                         "InstitusjonId",
                         "Avdeling",
                         "NumberPeople",
                         "NumberPeopleSomGisAntibiotika"
                       ),
                       with = F
                       ])

  daTemp1 <- daTemp1[,
                     .(
                       antallSykehjemAB = length(unique(InstitusjonId)),
                       antallBeboereAB = sum(NumberPeople),
                       antallBeboerePaAB = sum(NumberPeopleSomGisAntibiotika)
                     ),
                     by = .(get(varGrouping))
                     ]

  daTemp2 <- da[PrevalensDato == DATE_USE,
                .(AntallForskrivningerAB = sum(!is.na(ATCSubstans))),
                by = .(get(varGrouping))
                ]

  t2a <- merge(daTemp1, daTemp2, by = "get", all = T)

  daTemp1 <- unique(da[PrevalensDato == DATE_USE,
                       c(
                         varGrouping,
                         "InstitusjonId",
                         "Avdeling",
                         "NumberPeople",
                         "NumberPeopleSomGisAntibiotika"
                       ),
                       with = F
                       ])

  daTemp1 <- daTemp1[,
                     .(
                       antallSykehjemAB = length(unique(InstitusjonId)),
                       antallBeboereAB = sum(NumberPeople),
                       antallBeboerePaAB = sum(NumberPeopleSomGisAntibiotika)
                     )
                     ]

  daTemp2 <- da[PrevalensDato == DATE_USE,
                .(AntallForskrivningerAB = sum(!is.na(ATCSubstans)))
                ]
  if(nrow(daTemp2)==0){
    daTemp2 <- data.table(AntallForskrivningerAB=0)
  }

  t2b <- cbind(daTemp1, daTemp2)
  t2b[,get:="ØØØ"]

  t2 <- rbind(t2a,t2b)
  setnames(t2, "get", varGrouping)

  t2[, AndelBeboereSomGisAB := RAWmisc::Format(100 * antallBeboerePaAB / antallBeboereAB, 1)]
  t2[, PrevalensAvABbruk := RAWmisc::Format(100 * AntallForskrivningerAB / antallBeboereAB, 1)]

  tab <- merge(t1, t2, by = varGrouping, all = T)
  tab[get(varGrouping)=="ØØØ",(varGrouping):="Sammenslått"]

  # fixing missing
  tab[is.na(antallSykehjemAB),antallSykehjemAB:=0]

  # making pretty
  tab[,xandelBeboereMedInfeksjonHAI:=sprintf("%s%% (n=%s)",andelBeboereMedInfeksjonHAI,antallBeboereMedInfeksjonHAI)]
  tab[,xprevalensAvInfeksjonerHAI:=sprintf("%s%% (n=%s)",prevalensAvInfeksjonerHAI,antallInfeksjonerHAI)]

  tab[,xAndelBeboereSomGisAB:=sprintf("%s%% (n=%s)",AndelBeboereSomGisAB,antallBeboerePaAB)]
  tab[,xPrevalensAvABbruk:=sprintf("%s%% (n=%s)",PrevalensAvABbruk,AntallForskrivningerAB)]

  # fixing percentages w/ denoms of 0
  tab[antallSykehjemHAI==0,antallBeboereHAI:=0]
  tab[antallSykehjemHAI==0,xandelBeboereMedInfeksjonHAI:="-"]
  tab[antallSykehjemHAI==0,xprevalensAvInfeksjonerHAI:="-"]

  tab[antallSykehjemAB==0,antallBeboereAB:=0]
  tab[antallSykehjemAB==0,xAndelBeboereSomGisAB:="-"]
  tab[antallSykehjemAB==0,xPrevalensAvABbruk:="-"]


  tab <- tab[, c(
    varGrouping,
    "antallSykehjemHAI",
    "antallBeboereHAI",
    "xandelBeboereMedInfeksjonHAI",
    "xprevalensAvInfeksjonerHAI",
    "antallSykehjemAB",
    "antallBeboereAB",
    "xAndelBeboereSomGisAB",
    "xPrevalensAvABbruk"
  ), with = F]
  tab <- as.matrix(tab)

  return(tab)
}


#' Table_DeltagelseForekomstHAIogABiSykehjem
#' @param di a
#' @param da a
#' @param level a
#' @param DATE_USE a
#' @param type sykehjem
#' @import data.table
#' @import xtable
#' @export Table_DeltagelseForekomstHAIogABiSykehjem
Table_DeltagelseForekomstHAIogABiSykehjem <- function(di, da, level, DATE_USE, type="sykehjem") {
  tab <- Data_DeltagelseForekomstHAIogABiSykehjem(di = di, da = da, level = level, DATE_USE = DATE_USE, type=type)

  if(type=="sykehjem"){
    if (!level %in% c("landsdekkende", "fylke", "kommune")) stop("Bad level")

    if (level == "landsdekkende") {
      varGrouping <- "Fylke"
    } else if (level == "fylke") {
      varGrouping <- "Kommune"
    } else if (level == "kommune") {
      varGrouping <- "Institusjon"
    }
    di[,NumberPeople:=AntallBeboereKl8]
    peopleLabel <- "beboere"
  } else {
    if (!level %in% c("landsdekkende", "helseforetak", "institusjon")) stop("Bad level")

    if (level == "landsdekkende") {
      varGrouping <- "Helseforetak"
    } else if (level == "helseforetak") {
      varGrouping <- "Sykehus"
    } else if (level == "institusjon") {
      varGrouping <- "Sykehus"
    }
    di[,NumberPeople:=AntallPasienterKl8]
    peopleLabel <- "pasienter"
  }

  addtorow <- list()
  addtorow$pos <- list(0, 0, 0)
  addtorow$command <- c(
    "& \\multicolumn{4}{c||}{HAI} & \\multicolumn{4}{c|}{Bruk av antibiotika} \\\\\n",
    " \\hline\n",
    paste0(c(
      glue::glue("\\multicolumn{{1}}{{|r||}}{{{varGrouping}}}"),
      glue::glue("\\multicolumn{{1}}{{p{{0.9cm}}}}{{Antall {type}}}"),
      glue::glue("\\multicolumn{{1}}{{p{{1.1cm}}}}{{Antall {peopleLabel}}}"),
      glue::glue("\\multicolumn{{1}}{{p{{1.7cm}}}}{{Andel (antall) {peopleLabel} med minst én HAI (\\%)}}"),
      "\\multicolumn{1}{p{1.3cm}||}{Prevalens av (antall) HAI (\\%)}",
      glue::glue("\\multicolumn{{1}}{{p{{0.9cm}}}}{{Antall {type}}}"),
      glue::glue("\\multicolumn{{1}}{{p{{1.1cm}}}}{{Antall {peopleLabel}}}"),
      glue::glue("\\multicolumn{{1}}{{p{{2cm}}}}{{Andel (antall) {peopleLabel} som fikk minst ett antibiotikum (\\%)}}"),
      "\\multicolumn{1}{p{2.75cm}|}{Prevalens av (antall) antibiotikaforskrivninger (\\%)}  \\\\\n"
    ), collapse = " & ")
  )

  xtab <- xtable::xtable(tab,
    caption = sprintf("Deltagelse, forekomst av helsetjenesteassosierte infeksjoner og bruk av antibiotika i %s",type)
  )

  xtable::align(xtab) <- "r|r||r|r|r|r||r|r|r|r|"

  xtable::print.xtable(xtab,
    include.rownames = FALSE,
    include.colnames = FALSE,
    sanitize.colnames.function = function(x) {
      x
    },
    caption.placement = "top",
    add.to.row = addtorow,
    hline.after = c(-1, 0, nrow(xtab) - 1, nrow(xtab)),
    comment = F
  )
}
