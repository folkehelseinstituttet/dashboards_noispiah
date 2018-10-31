#' Data_DeltagelseForekomstHAIogABiSykehjem
#' @param di a
#' @param da a
#' @param level a
#' @param DATE_USE a
#' @import data.table
#' @export Data_DeltagelseForekomstHAIogABiSykehjem
Data_DeltagelseForekomstHAIogABiSykehjem <- function(di, da, level, DATE_USE) {
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

  if (!level %in% c("landsdekkende", "fylke", "kommune")) stop("Bad level")

  if (level == "landsdekkende") {
    varGrouping <- "Fylke"
  } else if (level == "fylke") {
    varGrouping <- "Kommune"
  } else if (level == "kommune") {
    varGrouping <- "Institusjon"
  }

  t1 <- di[InstitusjonType == "Sykehjem" & PrevalensDato == DATE_USE,
    .(
      antallSykehjemHAI = length(unique(InstitusjonId)),
      antallBeboereHAI = sum(AntallBeboereKl8),
      antallBeboereMedInfeksjonHAI = sum(AntallBeboereMedInfeksjon),
      antallInfeksjonerHAI = sum(AntallUrinveisInfeksjonerUtenUrinveiskateter_EgenInstitusjon +
        AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehus +
        AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehjem +
        AntallUrinveisInfeksjonerMedUrinveiskateter_EgenInstitusjon +
        AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehus +
        AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehjem +
        AntallNedreLuftveisInfeksjoner_EgenInstitusjon +
        AntallNedreLuftveisInfeksjoner_AnnetSykehus +
        AntallNedreLuftveisInfeksjoner_AnnetSykehjem +
        AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon +
        AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus +
        AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem +
        AntallDypePostOpSarinfeksjoner_EgenInstitusjon +
        AntallDypePostOpSarinfeksjoner_AnnetSykehus +
        AntallDypePostOpSarinfeksjoner_AnnetSykehjem +
        AntallHudInfeksjoner_EgenInstitusjon +
        AntallHudInfeksjoner_AnnetSykehus +
        AntallHudInfeksjoner_AnnetSykehjem)
    ),
    by = .(get(varGrouping))
  ]
  t1[, andelBeboereMedInfeksjonHAI := RAWmisc::Format(100 * antallBeboereMedInfeksjonHAI / antallBeboereHAI, 1)]
  t1[, prevalensAvInfeksjonerHAI := RAWmisc::Format(100 * antallInfeksjonerHAI / antallBeboereHAI, 1)]
  setnames(t1, "get", varGrouping)

  daTemp1 <- unique(da[InstitusjonType == "Sykehjem" & PrevalensDato == DATE_USE,
    c(
      varGrouping,
      "InstitusjonId",
      "Avdeling",
      "AntallBeboereKl8",
      "AntallBeboereSomGisAntibiotika"
    ),
    with = F
  ])

  daTemp1 <- daTemp1[,
    .(
      antallSykehjemAB = length(unique(InstitusjonId)),
      antallBeboereAB = sum(AntallBeboereKl8),
      antallBeboerePaAB = sum(AntallBeboereSomGisAntibiotika)
    ),
    by = .(get(varGrouping))
  ]

  daTemp2 <- da[InstitusjonType == "Sykehjem" & PrevalensDato == DATE_USE,
    .(AntallForskrivningerAB = sum(!is.na(ATCSubstans))),
    by = .(get(varGrouping))
  ]

  t2 <- merge(daTemp1, daTemp2, by = "get", all = T)
  setnames(t2, "get", varGrouping)

  t2[, AndelBeboereSomGisAB := RAWmisc::Format(100 * antallBeboerePaAB / antallBeboereAB, 1)]
  t2[, PrevalensAvABbruk := RAWmisc::Format(100 * AntallForskrivningerAB / antallBeboereAB, 1)]

  tab <- merge(t1, t2, by = varGrouping, all = T)

  tab <- tab[, c(
    varGrouping,
    "antallSykehjemHAI",
    "antallBeboereHAI",
    "andelBeboereMedInfeksjonHAI",
    "prevalensAvInfeksjonerHAI",
    "antallSykehjemAB",
    "antallBeboereAB",
    "AndelBeboereSomGisAB",
    "PrevalensAvABbruk"
  ), with = F]
  tab <- as.matrix(tab)

  return(tab)
}


#' Table_DeltagelseForekomstHAIogABiSykehjem
#' @param di a
#' @param da a
#' @param level a
#' @param DATE_USE a
#' @import data.table
#' @import xtable
#' @export Table_DeltagelseForekomstHAIogABiSykehjem
Table_DeltagelseForekomstHAIogABiSykehjem <- function(di, da, level, DATE_USE) {
  tab <- Data_DeltagelseForekomstHAIogABiSykehjem(di = di, da = da, level = level, DATE_USE = DATE_USE)

  if (level == "landsdekkende") {
    varGrouping <- "Fylke"
  } else if (level == "fylke") {
    varGrouping <- "Kommune"
  } else if (level == "kommune") {
    varGrouping <- "Institusjon"
  }

  addtorow <- list()
  addtorow$pos <- list(0, 0, 0)
  addtorow$command <- c(
    "& \\multicolumn{4}{c||}{HAI} & \\multicolumn{4}{c|}{Bruk av antibiotika} \\\\\n",
    " \\hline\n",
    paste0(c(
      sprintf("\\multicolumn{1}{|r||}{%s}", varGrouping),
      "\\multicolumn{1}{p{1.0cm}}{Antall sykehjem}",
      "\\multicolumn{1}{p{1.1cm}}{Antall beboere}",
      "\\multicolumn{1}{p{1.5cm}}{Andel beboere med HAI}",
      "\\multicolumn{1}{p{1.6cm}||}{Prevalens av HAI i prosent}",
      "\\multicolumn{1}{p{1.0cm}}{Antall sykehjem}",
      "\\multicolumn{1}{p{1.1cm}}{Antall beboere}",
      "\\multicolumn{1}{p{2cm}}{Andel beboere som fikk antibiotika}",
      "\\multicolumn{1}{p{2.75cm}|}{Prevalens av antibiotikabruk i prosent}  \\\\\n"
    ), collapse = " & ")
  )

  xtab <- xtable::xtable(tab,
    caption = "Deltagelse, forekomst av helsetjenesteassosierte infeksjoner og bruk av antibiotika i sykehjem"
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
    hline.after = c(-1, 0, nrow(xtab)),
    comment = F
  )
}
