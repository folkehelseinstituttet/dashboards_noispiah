#' Data_DeltagelseForekomstHAIogABiSykehjem
#' @param data a
#' @param arg sykehjem
#' @import data.table
#' @export
Data_DeltagelseForekomstHAIogABiSykehjem <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_tab1")$arg
  # arg <- plan$analysis_get("table1_sykehjem")$arg

  t1a <- data$di[PrevalensDato == arg$DATE_USE,
    .(
      antallSykehjemHAI = length(unique(InstitusjonId)),
      antallBeboereHAI = sum(NumberPeople),
      antallBeboereMedInfeksjonHAI = sum(NumberPeopleMedInfeksjon),
      antallInfeksjonerHAI = sum(antallInfeksjonerHAI)
    ),
    by = .(get(arg$varGrouping))
  ]
  t1b <- data$di[PrevalensDato == arg$DATE_USE,
            .(
              antallSykehjemHAI = length(unique(InstitusjonId)),
              antallBeboereHAI = sum(NumberPeople),
              antallBeboereMedInfeksjonHAI = sum(NumberPeopleMedInfeksjon),
              antallInfeksjonerHAI = sum(antallInfeksjonerHAI)
            )
            ]
  t1b[,get:="ÅÅÅ"]
  t1 <- rbind(t1a,t1b)

  t1[, andelBeboereMedInfeksjonHAI := RAWmisc::Format(100 * antallBeboereMedInfeksjonHAI / antallBeboereHAI, 1)]
  t1[, prevalensAvInfeksjonerHAI := RAWmisc::Format(100 * antallInfeksjonerHAI / antallBeboereHAI, 1)]
  setnames(t1, "get", arg$varGrouping)

  # creating T2
  daTemp1 <- unique(data$da[PrevalensDato == arg$DATE_USE,
                       c(
                         arg$varGrouping,
                         arg$da_unique_structure_vars,
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
                     by = .(get(arg$varGrouping))
                     ]

  daTemp2 <- data$da[PrevalensDato == arg$DATE_USE,
                .(AntallForskrivningerAB = sum(!is.na(ATCSubstans))),
                by = .(get(arg$varGrouping))
                ]

  t2a <- merge(daTemp1, daTemp2, by = "get", all = T)

  daTemp1 <- unique(data$da[PrevalensDato == arg$DATE_USE,
                       c(
                         arg$varGrouping,
                         arg$da_unique_structure_vars,
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

  daTemp2 <- data$da[PrevalensDato == arg$DATE_USE,
                .(AntallForskrivningerAB = sum(!is.na(ATCSubstans)))
                ]
  if(nrow(daTemp2)==0){
    daTemp2 <- data.table(AntallForskrivningerAB=0)
  }

  t2b <- cbind(daTemp1, daTemp2)
  t2b[,get:="ÅÅÅ"]

  t2 <- rbind(t2a,t2b)
  setnames(t2, "get", arg$varGrouping)

  t2[, AndelBeboereSomGisAB := RAWmisc::Format(100 * antallBeboerePaAB / antallBeboereAB, 1)]
  t2[, PrevalensAvABbruk := RAWmisc::Format(100 * AntallForskrivningerAB / antallBeboereAB, 1)]

  tab <- merge(t1, t2, by = arg$varGrouping, all = T)
  tab <- tab[stringr::str_sort(get(arg$varGrouping), locale = "nb")]
  tab[get(arg$varGrouping)=="ÅÅÅ",(arg$varGrouping):="Sammenslått"]

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
    arg$varGrouping,
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

  for(i in 1:ncol(tab)){
    vals <- tab[,i]
    tab[vals=="NA% (n=NA)",i] <- ""
  }

  return(tab)
}


#' Table_DeltagelseForekomstHAIogABiSykehjem
#' @param data a
#' @param arg a
#' @import data.table
#' @import xtable
#' @export Table_DeltagelseForekomstHAIogABiSykehjem
Table_DeltagelseForekomstHAIogABiSykehjem <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_tab1")$arg
  # arg <- plan$analysis_get("table1_sykehjem")$arg

  tab_source <- Data_DeltagelseForekomstHAIogABiSykehjem(
    data = data,
    arg = arg
    )
  splitting <- fhi::split_equal(1:nrow(tab_source), size = 25)

  for(i in seq_along(splitting)){
    if(i>1) cat("\\newpage\n\n")
    tab <- tab_source[splitting[[i]],]

    if(arg$type=="sykehjem"){
      if (!arg$level %in% c("landsdekkende", "fylke", "kommune")) stop("Bad level")

      if (arg$level == "landsdekkende") {
        varGrouping <- "Fylke"
      } else if (arg$level == "fylke") {
        varGrouping <- "Kommune"
      } else if (arg$level == "kommune") {
        varGrouping <- "Institusjon"
      }
      #di[,NumberPeople:=AntallBeboereKl8]
      peopleLabel <- "beboere"
    } else {
      if (!arg$level %in% c("landsdekkende", "helseforetak", "institusjon")) stop("Bad level")

      if (arg$level == "landsdekkende") {
        varGrouping <- "Helseforetak"
      } else if (arg$level == "helseforetak") {
        varGrouping <- "Sykehus"
      } else if (arg$level == "institusjon") {
        varGrouping <- "Sykehus"
      }
      #di[,NumberPeople:=AntallPasienterKl8]
      peopleLabel <- "pasienter"
    }

    addtorow <- list()
    addtorow$pos <- list(0, 0, 0)
    addtorow$command <- c(
      "& \\multicolumn{4}{c||}{HAI} & \\multicolumn{4}{c|}{Bruk av antibiotika} \\\\\n",
      " \\hline\n",
      paste0(c(
        glue::glue("\\multicolumn{{1}}{{|r||}}{{{arg$varGrouping}}}"),
        glue::glue("\\multicolumn{{1}}{{p{{0.9cm}}}}{{Antall {arg$type}}}"),
        glue::glue("\\multicolumn{{1}}{{p{{1.1cm}}}}{{Antall {arg$peopleLabel}}}"),
        glue::glue("\\multicolumn{{1}}{{p{{1.7cm}}}}{{Andel (antall) {arg$peopleLabel} med minst én HAI (\\%)}}"),
        "\\multicolumn{1}{p{1.3cm}||}{Prevalens av (antall) HAI (\\%)}",
        glue::glue("\\multicolumn{{1}}{{p{{0.9cm}}}}{{Antall {arg$type}}}"),
        glue::glue("\\multicolumn{{1}}{{p{{1.1cm}}}}{{Antall {arg$peopleLabel}}}"),
        glue::glue("\\multicolumn{{1}}{{p{{2cm}}}}{{Andel (antall) {arg$peopleLabel} som fikk minst ett antibiotikum (\\%)}}"),
        "\\multicolumn{1}{p{2.75cm}|}{Prevalens av (antall) antibiotikaforskrivninger (\\%)}  \\\\\n"
      ), collapse = " & ")
    )

    if(i==1){
      xtab <- xtable::xtable(
        tab,
        caption = sprintf("Deltagelse, forekomst av helsetjenesteassosierte infeksjoner og bruk av antibiotika i %s",arg$type)
      )
    } else {
      xtab <- xtable::xtable(
        tab,
        caption = NULL
      )
    }

    xtable::align(xtab) <- "r|r||r|r|r|r||r|r|r|r|"

    if(i==length(splitting)){
      hline.after <- c(-1, 0, nrow(xtab) - 1, nrow(xtab))
    } else {
      hline.after <- c(-1, 0, nrow(xtab))
    }
    xtable::print.xtable(xtab,
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.colnames.function = function(x) {
        x
      },
      caption.placement = "top",
      add.to.row = addtorow,
      hline.after = hline.after,
      comment = F
    )
  }
}
