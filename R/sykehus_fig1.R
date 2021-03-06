#' Data_PrevalensHAIPerSpesialitet
#' @param data a
#' @param arg a
#' @import data.table
#' @export Data_PrevalensHAIPerSpesialitet
Data_PrevalensHAIPerSpesialitet <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig1")$arg

  t1a <- data$di[PrevalensDato == arg$DATE_USE,
    .(
      antallBeboere = sum(NumberPeople),
      antallInfeksjonerHAIIperasjonsOmradet = sum(antallInfeksjonerHAIIperasjonsOmradet),
      antallInfeksjonerHAIUrinveis = sum(antallInfeksjonerHAIUrinveis),
      antallInfeksjonerHAINedreLuftveis = sum(antallInfeksjonerHAINedreLuftveis),
      antallInfeksjonerHAIBlodbane = sum(antallInfeksjonerHAIBlodbane)
    ),
    keyby = .(c_spesialitet)
  ]

  t1b <- data$di[
    PrevalensDato == arg$DATE_USE,
    .(
      antallBeboere = sum(NumberPeople),
      antallInfeksjonerHAIIperasjonsOmradet = sum(antallInfeksjonerHAIIperasjonsOmradet),
      antallInfeksjonerHAIUrinveis = sum(antallInfeksjonerHAIUrinveis),
      antallInfeksjonerHAINedreLuftveis = sum(antallInfeksjonerHAINedreLuftveis),
      antallInfeksjonerHAIBlodbane = sum(antallInfeksjonerHAIBlodbane)
    )
  ]

  t1 <- rbind(t1a, t1b, fill = T)
  t1[, xCat := as.character(c_spesialitet)]
  t1[is.na(xCat), xCat := "Alle spesialiteter samlet"]
  t1[, xCat := factor(xCat, levels = c(rev(levels(data$di$c_spesialitet)), "Alle spesialiteter samlet"))]
  t1[, c_spesialitet := NULL]

  t1 <- melt.data.table(t1, id.vars = c("xCat", "antallBeboere"))
  t1[, prop := value / antallBeboere]

  return(t1)
}


#' Figure_PrevalensHAIPerSpesialitet
#' @param data a
#' @param arg a
#' @import data.table
#' @export Figure_PrevalensHAIPerSpesialitet
Figure_PrevalensHAIPerSpesialitet <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig1")$arg
  tab <- Data_PrevalensHAIPerSpesialitet(data = data, arg = arg)
  if (nrow(tab) == 0) {
    return(no_data_graph())
  }

  x <- tab[xCat == "Alle spesialiteter samlet"]
  setorder(x, variable)
  levels(tab$variable) <- glue::glue("{lab} (n={n})",
    lab = c(
      glue::glue("Infeksjon i operasjonsomr{fhi::nb$aa}det"),
      "Urinveisinfeksjon",
      "Nedre luftveisinfeksjon",
      "Blodbaneinfeksjon"
    ),
    n = x$value
  )
  # tab[,variable:=factor(variable,levels=rev(levels(tab$variable)))]

  # ordering <- unique(tab[,c("antallBeboere","xCat")])
  ordering <- tab[, .(
    prop = sum(prop),
    antallBeboere = unique(antallBeboere)
  ),
  keyby = .(xCat)
  ]
  ordering[, xLab := sprintf("%s (n=%s)", xCat, FormatNorwegian(antallBeboere))]
  setorder(ordering, prop)
  ordering[, o := 1:.N]
  ordering[stringr::str_detect(xLab, "Alle spesialiteter samlet"), o := 100000]
  setorder(ordering, o)
  ordering[, xLab := factor(xLab, levels = xLab)]

  tab <- merge(tab, ordering[, c("xCat", "xLab")], by = "xCat")

  tab <- tab[antallBeboere > 0]
  tab[, total_height := sum(prop), by = .(xLab)]
  if (sum(tab$prop > 0) > 0) {
    limits <- c(0, max(tab$total_height) * 1.1)
  } else {
    limits <- c(0, 1)
  }
  tab[prop == 0, prop := 0.0001]

  q <- ggplot(tab, aes(x = xLab, y = prop, fill = variable))
  q <- q + geom_col(alpha = 1, col = "black")
  q <- q + coord_flip()
  q <- q + scale_fill_manual(
    "",
    values = c(
      "green",
      "yellow",
      "blue",
      "red"
    ),
    drop = F, guide = guide_legend(ncol = 2, byrow = T, reverse = T)
  )
  q <- q + scale_x_discrete("Spesialitet (n=antall pasienter)")
  q <- q + scale_y_continuous("Prevalens av helsetjenesteassosierte infeksjoner (%)",
    labels = scales::percent,
    limits = limits,
    expand = c(0, 0)
  )
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(legend.position = "bottom")
  q
  return(lemon::grid_arrange_shared_legend(q))
}
