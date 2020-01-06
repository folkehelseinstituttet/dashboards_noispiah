#' Data_ForekomstHAIiSykehjemPerAvdelingstype
#' @param data a
#' @param arg a
#' @import data.table
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForekomstHAIiSykehjemPerAvdelingstype
Data_ForekomstHAIiSykehjemPerAvdelingstype <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehjem_fig1")$arg

  tab <- data$di[PrevalensDato == arg$DATE_USE,
    .(
      AntallBeboereKl8 = sum(AntallBeboereKl8),
      antallInfeksjonerUrine = sum(
        AntallUrinveisInfeksjonerUtenUrinveiskateter_EgenInstitusjon +
          AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehus +
          AntallUrinveisInfeksjonerUtenUrinveiskateter_AnnetSykehjem +
          AntallUrinveisInfeksjonerMedUrinveiskateter_EgenInstitusjon +
          AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehus +
          AntallUrinveisInfeksjonerMedUrinveiskateter_AnnetSykehjem
      ),
      antallInfeksjonerNedreLuftveis = sum(
        AntallNedreLuftveisInfeksjoner_EgenInstitusjon +
          AntallNedreLuftveisInfeksjoner_AnnetSykehus +
          AntallNedreLuftveisInfeksjoner_AnnetSykehjem
      ),
      antallInfeksjonerOperasjonsOmrade = sum(
        AntallOverflatiskePostOpSarinfeksjoner_EgenInstitusjon +
          AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehus +
          AntallOverflatiskePostOpSarinfeksjoner_AnnetSykehjem +
          AntallDypePostOpSarinfeksjoner_EgenInstitusjon +
          AntallDypePostOpSarinfeksjoner_AnnetSykehus +
          AntallDypePostOpSarinfeksjoner_AnnetSykehjem
      ),
      antallInfeksjonerHud = sum(
        AntallHudInfeksjoner_EgenInstitusjon +
          AntallHudInfeksjoner_AnnetSykehus +
          AntallHudInfeksjoner_AnnetSykehjem
      )
    ),
    by = .(Avdelingstype)
  ]
  tabx <- copy(tab)
  tabx[,Avdelingstype:=NULL]
  tabx <- tabx[, lapply(.SD, sum, na.rm= TRUE)]
  tabx[,Avdelingstype:="Alle avdelinger samlet"]
  tab <- rbind(tab,tabx)

  tab <- melt.data.table(tab, id.vars = c("Avdelingstype", "AntallBeboereKl8"))
  tab[, prop := value / AntallBeboereKl8]
  RAWmisc::RecodeDT(
    tab,
    switch = c(
      "antallInfeksjonerUrine" = "Urinveisinfeksjon",
      "antallInfeksjonerNedreLuftveis" = "Nedre luftveisinfeksjon",
      "antallInfeksjonerOperasjonsOmrade" = "Infeksjon i operasjonsomr\u00E5det",
      "antallInfeksjonerHud" = "Hudinfeksjon"
    ),
    var = "variable"
  )

  tab[, variable := factor(variable, levels = c(
    "Hudinfeksjon",
    "Infeksjon i operasjonsomr\u00E5det",
    "Nedre luftveisinfeksjon",
    "Urinveisinfeksjon"
  ))]

  RAWmisc::RecodeDT(
    tab,
    switch = c(
      "Kombinert kort- og langtids" = "Kombinert kort- og langtidsavdeling"
    ),
    var = "Avdelingstype"
  )

  return(tab)
}


#' Figure_ForekomstHAIiSykehjemPerAvdelingstype
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @export
Figure_ForekomstHAIiSykehjemPerAvdelingstype <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehjem_fig1")$arg

  tab <- Data_ForekomstHAIiSykehjemPerAvdelingstype(
    data = data,
    arg = arg
  )

  if(nrow(tab)==0) return(no_data_graph())
  if(sum(tab$AntallBeboereKl8)==0) return(no_data_graph())

  x <- tab[Avdelingstype=="Alle avdelinger samlet"]
  setorder(x,variable)
  levels(tab$variable) <- glue::glue("{lab} (n={n})",
                                     lab=levels(tab$variable),
                                     n=FormatNorwegian(x$value))
  #tab[,variable:=factor(variable,levels=rev(levels(tab$variable)))]

  tab[, xLab := sprintf("%s (n=%s)", Avdelingstype, FormatNorwegian(AntallBeboereKl8))]
  tab[, total:=sum(prop),by=xLab]
  ordering <- unique(tab[,c("total","xLab")])
  setorder(ordering,total)
  ordering[,o:=1:.N]
  ordering[stringr::str_detect(xLab,"Alle avdelinger samlet"),o:=100000]
  setorder(ordering,o)
  tab[,xLab:=factor(xLab,levels=ordering$xLab)]

  tab[, total_height := sum(prop), by=.(xLab)]

  q <- ggplot(tab, aes(x = xLab, y = prop, fill = variable))
  q <- q + geom_bar(stat = "identity", colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                             values=c("red",
                                      "green",
                                      "blue",
                                      "yellow"),
                             drop=F, guide = guide_legend(ncol = 2, byrow = T, reverse = T))
  q <- q + scale_x_discrete("Avdelingstype (n=antall beboere)")
  q <- q + scale_y_continuous("Prevalens av helsetjenesteassosierte infeksjoner (%)",
                              labels = scales::percent,
                              expand=c(0,0),
                              limits=c(0,max(tab$total_height)*1.1),)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + theme(legend.position = "bottom")
  q <- q + fhiplot::theme_fhi_lines()
  lemon::grid_arrange_shared_legend(q)
}









