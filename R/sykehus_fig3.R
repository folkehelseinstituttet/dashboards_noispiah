#' Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling
#' @param data a
#' @param arg a
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom RAWmisc RecodeDT
#' @export Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling
Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling <- function(
  data,
  arg
  ){
  # function(di,
  # da,
  # DATE_USE,
  # ab="ABBredMethAndre",
  # group1="IndikasjonCategory",
  # group2="forebyggVsBehandOgMethVsAndre") {

  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig3")$arg
  # arg <- plan$analysis_get("sykehus_fig4")$arg

  temp <- data$da[PrevalensDato == arg$DATE_USE & !is.na(forebyggingVsBehandling)]

  tab <- temp[, .(n = .N), by = .(
    forebyggingVsBehandling,
    "ab"=get(arg$ab),
    "group1"=get(arg$group1),
    "group2"=get(arg$group2))]

  if(!arg$ab %in% names(tab)) setnames(tab,"ab",arg$ab)
  if(!arg$group1 %in% names(tab)) setnames(tab,"group1",arg$group1)
  if(!arg$group2 %in% names(tab)) setnames(tab,"group2",arg$group2)

  return(tab)
}

#' Figure_ForskrivningerAvAntibiotikaTilBehandlingPerIndikasjon
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @export
Figure_ForskrivningerAvAntibiotikaTilBehandlingPerIndikasjon <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig3")$arg

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(
    data = data,
    arg = arg
  )
  if(nrow(tab)==0) return(no_data_graph())
  tab <- tab[forebyggingVsBehandling=="Behandling"]
  if(nrow(tab)==0) return(no_data_graph())
  tab[, denom := sum(n)]
  tab[, catFill := sykehusAB1]
  tab[, nFill := sum(n), by=catFill]
  tab[, labFill := sprintf("%s (n=%s)", catFill, nFill)]

  skeleton <- data.table(
    catFill=levels(tab$catFill)
  )
  ordering <- unique(tab[,c("labFill","catFill")])
  ordering <- merge(skeleton,ordering,by="catFill",all=T)
  ordering[is.na(labFill),labFill:=glue::glue("{catFill} (n=0)",catFill=catFill)]

  ordering[,catFill:=factor(catFill,levels=levels(tab$catFill))]
  setorder(ordering,-catFill)
  tab[,labFill:=factor(labFill,levels=ordering$labFill)]

  tab[, catX := IndikasjonCategorySykehusMedKlassifiseringNedreLuftveisinfeksjon]
  tab[, nX := sum(n), by=catX]
  tab[, labX := sprintf("%s (n=%s)", catX, nX)]

  tab[, total_height := sum(n/denom), by=.(labX)]

  ordering <- unique(tab[,c("labX","catX","nX")])
  setorder(ordering,nX,catX)
  tab[,labX:=factor(labX,levels=ordering$labX)]

  yColours <- rep("black",length=nrow(ordering))
  yColours[stringr::str_detect(ordering$catX,"nedre luftveisinfeksjon")] <- "red"

  q <- ggplot(tab, aes(x = labX, y = n / denom, fill = labFill))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                             values=c(
                               "blue",
                               "purple",
                               "cyan",
                               "green",
                               "red",
                               "orange"
                             ),
                             drop=F, guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Indikasjon (n=antall forskrivninger)")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til behandling\n(n=%s)", sum(tab$n)),
                              labels=scales::percent,
                              expand=c(0,0),
                              limits=c(0,max(tab$total_height)*1.1))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(legend.position = "bottom")
  q <- q + theme(axis.text.y = element_text(colour=yColours))
  lemon::grid_arrange_shared_legend(q, plot=F)

}


#' Figure_ForskrivningerAvAntibiotikaTilBehandlingPerSpesialitet
#' @param data a
#' @param arg a
#' @import data.table
#' @import ggplot2
#' @export
Figure_ForskrivningerAvAntibiotikaTilBehandlingPerSpesialitet <- function(data, arg) {
  # data <- plan$data_get()
  # arg <- plan$analysis_get("sykehus_fig4")$arg

  tab <- Data_ForskrivningerAvAntibiotikaTilForebyggingOgBehandling(
    data = data,
    arg = arg
  )
  tab <- tab[forebyggingVsBehandling=="Behandling"]
  if(nrow(tab)==0) return(no_data_graph())

  tab[, denom := sum(n)]
  tab[, catFill := sykehusAB1]
  tab[, nFill := sum(n), by=catFill]
  tab[, labFill := sprintf("%s (n=%s)", catFill, nFill)]

  skeleton <- data.table(
    catFill=levels(tab$catFill)
  )
  ordering <- unique(tab[,c("labFill","catFill")])
  ordering <- merge(skeleton,ordering,by="catFill",all=T)
  ordering[is.na(labFill),labFill:=glue::glue("{catFill} (n=0)",catFill=catFill)]

  ordering[,catFill:=factor(catFill,levels=levels(tab$catFill))]
  setorder(ordering,-catFill)
  tab[,labFill:=factor(labFill,levels=ordering$labFill)]

  tab[, catX := c_spesialitet]
  tab[, nX := sum(n), by=catX]
  tab[, labX := sprintf("%s (n=%s)", catX, nX)]

  tab[, total_height := sum(n/denom), by=.(labX)]

  ordering <- unique(tab[,c("labX","catX","nX")])
  setorder(ordering,nX)
  tab[,labX:=factor(labX,levels=ordering$labX)]

  q <- ggplot(tab, aes(x = labX, y = n / denom, fill = labFill))
  q <- q + geom_col(colour = "black", alpha = 1)
  q <- q + scale_fill_manual("",
                             values=c(
                               "blue",
                               "purple",
                               "cyan",
                               "green",
                               "red",
                               "orange"
                             ),
                             drop=F, guide = guide_legend(ncol = 3, byrow = T, reverse = TRUE))
  q <- q + scale_x_discrete("Spesialitet (n=antall forskrivninger)")
  q <- q + scale_y_continuous(sprintf("Andel av forskrivninger til behandling (n=%s)", sum(tab$n)),
                              labels=scales::percent,
                              expand=c(0,0),
                              limits=c(0,max(tab$total_height)*1.1))
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + coord_flip()
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + theme(legend.position = "bottom")
  lemon::grid_arrange_shared_legend(q, plot=F)
}
