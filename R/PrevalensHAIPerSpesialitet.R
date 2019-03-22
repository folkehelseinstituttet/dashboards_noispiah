#' Data_PrevalensHAIPerSpesialitet
#' @param di a
#' @param da a
#' @param level a
#' @param DATE_USE a
#' @import data.table
#' @export Data_PrevalensHAIPerSpesialitet
Data_PrevalensHAIPerSpesialitet <- function(di, da, level, DATE_USE) {

  t1a <- di[PrevalensDato == DATE_USE,
            .(
              antallBeboere = sum(NumberPeople),
              antallInfeksjonerHAIIperasjonsOmradet = sum(antallInfeksjonerHAIIperasjonsOmradet),
              antallInfeksjonerHAIUrinveis = sum(antallInfeksjonerHAIUrinveis),
              antallInfeksjonerHAINedreLuftveis = sum(antallInfeksjonerHAINedreLuftveis),
              antallInfeksjonerHAIBlodbane = sum(antallInfeksjonerHAIBlodbane)
            ),
            keyby = .(c_spesialitet)
            ]

  t1b <- di[PrevalensDato == DATE_USE,
           .(
             antallBeboere = sum(NumberPeople),
             antallInfeksjonerHAIIperasjonsOmradet = sum(antallInfeksjonerHAIIperasjonsOmradet),
             antallInfeksjonerHAIUrinveis = sum(antallInfeksjonerHAIUrinveis),
             antallInfeksjonerHAINedreLuftveis = sum(antallInfeksjonerHAINedreLuftveis),
             antallInfeksjonerHAIBlodbane = sum(antallInfeksjonerHAIBlodbane)
           )
           ]

  t1 <- rbind(t1a,t1b,fill=T)
  t1[,xCat:=as.character(c_spesialitet)]
  t1[is.na(xCat),xCat:="Alle spesialiteter samlet"]
  t1[,xCat:=factor(xCat,levels=c(rev(levels(di$c_spesialitet)),"Alle spesialiteter samlet"))]
  t1[,c_spesialitet:=NULL]

  t1 <- melt.data.table(t1,id.vars=c("xCat","antallBeboere"))
  t1[,prop:=value/antallBeboere]

  return(t1)
}


#' Figure_PrevalensHAIPerSpesialitet
#' @param di a
#' @param da a
#' @param level a
#' @param DATE_USE a
#' @import data.table
#' @export Figure_PrevalensHAIPerSpesialitet
Figure_PrevalensHAIPerSpesialitet <- function(di, da, level, DATE_USE) {
  tab <- Data_PrevalensHAIPerSpesialitet(di = di, da = da, level = level, DATE_USE = DATE_USE)
  if(nrow(tab)==0) return(no_data_graph())

  x <- tab[xCat=="Alle spesialiteter samlet"]
  setorder(x,variable)
  levels(tab$variable) <- glue::glue("{lab} (n={n})",
                                     lab=c(
                                       "Infeksjon i operasjonsområdet",
                                       "Urinveisinfeksjon",
                                       "Nedre luftveisinfeksjon",
                                       "Bodbaneinfeksjon"),
                                     n=x$value)
  tab[,variable:=factor(variable,levels=rev(levels(tab$variable)))]

  ordering <- unique(tab[,c("antallBeboere","xCat")])
  ordering[,xLab:=sprintf("%s (N=%s)",xCat,antallBeboere)]
  setorder(ordering,xCat)
  ordering[,xLab:=factor(xLab,levels=xLab)]

  tab <- merge(tab,ordering[,c("xCat","xLab")],by="xCat")

  tab <- tab[antallBeboere>0]
  if(sum(tab$prop>0)>0){
    limits <- NULL
  } else {
    limits <- c(0,1)
  }
  tab[prop==0,prop:=0.0001]

  q <- ggplot(tab, aes(x = xLab,y=prop,fill=variable))
  q <- q + geom_col(alpha=0.75, col="black")
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",
                             values=c("red",
                                      "blue",
                                      "yellow",
                                      "green"),
                             drop=F, guide = guide_legend(ncol = 2, byrow = T, reverse = T))
  q <- q + scale_x_discrete("Spesialitet (antall pasienter)")
  q <- q + scale_y_continuous("Prevalens av helsetjeneseassosierte infeksjoner (%)",
                              labels=scales::percent,
                              limits=limits)
  q <- q + labs(main = "Prevalens av helsetjenesteassosierte infeksjoner etter avdelingstype")
  q <- q + theme(legend.position = "bottom")
  return(lemon::grid_arrange_shared_legend(q))
}
