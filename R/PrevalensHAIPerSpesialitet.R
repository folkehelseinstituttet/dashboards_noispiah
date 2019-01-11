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
            by = .(c_spesialitet)
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
  t1[,xCat:=factor(xCat,levels=c(levels(di$c_spesialitet),"Alle spesialiteter samlet"))]
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


}
