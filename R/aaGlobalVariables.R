#' CONFIG
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$FORCE_TESTING <- FALSE
CONFIG$FILES_DATA <- c(
  "AntibiotikadataPrimer.xlsx",
  "InfeksjonsdataPrimer.xlsx",
  "AntibiotikadataSpesialist.xlsx",
  "InfeksjonsdataSpesialist.xlsx"
)
CONFIG$FILES_RMD_RAW <- c("sykehjem.Rmd", "sykehus.Rmd")
CONFIG$FILES_RMD_USE_SYKEHJEM <- ""
CONFIG$FILES_RMD_USE_SYKEHUS <- ""
CONFIG$FYLKE <- c(
  "Akershus",
  "Aust-Agder",
  "Buskerud",
  "Finnmark",
  "Hedmark",
  "Hordaland",
  "M\u00F8re og Romsdal",
  "Nordland",
  "Oppland",
  "Oslo",
  "Rogaland",
  "Sogn og Fjordane",
  "Telemark",
  "Troms",
  "Tr\u00F8ndelag",
  "Vest-Agder",
  "Vestfold",
  "\u00D8stfold"
)

#' If folders are setup according to the
#' dashboard philosophy, then this function
#' sets RPROJ
#' @param var a
#' @param val a
#' @export SetConfig
SetConfig <- function(
                      var,
                      val) {
  CONFIG[[var]] <- val
}
