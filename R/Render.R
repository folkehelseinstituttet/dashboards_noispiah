

#' ExtractEnglish
#' @param var a
#' @importFrom stringr str_extract_all
#' @export ExtractEnglish
ExtractEnglish <- function(var) {
  unlist(lapply(stringr::str_extract_all(var, "[a-zA-Z]"), paste0, collapse = ""))
}

#' GenStackSykehjem
#' @param dev a
#' @param outputDir a
#' @param FILES_RMD_USE_SYKEHJEM a
#' @param FILES_RMD_USE_SYKEHUS a
#' @import data.table
#' @import fhi
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom lubridate today
#' @export GenStackSykehjem
GenStackSykehjem <- function(
  dev = FALSE,
  outputDir = fhi::DashboardFolder("results"),
  FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS) {
  Fylke <- NULL

  outputDirDaily <- file.path(outputDir, lubridate::today())
  dir.create(outputDirDaily)
  dir.create(file.path(outputDirDaily, "Sykehjem"))
  dir.create(file.path(outputDirDaily, "Sykehjem", "Landsdekkende"))
  dir.create(file.path(outputDirDaily, "Sykehjem", "Fylke"))
  dir.create(file.path(outputDirDaily, "Sykehjem", "Kommune"))

  da <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw", "AntibiotikadataPrimer.xlsx")))
  di <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw", "InfeksjonsdataPrimer.xlsx")))

  maxDate <- max(da$PrevalensDato, di$PrevalensDato)
  da <- da[PrevalensDato == maxDate]
  di <- di[PrevalensDato == maxDate]

  fylkeKommune <- unique(rbind(da[, c("Fylke", "Kommune")], di[, c("Fylke", "Kommune")]))
  setorder(fylkeKommune, Fylke, Kommune)
  for (f in unique(fylkeKommune$Fylke)) {
    dir.create(file.path(outputDirDaily, "Sykehjem", "Kommune", f))
  }

  stack <- rbind(
    data.table(
      level = "landsdekkende",
      location = "Landsdekkende"
    ),
    data.table(
      level = "fylke",
      location = unique(c(da$Fylke,di$Fylke))
    ),
    data.table(
      level = "kommune",
      location = unique(fylkeKommune$Kommune)
    )
  )
  stack[, order := 1:.N]
  stack <- merge(stack, fylkeKommune, by.x = "location", by.y = "Kommune", all.x = T)
  setorder(stack, order)

  stack[level == "landsdekkende", outputDirUse := file.path(outputDirDaily, "Sykehjem", "Landsdekkende")]
  stack[level == "fylke", outputDirUse := file.path(outputDirDaily, "Sykehjem", "Fylke")]
  stack[level == "kommune", outputDirUse := file.path(outputDirDaily, "Sykehjem", "Kommune", Fylke)]
  stack[,Fylke:=NULL]

  stack[, RMD := FILES_RMD_USE_SYKEHJEM]
  stack[, dev := dev]
  stack[, DATE_USE := maxDate]

  return(stack)
}

#' GenStackSykehus
#' @param dev a
#' @param outputDir a
#' @param FILES_RMD_USE_SYKEHJEM a
#' @param FILES_RMD_USE_SYKEHUS a
#' @import data.table
#' @import fhi
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom lubridate today
#' @export GenStackSykehus
GenStackSykehus <- function(
  dev = FALSE,
  outputDir = fhi::DashboardFolder("results"),
  FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS) {
  Fylke <- NULL

  outputDirDaily <- file.path(outputDir, lubridate::today())
  dir.create(outputDirDaily)
  dir.create(file.path(outputDirDaily, "Sykehus"))
  dir.create(file.path(outputDirDaily, "Sykehus", "Landsdekkende"))
  dir.create(file.path(outputDirDaily, "Sykehus", "Helseforetak"))
  dir.create(file.path(outputDirDaily, "Sykehus", "Institusjon"))

  da <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw", "AntibiotikadataSpesialist.xlsx")))
  di <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw", "InfeksjonsdataSpesialist.xlsx")))

  maxDate <- max(da$PrevalensDato, di$PrevalensDato)
  da <- da[PrevalensDato == maxDate]
  di <- di[PrevalensDato == maxDate]

  stack <- rbind(
    data.table(
      level = "landsdekkende",
      location = "Landsdekkende"
    ),
    data.table(
      level = "helseforetak",
      location = unique(c(da$HelseForetak,di$HelseForetak))
    ),
    data.table(
      level = "institusjon",
      location = unique(c(da$Institusjon,di$Institusjon))
    )
  )
  stack[, order := 1:.N]
  setorder(stack, order)

  stack[level == "landsdekkende", outputDirUse := file.path(outputDirDaily, "Sykehus", "Landsdekkende")]
  stack[level == "helseforetak", outputDirUse := file.path(outputDirDaily, "Sykehus", "Helseforetak")]
  stack[level == "institusjon", outputDirUse := file.path(outputDirDaily, "Sykehus", "Institusjon")]

  stack[, RMD := FILES_RMD_USE_SYKEHUS]
  stack[, dev := dev]
  stack[, DATE_USE := maxDate]

  return(stack)

}
