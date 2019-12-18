abonnenter_to_cat <- function(to_cat, file, append=TRUE){
  if(nrow(to_cat)==0) return()
  for(i in 1:nrow(to_cat)){
    x <- paste0(paste0(names(to_cat),": ",to_cat[i]),collapse=" / ")
    x <- paste0(x,"\r\n")
    cat(x, file=file, append=append)
  }
}

#' ExtractEnglish
#' @param var a
#' @importFrom stringr str_extract_all
#' @export ExtractEnglish
ExtractEnglish <- function(var) {
  unlist(lapply(stringr::str_extract_all(var, "[a-zA-Z]"), paste0, collapse = ""))
}

get_abonnenter_sykehjem <- function(){
  abonnenter <- readxl::read_excel(fd::path("config","abonnenter-2019-09-25.xlsx"), sheet = "SYKEHJEM")
  setDT(abonnenter)
  abonnenter <- abonnenter[!is.na(epost)]
  abonnenter[,kommune_code:=tolower(kommune_code)]
  abonnenter[,fylke_code:=tolower(fylke_code)]
  abonnenter[,nasjonal:=tolower(nasjonal)]

  return(abonnenter)
}

#' GenStackSykehjem
#' @param dev a
#' @param outputDir a
#' @param FILES_RMD_USE_SYKEHJEM a
#' @param FILES_RMD_USE_SYKEHUS a
#' @param requested_date a
#' @import data.table
#' @import fhi
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom lubridate today
#' @export GenStackSykehjem
GenStackSykehjem <- function(
  dev = FALSE,
  outputDir = fd::path("results"),
  FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS,
  requested_date=NULL) {
  Fylke <- NULL

  outputDirDaily <- file.path(outputDir, lubridate::today())
  dir.create(outputDirDaily)
  dir.create(file.path(outputDirDaily, "Sykehjem"))
  dir.create(file.path(outputDirDaily, "Sykehjem", "Landsdekkende"))
  dir.create(file.path(outputDirDaily, "Sykehjem", "Fylke"))
  dir.create(file.path(outputDirDaily, "Sykehjem", "Kommune"))

  da <- data.table(readxl::read_excel(fd::path("data_raw", "AntibiotikadataPrimer.xlsx")))
  di <- data.table(readxl::read_excel(fd::path("data_raw", "InfeksjonsdataPrimer.xlsx")))

  if(is.null(requested_date)){
    maxDate <- as.character(max(da$PrevalensDato, di$PrevalensDato))
  } else {
    maxDate <- requested_date
  }
  da <- da[PrevalensDato == maxDate]
  di <- di[PrevalensDato == maxDate]

  fylkeKommune <- unique(rbind(da[, c("Fylke", "Kommune")], di[, c("Fylke", "Kommune")]))
  setorder(fylkeKommune, Fylke, Kommune)
  setnames(fylkeKommune,c("Fylke","Kommune"),c("fylke","kommune"))
  for (f in unique(fylkeKommune$fylke)) {
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
      location = unique(fylkeKommune$kommune)
    )
  )
  stack[, order := 1:.N]
  stack <- merge(stack, fylkeKommune, by.x = "location", by.y = "kommune", all.x = T)
  setorder(stack, order)
  stack[level=="fylke",fylke:=location]
  stack[level=='landsdekkende',fylke:="Norge"]

  stack[level == "landsdekkende", outputDirUse := file.path(outputDirDaily, "Sykehjem", "Landsdekkende")]
  stack[level == "fylke", outputDirUse := file.path(outputDirDaily, "Sykehjem", "Fylke")]
  stack[level == "kommune", outputDirUse := file.path(outputDirDaily, "Sykehjem", "Kommune", fylke)]

  stack[,fylke_fhi:=convert_fylke_to_fhidata(fylke)]
  stack[unique(fhidata::norway_locations_b2019[,c("county_code","county_name")]),on="fylke_fhi==county_name",county_code:=county_code]
  stack[,fylke_fhi:=NULL]

  convert_kommune_to_fhidata(stack)
  stack[level=="kommune" & is.na(clean),clean:=location]
  stack[unique(fhidata::norway_locations_b2019[,c("county_code","municip_code","municip_name")]),
        on=c(county_code="county_code",clean="municip_name"),
        municip_code:=municip_code
        ]
  stack[level=="kommune" & is.na(municip_code)]
  stack[,clean:=NULL]
  stack[level!="fylke",county_code:=NA]

  stack[,pdf := glue::glue("{location}.pdf",
    location=location)]

  stack[,output_pdf := glue::glue(
    "{outputDirUse}/{location}.pdf",
    outputDirUse=outputDirUse,
    location=location)]

  stack[, RMD := FILES_RMD_USE_SYKEHJEM]
  stack[, dev := dev]
  stack[, DATE_USE := maxDate]
  stack[, date_display := convert_date_to_season(DATE_USE)]

  stack[, uuid := replicate(.N, uuid::UUIDgenerate(F))]

  abonnenter <- get_abonnenter_sykehjem()
  #compare_subset_to_ref(sub=na.omit(unique(abonnenter$kommune_code)), ref=stack$municip_code)
  abonnenter[stack[level=="kommune"], on="kommune_code==municip_code", uuid_1:=uuid]

  #compare_subset_to_ref(sub=na.omit(unique(abonnenter$helseforetak)), ref=stack$location_lower)
  abonnenter[stack[level=="fylke"], on="fylke_code==county_code", uuid_2:=uuid]

  abonnenter[nasjonal=="ja", uuid_3 := stack[level=="landsdekkende"]$uuid]
  abonnenter[,base_dir:=fs::path(outputDirDaily,"Epost","Sykehjem",epost)]
  for(i in unique(abonnenter$base_dir)) fs::dir_create(i)

  abonnenter[stack, on="uuid_1==uuid", from_1:=output_pdf]
  abonnenter[stack, on="uuid_1==uuid", pdf_1:=pdf]
  abonnenter[stack, on="uuid_1==uuid", to_1:=fs::path(base_dir,pdf)]

  abonnenter[stack, on="uuid_2==uuid", from_2:=output_pdf]
  abonnenter[stack, on="uuid_2==uuid", pdf_2:=pdf]
  abonnenter[stack, on="uuid_2==uuid", to_2:=fs::path(base_dir,pdf)]

  abonnenter[stack, on="uuid_3==uuid", from_3:=output_pdf]
  abonnenter[stack, on="uuid_3==uuid", pdf_3:=pdf]
  abonnenter[stack, on="uuid_3==uuid", to_3:=fs::path(base_dir,pdf)]

  # create text files
  for(i in unique(abonnenter$epost)){
    to_email <- abonnenter[epost==i]
    to_file <- fs::path(to_email$base_dir[1],"_EPOST.txt")
    cat("Rapportene:\r\n\r\n", file=to_file)

    # national
    to_cat <- to_email[!is.na(nasjonal),c("nasjonal","pdf_3")]
    setnames(to_cat,"pdf_3","pdf")
    to_cat[is.na(pdf),pdf:="Ingen data"]
    abonnenter_to_cat(to_cat, file=to_file, append=T)

    to_cat <- to_email[!is.na(fylke_name),c("fylke_name","pdf_2")]
    setnames(to_cat, "fylke_name", "fylke")
    setnames(to_cat,"pdf_2","pdf")
    to_cat[is.na(pdf),pdf:="Ingen data"]
    abonnenter_to_cat(to_cat, file=to_file, append=T)

    to_cat <- to_email[!is.na(kommune_name),c("kommune_name","pdf_1")]
    setnames(to_cat, "kommune_name", "kommune")
    setnames(to_cat,"pdf_1","pdf")
    to_cat[is.na(pdf),pdf:="Ingen data"]
    abonnenter_to_cat(to_cat, file=to_file, append=T)
  }

  abonnenter <- abonnenter[,c("from_1","to_1","uuid_1","from_2","to_2","uuid_2","from_3","to_3","uuid_3")]
  abonnenter <- melt.data.table(abonnenter, measure = patterns("^from_", "^to_", "^uuid_"), value.name = c("from", "to", "uuid"))
  abonnenter <- na.omit(abonnenter)
  abonnenter[,variable:=NULL]

  return(list(
    stack=stack,
    abonnenter=abonnenter
    ))
}

get_abonnenter_sykehus <- function(){
  abonnenter <- readxl::read_excel(fd::path("config","abonnenter-2019-09-25.xlsx"), sheet = "SYKEHUS")
  setDT(abonnenter)
  abonnenter <- abonnenter[!is.na(epost)]
  abonnenter[,sykehus:=tolower(sykehus)]
  abonnenter[,helseforetak:=tolower(helseforetak)]
  abonnenter[,nasjonal:=tolower(nasjonal)]

  return(abonnenter)
}

#' GenStackSykehus
#' @param dev a
#' @param outputDir a
#' @param FILES_RMD_USE_SYKEHJEM a
#' @param FILES_RMD_USE_SYKEHUS a
#' @param requested_date a
#' @import data.table
#' @import fhi
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom lubridate today
#' @export GenStackSykehus
GenStackSykehus <- function(
  dev = FALSE,
  outputDir = fd::path("results"),
  FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS,
  requested_date=NULL) {
  Fylke <- NULL

  outputDirDaily <- file.path(outputDir, lubridate::today())
  dir.create(outputDirDaily)
  dir.create(file.path(outputDirDaily, "Sykehus"))
  dir.create(file.path(outputDirDaily, "Sykehus", "Landsdekkende"))
  dir.create(file.path(outputDirDaily, "Sykehus", "Helseforetak"))
  dir.create(file.path(outputDirDaily, "Sykehus", "Institusjon"))

  da <- data.table(readxl::read_excel(fd::path("data_raw", "AntibiotikadataSpesialist.xlsx")))
  di <- data.table(readxl::read_excel(fd::path("data_raw", "InfeksjonsdataSpesialist.xlsx")))

  if(is.null(requested_date)){
    maxDate <- as.character(max(da$PrevalensDato, di$PrevalensDato))
  } else {
    maxDate <- requested_date
  }
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

  stack[,pdf := glue::glue("{location}.pdf",
                           location=location)]

  stack[,output_pdf := glue::glue(
    "{outputDirUse}/{location}.pdf",
    outputDirUse=outputDirUse,
    location=location)]

  stack[, RMD := FILES_RMD_USE_SYKEHUS]
  stack[, dev := dev]
  stack[, DATE_USE := maxDate]
  stack[, date_display := convert_date_to_kvartal(DATE_USE)]

  stack[,location_lower:=tolower(location)]
  stack[, uuid := replicate(.N, uuid::UUIDgenerate(F))]

  abonnenter <- get_abonnenter_sykehus()
  compare_subset_to_ref(sub=na.omit(unique(abonnenter$sykehus)), ref=stack$location_lower)
  abonnenter[stack, on="sykehus==location_lower", uuid_1:=uuid]

  compare_subset_to_ref(sub=na.omit(unique(abonnenter$helseforetak)), ref=stack$location_lower)
  abonnenter[stack, on="helseforetak==location_lower", uuid_2:=uuid]

  abonnenter[nasjonal=="ja", uuid_3 := stack[location_lower=="landsdekkende"]$uuid]
  stack[, location_lower:=NULL]

  abonnenter[,base_dir:=fs::path(outputDirDaily,"Epost","Sykehus",epost)]
  for(i in unique(abonnenter$base_dir)) fs::dir_create(i)

  # create

  abonnenter[stack, on="uuid_1==uuid", from_1:=output_pdf]
  abonnenter[stack, on="uuid_1==uuid", pdf_1:=pdf]
  abonnenter[stack, on="uuid_1==uuid", to_1:=fs::path(base_dir,pdf)]

  abonnenter[stack, on="uuid_2==uuid", from_2:=output_pdf]
  abonnenter[stack, on="uuid_2==uuid", pdf_2:=pdf]
  abonnenter[stack, on="uuid_2==uuid", to_2:=fs::path(base_dir,pdf)]

  abonnenter[stack, on="uuid_3==uuid", from_3:=output_pdf]
  abonnenter[stack, on="uuid_3==uuid", pdf_3:=pdf]
  abonnenter[stack, on="uuid_3==uuid", to_3:=fs::path(base_dir,pdf)]

  # create text files
  for(i in unique(abonnenter$epost)){
    to_email <- abonnenter[epost==i]
    to_file <- fs::path(to_email$base_dir[1],"_EPOST.txt")
    cat("Rapportene:\r\n\r\n", file=to_file)

    # national
    to_cat <- to_email[!is.na(nasjonal),c("nasjonal","pdf_3")]
    setnames(to_cat,"pdf_3","pdf")
    to_cat[is.na(pdf),pdf:="Ingen data"]
    abonnenter_to_cat(to_cat, file=to_file, append=T)

    to_cat <- to_email[!is.na(helseforetak),c("helseforetak","pdf_2")]
    setnames(to_cat,"pdf_2","pdf")
    to_cat[is.na(pdf),pdf:="Ingen data"]
    abonnenter_to_cat(to_cat, file=to_file, append=T)

    to_cat <- to_email[!is.na(sykehus),c("sykehus","pdf_1")]
    setnames(to_cat,"pdf_1","pdf")
    to_cat[is.na(pdf),pdf:="Ingen data"]
    abonnenter_to_cat(to_cat, file=to_file, append=T)
  }

  abonnenter <- abonnenter[,c("from_1","to_1","uuid_1","from_2","to_2","uuid_2","from_3","to_3","uuid_3")]
  abonnenter <- melt.data.table(abonnenter, measure = patterns("^from_", "^to_", "^uuid_"), value.name = c("from", "to", "uuid"))
  abonnenter <- na.omit(abonnenter)
  abonnenter[,variable:=NULL]

  return(list(
    stack=stack,
    abonnenter=abonnenter
  ))
}





