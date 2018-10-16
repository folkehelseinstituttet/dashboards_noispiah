#' RenderExternally
#' @param input a
#' @param output_file a
#' @param output_dir a
#' @param params a
#' @importFrom processx run
#' @importFrom uuid UUIDgenerate
#' @export RenderExternally
RenderExternally <- function(input, output_file, output_dir, params="x=1"){
  tmp_dir <- tempdir()
  tmp_name <- sprintf("%s.pdf",uuid::UUIDgenerate())

  numberFails <- 0
  succeed <- FALSE
  while(numberFails < 3 & succeed==FALSE){
    x <- processx::run(
      command='R',
      args=c(
        '-e',
        sprintf('rmarkdown::render(\"%s\",output_file=\"%s\",output_dir=\"%s\",params=list(%s))',
                input,
                tmp_name,
                tmp_dir,
                params
        )
      ),
      error_on_status = F
    )
    if(x$status!=0){
      print(x)
      print(tmp_dir)
      print(tmp_name)
      numberFails <- numberFails + 1
      print(sprintf("FAILED %s TIMES, RETRYING",numberFails))
    } else {
      succeed <- TRUE
    }
  }
  if(succeed){
    file.copy(file.path(tmp_dir,tmp_name),file.path(output_dir,output_file))
  } else {
    stop("ERROR!!!")
  }
  #file.remove(file.path(tmp_dir,tmp_name))

  return(x)
}

#' ExtractEnglish
#' @param var a
#' @importFrom stringr str_extract_all
#' @export ExtractEnglish
ExtractEnglish <- function(var){
  unlist(lapply(stringr::str_extract_all(var,"[a-zA-Z]"),paste0,collapse=""))
}

#' GenStack
#' @param dev a
#' @param outputDir a
#' @param FILES_RMD_USE_SYKEHJEM a
#' @param FILES_RMD_USE_SYKEHUS a
#' @import data.table
#' @import fhi
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom lubridate today
#' @export GenStack
GenStack <- function(
  dev=FALSE,
  outputDir=fhi::DashboardFolder("results"),
  FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS
  ){

  Fylke <- NULL

  outputDirDaily <- file.path(outputDir,lubridate::today())
  dir.create(outputDirDaily)
  dir.create(file.path(outputDirDaily,"Sykehjem"))
  dir.create(file.path(outputDirDaily,"Sykehjem","Landsdekkende"))
  dir.create(file.path(outputDirDaily,"Sykehjem","Fylke"))
  dir.create(file.path(outputDirDaily,"Sykehjem","Kommune"))

  da <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw","AntibiotikadataPrimer.xlsx")))
  di <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw","InfeksjonsdataPrimer.xlsx")))

  maxDate <- max(da$PrevalensDato,di$PrevalensDato)
  da <- da[PrevalensDato==maxDate]
  di <- di[PrevalensDato==maxDate]

  fylkeInstitution <- unique(rbind(da[,c("Fylke","Institusjon")],di[,c("Fylke","Institusjon")]))
  setorder(fylkeInstitution,Fylke,Institusjon)
  for(f in unique(fylkeInstitution$Fylke)){
    dir.create(file.path(outputDirDaily,"Sykehjem","Kommune",f))
  }

  stack <- rbind(
    data.table(level="landsdekkende",
               location="Landsdekkende"),
    data.table(level="fylke",
               location=CONFIG$FYLKE),
    data.table(level="kommune",
               location=unique(fylkeInstitution$Institusjon))
  )
  stack[,order:=1:.N]
  stack <- merge(stack,fylkeInstitution,by.x="location",by.y="Institusjon",all.x=T)
  setorder(stack,order)

  stack[level=="landsdekkende",outputDirUse:=file.path(outputDirDaily,"Sykehjem","Landsdekkende")]
  stack[level=="fylke",outputDirUse:=file.path(outputDirDaily,"Sykehjem","Fylke")]
  stack[level=="kommune",outputDirUse:=file.path(outputDirDaily,"Sykehjem","Kommune",Fylke)]

  stack[,RMD:=FILES_RMD_USE_SYKEHJEM]
  stack[,dev:=dev]

  return(stack)
  pb <- RAWmisc::ProgressBarCreate(max=nrow(stack))
  for(i in 1:nrow(stack)){
    RAWmisc::ProgressBarSet(pb,i)

    Sys.sleep(1)
    RenderExternally(input=FILES_RMD_USE_SYKEHJEM,
                     output_file=sprintf("%s.pdf",stack$location[i]),
                     output_dir=stack$outputDirUse[i],
                     params=sprintf("dev=\"%s\",level=\"%s\",location=\"%s\"",dev,stack$level[i],stack$location[i]))
  }

  dir.create(file.path(outputDirDaily,"Sykehus"))
  dir.create(file.path(outputDirDaily,"Sykehus","Landsdekkende"))
  dir.create(file.path(outputDirDaily,"Sykehus","Helseforetak"))
  dir.create(file.path(outputDirDaily,"Sykehus","Institusjon"))

  da <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw","AntibiotikadataSpesialist.xlsx")))
  di <- data.table(readxl::read_excel(fhi::DashboardFolder("data_raw","InfeksjonsdataSpesialist.xlsx")))

  stack <- rbind(
    data.table(level="landsdekkende",location="Landsdekkende"),
    data.table(level="helseforetak",location=unique(c(da$HelseForetak,di$HelseForetak))),
    data.table(level="institusjon",location=unique(c(da$Institusjon,di$Institusjon)))
  )


  for(location in c("Landsdekkende",unique(c(da$Institusjon,di$Institusjon)))){
    if(location=="Landsdekkende"){
      outputDirUse <- file.path(outputDirDaily,"Sykehus","Landsdekkende")
      level <- "landsdekkende"
    } else if(location=="Helseforetak") {
      outputDirUse <- file.path(outputDirDaily,"Sykehus","Helseforetak")
      level <- "helseforetak"
    } else {
      outputDirUse <- file.path(outputDirDaily,"Sykehus","Institusjon")
      level <- "institusjon"
    }
    Sys.sleep(1)
    print(location)
    RenderExternally(input=FILES_RMD_USE_SYKEHUS,
                     output_file=sprintf("%s.pdf",location),
                     output_dir=outputDirUse,
                     params=sprintf("dev=\"%s\",level=\"%s\",location=\"%s\"",dev,level,location))
  }

}

