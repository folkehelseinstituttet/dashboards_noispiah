#' test
#' @param raw a
#' @import data.table
#' @import fhi
#' @importFrom lubridate today
#' @importFrom stringr str_detect
#' @export CheckData
CheckData <- function(raw=fhi::DashboardFolder("data_raw")){
  if(file.exists(file.path(raw,"DONE.txt")) & CONFIG$FORCE_TESTING==FALSE){
    cat(raw,"\n")
    cat(CONFIG$FORCE_TESTING,"\n")
    cat(sprintf("%s/%s/R/noispiah DONE.txt exists",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    quit(save="no")
  }

  if(!dir.exists(file.path(raw,"rapporter"))) dir.create(file.path(raw,"rapporter"))

  dataFilesExist <- TRUE
  for(f in noispiah::CONFIG$FILES_DATA){
    if(!RAWmisc::IsFileStable(file.path(raw,f))) dataFilesExist <- FALSE
  }
  if(!dataFilesExist){
    cat(sprintf("%s/%s/R/noispiah No new data files",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    quit(save="no")
  }

  for(f in CONFIG$LATEX_RAW){
    file.copy(system.file("extdata", f, package = "noispiah"),file.path(raw,"rapporter",f), overwrite = TRUE)
  }

  for(f in CONFIG$FILES_RMD_RAW){
    files <- list.files(file.path(raw,"rapporter"))
    filesTypeF <- files[stringr::str_detect(files,f)]
    if(length(filesTypeF)==0 | CONFIG$FORCE_TESTING){
      file.copy(system.file("extdata", f, package = "noispiah"),file.path(raw,"rapporter",sprintf("%s_%s",lubridate::today(),f)), overwrite = TRUE)
    }

    files <- list.files(file.path(raw,"rapporter"))
    filesTypeF <- max(files[stringr::str_detect(files,f)])
    switch(f,
           sykehjem.Rmd={
             SetConfig("FILES_RMD_USE_SYKEHJEM",file.path(raw,"rapporter",filesTypeF))
           },
           sykehus.Rmd={
             SetConfig("FILES_RMD_USE_SYKEHUS",file.path(raw,"rapporter",filesTypeF))
           },
           {
             print('ERROR')
           }
    )
  }
}
