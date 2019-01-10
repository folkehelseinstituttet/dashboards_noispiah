fhi::DashboardInitialiseOpinionated("noispiah")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(foreach))
suppressMessages(library(doSNOW))
suppressMessages(library(iterators))

StackIterator <- function(stack, progressFunction) {
  library(data.table)
  it <- icount(nrow(stack))

  nextEl <- function() {
    i <- nextElem(it)
    progressFunction(i)
    stack[i]
    #list("stack"=stack[i],"data"=data[variable==stack$type[i] & location==stack$location[i] & age==stack$age[i]])
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}

CheckData()

stackA <- GenStackSykehjem(dev=fhi::DashboardIsDev(),
                         FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                         FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS)
stackB <- GenStackSykehus(dev=fhi::DashboardIsDev(),
                         FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                         FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS)
stack <- rbind(stackB,stackA)


pb <- RAWmisc::ProgressBarCreate(max=nrow(stack))
assign("pb", pb, envir = .GlobalEnv)

progressIndex <- 0
assign("progressIndex", progressIndex, envir = .GlobalEnv)

ProgressFunction <- function(n) RAWmisc::ProgressBarSet(pb, progressIndex + n)
assign("ProgressFunction", ProgressFunction, envir = .GlobalEnv)

opts <- list(progress=ProgressFunction)
assign("opts", opts, envir = .GlobalEnv)

if(!fhi::DashboardIsDev()){
  cl <- makeCluster(parallel::detectCores())
  registerDoSNOW(cl)
}

# copy over the resources
fhi::noispiah_resources_copy(dirname(CONFIG$FILES_RMD_USE_SYKEHJEM))

if(fhi::DashboardIsDev()){
  analysisIter=stack[level=="landsdekkende" & stringr::str_detect(RMD,"sykehjem")]
  #analysisIter=stack[20]
  file.remove(file.path(analysisIter$outputDirUse,sprintf("%s.pdf",analysisIter$location)))
  fhi::RenderExternally(input=analysisIter$RMD,
                        output_file=sprintf("%s.pdf",analysisIter$location),
                        output_dir=analysisIter$outputDirUse,
                        params=sprintf("dev=%s,package_dir=\"%s\",level=\"%s\",location=\"%s\",DATE_USE=\"%s\"",
                                       analysisIter$dev,
                                       getwd(),
                                       analysisIter$level,
                                       analysisIter$location,
                                       analysisIter$DATE_USE
                        ))
}

res <- foreach(analysisIter=StackIterator(stack, ProgressFunction)) %dopar% {
  Sys.sleep(1)
  fhi::RenderExternally(input=analysisIter$RMD,
                        output_file=sprintf("%s.pdf",analysisIter$location),
                        output_dir=analysisIter$outputDirUse,
                        params=sprintf("dev=%s,package_dir=\"%s\",level=\"%s\",location=\"%s\",DATE_USE=\"%s\"",
                                       analysisIter$dev,
                                       getwd(),
                                       analysisIter$level,
                                       analysisIter$location,
                                       analysisIter$DATE_USE
                                       ))
}
# delete the resources
fhi::noispiah_resources_remove(dirname(CONFIG$FILES_RMD_USE_SYKEHJEM))

file.create(DashboardFolder("data_raw","DONE.txt"))


if(!fhi::DashboardIsDev()) quit(save="no")

#"Møre og Romsdal - Fræna" -
#"Møre og Romsdal - Haram"
