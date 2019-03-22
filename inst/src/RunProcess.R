fhi::DashboardInitialiseOpinionated("noispiah")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))

convert_stack_to_list <- function(stack){
  retval <- vector("list", length = nrow(stack))
  for (i in seq_along(retval)) {
    retval[[i]] <- list("stack" = stack[i])
  }
  retval
}

CheckData()

stackA <- GenStackSykehus(dev=fhi::DashboardIsDev(),
                         FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                         FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS)
stackB <- GenStackSykehjem(dev=fhi::DashboardIsDev(),
                           FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                           FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS)
stack <- rbind(stackA,stackB)
#stack <- stack[stringr::str_detect(outputDirUse,"Sykehjem")]
stack_list <- convert_stack_to_list(stack)

# copy over the resources
fhi::noispiah_resources_copy(dirname(CONFIG$FILES_RMD_USE_SYKEHJEM))

which(stringr::str_detect(stack$output_pdf,"øre"))
#x <- stack_list[[153]]
res <- pbmclapply(stack_list,
                  function(x){
                    Sys.sleep(1)
                    fhi::RenderExternally(
                      input=x$stack$RMD,
                      output_file=sprintf("%s.pdf",x$stack$location),
                      output_dir=x$stack$outputDirUse,
                      params=as.character(glue::glue("dev={dev},\\
                                        package_dir=\"{package_dir}\",\\
                                        level=\"{level}\",\\
                                        location=\"{location}\",\\
                                        DATE_USE=\"{DATE_USE}\"",
                                     dev=x$stack$dev,
                                     package_dir=getwd(),
                                     level=x$stack$level,
                                     location=x$stack$location,
                                     DATE_USE=x$stack$DATE_USE
                      )))
                  },
                  mc.cores = parallel::detectCores()
)

# Check to see if all files have been created
fail <- FALSE
for(i in stack$output_pdf){
  if(!fs::file_exists(i)){
    fhi::DashboardMsg(glue::glue("{i} does not exist"),newLine = T)
    fail <- TRUE
  }
}

if(fail){
  fhi::DashboardMsg("Missing files",type = "err")
} else {
  fhi::DashboardMsg("All files created properly")
}


# delete the resources
fhi::noispiah_resources_remove(dirname(CONFIG$FILES_RMD_USE_SYKEHJEM))

file.create(fhi::DashboardFolder("data_raw","DONE.txt"))


if(!fhi::DashboardIsDev()) quit(save="no")

#"Møre og Romsdal - Fræna" -
#"Møre og Romsdal - Haram"
