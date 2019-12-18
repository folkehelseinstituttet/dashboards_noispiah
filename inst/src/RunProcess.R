fd::initialize("noispiah")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(foreach))

convert_stack_to_list <- function(stack){
  retval <- vector("list", length = nrow(stack))
  for (i in seq_along(retval)) {
    retval[[i]] <- list("stack" = stack[i])
  }
  retval
}

CheckData()

if(!fd::config$is_production){
  requested_date <- "2019-05-22"
} else {
  requested_date <- NULL
}



stackA <- GenStackSykehus(dev=!fd::config$is_production,
                          FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                          FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS,
                          requested_date=requested_date)
stackB <- GenStackSykehjem(dev=!fd::config$is_production,
                           FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                           FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS,
                           requested_date=requested_date)

abonnenter <- rbind(stackA$abonnenter,stackB$abonnenter)
stack <- rbind(stackA$stack,stackB$stack,fill=T)
#stack <- stack[stringr::str_detect(outputDirUse,"Sykehus")]
stack_list <- convert_stack_to_list(stack)

# copy over the resources
fhi::noispiah_resources_copy(dirname(CONFIG$FILES_RMD_USE_SYKEHJEM))
fhi::noispiah_resources_copy(dirname(CONFIG$FILES_RMD_USE_SYKEHUS))

which(stringr::str_detect(stack$output_pdf,"Buskerud"))
x <- stack_list[[91]]

pb <- fhi::txt_progress_bar(max=length(stack_list))

for(i in 1:length(stack_list)){
  setTxtProgressBar(pb, i)
  x <- stack_list[[i]]

  Sys.sleep(2)

  temp_dir <- fhi::temp_dir()
  temp_uuid <- uuid::UUIDgenerate()
  temp_file <- glue::glue("{temp_uuid}.pdf")

  fails <- 0
  success <- 0
  while(success<1 & fails<2){
    tryCatch({
      rmarkdown::render(
        input=x$stack$RMD,
        output_file=temp_file,
        output_dir=temp_dir,
        params=list(
          level=x$stack$level,
          location=x$stack$location,
          fylke=x$stack$fylke,
          DATE_USE=x$stack$DATE_USE,
          date_display=x$stack$date_display
        ),
        envir=new.env(),
        quiet=TRUE,
        clean=T
      )
      success <<- success + 1
    }, error=function(e){
      fails <<- fails + 1
    })
  }
  if(fails>=2) stop("too many fails!")

  fs::file_copy(fs::path(temp_dir,temp_file),x$stack$output_pdf, overwrite=TRUE)
  unlink(fd::path("data_raw","rapporter",glue::glue("{temp_uuid}.log")))

  to_copy <- abonnenter[uuid == x$stack$uuid]
  if(nrow(to_copy)>0){
    for(j in 1:nrow(to_copy)) fs::file_copy(to_copy$from[j],to_copy$to[j], overwrite=TRUE)
  }
}

# Check to see if all files have been created
fail <- FALSE
for(i in stack$output_pdf){
  if(!fs::file_exists(i)){
    fd::msg(glue::glue("{i} does not exist"),newLine = T)
    fail <- TRUE
  }
}

if(fail){
  fd::msg("Missing files",type = "err")
} else {
  fd::msg("All files created properly")
}


# delete the resources
fhi::noispiah_resources_remove(dirname(CONFIG$FILES_RMD_USE_SYKEHJEM))

file.create(fd::path("data_raw","DONE.txt"))


if(!fhi::DashboardIsDev()) quit(save="no")

#"Møre og Romsdal - Fræna" -
#"Møre og Romsdal - Haram"
