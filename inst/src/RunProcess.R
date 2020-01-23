fd::initialize("noispiah")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

CheckData()

if(!fd::config$is_production){
  requested_date <- "2019-11-06"
  abonnenter_file <- "abonnenter-2019-12-17.xlsx"
} else {
  requested_date <- "2019-11-06"
  abonnenter_file <- "abonnenter-2019-12-17.xlsx"
}

# copy over the rmd
copy_rmd()

analysis_sykehus <- gen_stack_sykehus(
  dev=!fd::config$is_production,
  FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS,
  requested_date=requested_date,
  abonnenter_file=abonnenter_file
  )

analysis_sykehjem <- gen_stack_sykehjem(
  dev=!fd::config$is_production,
  FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
  FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS,
  requested_date=requested_date,
  abonnenter_file=abonnenter_file
  )
# sort(CONFIG$VALID_SYKEHUS)
# sort(CONFIG$VALID_SYKEHJEM)
abonnenter <- rbind(analysis_sykehus$abonnenter,analysis_sykehjem$abonnenter)

# create the plan for emails
plan_email <- gen_plan_email(
  dev=!fd::config$is_production,
  abonnenter=abonnenter,
  DATE_USE = analysis_sykehjem$stack$DATE_USE[1]
  )
plan_email$apply_analysis_fn_to_all(fn_email)

# create the plan
plan_pdf <- plnr::Plan$new(argset_name = "argset_pdf")
plan_pdf$add_data(direct = abonnenter, name = "abonnenter")
plan_pdf$add_argset_from_df(df = rbind(analysis_sykehus$stack, analysis_sykehjem$stack, fill=T))
#plan_pdf$analysis_add_from_df(df = analysis_sykehjem$stack[stringr::str_detect(location_name,"lesberg")])
#plan_pdf$analysis_add_from_df(df = rbind(stackA$stack[1], stackB$stack[1], fill=T))

# add the analysis function to the plan
plan_pdf$apply_analysis_fn_to_all(fn_analysis)

# argset_pdf <- plan_pdf$argset_get(99)
plan_pdf$run_all()
# plan_pdf$run_one(1)

# Check to see if all files have been created
files_required <- unique(c(abonnenter$from, abonnenter$to))

fail <- FALSE
for(i in files_required){
  if(!fs::file_exists(i)){
    fd::msg(glue::glue("{i} does not exist"),newLine = T)
    fail <- TRUE
  }
}

if(fail){
  fd::msg("Missing files",type = "err", slack=T)
} else {
  fd::msg("All files created properly", slack=T)
}

# plan_email$len()
# plan_email$get_argset(1)
# plan_email$run_one(2)
# plan_email$run_all()

file.create(fd::path("data_raw","DONE.txt"))


if(!fhi::DashboardIsDev()) quit(save="no")

#"Møre og Romsdal - Fræna" -
#"Møre og Romsdal - Haram"
