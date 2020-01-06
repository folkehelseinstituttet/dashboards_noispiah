fd::initialize("noispiah")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

CheckData()

if(!fd::config$is_production){
  requested_date <- "2019-11-06"
  abonnenter_file <- "abonnenter-2019-09-25.xlsx"
} else {
  requested_date <- "2019-11-06"
  abonnenter_file <- "abonnenter-2019-09-25.xlsx"
}

# copy over the rmd
copy_rmd()

analysis_sykehus <- gen_stack_sykehus(dev=!fd::config$is_production,
                          FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                          FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS,
                          requested_date=requested_date,
                          abonnenter_file=abonnenter_file)
analysis_sykehjem <- gen_stack_sykehjem(dev=!fd::config$is_production,
                           FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                           FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS,
                           requested_date=requested_date,
                           abonnenter_file=abonnenter_file)
abonnenter <- rbind(analysis_sykehus$abonnenter,analysis_sykehjem$abonnenter)

# create the plan for emails
plan_email <- gen_plan_email(
  dev=!fd::config$is_production,
  abonnenter=abonnenter
  )
plan_email$analysis_fn_apply_to_all(fn_email)

# create the plan
plan_pdf <- plnr::Plan$new(name_arg = "arg_pdf")
plan_pdf$data_add(df = abonnenter, name = "abonnenter")
plan_pdf$analysis_add_from_df(df = rbind(analysis_sykehus$stack, analysis_sykehjem$stack, fill=T))
#plan_pdf$analysis_add_from_df(df = rbind(stackA$stack[1], stackB$stack[1], fill=T))

# add the analysis function to the plan
plan_pdf$analysis_fn_apply_to_all(fn_analysis)

# arg_pdf <- plan_pdf$analysis_get(1)$arg_pdf
# plan_pdf$run_all()
# plan_pdf$run_one(28)
plnr::run_all_parallel(
  plan_pdf,
  cores = 4,
  future.chunk.size = 1,
  multisession = T
)

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

file.create(fd::path("data_raw","DONE.txt"))


if(!fhi::DashboardIsDev()) quit(save="no")

#"Møre og Romsdal - Fræna" -
#"Møre og Romsdal - Haram"


run_all_parallel <- function(plan, cores = parallel::detectCores(), verbose = interactive()){
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  data <- plan$data_get()
  if(verbose) pb <- fhi::txt_progress_bar(max=plan$len())

  foreach(
    i = plan$x_seq_along(),
    .packages = c("data.table"),
    .export = "plan"
  ) %dopar% {
    plan$parent_env <- environment()
    plan$run_one_with_data(index_analysis = i, data = data)
    if(verbose) utils::setTxtProgressBar(pb, value = i)
  }

}
