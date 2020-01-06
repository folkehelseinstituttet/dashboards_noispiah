#' copy_rmd
#' @export
copy_rmd <- function(){

  if (!dir.exists(fd::path("data_raw", "rapporter"))) dir.create(fd::path("data_raw", "rapporter"))
  #fhi::noispiah_resources_copy(fd::path("data_raw", "rapporter"))

  for (f in CONFIG$FILES_RMD_RAW) {
    file.copy(system.file("extdata", f, package = "noispiah"), fd::path("data_raw", "rapporter", f), overwrite = TRUE)

    files <- list.files(fd::path("data_raw", "rapporter"))
    filesTypeF <- max(files[stringr::str_detect(files, f)])
    switch(f,
           sykehjem.Rmd = {
             SetConfig("FILES_RMD_USE_SYKEHJEM", fd::path("data_raw", "rapporter", filesTypeF))
           },
           sykehus.Rmd = {
             SetConfig("FILES_RMD_USE_SYKEHUS", fd::path("data_raw", "rapporter", filesTypeF))
           }, {
             fd::msg("ERROR")
           }
    )
  }
}
