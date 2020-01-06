#' test
#' @param raw a
#' @import data.table
#' @import fhi
#' @importFrom lubridate today
#' @importFrom stringr str_detect
#' @export CheckData
CheckData <- function(raw = fd::path("data_raw")) {
  if (file.exists(fd::path("data_raw", "DONE.txt")) & !fd::config$is_dev) {
    fd::msg("DONE.txt exists")
    quit(save = "no")
  }

  dataFilesExist <- TRUE
  for (f in CONFIG$FILES_DATA) {
    if (!fhi::file_stable(fd::path("data_raw", f))){
      fd::msg(f)
      dataFilesExist <- FALSE
    }
  }
  if (!dataFilesExist) {
    fd::msg("No new data files")
    quit(save = "no")
  }

  unlink(fd::path("results", lubridate::today(),"details.txt"))

}
