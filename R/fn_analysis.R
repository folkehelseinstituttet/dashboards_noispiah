#' fn_analysis
#' @param data a
#' @param arg_pdf a
#' @export
fn_analysis <- function(data, arg_pdf) {
  # data <- plan_pdf$data_get()
  # arg_pdf <- plan_pdf$analysis_get(89)$arg_pdf

  temp_dir <- arg_pdf$temp_dir
  temp_uuid <- uuid::UUIDgenerate()
  temp_pdf <- glue::glue("{temp_uuid}.pdf")

  fs::file_copy(arg_pdf$base_RMD, arg_pdf$RMD, overwrite = T)
  fhi::noispiah_resources_copy(arg_pdf$temp_dir)
  # on.exit(unlink(temp_dir, recursive=T, force=T))

  # Sys.sleep(runif(1))

  fails <- 0
  success <- 0
  while (success < 1 & fails < 2) {
    cat(arg_pdf$index_analysis, " - ", arg_pdf$output_pdf, "TRY \n", file = fd::path("results", lubridate::today(), "details.txt", package = "noispiah"), append = T)

    tryCatch({
      rmarkdown::render(
        input = arg_pdf$RMD,
        output_file = temp_pdf,
        output_dir = temp_dir,
        params = list(
          arg_pdf = arg_pdf
        ),
        quiet = T,
        envir = new.env()
      )
      success <- success + 1
    }, error = function(e) {
      cat(arg_pdf$index_analysis, " ERROR", "\n", file = fd::path("results", lubridate::today(), "details.txt", package = "noispiah"), append = T)
      Sys.sleep(stats::runif(1, min = 1, max = 2.5))
      fails <<- fails + 1
    })
  }
  if (fails >= 2) stop(glue::glue("too many fails at index: {arg_pdf$index_analysis}"))

  file.copy(fs::path(temp_dir, temp_pdf), arg_pdf$output_pdf, overwrite = TRUE)
  if (!file.exists(arg_pdf$output_pdf)) fd::msg(glue::glue("index: {arg_pdf$index_analysis} couldn't copy"), type = "err")

  cat(arg_pdf$index_analysis, " - ", arg_pdf$output_pdf, "SUCCEED \n", file = fd::path("results", lubridate::today(), "details.txt", package = "noispiah"), append = T)

  to_copy <- data$abonnenter[uuid == arg_pdf$uuid]
  if (nrow(to_copy) > 0) {
    for (j in 1:nrow(to_copy)) fs::file_copy(to_copy$from[j], to_copy$to[j], overwrite = TRUE)
  }
}
