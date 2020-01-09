#' fn_analysis
#' @param data a
#' @param argset_pdf a
#' @export
fn_analysis <- function(data, argset_pdf) {
  # data <- plan_pdf$data_get()
  # argset_pdf <- plan_pdf$argset_get(89)

  temp_dir <- argset_pdf$temp_dir
  temp_uuid <- uuid::UUIDgenerate()
  temp_pdf <- glue::glue("{temp_uuid}.pdf")

  fs::file_copy(argset_pdf$base_RMD, argset_pdf$RMD, overwrite=T)
  fhi::noispiah_resources_copy(argset_pdf$temp_dir)
  # on.exit(unlink(temp_dir, recursive=T, force=T))

  # Sys.sleep(runif(1))

  fails <- 0
  success <- 0
  while (success < 1 & fails < 2) {
    cat(argset_pdf$index_analysis, " - ", argset_pdf$output_pdf, "TRY \n", file = fd::path("results", lubridate::today(), "details.txt", package = "noispiah"), append = T)

    tryCatch({
      rmarkdown::render(
        input = argset_pdf$RMD,
        output_file = temp_pdf,
        output_dir = temp_dir,
        params = list(
          argset_pdf = argset_pdf
        ),
        quiet = T,
        envir = new.env()
      )
      success <- success + 1
    }, error = function(e) {
      cat(argset_pdf$index_analysis, " ERROR", "\n", file = fd::path("results", lubridate::today(), "details.txt", package = "noispiah"), append = T)
      Sys.sleep(stats::runif(1, min = 1, max = 2.5))
      fails <<- fails + 1
    })
  }
  if (fails >= 2) stop(glue::glue("too many fails at index: {argset_pdf$index_analysis}"))

  file.copy(fs::path(temp_dir, temp_pdf), argset_pdf$output_pdf, overwrite = TRUE)
  if (!file.exists(argset_pdf$output_pdf)) fd::msg(glue::glue("index: {argset_pdf$index_analysis} couldn't copy"), type = "err")

  cat(argset_pdf$index_analysis, " - ", argset_pdf$output_pdf, "SUCCEED \n", file = fd::path("results", lubridate::today(), "details.txt", package = "noispiah"), append = T)

  to_copy <- data$abonnenter[uuid == argset_pdf$uuid]
  if (nrow(to_copy) > 0) {
    for (j in 1:nrow(to_copy)) fs::file_copy(to_copy$from[j], to_copy$to[j], overwrite = TRUE)
  }
}
