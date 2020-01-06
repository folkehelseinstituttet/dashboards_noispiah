compare_new_to_ref <- function(xnew, xref) {
  fail <- FALSE
  # reference files missing in new export
  trouble <- xref[!xref %in% xnew]
  if (length(trouble) > 0) {
    message(crayon::red(glue::glue("  \u2716 Missing in new: {trouble}\n\n")))
    fail <- TRUE
  }

  # new export files
  trouble <- xnew[!xnew %in% xref]
  if (length(trouble) > 0) {
    cat(crayon::red(glue::glue("  \u2716 Something new: {trouble}\n\n")))
    fail <- TRUE
  }

  if (fail) stop("Not the same")
}

compare_subset_to_ref <- function(sub, ref) {
  fail <- FALSE

  # new export files
  trouble <- sub[!sub %in% ref]
  if (length(trouble) > 0) {
    cat(crayon::red(glue::glue("  \u2716 Something new: {trouble}\n\n")))
    fail <- TRUE
  }

  if (fail) stop("Subset contains new items")
}
