#' convert_fylke_to_fhidata
#' @param x a
#' @export
convert_fylke_to_fhidata <- function(x) {
  x[x == "Finnmark"] <- "Finnmark-Finnmarku"
  x[x == "Sogn og Fjordane"] <- "Sogn-og-fjordane"
  return(x)
}

convert_kommune_to_fhidata <- function(x) {
  to_change <- list()
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county09",
    location = "Evje og Hornnes",
    clean = "Evje-og-Hornnes"
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county06",
    location = "Nedre Eiker",
    clean = "Nedre-Eiker"
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county06",
    location = "Nore og Uvdal",
    clean = "Nore-og-Uvdal"
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county06",
    location = glue::glue("{fhi::nb$OE}vre Eiker"),
    clean = glue::glue("{fhi::nb$OE}vre-Eiker")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county20",
    # location="Porsanger Porsángu Porsanki",
    location = "Porsanger Pors\u00e1ngu Porsanki",
    clean = "Porsanger"
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = "Nordre Land",
    clean = "Nordre-Land"
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = glue::glue("S{fhi::nb$oe}ndre Land"),
    clean = glue::glue("S{fhi::nb$oe}ndre-Land")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = glue::glue("{fhi::nb$OE}stre Toten"),
    clean = glue::glue("{fhi::nb$OE}stre-Toten")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = glue::glue("{fhi::nb$OE}ystre Slidre"),
    clean = glue::glue("{fhi::nb$OE}ystre-Slidre")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = glue::glue("Vestre Slidre"),
    clean = glue::glue("Vestre-Slidre")
  )


  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county20",
    location = glue::glue("Deatnu Tana"),
    clean = glue::glue("Tana")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county20",
    location = glue::glue("Guovdageaidnu Kautokeino"),
    clean = glue::glue("Kautokeino")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county20",
    location = glue::glue("K\u00e1r\u00e1sjohka Karasjok"),
    clean = glue::glue("Karasjok")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county20",
    location = glue::glue("Unj\u00e1rga Nesseby"),
    clean = glue::glue("Nesseby")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = glue::glue("Vestre Toten"),
    clean = glue::glue("Vestre-Toten")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county19",
    location = glue::glue("G\u00e1ivuotna K{fhi::nb$aa}fjord"),
    clean = glue::glue("K{fhi::nb$aa}fjord")
  )

  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county50",
    location = glue::glue("Indre Fosen"),
    clean = glue::glue("Indre-Fosen")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county50",
    location = glue::glue("Midtre Gauldal"),
    clean = glue::glue("Midtre-Gauldal")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county50",
    location = glue::glue("Sn{fhi::nb$aa}sa"),
    clean = glue::glue("Sn{fhi::nb$aa}ase-Sn{fhi::nb$aa}sa")
  )


  for (i in seq_along(to_change)) {
    for (j in 1:ncol(to_change[[i]])) {
      to_change[[i]][[j]] <- as.character(to_change[[i]][[j]])
    }
  }
  to_change <- rbindlist(to_change)
  x[to_change, on = .(county_code, location), clean := clean]
}
