convert_fylke_to_fhidata <- function(x) {
  x[x == "Finnmark"] <- "Finnmark-Finnmarku"
  x[x == glue::glue("M{fhi::nb$oe}re og Romsdal")] <- glue::glue("M{fhi::nb$oe}re-og-Romsdal")
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
    location = glue::glue("{fhi$nb$OE}stre Toten"),
    clean = glue::glue("{fhi$nb$OE}stre-Toten")
  )
  to_change[[length(to_change) + 1]] <- data.table(
    county_code = "county05",
    location = glue::glue("{fhi$nb$OE}ystre Slidre"),
    clean = glue::glue("{fhi$nb$OE}ystre-Slidre")
  )
  to_change <- rbindlist(to_change)
  x[to_change, on = .(county_code, location), clean := clean]
}
