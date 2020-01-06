convert_fylke_to_fhidata <- function(x){
  x[x=="Finnmark"] <- "Finnmark-Finnmarku"
  x[x=="Møre og Romsdal"] <- "Møre-og-Romsdal"
  x[x=="Sogn og Fjordane"] <- "Sogn-og-fjordane"
  return(x)
}

convert_kommune_to_fhidata <- function(x){
  to_change <- list()
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county09",
    location="Evje og Hornnes",
    clean="Evje-og-Hornnes"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county06",
    location="Nedre Eiker",
    clean="Nedre-Eiker"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county06",
    location="Nore og Uvdal",
    clean="Nore-og-Uvdal"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county06",
    location="Øvre Eiker",
    clean="Øvre-Eiker"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county20",
    location="Porsanger Porsángu Porsanki",
    clean="Porsanger"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county05",
    location="Nordre Land",
    clean="Nordre-Land"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county05",
    location="Søndre Land",
    clean="Søndre-Land"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county05",
    location="Østre Toten",
    clean="Østre-Toten"
  )
  to_change[[length(to_change)+1]] <- data.table(
    county_code="county05",
    location="Øystre Slidre",
    clean="Øystre-Slidre"
  )
  to_change <- rbindlist(to_change)
  x[to_change,on=.(county_code,location),clean:=clean]
}
