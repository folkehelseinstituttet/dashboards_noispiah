convert_date_to_kvartal.int <- function(date){
  kvartal <- c("2.","4.")
  kvartal <- kvartal[as.numeric(lubridate::month(date)>6)+1]
  retval <- glue::glue("{kvartal} kvartal {lubridate::year(date)}")
  return(retval)
}

convert_date_to_kvartal <- Vectorize(convert_date_to_kvartal.int)

convert_date_to_season.int <- function(date){
  season <- c("vinter","v\u00E5r","sommer","h\u00F8st")
  season <- season[lubridate::quarter(date, fiscal_start = 12)]
  if(lubridate::month(date)==12){
    retval <- glue::glue("{season} {lubridate::year(date)}/{lubridate::year(date)+1}")
  } else if(lubridate::month(date) %in% c(1:2)){
    retval <- glue::glue("{season} {lubridate::year(date)-1}/{lubridate::year(date)}")
  } else {
    retval <- glue::glue("{season} {lubridate::year(date)}")
  }
  return(retval)
}

convert_date_to_season <- Vectorize(convert_date_to_season.int)
