# download the HDE stations table and make a column which has the url for daily mean flows for each station

get_hde_stations <- function(){

  stations <- download.file("http://environment.data.gov.uk/hydrology/id/stations.csv", destfile = "stations.csv", mode ="wb")

  stations <- readr::read_csv("stations.csv")

  stations <- stations %>%
    dplyr::select(wiskiID, label, stationReference, riverName, easting, northing, measures) %>%
    tidyr::separate(measures, c("col1", "col2"), sep = "\\|", fill = 'right') %>%
    tidyr::pivot_longer(cols = c(col1, col2), names_to = 'col') %>%
    dplyr::filter(str_detect(value, '86400'))

  return(stations)

}
