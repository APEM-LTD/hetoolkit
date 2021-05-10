# returns a single value which is a url string, given a HDE station id

get_hde_url <- function(my.station, global.stations){

  global.stations %>%
    dplyr::filter(wiskiID == my.station) %>%
    dplyr::select(value) %>%
    dplyr::pull()

}
