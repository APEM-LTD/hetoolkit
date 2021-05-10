

get_hde_flows <- function(my.station, global.stations, min.date, max.date = date(today()), SAFE = TRUE, PEEKDATA = TRUE){

  my.station.url <- get_hde_url(my.station, global.stations)
  full.url <- paste0(my.station.url, "/readings.csv?mineq-date=", min.date, "&&maxeq-date=", max.date )
  print(paste('getting flow data from:', my.station))
  #print(paste('url:', full.url)) ; flush.console()

  # value column gets read in error as logical by default, so force to read as numeric
  # maybe this is when have NAs at the beginning

  if(SAFE == TRUE){
    print('Using SAFE mode, downloading file first')
    download.file(full.url, destfile = 'temp_download.csv')
    if (file.info('temp_download.csv')$size >0){
      flow.series <- readr::read_csv('temp_download.csv', col_types = readr::cols(value = readr::col_double()) ) %>%
        dplyr::mutate(wiskiID = my.station) %>% dplyr::select(wiskiID, date, value, quality) #, everything())
      # if file is of length zero then just don't read it
      # optional, remove temp file
    } else{
      # create empty df
      flow.series = tibble::tibble(wiskiID = character(), date = lubridate::ymd(), value = double(), quality = character())
    }
  } else {
    print('Using direct access mode')
    # unsafe method, will fail with empty file
    flow.series <- readr::read_csv(full.url, col_types = cols(value = col_double()) ) %>%
      mutate(wiskiID = my.station) %>%
      dplyr::select(wiskiID, date, value, quality) #, everything())
  }

  if(PEEKDATA == TRUE){
    print(head(flow.series))
    print(tail(flow.series))
  }

  flush.console()

  return(flow.series)

}
