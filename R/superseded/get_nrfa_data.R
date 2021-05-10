# Import_nrfa download data for one site (station)

get_nrfa_data <- function(my.site) {

  url.stub <- 'https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gdf&station='

  data_nrfa <- tryCatch(
    {
      download.file(paste0(url.stub, my.site), destfile = paste0(my.site, ".csv"))

      # was originally hard-wired to skip = 19, but some sites have <19 or > 19 metadata rows
      #nrfa_file <- readr::read_csv(paste0(my.site, '.csv'), col_names = FALSE, skip = 19)

      # instead check data to identify final metadata row and filter data accordingly
      nrfa_file <- readr::read_csv(paste0(my.site, '.csv'), col_names = FALSE)
      nrfa_file <- nrfa_file[ ,c(1,2)]
      # identify final metadata row (assumed to be have "last" in the second column)
      nrfa_file$X3 <- ifelse(nrfa_file$X2 == "last", 1, 0)
      nrfa_file <- nrfa_file %>%
        mutate(X4 = cumsum(X3)) %>%
        filter(X4 == 1)
      #drop final metadata row and redundant X3 and X4 columns
      nrfa_file <- nrfa_file[-1, -c(3,4)]

      # Add flow_site_id and quality columns
      #nrfa_file$Station <- my.site
      nrfa_file$flow_site_id <- my.site
      nrfa_file$quality <- NA

      # format and name columns
      nrfa_file <- nrfa_file %>%
        rename("date" = "X1", "flow" = "X2")

      nrfa_file$date <- as.Date(nrfa_file$date)
      nrfa_file$flow <- as.numeric(nrfa_file$flow)
      nrfa_file$quality <- as.character(nrfa_file$quality)

      nrfa_file <- nrfa_file

    },

    # Choose a return value in case of error
    error=function(cond) {
      message(paste("URL does not seem to exist for site:", my.site, ", site name is probably incorrect."))

      # create date vector of length 0
      a <- as.Date("2020-01-01"); a <- a[-1]
      nfra_file <- data.frame(date=a, flow=double(0), flow_site_id=character(0), quality = character(0))

    },
    #warning=function(cond) {
     # message(paste("URL does not seem to exist for this site:", my.site, ", site name is probably incorrect."))

      # create date vector of length 0
    #  a <- as.Date("2020-01-01"); a <- a[-1]
    #  nfra_file <- data.frame(date=a, flow=double(0), flow_site_id=character(0), quality = character(0))

   # },

    finally={}
  )

}


