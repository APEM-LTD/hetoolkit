% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_inv.R
\name{import_inv}
\alias{import_inv}
\title{Importing macroinvertebrate sampling data from the EA Ecology and Fish Data Explorer}
\usage{
import_inv(source = "parquet", sites = NULL, start_date = NULL, end_date = NULL, save = FALSE, save_dwnld = FALSE, save_dir = getwd(), biol_dir = NULL)
}
\arguments{
\item{source}{Specify source of macroinvertebrate data: "parquet" or "csv" to automatically download data from EDE, or provide path to local .csv, .rds or .parquet file. (Alternatively set \code{source = NULL} and instead use deprecated \code{biol_dir} argument to provide path to local file). Default = "parquet".}

\item{sites}{Vector of site ids to filter by.}

\item{start_date}{Required start date (in \code{yyyy-mm-dd} format); older records are filtered out. Default = \code{NULL} to keep all available data.}

\item{end_date}{Required end date (in \code{yyyy-mm-dd} format); more recent records are filtered out. Default = \code{NULL} to keep all available data.}

\item{save}{Specifies whether (\code{TRUE}) or not (\code{FALSE}) the filtered data should be saved as an rds file (for future use, or audit trail). Default = \code{FALSE}.}

\item{save_dwnld}{Specifies whether (\code{TRUE}) or not (\code{FALSE}) the unfiltered parquet or csv file download should be saved, in .rds format. Default = \code{FALSE}.}

\item{save_dir}{Path to folder where downloaded and/or filtered data are to be saved. Default = Current working directory.}

\item{biol_dir}{Deprecated. Path to local .csv, .rds or parquet file containing macroinvertebrate data. Default = \code{NULL} (download data from EDE).}
}
\value{
Tibble containing imported macroinvertebrate data.
}
\description{
The \code{import_inv} function imports macroinvertebrate sampling data from the Environment Agency's Ecology and Fish Data Explorer (EDE). The data can either be downloaded automatically in .parquet or .csv format, or read in from a previously saved .csv or .rds file. The data can be optionally filtered by site ID and sample date, and the filtered data saved as a .rds file.
}
\details{
If automatically downloading data from EDE, the parquet file format is faster to download than csv, and has data types pre-formatted.

If saving a copy of the downloaded data, the name of the rds file is hard-wired to \code{INV_OPEN_DATA_METRICS_ALL.RDS}. If saving after filtering on site and/or date, the name of the rds file is hard-wired to \code{INV_OPEN_DATA_METRICS_F.RDS}.

Downloaded raw data files (in .parquet and .csv format) will be automatically removed from the working directory following completed execution of the function.

The function automatically modifies the output from EDE, renaming "SITE_ID" to "biol_site_id" (\code{hetoolkit}'s standardised column header for biology site ids).
}
\examples{

# Bulk download of EDE data for all sites in parquet format and save as .rds file for future use:
# import_inv(save_dwnld = TRUE, save_dir = getwd())

# Bulk download of EDE data for all sites in parquet format:
# import_inv(source = "csv")

# Read in local .rds file and filter on selected sites and dates (up to the present day):
# import_inv(source = "data/INV_OPEN_DATA_METRICS_ALL.rds",
#                sites = c("34310", "34343"),
#                start_date = "1995-01-01",
#                end_date = Sys.Date())

# Read in local .csv file, filter on selected sites, and save the result as a .rds file:
# import_inv(source = "data/INV_OPEN_DATA_METRICS.csv",
#          sites = c("34310", "34343"),
#          save = TRUE,
#          save_dir = getwd())
}
