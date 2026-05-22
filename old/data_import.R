# Packages
library(dplyr)
library(lubridate)
library(readr)
library(DBI)
library(duckdb)
library(tibble)
library(R.utils)
library(zip)
library(fs)

# Data folder
data_folder <- "data/lai/pedido_25072008417202645/csv/"

# File list
file_list <- list.files(
  path = data_folder,
  pattern = "*.csv",
  full.names = TRUE
)

# Database
con <- dbConnect(duckdb(), "data/export/siaps_data.duckdb")

# Drop table
if (dbExistsTable(con, "siaps_data")) {
  dbRemoveTable(con, "siaps_data")
}

# Read files
for (f in file_list) {
  message(basename(f))
  n_lines <- countLines(f)
  if (basename(f) == "query_pca_202407.csv") {
    next
  } else {
    tmp <- read_csv(
      file = f,
      skip = 2,
      n_max = n_lines - 4,
      col_types = "iiicci"
    )
  }

  dbWriteTable(conn = con, name = "siaps_data", value = tmp, append = TRUE)
}

# Check
tbl(con, "siaps_data")

# Export data
dbExecute(
  con,
  "COPY (SELECT * FROM 'siaps_data') TO 'siaps_data.parquet' (FORMAT 'PARQUET')"
)
dbExecute(
  con,
  "COPY (SELECT * FROM 'siaps_data') TO 'siaps_data.csv' (FORMAT 'CSV')"
)
zip(zipfile = "siaps_data.csv.zip", files = "siaps_data.csv")
file_delete(path = "siaps_data.csv")
file_move(
  path = "siaps_data.csv.zip",
  new_path = "data/export/siaps_data.csv.zip"
)
file_move(
  path = "siaps_data.parquet",
  new_path = "data/export/siaps_data.parquet"
)

# Database diconnect
dbDisconnect(con)
