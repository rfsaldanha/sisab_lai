# Packages
library(dplyr)
library(lubridate)
library(arrow)
library(glue)
library(ggplot2)

siaps_data <- open_dataset(sources = "data/export/siaps_data.parquet")

siaps_data |> head() |> collect()

res <- siaps_data |>
  filter(CO_MUNICIPIO_IBGE == 230440) |>
  filter(TP_PCA == "CIAP" & PCA == "A03") |>
  arrange(NU_COMPETENCIA) |>
  collect() |>
  mutate(
    date = ymd(glue(
      "{substr(NU_COMPETENCIA, 0, 4)}-{substr(NU_COMPETENCIA, 5, 6)}-01"
    ))
  )

ggplot(data = res, aes(x = date, y = QT_ATENDIMENTOS)) +
  geom_line() +
  labs(title = "APS - Cuiabá, MT", subtitle = "CIAPS R05 - Tosse")

ggplot(
  data = res |> filter(year(date) >= 2022),
  aes(x = date, y = QT_ATENDIMENTOS)
) +
  geom_line() +
  labs(title = "APS - Cuiabá, MT", subtitle = "CIAPS R05 - Tosse")
