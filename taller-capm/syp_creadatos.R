# datos de acciones para clase

library(quantmod)
library(janitor)
library(tidyverse)
library(here)

sp500 <- read_csv(
  "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/main/data/constituents.csv"
) |>
  clean_names()
glimpse(sp500)
# tickers_all <- sort(gsub(".", "-", sp500$Symbol, fixed = TRUE))

tickers <- c("SPY", "WMT", "F", "AMZN", "KO") # SPY = ETF del S&P 500
desde <- "2015-01-01"

# Precios diarios ajustados -> retornos mensuales
getSymbols(tickers, src = "yahoo", from = desde)
ret <- map(tickers, \(t) monthlyReturn(Ad(get(t)), type = "arithmetic")) |>
  reduce(merge)
colnames(ret) <- tickers

# Tasa libre de riesgo: letras del Tesoro a 3 meses (% anual) -> mensual
rf1 <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=TB3MS")
names(rf1) # debería ser: observation_date, TB3MS

rf <- rf1 |>
  mutate(fecha = as_date(observation_date), rf = rf1$TB3MS / 100 / 12) |>
  filter(fecha >= as_date(desde)) |>
  select(fecha, rf)


# Datos para guardar retornos en exceso
coredata(ret) |>
  as_tibble() |> # datos de retorno
  mutate(fecha = index(ret) |> as_date() |> floor_date("month")) |> # fecha dia 1 mes
  inner_join(rf, by = "fecha") |> # datos activo libre riesgo
  mutate(across(all_of(tickers), \(x) x - rf)) |> # obtener exeso de retorno
  rename(mercado = SPY) |> # syp500 como mercado
  write_csv(file = "sp_partial_mensual.csv") # guardamos
