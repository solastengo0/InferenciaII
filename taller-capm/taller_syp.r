# =============================================================
# Taller: CAPM bayesiano con Stan (cmdstanr)
# Basado en "Modern Business Analytics" (Taddy, Hendrix & Harding)
# Datos: retornos mensuales en exceso (sobre la letra del Tesoro a 3 meses),
#        enero 2015 en adelante, SPY como proxy del mercado.
# =============================================================

# 0) biblotecas e instalacion

# install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))

library(cmdstanr) # operar stan

# check_cmdstan_toolchain()      # verifica que g++ y make funcionen
# install_cmdstan(cores = 2)     # demora

library(tidyverse)
library(posterior)
library(bayesplot)
library(tsibble)

theme_set(theme_minimal())

# -------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------
# 'sypdt' es la tabla armada con quantmod + FRED:
#   fecha, mercado, WMT, F, AMZN, KO, rf  (retornos ya en exceso sobre rf)
# Si está guardada en disco:
# sypdt <- read_csv("capm_mensual.csv")

firmas <- c("WMT", "F", "AMZN", "KO")

sypdt <- read_csv('sp_partial_mensual.csv') |>
  mutate(
    fecha = yearmonth(fecha),
    across(c(mercado, all_of(firmas)), \(r) 100 * r)
  ) # a porcentaje

sypdt_long <- sypdt |>
  pivot_longer(all_of(firmas), names_to = "firma", values_to = "ret")

# -------------------------------------------------------------
# 2) explorar los datos
# -------------------------------------------------------------

sypdt |> select(-fecha) |> summary()

# Series de tiempo
sypdt_long |>
  ggplot() +
  geom_line(aes(fecha, ret)) +
  geom_line(aes(fecha, mercado), color = 'Steelblue') +
  facet_wrap(~firma) +
  labs(x = NULL, y = "Retorno mensual en exceso (%)")

# Firma vs mercado, con la recta de mínimos cuadrados
ggplot(sypdt_long, aes(mercado, ret)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) + # beta = 1
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~firma) +
  labs(x = "Mercado (%)", y = "Firma (%)", caption = "Línea punteada: beta = 1")

# -------------------------------------------------------------
# 3) Modelo para una firma
# -------------------------------------------------------------

# CAPM: modelo financiero de valuación de activos
# https://es.wikipedia.org/wiki/Modelo_de_valoraci%C3%B3n_de_activos_financieros

# Modelo para datos - regresion simple:
#            respuesta: retornos observados del activo (y)
#            explicativa: retornos del mercado         (x)
#  y ~ N(alpha + beta*x, sigma^2)
#
# beta: como reacciona el activo ante cambios en el mercado (amplifica o amortigua?)
# alpha: retorno extra respecto del de mercado
# sigma: representa el riesgo de la firma (volatilidad)

# Previas ??

# Escribir modelo en STAN

# ------------------------------------------------------------
# 4) Estimación con Stan: usamos activo Ford
# -------------------------------------------------------------

datos_firma <- function(tk, s) {
  list(N = nrow(sypdt), x = sypdt$mercado, y = sypdt[[tk]], s = s)
}

datos_firma("F")

# Compilación: una sola vez
system.time(mod <- cmdstan_model("syp_taller.stan"))

# Ajustar el modelo
system.time(res <- mod$sample(data = datos_firma('F', .2)))


# res queda guardado el resultado del mcmc
# mediante res$ZZZ() aplicamos el método ZZZ al objeto res,
# ZZZ representa lo que queremos hacer:
# resumir resultaods, diagnosticar cadenas, extraer simulaciones posterior

# extraemmos simulaciones con draws(parametro, formato)

# -------------------------------------------------------------
# 5) Diagnósticos de las cadenas
# -------------------------------------------------------------
pars <- c("alpha", "beta", "sigma")

# vemos resultados con print()

# combinamos draws() con mcmc_trace para gráfico de traza

# mcmc_acf para correlaciones en la simulacion

# diagnostic_summary() muestra otros diagnosticos específicos de HMC

# -------------------------------------------------------------
# 6) Resultados
# -------------------------------------------------------------
# simulaciones de posterior como tibble
post <- res$draws(pars, format = 'df') |> as_tibble()
head(post)

# histogramas posteriores, mcmc_hist o directo con post

# intervaloes mcmc_intervals(prob, prob.outer)

# amortiguamos el mercado? le ganamos al mercado?
post |>
  summarise(p_beta_mayor_1 = mean(beta > 1), p_alpha_mayor_0 = mean(alpha > 0))

# volatilidad total del  activo
# var(y) = beta^2*var(x) + sigma^2, es una función de los parámetros, h(theta)
# cual es el ratio de volatilidad respecto del mercado?
# que proporción de la volatilidad es explicada por el mercado?

sd_x <- sd(sypdt$mercado)


# -------------------------------------------------------------
# 7) Para hacer en casa (?)
# -------------------------------------------------------------

# escenarios: usar el bloque genrated quantities
# predicciones de retorno bajo escenarios de caida o subida del mercado
escenarios <- c(caida = -10, neutro = 0, suba = 5) # retorno del mercado (%)

# comparar con otros activos: repetir modelo con los demas activos y comparar

# diagnosticar el ajuste del modelo
