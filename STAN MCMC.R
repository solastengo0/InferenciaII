### Fuente: libro de Alicia A. Johnson, Miles Q. Ott y Mine Dogucu, Bayes Rules!, Texts in Statistical Science, Chapman & Hall/CRC, 2022

rm(list=ls())

##########
library(tidyverse)
library(rstan)  # Interfaz para el software Stan 
library(bayesplot)  # Gráficos para visualización bayesiana 

# Técnica de aproximación de la distribución a posteriori que, mediante simulación, produce una cadena de N valores dependientes de θ, {θ(1),θ(2),…,θ(N)}, que no son generados a partir de la función de densidad a posteriori f(θ|y)).

# Ejemplo Beta-Binomial
# 
# PASO 1: Definición de la estructura del modelo bayesiano
bb_model <- "
  data {
    int<lower = 0, upper = 10> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(10, pi);
    pi ~ beta(2, 2);
  }
"
# PASO 2: Simulación de la distribución a posteriori con la función stan() del paquete R 'rstan'
bb_sim <- stan(model_code = bb_model, data = list(Y = 9), chains = 4, iter = 5000*2, seed = 84735)

# Visualización de los primeros cuatro valores de las realizaciones de cada una de las cuatro cadenas
as.array(bb_sim, pars = "pi") %>% 
  head(4)

# Extraer los primeros 20 elementos de chain:1 y chain:3
chain_1 <- as.array(bb_sim, pars = "pi")[1:20, 1, 1]
chain_3 <- as.array(bb_sim, pars = "pi")[1:20, 3, 1]

library(ggplot2)
library(gridExtra)

# Gráfico para chain:1
plot1 <- ggplot(data.frame(x = 1:20, y = chain_1), aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Primeros 20 elementos de chain:1",
       x = "iteración", y = expression("valor de " * pi)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))

# Gráfico para chain:3
plot2 <- ggplot(data.frame(x = 1:20, y = chain_3), aes(x = x, y = y)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Primeros 20 elementos de chain:3",
       x = "iteración", y = expression("valor de " * pi)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))

# Combinar los gráficos
grid.arrange(plot1, plot2, ncol = 2)

# Extraer todas las cadenas para el parámetro "pi"
cadenas <- rstan::extract(bb_sim, pars = "pi")$pi

# Verificar la estructura de cadenas
str(cadenas)

# Primeros 20 valores
primeros_20 <- cadenas[1:20]

# Primeros 200 valores
primeros_200 <- cadenas[1:200]

# Gráfico de los primeros 20 valores
grafico_20 <- ggplot(data.frame(x = 1:20, y = primeros_20), aes(x = x, y = y)) +
  geom_line() +
  geom_point() +
  labs(title = "Primeros 20 valores de la cadena 1 \nluego de burn-in",
       x = "iteración", y = expression("valor de " * pi)) +
  theme_minimal()

# Gráfico de los primeros 200 valores
grafico_200 <- ggplot(data.frame(x = 1:200, y = primeros_200), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Primeros 200 valores de la cadena 1 \nluego de burn-in",
       x = "iteración", y = expression("valor de " * pi)) +
  theme_minimal()

# Mostrar los gráficos
print(grafico_20)
print(grafico_200)

# Trazas de las cuatro cadenas de Markov para pi
mcmc_trace(bb_sim, pars = "pi", size = 0.1)

# Extraer el array a posteriori de pi
samples_array <- as.array(bb_sim, pars = "pi")

# Extraes para cada cadena el vector de muestras (todas las iteraciones)
chain_1 <- samples_array[, 1, 1]  # Cadena 1
chain_2 <- samples_array[, 2, 1]  # Cadena 2
chain_3 <- samples_array[, 3, 1]  # Cadena 3
chain_4 <- samples_array[, 4, 1]  # Cadena 4

# Función para crear el gráfico de una cadena
grafico_cadena <- function(valores, titulo) {
  ggplot(data.frame(iteracion = 1:length(valores), pi = valores),
         aes(x = iteracion, y = pi)) +
    geom_line(color = "royalblue") +
    labs(title = titulo, x = "iteración", y = expression("valor de " * pi)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) # Centrar el título
}

# Crear los gráficos para cada cadena
plot1 <- grafico_cadena(chain_1, "Cadena 1")
plot2 <- grafico_cadena(chain_2, "Cadena 2")
plot3 <- grafico_cadena(chain_3, "Cadena 3")
plot4 <- grafico_cadena(chain_4, "Cadena 4")

# Combinar los cuatro gráficos en un panel
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Histograma de los 20000 valores de la cadena de Markov
mcmc_hist(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + xlab(expression(pi)) +
  ylab("frecuencia")

# Estimación de densidad de los 20000 valores de la cadena de Markov y densidad de la probabilidad de la distribución objetivo Beta(11,3)
mcmc_dens(bb_sim, pars = "pi") +
  yaxis_text(TRUE) +
  xlab(expression(pi)) +
  ylab("densidad") +
  stat_function(
    fun = dbeta,
    args = list(shape1 = 11, shape2 = 3),
    linewidth = 1) 

# De manera alternativa:
# Gráfico de la estimación de densidad de los valores MCMC del parámetro pi, superpuesto con la función de densidad de la probabilidad de la distribución objetivo Beta(11,3), 
# Esto permite comparar visualmente la aproximación MCMC de la distribución a posteriori con la distribución a posteriori exacta (conocida en este caso).

chain_all_pi <- c(chain_1, chain_2, chain_3, chain_4)

# Data.frame de la muestra MCMC
df <- data.frame(pi = chain_all_pi )

ggplot(df, aes(x = pi)) +
  geom_density(
    aes(color = "Aproximación MCMC", linetype = "Aproximación MCMC"),
    fill = "skyblue",
    alpha = 0.6, 
    linewidth = 1
  ) +
  stat_function(
    fun = dbeta,
    args = list(shape1 = 11, shape2 = 3),
    aes(color = "Distribución Beta(11,3)", linetype = "Distribución Beta(11,3)"),
    linewidth = 1
  ) +
  xlab(expression(pi)) +
  ylab("densidad") +
  scale_color_manual(
    values = c("Aproximación MCMC" = "black", "Distribución Beta(11,3)" = "red"),
    labels = c("Aproximación MCMC", "Distribución Beta(11,3)")
  ) +
  scale_linetype_manual(
    values = c("Aproximación MCMC" = "solid", "Distribución Beta(11,3)" = "dashed"),
    labels = c("Aproximación MCMC", "Distribución Beta(11,3)")
  ) +
  guides(
    color = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL)
  ) +
  labs(
    title = "Comparación entre la aproximación MCMC \nde la densidad a posteriori de \u03C0 \ny la distribución Beta(11,3)"
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 10)
  )

# Generamos los gráficos de densidad para cada una de las cadenas individuales de la variable pi, superponiéndolos para comparar visualmente si las cadenas están aproximando la misma distribución a posteriori.
mcmc_dens_overlay(bb_sim, pars = "pi") + 
  ylab("density")

# Simulación para aproximar la posteriori en un modelo beta-binomial usando muy pocas iteraciones, lo que sirve para ilustrar el impacto del tamaño muestral pequeño en la convergencia y estabilidad de las cadenas.
bb_sim_short <- stan(model_code = bb_model, data = list(Y = 9), chains = 4, iter = 50*2, seed = 84735)

# Mostramos las trazas de las cadenas cortas, permitiendo ver cómo las cadenas exploran el espacio de parámetros con tan pocos pasos y evidenciando una mayor variabilidad o menor estabilidad que en cadenas largas.
mcmc_trace(bb_sim_short, pars = "pi")

# Graficamos la densidad de cada cadena cuando se usan pocas iteraciones, mostrando que en este escenario las cadenas pueden diferir bastante entre sí.
mcmc_dens_overlay(bb_sim_short, pars = "pi")

# Calculamos la razón del tamaño efectivo de muestra (effective sample size ratio) para el parámetro pi, que indica qué tan independientes son las muestras generadas por la cadena de Markov. Valores bajos sugieren alta autocorrelación y menor eficiencia.
neff_ratio(bb_sim, pars = c("pi"))

# Volvemos a mostrar las trazas de las cuatro cadenas simuladas con 5000 realizaciones cada una.
mcmc_trace(bb_sim, pars = "pi")

# Visualizamos la función de autocorrelación para pi en las cadenas simuladas, lo que ayuda a diagnosticar cuán independientes son las muestras sucesivas y si hace falta aplicar afinamiento ("thinning").
mcmc_acf(bb_sim, pars = "pi")

# Realizamos una simulación para aproximar la posteriori pero guardando solo una de cada diez muestras (afinamiento, thinning), con el objetivo de reducir la autocorrelación y obtener muestras más independientes.
# Simulamos una muestra MCMC afinada
thinned_sim <- stan(model_code = bb_model, data = list(Y = 9), chains = 4, iter = 5000*2, seed = 84735, thin = 10)

# Mostramos las trazas de las cadenas afinadas con 500 realizaciones cada una: menor correlación y mayor dispersión entre iteraciones consecutivas.
mcmc_trace(thinned_sim, pars = "pi")

# Extraer el array a posteriori de pi
thinned_array <- as.array(thinned_sim, pars = "pi")

# Para cada cadena, extrae el vector de muestras (todas las iteraciones)
chain_thin1 <- thinned_array[, 1, 1]  # Cadena 1
chain_thin2 <- thinned_array[, 2, 1]  # Cadena 2
chain_thin3 <- thinned_array[, 3, 1]  # Cadena 3
chain_thin4 <- thinned_array[, 4, 1]  # Cadena 4

# Crear los gráficos para cada cadena afinada
plotthin1 <- grafico_cadena(chain_thin1, "Cadena 1")
plotthin2 <- grafico_cadena(chain_thin2, "Cadena 2")
plotthin3 <- grafico_cadena(chain_thin3, "Cadena 3")
plotthin4 <- grafico_cadena(chain_thin4, "Cadena 4")

# Combinar los cuatro gráficos en un panel
grid.arrange(plotthin1, plotthin2, plotthin3, plotthin4, ncol = 2, nrow = 2)

# Graficamos la autocorrelación de las cadenas afinadas, mostrando una caída más rápida de la dependencia entre muestras gracias al afinamiento.
mcmc_acf(thinned_sim, pars = "pi")

# Calculamos el estadístico R-hat (diagnóstico de Gelman-Rubin) para el parámetro pi, fundamental para evaluar la convergencia entre cadenas: valores cercanos a 1 indican convergencia adecuada.
rhat(bb_sim, pars = "pi")
#####