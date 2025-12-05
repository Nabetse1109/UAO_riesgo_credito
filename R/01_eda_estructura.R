# ======================================================================
# 01_eda_estructura.R
# ANÁLISIS ESTRUCTURAL DE DATOS – PROYECTO RIESGO CREDITICIO UAO
# ======================================================================

# A) Setup general (carga de paquetes y funciones auxiliares)
source(here::here("R", "00_setup_eda.R"))
library(lubridate)   # Paquete adicional no incluido en 00_setup_eda.R

cat("=========== Inicio de 01_eda_estructura.R ===========\n")

# ----------------------------------------------------------------------
# B) Cargar datos procesados
# ----------------------------------------------------------------------
creditos_raw <- readRDS(here("data", "processed", "creditos_raw.rds"))
cartera_raw  <- readRDS(here("data", "processed", "cartera_raw.rds"))

if (!dir.exists(here("outputs", "tablas"))) {
  dir.create(here("outputs", "tablas"), recursive = TRUE)
}

# ----------------------------------------------------------------------
# C) Resumen general
# ----------------------------------------------------------------------
mensaje_resumen(creditos_raw, "creditos_raw")
mensaje_resumen(cartera_raw, "cartera_raw")

# ----------------------------------------------------------------------
# D) Diccionario de variables
# ----------------------------------------------------------------------
dicc_creditos <- generar_diccionario(creditos_raw, "creditos_raw")
dicc_cartera  <- generar_diccionario(cartera_raw, "cartera_raw")

formatear_tabla(dicc_creditos, "Diccionario – Créditos", n = 30)
formatear_tabla(dicc_cartera,  "Diccionario – Cartera",  n = 30)

write_csv(dicc_creditos, here("outputs", "tablas", "diccionario_creditos.csv"))
write_csv(dicc_cartera,  here("outputs", "tablas", "diccionario_cartera.csv"))

# ----------------------------------------------------------------------
# E) Análisis de duplicados completos
# ----------------------------------------------------------------------
duplicados_creditos <- creditos_raw %>% 
  group_by(across(everything())) %>% 
  filter(n() > 1) %>% 
  ungroup()

duplicados_cartera <- cartera_raw %>% 
  group_by(across(everything())) %>% 
  filter(n() > 1) %>% 
  ungroup()

cat("Duplicados completos en créditos:", nrow(duplicados_creditos), "\n")
cat("Duplicados completos en cartera :", nrow(duplicados_cartera),  "\n")

write_csv(duplicados_creditos, here("outputs", "tablas", "duplicados_creditos.csv"))
write_csv(duplicados_cartera,  here("outputs", "tablas", "duplicados_cartera.csv"))

# ----------------------------------------------------------------------
# F) Créditos por número de registros (transacciones)
# ----------------------------------------------------------------------
creditos_por_credito <- creditos_raw %>%
  count(no_creditos, name = "n_registros") %>%
  arrange(desc(n_registros))

cartera_por_credito <- cartera_raw %>%
  count(no_creditos, name = "n_registros") %>%
  arrange(desc(n_registros))

formatear_tabla(creditos_por_credito %>% head(20),
                "Top 20 créditos con más registros – Créditos")

formatear_tabla(cartera_por_credito %>% head(20),
                "Top 20 créditos con más registros – Cartera")

write_csv(creditos_por_credito, here("outputs", "tablas", "creditos_por_credito.csv"))
write_csv(cartera_por_credito,  here("outputs", "tablas", "cartera_por_credito.csv"))

# ----------------------------------------------------------------------
# G) Análisis temporal básico
# ----------------------------------------------------------------------
creditos_por_anio <- creditos_raw %>%
  distinct(no_creditos, fecha_aprobacion) %>%
  mutate(anio_aprobacion = year(fecha_aprobacion)) %>%
  count(anio_aprobacion, name = "n_creditos")

formatear_tabla(creditos_por_anio,
                "Distribución de créditos ÚNICOS por año de aprobación")

write_csv(creditos_por_anio,
          here("outputs", "tablas", "creditos_unicos_por_anio.csv"))

# ----------------------------------------------------------------------
# H) Créditos otorgados durante COVID
# ----------------------------------------------------------------------
creditos_covid <- creditos_por_anio %>%
  filter(anio_aprobacion %in% c(2020, 2021, 2022))

total_creditos_covid <- sum(creditos_covid$n_creditos)

cat("Créditos ÚNICOS otorgados durante COVID-19:", total_creditos_covid, "\n")

formatear_tabla(creditos_covid,
                "Créditos ÚNICOS otorgados durante COVID-19 por año")

write_csv(creditos_covid,
          here("outputs", "tablas", "creditos_unicos_covid_por_anio.csv"))

# =============================================================
# I) Tablas descriptivas 
# =============================================================

desc_creditos <- tabla_descriptiva(creditos_raw)
desc_cartera  <- tabla_descriptiva(cartera_raw)

# Mostrar descriptivas numéricas
formatear_tabla(
  desc_creditos$numericas,
  "Descriptivas numéricas – Créditos"
)

formatear_tabla(
  desc_cartera$numericas,
  "Descriptivas numéricas – Cartera"
)

# Mostrar descriptivas categóricas
formatear_tabla(
  desc_creditos$categoricas,
  "Descriptivas categóricas – Créditos"
)

formatear_tabla(
  desc_cartera$categoricas,
  "Descriptivas categóricas – Cartera"
)


# ----------------------------------------------------------------------
# J) Créditos otorgados, créditos en mora y % de mora por año
# ----------------------------------------------------------------------

mora_por_credito <- cartera_raw %>%
  group_by(no_creditos) %>%
  summarise(
    entro_mora = any(dias > 30 & !is.na(dias)),
    .groups = "drop"
  )

creditos_mora_anio <- creditos_raw %>%
  distinct(no_creditos, fecha_aprobacion) %>%
  mutate(anio_aprobacion = year(fecha_aprobacion)) %>%
  left_join(mora_por_credito, by = "no_creditos") %>%
  mutate(entro_mora = if_else(is.na(entro_mora), FALSE, entro_mora)) %>%
  group_by(anio_aprobacion) %>%
  summarise(
    n_creditos      = n(),
    n_creditos_mora = sum(entro_mora),
    pct_mora        = round(100 * n_creditos_mora / n_creditos, 2),
    .groups = "drop"
  ) %>%
  arrange(anio_aprobacion)

formatear_tabla(
  creditos_mora_anio,
  "Créditos otorgados y créditos en mora (>30 días) por año",
  n = 30  
)


write_csv(creditos_mora_anio,
          here("outputs", "tablas", "creditos_mora_por_anio.csv"))



# ============================================================
# K) Gráficos descriptivos univariados
#    - Histogramas para variables numéricas
#    - Barras para variables categóricas
#    (Guardados en outputs/figuras)
# ============================================================

# --- 1. Función para histogramas numéricos ------------------

graficar_histograma <- function(df, variable, nombre_df, bins = 40) {
  
  # Extraer la columna como vector numérico
  valores <- df %>% dplyr::pull(variable)
  
  # Eliminar NA
  valores <- valores[!is.na(valores)]
  
  # Construir tibble limpio para graficar
  df_plot <- tibble::tibble(valor = valores)
  
  # Gráfico
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = valor)) +
    ggplot2::geom_histogram(bins = bins, fill = "#2c3e50", color = "white") +
    ggplot2::labs(
      title = paste("Distribución de", variable, "-", nombre_df),
      x = variable,
      y = "Frecuencia"
    ) +
    ggplot2::theme_minimal()
  
  print(p)
  
  # Guardar imagen
  ggplot2::ggsave(
    filename = here::here("outputs", "figuras",
                          paste0("hist_", nombre_df, "_", variable, ".png")),
    plot   = p,
    width  = 7,
    height = 4,
    dpi    = 300
  )
}

# --- 2. Función para barras de categóricas ------------------

graficar_barras <- function(df, variable, nombre_df, top_n = 10) {
  
  # Si la variable no existe en el df, la omitimos
  if (!variable %in% names(df)) {
    message("Variable '", variable, "' no existe en ", nombre_df, " — se omite.")
    return(invisible(NULL))
  }
  
  # Extraer columna como vector
  valores <- df %>% dplyr::pull(variable)
  valores <- valores[!is.na(valores)]
  
  # Tabla de frecuencias (Top N)
  df_plot <- tibble::tibble(categoria = valores) %>%
    dplyr::count(categoria, sort = TRUE) %>%
    dplyr::slice(1:top_n) %>%
    dplyr::mutate(categoria = stats::reorder(categoria, n))
  
  # Gráfico
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = categoria, y = n)) +
    ggplot2::geom_col(fill = "#2980b9") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", top_n, "categorías de", variable, "-", nombre_df),
      x = variable,
      y = "Frecuencia"
    ) +
    ggplot2::theme_minimal()
  
  print(p)
  
  # Guardar imagen
  ggplot2::ggsave(
    filename = here::here("outputs", "figuras",
                          paste0("bar_", nombre_df, "_", variable, ".png")),
    plot   = p,
    width  = 7,
    height = 4,
    dpi    = 300
  )
}

# --- 3. Listas de variables a graficar ----------------------

# Numéricas de créditos
vars_num_creditos <- c(
  "valor_nota_debito",
  "valor_pagado_nota_debito",
  "saldo_nota_debito",
  "percent_recaudo",
  "cuotas"
)

# Numéricas de cartera
vars_num_cartera <- c(
  "saldo",
  "valor_total",
  "valor_afectado",
  "dias",
  "no_creditos",
  "cuota"
)

# Categóricas de créditos
vars_cat_creditos <- c(
  "sexo",
  "estrato_socioeconomico",
  "departamento",
  "estado_estudiante",
  "linea_credito",
  "periodo"
)

# Categóricas de cartera (SIN 'rango')
vars_cat_cartera <- c(
  "edad_cartera",
  "tipo_cartera",
  "corto_largo_plazo",
  "activa_inactiva",
  "linea_credito",
  "genera_mora",
  "tasa_interes_mora",
  "periodo"
)

# --- 4. Ejecutar gráficos: Créditos -------------------------

# Histogramas numéricos - Créditos
for (v in vars_num_creditos) {
  graficar_histograma(creditos_raw, v, "Creditos")
}

# Barras categóricas - Créditos
for (v in vars_cat_creditos) {
  graficar_barras(creditos_raw, v, "Creditos", top_n = 10)
}

# --- 5. Ejecutar gráficos: Cartera --------------------------

# Histogramas numéricos - Cartera
for (v in vars_num_cartera) {
  graficar_histograma(cartera_raw, v, "Cartera")
}

# Barras categóricas - Cartera
for (v in vars_cat_cartera) {
  graficar_barras(cartera_raw, v, "Cartera", top_n = 10)
}

cat("\n===== Gráficos descriptivos generados y guardados en outputs/figuras =====\n")

# Guardar diccionarios para usarlos en el siguiente script

readr::write_csv(dicc_creditos, here::here("outputs", "tablas", "dicc_creditos.csv"))
readr::write_csv(dicc_cartera, here::here("outputs", "tablas", "dicc_cartera.csv"))

cat("=========== Fin de 01_eda_estructura.R ===========\n")