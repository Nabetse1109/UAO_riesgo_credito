# ================================================================
# 01a_eda_temporal.R
# Análisis temporal de créditos UAO (2002–2025)
# Proyecto: GESTIÓN DEL RIESGO Y COBRANZA MEDIANTE MODELOS PREDICTIVOS
# Autores: Paula Andrea Tovar, Edgar Esteban Grajales C.
# ================================================================

# ------------------------------------------------
# A) Setup: paquetes, rutas y funciones comunes
# ------------------------------------------------
# Carga 00_setup_eda.R, donde ya están:
# - Paquetes (tidyverse, lubridate, here, etc.)
# - Funciones: formatear_tabla(), mensaje_resumen(), etc.

source("R/00_setup_eda.R")

cat("========== Inicio de 01a_eda_temporal.R ==========\n\n")

# ------------------------------------------------
# B) Carga de datos procesados
# ------------------------------------------------

creditos_raw <- readRDS(here::here("data", "processed", "creditos_raw.rds"))
cartera_raw  <- readRDS(here::here("data", "processed", "cartera_raw.rds"))

mensaje_resumen(creditos_raw, "creditos_raw")
mensaje_resumen(cartera_raw,  "cartera_raw")

# ------------------------------------------------
# C) Creación de variables temporales clave
# ------------------------------------------------

creditos_raw <- creditos_raw %>%
  dplyr::mutate(
    anio_aprobacion = lubridate::year(fecha_aprobacion),
    anio_vencimiento = lubridate::year(fecha_vencimiento_ndb),
    periodo_covid = dplyr::case_when(
      anio_aprobacion < 2020 ~ "Pre-COVID",
      anio_aprobacion >= 2020 & anio_aprobacion <= 2022 ~ "COVID",
      anio_aprobacion > 2022 ~ "Post-COVID",
      TRUE ~ NA_character_
    )
  )

# Filtramos años válidos por seguridad
creditos_raw <- creditos_raw %>%
  dplyr::filter(!is.na(anio_aprobacion),
                anio_aprobacion >= 2002,
                anio_aprobacion <= 2025)

# ------------------------------------------------
# D) Evolución anual de créditos, montos y saldos
# ------------------------------------------------

creditos_por_anio <- creditos_raw %>%
  dplyr::group_by(anio_aprobacion) %>%
  dplyr::summarise(
    n_creditos      = dplyr::n_distinct(no_creditos),
    monto_promedio  = mean(valor_nota_debito, na.rm = TRUE),
    saldo_promedio  = mean(saldo_nota_debito, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(anio_aprobacion)

# Tabla formateada
formatear_tabla(
  creditos_por_anio,
  "Créditos ÚNICOS otorgados, monto y saldo promedio por año"
)

# Guardar tabla
readr::write_csv(
  creditos_por_anio,
  here::here("outputs", "tablas", "creditos_por_anio.csv")
)

# Gráfico: número de créditos por año
g_creditos_anio <- creditos_por_anio %>%
  ggplot2::ggplot(ggplot2::aes(x = anio_aprobacion, y = n_creditos)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(
    title = "Evolución del número de créditos ÚNICOS otorgados",
    x     = "Año de aprobación",
    y     = "Número de créditos únicos"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("outputs", "figuras", "creditos_unicos_por_anio.png"),
  plot     = g_creditos_anio,
  width    = 9, height = 5
)

# Gráfico: monto promedio por año
g_monto_promedio <- creditos_por_anio %>%
  ggplot2::ggplot(ggplot2::aes(x = anio_aprobacion, y = monto_promedio)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Evolución del monto promedio de créditos",
    x     = "Año de aprobación",
    y     = "Monto promedio del crédito"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("outputs", "figuras", "monto_promedio_creditos_por_anio.png"),
  plot     = g_monto_promedio,
  width    = 9, height = 5
)

# ------------------------------------------------
# E) Estacionalidad por periodo académico
# ------------------------------------------------

creditos_por_periodo <- creditos_raw %>%
  dplyr::count(periodo, name = "n_creditos") %>%
  dplyr::arrange(dplyr::desc(n_creditos))

# Tabla (top 20 periodos)
formatear_tabla(
  creditos_por_periodo %>% dplyr::slice_head(n = 20),
  "Top 20 periodos académicos con más créditos"
)

readr::write_csv(
  creditos_por_periodo,
  here::here("outputs", "tablas", "creditos_por_periodo.csv")
)

g_creditos_periodo <- creditos_por_periodo %>%
  dplyr::slice_head(n = 20) %>%
  ggplot2::ggplot(ggplot2::aes(x = reorder(periodo, n_creditos), y = n_creditos)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Top 20 periodos académicos con más créditos",
    x     = "Periodo académico",
    y     = "Número de créditos"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("outputs", "figuras", "top20_periodos_creditos.png"),
  plot     = g_creditos_periodo,
  width    = 8, height = 6
)

# ------------------------------------------------
# F) Comparativo Pre-COVID / COVID / Post-COVID
# ------------------------------------------------

resumen_covid <- creditos_raw %>%
  dplyr::filter(!is.na(periodo_covid)) %>%
  dplyr::group_by(periodo_covid) %>%
  dplyr::summarise(
    n_creditos     = dplyr::n_distinct(no_creditos),
    monto_promedio = mean(valor_nota_debito, na.rm = TRUE),
    saldo_promedio = mean(saldo_nota_debito, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(periodo_covid)

formatear_tabla(
  resumen_covid,
  "Resumen por periodo: Pre-COVID, COVID y Post-COVID"
)

readr::write_csv(
  resumen_covid,
  here::here("outputs", "tablas", "resumen_covid_creditos.csv")
)

g_resumen_covid <- resumen_covid %>%
  ggplot2::ggplot(ggplot2::aes(x = periodo_covid, y = n_creditos)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Créditos ÚNICOS por periodo: Pre-COVID, COVID y Post-COVID",
    x     = "Periodo",
    y     = "Número de créditos únicos"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("outputs", "figuras", "creditos_por_periodo_covid.png"),
  plot     = g_resumen_covid,
  width    = 7, height = 5
)

# ------------------------------------------------
# G) Definición de conjuntos de Entrenamiento / Validación
#      Opción A: Entrenamiento ≤ 2022, Validación 2023–2025
# ------------------------------------------------

tabla_corte <- creditos_por_anio %>%
  dplyr::mutate(
    conjunto = dplyr::if_else(
      anio_aprobacion <= 2022,
      "Entrenamiento (≤ 2022)",
      "Validación (2023–2025)"
    )
  ) %>%
  dplyr::group_by(conjunto) %>%
  dplyr::summarise(
    anio_min       = min(anio_aprobacion),
    anio_max       = max(anio_aprobacion),
    n_creditos     = sum(n_creditos),
    monto_promedio = mean(monto_promedio, na.rm = TRUE),
    saldo_promedio = mean(saldo_promedio, na.rm = TRUE),
    .groups = "drop"
  )

formatear_tabla(
  tabla_corte,
  "Definición de conjuntos: Entrenamiento vs Validación"
)

readr::write_csv(
  tabla_corte,
  here::here("outputs", "tablas", "conjuntos_entrenamiento_validacion.csv")
)

cat("\nConjunto de ENTRENAMIENTO: años 2002–2022\n")
cat("Conjunto de VALIDACIÓN : años 2023–2025\n\n")

cat("========== Fin de 01a_eda_temporal.R ==========\n")
