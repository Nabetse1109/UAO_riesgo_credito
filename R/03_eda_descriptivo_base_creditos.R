# ============================================================
# 03_eda_descriptivo_base_creditos.R
# Análisis descriptivo univariado, bivariado y multivariado
# sobre base_modelo_creditos (unidad = crédito)
# ============================================================

# A) Setup ----------------------------------------------------
source(here::here("R", "00_setup_eda.R"))   # paquetes, tema ggplot, formatear_tabla()

# Cargar base agregada para modelado
base_creditos <- readRDS(
  here::here("data", "processed", "base_modelo_creditos.rds")
)

cat("Dimensiones base_creditos:", paste(dim(base_creditos), collapse = " x "), "\n\n")

# Crear carpetas de salida si no existen
ruta_figuras <- here::here("outputs", "figuras", "03_eda_descriptivo")
if (!dir.exists(ruta_figuras)) dir.create(ruta_figuras, recursive = TRUE)

ruta_tablas  <- here::here("outputs", "tablas")
if (!dir.exists(ruta_tablas)) dir.create(ruta_tablas, recursive = TRUE)

# B) Definición de grupos de variables ------------------------

# Identificadoras (no se analizan como explicativas)
vars_id <- c("no_creditos", "id_estudiante")

# Numéricas relevantes
vars_num <- c(
  "estrato_socioeconomico",
  "valor_nota_debito",
  "valor_pagado_nota_debito",
  "saldo_nota_debito",
  "percent_recaudo",
  "cuotas",
  "saldo_cliente",
  "valor_financiacion",
  "valor_pagado",
  "n_registros_cartera",
  "max_dias_mora",
  "n_cuotas_vencidas",
  "saldo_total_cartera"
)

# Categóricas (sin incluir la variable objetivo)
vars_cat <- c(
  "departamento",
  "estado_estudiante",
  "primiparo",
  "graduado",
  "sexo",
  "linea_credito",
  "periodo",
  "genera_mora_flag"
)

# Variable objetivo
var_objetivo <- "incumplimiento"

# Verificación rápida de existencia de columnas
vars_num   <- intersect(vars_num,   names(base_creditos))
vars_cat   <- intersect(vars_cat,   names(base_creditos))
vars_id    <- intersect(vars_id,    names(base_creditos))
stopifnot(var_objetivo %in% names(base_creditos))

cat("Variables numéricas usadas:\n"); print(vars_num); cat("\n")
cat("Variables categóricas usadas:\n"); print(vars_cat); cat("\n")

# C) Funciones auxiliares para tablas y gráficos --------------

# 1) Tabla de frecuencias para categóricas
tabla_frecuencias <- function(df, var, caption = NULL, n_max = 20) {
  v <- rlang::sym(var)
  
  tab <- df %>%
    dplyr::count(!!v, name = "n") %>%
    dplyr::mutate(
      pct = 100 * n / sum(n)
    ) %>%
    dplyr::arrange(dplyr::desc(n))
  
  # Limitar a n_max categorías en la tabla (para variables con muchas categorías)
  if (nrow(tab) > n_max) {
    tab <- tab %>% dplyr::slice_max(n, n = n_max)
  }
  
  readr::write_csv(
    tab,
    here::here(ruta_tablas, paste0("03_freq_", var, ".csv"))
  )
  
  formatear_tabla(
    tab,
    caption = caption %||% paste("Distribución de", var),
    n = nrow(tab)
  )
}

# 2) Tabla descriptiva para numéricas (N, media, sd, p25, mediana, p75, min, max)
tabla_descriptiva_numerica <- function(df, vars) {
  df %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        list(
          n       = ~ sum(!is.na(.)),
          media   = ~ mean(., na.rm = TRUE),
          sd      = ~ stats::sd(., na.rm = TRUE),
          p25     = ~ stats::quantile(., 0.25, na.rm = TRUE),
          mediana = ~ stats::median(., na.rm = TRUE),
          p75     = ~ stats::quantile(., 0.75, na.rm = TRUE),
          min     = ~ min(., na.rm = TRUE),
          max     = ~ max(., na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("variable", "estadistico"),
      names_pattern = "^(.*)_(n|media|sd|p25|mediana|p75|min|max)$",
      values_to = "valor"
    ) %>%
    tidyr::pivot_wider(
      names_from = "estadistico",
      values_from = "valor"
    ) %>%
    dplyr::arrange(variable)
}

# 3) Histogramas/densidades para numéricas (univariado)
graficar_histograma <- function(df, var, titulo, archivo) {
  v <- rlang::sym(var)
  
  g <- ggplot(df, aes(x = !!v)) +
    geom_histogram(bins = 30) +
    labs(
      title = titulo,
      x = var,
      y = "Frecuencia"
    ) +
    theme_minimal()
  
  ggplot2::ggsave(
    filename = here::here(ruta_figuras, archivo),
    plot = g,
    width = 8,
    height = 4.5
  )
}

# 4) Boxplot vs. incumplimiento para numéricas
graficar_boxplot_incumplimiento <- function(df, var, archivo) {
  v <- rlang::sym(var)
  
  g <- ggplot(
    df,
    aes(
      x = factor(.data[[var_objetivo]], labels = c("Cumple", "Incumple")),
      y = !!v
    )
  ) +
    geom_boxplot(outlier.alpha = 0.3) +
    labs(
      title = paste("Distribución de", var, "según incumplimiento"),
      x = "Incumplimiento",
      y = var
    ) +
    theme_minimal()
  
  ggplot2::ggsave(
    filename = here::here(ruta_figuras, archivo),
    plot = g,
    width = 7,
    height = 4.5
  )
}

# 5) Barras de tasa de incumplimiento por categoría
graficar_barras_incumplimiento <- function(df, var, archivo, min_n = 50) {
  v <- rlang::sym(var)
  
  resumen <- df %>%
    dplyr::group_by(!!v) %>%
    dplyr::summarise(
      n = dplyr::n(),
      tasa_incumplimiento = mean(.data[[var_objetivo]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n >= min_n) %>%      # filtrar categorías con muy pocos casos
    dplyr::arrange(dplyr::desc(tasa_incumplimiento))
  
  if (nrow(resumen) == 0) return(invisible(NULL))
  
  g <- ggplot(
    resumen,
    aes(x = reorder(as.character(!!v), tasa_incumplimiento), y = tasa_incumplimiento)
  ) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste("Tasa de incumplimiento por", var),
      x = var,
      y = "Tasa de incumplimiento"
    ) +
    theme_minimal()
  
  ggplot2::ggsave(
    filename = here::here(ruta_figuras, archivo),
    plot = g,
    width = 8,
    height = 5
  )
  
  # Guardar tabla
  readr::write_csv(
    resumen,
    here::here(ruta_tablas, paste0("03_tasa_incumplimiento_", var, ".csv"))
  )
}

# ============================================================
# D) ANÁLISIS UNIVARIADO
# ============================================================

cat("=== EDA univariado: numéricas ===\n")

# 1) Tabla descriptiva de numéricas
desc_num <- tabla_descriptiva_numerica(base_creditos, vars_num)

readr::write_csv(
  desc_num,
  here::here(ruta_tablas, "03_descriptivas_numericas_base_creditos.csv")
)

formatear_tabla(
  desc_num,
  caption = "Descriptivas de variables numéricas – Base de créditos",
  n = nrow(desc_num)
)

# 2) Histogramas para numéricas clave
for (v in vars_num) {
  cat("  - Histograma de", v, "\n")
  graficar_histograma(
    base_creditos,
    v,
    titulo  = paste("Distribución de", v),
    archivo = paste0("03_hist_", v, ".png")
  )
}

cat("\n=== EDA univariado: categóricas ===\n")

# 3) Tablas de frecuencia y barras simples para categóricas
for (v in vars_cat) {
  cat("  - Tabla de frecuencias de", v, "\n")
  
  tabla_frecuencias(
    base_creditos,
    v,
    caption = paste("Distribución de", v),
    n_max = 25
  )
  
  # Gráfico de barras simple
  g <- base_creditos %>%
    dplyr::count(.data[[v]], name = "n") %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::slice_max(n, n = 25) %>%
    ggplot(aes(x = reorder(as.character(.data[[v]]), n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Frecuencia de", v),
      x = v,
      y = "Número de créditos"
    ) +
    theme_minimal()
  
  ggplot2::ggsave(
    filename = here::here(ruta_figuras, paste0("03_bar_freq_", v, ".png")),
    plot = g,
    width = 8,
    height = 5
  )
}

# ============================================================
# E) ANÁLISIS BIVARIADO VS. INCUMPLIMIENTO
# ============================================================

cat("\n=== EDA bivariado: numéricas vs. incumplimiento ===\n")

# 1) Boxplots por incumplimiento
for (v in vars_num) {
  cat("  - Boxplot de", v, "según incumplimiento\n")
  graficar_boxplot_incumplimiento(
    base_creditos,
    v,
    archivo = paste0("03_box_", v, "_por_incumplimiento.png")
  )
}

# 2) Tabla de resumen numérico por clase de incumplimiento
resumen_num_por_obj <- base_creditos %>%
  dplyr::group_by(!!rlang::sym(var_objetivo)) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_num),
      list(
        n       = ~ sum(!is.na(.)),
        media   = ~ mean(., na.rm = TRUE),
        mediana = ~ stats::median(., na.rm = TRUE),
        p25     = ~ stats::quantile(., 0.25, na.rm = TRUE),
        p75     = ~ stats::quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

readr::write_csv(
  resumen_num_por_obj,
  here::here(ruta_tablas, "03_resumen_numericas_por_incumplimiento.csv")
)

# 3) Categóricas: tasa de incumplimiento por categoría
cat("\n=== EDA bivariado: categóricas vs. incumplimiento ===\n")

for (v in vars_cat) {
  cat("  - Tasa de incumplimiento por", v, "\n")
  graficar_barras_incumplimiento(
    base_creditos,
    v,
    archivo = paste0("03_tasa_inc_", v, ".png"),
    min_n = 50
  )
}

# Tabla de distribución de la variable objetivo
tabla_obj <- base_creditos %>%
  dplyr::count(!!rlang::sym(var_objetivo), name = "n_creditos") %>%
  dplyr::mutate(pct = 100 * n_creditos / sum(n_creditos))

readr::write_csv(
  tabla_obj,
  here::here(ruta_tablas, "03_distribucion_incumplimiento.csv")
)

formatear_tabla(
  tabla_obj,
  caption = "Distribución de la variable objetivo (incumplimiento)",
  n = nrow(tabla_obj)
)

# ============================================================
# F) ANÁLISIS MULTIVARIADO – CORRELACIONES
# ============================================================

cat("\n=== EDA multivariado: correlaciones entre numéricas ===\n")

# Matriz de correlación (solo numéricas)
df_num <- base_creditos %>%
  dplyr::select(dplyr::all_of(vars_num)) %>%
  dplyr::select(where(is.numeric))

mat_cor <- stats::cor(df_num, use = "pairwise.complete.obs")

# Guardar matriz de correlación
readr::write_csv(
  as.data.frame(mat_cor),
  here::here(ruta_tablas, "03_matriz_correlacion_numericas.csv")
)

# Mapa de calor de correlación
cor_long <- as.data.frame(as.table(mat_cor))
names(cor_long) <- c("var1", "var2", "correlacion")

g_cor <- ggplot(cor_long, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile() +
  scale_fill_gradient2(
    limits = c(-1, 1),
    oob = scales::squish
  ) +
  labs(
    title = "Matriz de correlación – variables numéricas",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

ggplot2::ggsave(
  filename = here::here(ruta_figuras, "03_mapa_calor_correlacion_numericas.png"),
  plot = g_cor,
  width = 9,
  height = 7
)

cat("\n===== Fin de 03_eda_descriptivo_base_creditos.R =====\n")
