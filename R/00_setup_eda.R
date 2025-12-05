# ======================================================
# 00_setup_eda.R
# Fase: Carga inicial + revisión de estructura (NADA más)
# ======================================================

# ------------------------------------------------------
# A) Paquetes necesarios
# ------------------------------------------------------

paquetes <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "skimr",
  "here",
  "knitr",
  "kableExtra"
)

paquetes_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

invisible(lapply(paquetes, library, character.only = TRUE))

theme_set(theme_minimal())


# =============================================================
# B) Función de formato de tablas (para informes RMarkdown)
# =============================================================

formatear_tabla <- function(df, caption = NULL, n = 30) {
  
  df_formateada <- df %>%
    mutate(
      across(
        where(is.numeric),
        ~ format(round(.), big.mark = ",", decimal.mark = ".", trim = TRUE)
      )
    )
  
  df_formateada %>%
    head(n) %>%
    knitr::kable(
      caption  = caption,
      booktabs = TRUE,
      align    = "c"
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE
    ) %>%
    kableExtra::row_spec(0, background = "#003366", color = "white")
}


# ------------------------------------------------------
# C) Verificación de carpetas del proyecto
# ------------------------------------------------------

carpetas <- c(
  "data",
  "data/raw",
  "data/processed",
  "outputs",
  "outputs/tablas"
)

invisible(lapply(carpetas, dir.create, recursive = TRUE, showWarnings = FALSE))


# ------------------------------------------------------
# D) Rutas de los archivos
# ------------------------------------------------------

ruta_creditos <- here("data", "raw", "1_Creditos_EstudiantesREV.xlsx")
ruta_cartera  <- here("data", "raw", "2_Cartera_depuradaREV.xlsx")

if (!file.exists(ruta_creditos)) stop("ERROR: No se encontró el archivo de créditos.")
if (!file.exists(ruta_cartera)) stop("ERROR: No se encontró el archivo de cartera.")


# ------------------------------------------------------
# E) CARGA DE DATOS (sin transformaciones)
# ------------------------------------------------------

creditos_raw <- read_excel(ruta_creditos) %>% clean_names()
cartera_raw  <- read_excel(ruta_cartera)  %>% clean_names()

# Corrección de tipos de datos
creditos_raw$linea_credito <- as.character(creditos_raw$linea_credito)
cartera_raw$linea_credito  <- as.character(cartera_raw$linea_credito)

saveRDS(creditos_raw, here("data", "processed", "creditos_raw.rds"))
saveRDS(cartera_raw,  here("data", "processed", "cartera_raw.rds"))


# ------------------------------------------------------
# F) Revisión inicial solicitada:
#  - número de filas y columnas
#  - estructura
#  - tipos de datos
# ------------------------------------------------------

mensaje_resumen <- function(df, nombre) {
  cat("\n=============================================\n")
  cat("Dataset:", nombre, "\n")
  cat("Filas   :", nrow(df), "\n")
  cat("Columnas:", ncol(df), "\n")
  cat("=============================================\n\n")
}

mensaje_resumen(creditos_raw, "creditos_raw")
glimpse(creditos_raw)

mensaje_resumen(cartera_raw, "cartera_raw")
glimpse(cartera_raw)


# ------------------------------------------------------
# G) Resumen descriptivo de tipos de datos
# ------------------------------------------------------

skim(creditos_raw)
skim(cartera_raw)

# =============================================================
# H) Tabla descriptiva
# =============================================================

tabla_descriptiva <- function(df) {
  
  numeric_vars <- df %>% select(where(is.numeric))
  categorical_vars <- df %>% select(where(~ is.character(.) | is.factor(.)))
  
  # --- Descriptivas numéricas ---
  desc_num <- numeric_vars %>%
    gather(var, value) %>%
    group_by(var) %>%
    summarise(
      n           = sum(!is.na(value)),
      media       = mean(value, na.rm = TRUE),
      mediana     = median(value, na.rm = TRUE),
      sd          = sd(value, na.rm = TRUE),
      p25         = quantile(value, 0.25, na.rm = TRUE),
      p75         = quantile(value, 0.75, na.rm = TRUE),
      minimo      = min(value, na.rm = TRUE),
      maximo      = max(value, na.rm = TRUE)
    )
  
  # --- Descriptivas categóricas ---
  desc_cat <- categorical_vars %>%
    gather(var, value) %>%
    group_by(var) %>%
    summarise(
      n              = sum(!is.na(value)),
      n_categorias   = n_distinct(value),
      moda           = names(sort(table(value), decreasing = TRUE))[1],
      freq_moda      = max(table(value))
    )
  
  list(
    numericas   = desc_num,
    categoricas = desc_cat
  )
}



