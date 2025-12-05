# ============================================================
# 02_preparacion_datos.R
# Validaciones, limpieza e integración de datos
# ============================================================

# A) Setup ---------------------------------------------------
source(here::here("R", "00_setup_eda.R"))  # paquetes, opciones, formato tablas

# Funciones auxiliares robustas --------------------------------
sum_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

max_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

# B) Cargar datos procesados (salida de 01_eda_estructura.R) ---
creditos_raw <- readRDS(here::here("data", "processed", "creditos_raw.rds"))
cartera_raw  <- readRDS(here::here("data", "processed", "cartera_raw.rds"))

cat("Dimensiones creditos_raw:", paste(dim(creditos_raw), collapse = " x "), "\n")
cat("Dimensiones cartera_raw :", paste(dim(cartera_raw),  collapse = " x "), "\n\n")

# ============================================================
# C) VALIDACIONES DE NEGOCIO
# ============================================================

validaciones <- list()

# 1) saldo_nota_debito <= valor_nota_debito
validaciones$saldo_mayor_que_valor <- creditos_raw %>%
  dplyr::filter(saldo_nota_debito > valor_nota_debito) %>%
  nrow()

# 2) valor_pagado_nota_debito <= valor_nota_debito
validaciones$pago_mayor_que_valor <- creditos_raw %>%
  dplyr::filter(valor_pagado_nota_debito > valor_nota_debito) %>%
  nrow()

# 3) percent_recaudo en rango razonable (0–120%)
validaciones$percent_fuera_rango <- creditos_raw %>%
  dplyr::filter(
    !is.na(percent_recaudo),
    percent_recaudo < 0 | percent_recaudo > 120
  ) %>%
  nrow()

# 4) fechas invertidas: aprobación > vencimiento
validaciones$fechas_invertidas <- creditos_raw %>%
  dplyr::filter(fecha_aprobacion > fecha_vencimiento_ndb) %>%
  nrow()

# 5) fechas futuras improbables
validaciones$fechas_futuras <- creditos_raw %>%
  dplyr::filter(
    lubridate::year(fecha_aprobacion)      > 2025 |
      lubridate::year(fecha_vencimiento_ndb) > 2030
  ) %>%
  nrow()

cat("== Resumen validaciones de negocio ==\n")
print(validaciones)
cat("\n")

# Guardar tabla de validaciones
validaciones_df <- tibble::enframe(validaciones, name = "validacion", value = "n_errores")

readr::write_csv(
  validaciones_df,
  here::here("outputs", "tablas", "02_reporte_validaciones.csv")
)

# ============================================================
# D) LIMPIEZA BÁSICA
# ============================================================

creditos_clean <- creditos_raw %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    estrato_socioeconomico = dplyr::na_if(estrato_socioeconomico, 0)
  ) %>%
  dplyr::filter(
    dplyr::between(lubridate::year(fecha_aprobacion), 2003, 2025)
  )

cartera_clean <- cartera_raw %>%
  dplyr::distinct()

cat("Dimensiones creditos_clean:", paste(dim(creditos_clean), collapse = " x "), "\n")
cat("Dimensiones cartera_clean :", paste(dim(cartera_clean),  collapse = " x "), "\n\n")

# ============================================================
# E) INTEGRACIÓN DE DATASETS
#    Unión por nota_debito + id_estudiante
# ============================================================

datos_integrados <- creditos_clean %>%
  dplyr::left_join(
    cartera_clean,
    by = c("nota_debito", "id_estudiante"),
    suffix = c("_cred", "_cart")
  ) %>%
  dplyr::mutate(
    tiene_cartera = !is.na(dias)
  )

cat("Dimensiones datos_integrados (antes de normalizar claves):",
    paste(dim(datos_integrados), collapse = " x "), "\n\n")

# ------------------------------------------------------------
# Normalizar nombre de la llave de crédito: no_creditos
# ------------------------------------------------------------

nombres_di <- names(datos_integrados)

candidatos_no <- nombres_di[grepl("(?i)^no_creditos", nombres_di, perl = TRUE)]

if (length(candidatos_no) == 1) {
  nombre_no_creditos <- candidatos_no
} else if (length(candidatos_no) > 1) {
  cand_cred <- candidatos_no[grepl("_cred$", candidatos_no)]
  if (length(cand_cred) == 1) {
    nombre_no_creditos <- cand_cred
  } else {
    stop("❌ Múltiples columnas candidatas para 'no_creditos': ",
         paste(candidatos_no, collapse = ", "),
         ". Revisión manual necesaria.")
  }
} else {
  stop("❌ No se encontró ninguna columna cuyo nombre empiece por 'no_creditos' en datos_integrados.\n",
       "Nombres disponibles:\n",
       paste(nombres_di, collapse = ", "))
}

datos_integrados <- datos_integrados %>%
  dplyr::rename(no_creditos = !!rlang::sym(nombre_no_creditos))

cat("Llave de crédito normalizada a 'no_creditos' (antes: '",
    nombre_no_creditos, "')\n", sep = "")

# ------------------------------------------------------------
# Normalizar nombre de linea_credito (tomamos la de créditos)
# ------------------------------------------------------------

nombres_di <- names(datos_integrados)

if ("linea_credito_cred" %in% nombres_di) {
  datos_integrados <- datos_integrados %>%
    dplyr::rename(linea_credito = linea_credito_cred)
} else if (!"linea_credito" %in% nombres_di) {
  posibles_linea <- nombres_di[grepl("(?i)^linea_credito", nombres_di, perl = TRUE)]
  if (length(posibles_linea) >= 1) {
    datos_integrados <- datos_integrados %>%
      dplyr::rename(linea_credito = !!rlang::sym(posibles_linea[1]))
  } else {
    warning("⚠️ No se encontró ninguna columna para 'linea_credito'. Se omitirá en el agregado.")
  }
}

# ------------------------------------------------------------
# Normalizar nombre de periodo (tomamos el de créditos)
# ------------------------------------------------------------

nombres_di <- names(datos_integrados)

if ("periodo_cred" %in% nombres_di) {
  datos_integrados <- datos_integrados %>%
    dplyr::rename(periodo = periodo_cred)
} else if (!"periodo" %in% nombres_di) {
  posibles_periodo <- nombres_di[grepl("(?i)^periodo", nombres_di, perl = TRUE)]
  if (length(posibles_periodo) >= 1) {
    datos_integrados <- datos_integrados %>%
      dplyr::rename(periodo = !!rlang::sym(posibles_periodo[1]))
  } else {
    warning("⚠️ No se encontró ninguna columna para 'periodo'. Se omitirá en el agregado.")
  }
}

cat("Columnas disponibles después de normalizar claves:\n")
print(names(datos_integrados))
cat("\n")

# Guardar dataset integrado a nivel de registro
saveRDS(
  datos_integrados,
  here::here("data", "processed", "datos_integrados.rds")
)

# ============================================================
# F) AGREGACIÓN A NIVEL DE CRÉDITO (unidad de análisis)
# ============================================================

base_creditos <- datos_integrados %>%
  dplyr::group_by(no_creditos, id_estudiante) %>%
  dplyr::summarise(
    # ---- DEMOGRÁFICAS / ACADÉMICAS ----
    estrato_socioeconomico = dplyr::first(estrato_socioeconomico),
    departamento           = dplyr::first(departamento),
    estado_estudiante      = dplyr::first(estado_estudiante),
    primiparo              = dplyr::first(primiparo),
    graduado               = dplyr::first(graduado),
    sexo                   = dplyr::first(sexo),
    
    # ---- FINANCIERAS DEL CRÉDITO ----
    linea_credito             = dplyr::first(linea_credito),
    valor_nota_debito         = sum_na(valor_nota_debito),
    valor_pagado_nota_debito  = sum_na(valor_pagado_nota_debito),
    saldo_nota_debito         = max_na(saldo_nota_debito),
    percent_recaudo           = max_na(percent_recaudo),
    cuotas                    = max_na(cuotas),
    saldo_cliente             = max_na(saldo_cliente),
    valor_financiacion        = max_na(valor_financiacion),
    valor_pagado              = max_na(valor_pagado),
    
    # ---- TEMPORALES ----
    fecha_aprobacion      = suppressWarnings(min(fecha_aprobacion, na.rm = TRUE)),
    fecha_vencimiento_ndb = suppressWarnings(max(fecha_vencimiento_ndb, na.rm = TRUE)),
    periodo               = dplyr::first(periodo),
    
    # ---- COMPORTAMIENTO DESDE CARTERA ----
    n_registros_cartera = sum(!is.na(dias)),
    max_dias_mora       = max_na(dplyr::if_else(is.na(dias), 0L, dias)),
    n_cuotas_vencidas   = sum(
      dplyr::if_else(!is.na(dias) & dias > 0, 1L, 0L),
      na.rm = TRUE
    ),
    saldo_total_cartera = sum_na(valor_total),
    genera_mora_flag    = any(genera_mora == "SI", na.rm = TRUE),
    
    .groups = "drop"
  )

cat("Dimensiones base_creditos (agregado por crédito):",
    paste(dim(base_creditos), collapse = " x "), "\n\n")

# ============================================================
# G) VARIABLE OBJETIVO: incumplimiento (mora > 30 días)
# ============================================================

base_creditos <- base_creditos %>%
  dplyr::mutate(
    incumplimiento = dplyr::if_else(max_dias_mora > 30, 1L, 0L)
  )

tabla_objetivo <- base_creditos %>%
  dplyr::count(incumplimiento, name = "n_creditos")

cat("Distribución de la variable objetivo (incumplimiento):\n")
print(tabla_objetivo)
cat("\n")

# Guardar base final para modelado
saveRDS(
  base_creditos,
  here::here("data", "processed", "base_modelo_creditos.rds")
)

readr::write_csv(
  base_creditos,
  here::here("outputs", "tablas", "02_base_modelo_creditos.csv")
)

cat("===== Fin de 02_preparacion_datos.R =====\n")
