# ============================================================
# 01b_clasificacion_variables.R
# Clasificación de variables por rol analítico
# ============================================================

source("R/00_setup_eda.R")

cat("===== Inicio 01b_clasificacion_variables.R =====\n\n")

# ------------------------------------------------------------
# A) Cargar diccionarios estructurales
# ------------------------------------------------------------

dicc_creditos <- readr::read_csv(
  here::here("outputs", "tablas", "dicc_creditos.csv"),
  show_col_types = FALSE
)

dicc_cartera <- readr::read_csv(
  here::here("outputs", "tablas", "dicc_cartera.csv"),
  show_col_types = FALSE
)

dicc_total <- dplyr::bind_rows(dicc_creditos, dicc_cartera)

# ------------------------------------------------------------
# B) Clasificación manual por rol y uso en el modelo
# ------------------------------------------------------------

clasificacion_base <- tibble::tribble(
  ~dataset,       ~variable,                  ~rol,               ~usar_modelo,
  
  # ---------- CRÉDITOS (creditos_raw) ----------
  # Demográficas
  "creditos_raw", "estrato_socioeconomico",   "demografica",      TRUE,
  "creditos_raw", "departamento",             "demografica",      TRUE,
  "creditos_raw", "sexo",                     "demografica",      TRUE,
  
  # Académicas
  "creditos_raw", "estado_estudiante",        "academica",        TRUE,
  "creditos_raw", "primiparo",                "academica",        TRUE,
  "creditos_raw", "graduado",                 "academica",        TRUE,
  
  # Temporales
  "creditos_raw", "fecha_pago_nota_debito",   "temporal",         TRUE,
  "creditos_raw", "fecha_vencimiento_ndb",    "temporal",         TRUE,
  "creditos_raw", "fecha_solicitud",          "temporal",         TRUE,
  "creditos_raw", "fecha_aprobacion",         "temporal",         TRUE,
  "creditos_raw", "periodo",                  "temporal",         TRUE,
  
  # Identificadoras (no se usan en el modelo)
  "creditos_raw", "cliente",                  "identificadora",   FALSE,
  "creditos_raw", "id_estudiante",            "identificadora",   FALSE,
  "creditos_raw", "no_creditos",              "identificadora",   FALSE,
  "creditos_raw", "nota_debito",              "identificadora",   FALSE,
  "creditos_raw", "nombre_centro_costo",      "identificadora",   FALSE,
  
  # Financieras
  "creditos_raw", "linea_credito",            "financiera",       TRUE,
  "creditos_raw", "valor_nota_debito",        "financiera",       TRUE,
  "creditos_raw", "valor_pagado_nota_debito", "financiera",       TRUE,
  "creditos_raw", "saldo_nota_debito",        "financiera",       TRUE,
  "creditos_raw", "percent_recaudo",          "financiera",       TRUE,
  "creditos_raw", "cuotas",                   "financiera",       TRUE,
  "creditos_raw", "saldo_cliente",            "financiera",       TRUE,
  "creditos_raw", "valor_financiacion",       "financiera",       TRUE,
  "creditos_raw", "valor_pagado",             "financiera",       TRUE,
  
  # Texto descriptivo (no entrarían al modelo)
  "creditos_raw", "descripcion_linea_credito","texto_no_modelo",   FALSE,
  "creditos_raw", "estado_describe",          "texto_no_modelo",   FALSE,
  
  # ---------- CARTERA (cartera_raw) ----------
  # Identificadoras
  "cartera_raw",  "cliente",                  "identificadora",   FALSE,
  "cartera_raw",  "id_estudiante",            "identificadora",   FALSE,
  "cartera_raw",  "nota_debito",              "identificadora",   FALSE,
  "cartera_raw",  "nombre_centro_costo",      "identificadora",   FALSE,
  "cartera_raw",  "no_creditos",              "identificadora",   FALSE,
  
  # Comportamiento de cartera
  "cartera_raw",  "dias",                     "comportamiento",   TRUE,
  "cartera_raw",  "edad_cartera",             "comportamiento",   TRUE,
  "cartera_raw",  "tipo_cartera",             "comportamiento",   TRUE,
  "cartera_raw",  "genera_mora",              "comportamiento",   TRUE,
  # ✔ Corrección aquí: antes 'otros', ahora 'comportamiento'
  "cartera_raw",  "activa_inactiva",          "comportamiento",   TRUE,
  
  # Financieras
  "cartera_raw",  "saldo",                    "financiera",       TRUE,
  "cartera_raw",  "corto_largo_plazo",        "financiera",       TRUE,
  "cartera_raw",  "linea_credito",            "financiera",       TRUE,
  "cartera_raw",  "valor_total",              "financiera",       TRUE,
  "cartera_raw",  "tasa_interes_mora",        "financiera",       TRUE,
  "cartera_raw",  "valor_afectado",           "financiera",       TRUE,
  "cartera_raw",  "cuota",                    "financiera",       TRUE,
  
  # Temporales
  "cartera_raw",  "fecha_documento",          "temporal",         TRUE,
  "cartera_raw",  "fecha_vencimiento",        "temporal",         TRUE,
  "cartera_raw",  "periodo",                  "temporal",         TRUE,
  
  # Texto descriptivo
  "cartera_raw",  "nombre_causa_nota",        "texto_no_modelo",  FALSE,
  "cartera_raw",  "nombre_concepto_credito",  "texto_no_modelo",  FALSE
)

# ------------------------------------------------------------
# C) Unir clasificación con diccionarios (tipo, faltantes, etc.)
# ------------------------------------------------------------

clasificacion <- clasificacion_base %>%
  dplyr::left_join(
    dicc_total %>%
      dplyr::select(
        dataset, variable, tipo,
        n_unicos, n_faltantes, pct_faltantes, n_total_fil
      ),
    by = c("dataset", "variable")
  ) %>%
  dplyr::arrange(dataset, rol, variable)

# Guardar tabla para uso posterior
readr::write_csv(
  clasificacion,
  here::here("outputs", "tablas", "clasificacion_variables.csv")
)

# ------------------------------------------------------------
# D) Vista en el informe
# ------------------------------------------------------------

formatear_tabla(
  clasificacion,
  caption = "Clasificación de variables por rol analítico",
  n = nrow(clasificacion)
)

cat("\n===== Fin 01b_clasificacion_variables.R =====\n")
