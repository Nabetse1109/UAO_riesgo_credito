# ============================================================
# 04_eda_bivariado_multivariado.R
# Análisis bivariado y multivariado para riesgo crediticio
# ============================================================

# ----------------------------------------
# A) Setup
# ----------------------------------------
source(here::here("R", "00_setup_eda.R"))

# Cargar base final agregada por crédito
base_creditos <- readRDS(
  here::here("data", "processed", "base_modelo_creditos.rds")
)

# Crear carpeta de salida
dir_out <- here::here("outputs", "figuras", "04_eda_bivariado_multivariado")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

theme_set(theme_minimal(base_size = 13))

# ============================================================
# B) UTILIDADES
# ============================================================

guardar_plot <- function(plot, nombre, w = 8, h = 5) {
  ggsave(
    filename = paste0(nombre, ".png"),
    plot = plot,
    path = dir_out,
    width = w, height = h, dpi = 300
  )
}

tabla_tasas <- function(df, var) {
  df %>%
    dplyr::group_by({{ var }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      incumplimientos = sum(incumplimiento),
      tasa_incumplimiento = round(100 * mean(incumplimiento), 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(tasa_incumplimiento))
}

# ============================================================
# C) ANÁLISIS BIVARIADO — Numéricas vs Incumplimiento
# ============================================================

vars_numericas <- c(
  "valor_nota_debito",
  "valor_pagado_nota_debito",
  "saldo_nota_debito",
  "saldo_total_cartera",
  "cuotas",
  "max_dias_mora"
)

for (v in vars_numericas) {
  
  p1 <- ggplot(base_creditos,
               aes(x = factor(incumplimiento),
                   y = .data[[v]],
                   group = incumplimiento)) +
    geom_boxplot(fill = "#4a90e2") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Distribución de", v, "según incumplimiento"),
      x = "Incumplimiento (0 = No, 1 = Sí)",
      y = v
    )
  
  guardar_plot(p1, paste0("boxplot_", v))
}

# ============================================================
# D) Tests numéricos (t-test / Wilcoxon)
# ============================================================

tests_numericos <- list()

for (v in vars_numericas) {
  x0 <- base_creditos %>%
    dplyr::filter(incumplimiento == 0) %>%
    dplyr::pull(v)
  x1 <- base_creditos %>%
    dplyr::filter(incumplimiento == 1) %>%
    dplyr::pull(v)
  
  # Quitamos NA por seguridad
  x0 <- x0[is.finite(x0)]
  x1 <- x1[is.finite(x1)]
  
  if (length(unique(c(x0, x1))) > 10) {
    tests_numericos[[v]] <- t.test(x0, x1)
  } else {
    tests_numericos[[v]] <- wilcox.test(x0, x1)
  }
}

sink(here::here("outputs", "tablas", "04_tests_numericos.txt"))
print(tests_numericos)
sink()

# ============================================================
# E) Bivariado categóricas vs Incumplimiento
# ============================================================

vars_categoricas <- c(
  "sexo",
  "estrato_socioeconomico",
  "departamento",
  "estado_estudiante",
  "primiparo",
  "graduado",
  "linea_credito"
)

for (v in vars_categoricas) {
  
  p2 <- ggplot(base_creditos,
               aes(x = .data[[v]], fill = factor(incumplimiento))) +
    geom_bar(position = "fill") +
    scale_fill_manual(
      values = c("#4a90e2", "#d0021b"),
      name   = "Incumplimiento",
      labels = c("No", "Sí")
    ) +
    labs(
      title = paste("Proporción de incumplimiento según", v),
      x = v, y = "Proporción"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  guardar_plot(p2, paste0("cat_prop_", v))
  
  t <- tabla_tasas(base_creditos, !!sym(v))
  readr::write_csv(
    t,
    here::here("outputs", "tablas", paste0("04_tasas_", v, ".csv"))
  )
}

# ============================================================
# F) Tests Chi-cuadrado para categóricas
# ============================================================

tests_categoricas <- list()

for (v in vars_categoricas) {
  tabla <- table(base_creditos[[v]], base_creditos$incumplimiento)
  
  if (all(tabla > 0)) {
    tests_categoricas[[v]] <- chisq.test(tabla)
  } else {
    tests_categoricas[[v]] <-
      "Algunas celdas están vacías; no aplica Chi-cuadrado (o usar Fisher)."
  }
}

sink(here::here("outputs", "tablas", "04_tests_categoricas.txt"))
print(tests_categoricas)
sink()

# ============================================================
# G) MATRIZ DE CORRELACIÓN (numéricas relevantes)
# ============================================================

vars_corr <- c(
  "valor_nota_debito",
  "valor_pagado_nota_debito",
  "saldo_nota_debito",
  "saldo_total_cartera",
  "cuotas",
  "max_dias_mora"
)

df_corr <- base_creditos %>%
  dplyr::select(dplyr::all_of(vars_corr)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

mat_corr <- cor(df_corr, use = "pairwise.complete.obs")

# Si tienes ggcorrplot instalado:
if (requireNamespace("ggcorrplot", quietly = TRUE)) {
  p_corr <- ggcorrplot::ggcorrplot(
    mat_corr,
    lab = TRUE,
    hc.order = TRUE,
    type = "lower"
  )
  
  guardar_plot(p_corr, "matriz_correlacion")
}

# ============================================================
# H) PCA EXPLORATORIO (robusto a NA/Inf)
# ============================================================

# 1) Reemplazar infinitos por NA
df_pca <- df_corr %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ ifelse(is.infinite(.), NA_real_, .)
    )
  )

# 2) Eliminar filas con algún NA
df_pca <- tidyr::drop_na(df_pca)

# 3) Eliminar columnas con desviación estándar 0
sds <- apply(df_pca, 2, sd, na.rm = TRUE)
vars_validas <- names(sds[sds > 0])

df_pca <- df_pca[, vars_validas, drop = FALSE]

cat("Variables usadas en PCA:", paste(vars_validas, collapse = ", "), "\n")

if (ncol(df_pca) >= 2) {
  pca <- prcomp(df_pca, scale. = TRUE)
  
  var_df <- data.frame(
    componente = 1:length(pca$sdev),
    var_exp = (pca$sdev^2) / sum(pca$sdev^2)
  )
  
  p_pca <- ggplot(var_df, aes(x = componente, y = var_exp)) +
    geom_col(fill = "#4a90e2") +
    geom_line(group = 1) +
    labs(
      title = "PCA – Varianza explicada por componente",
      x = "Componente",
      y = "Varianza explicada"
    )
  
  guardar_plot(p_pca, "pca_varianza")
} else {
  warning("No hay suficientes variables con varianza > 0 para hacer PCA.")
}

# ============================================================
# I) Ranking exploratorio de importancia (Random Forest)
# ============================================================

if (requireNamespace("randomForest", quietly = TRUE)) {
  library(randomForest)
  
  df_rf <- base_creditos %>%
    dplyr::select(incumplimiento, dplyr::all_of(vars_corr)) %>%
    dplyr::mutate(
      incumplimiento = factor(incumplimiento)
    ) %>%
    tidyr::drop_na()
  
  # Verificamos que haya al menos 2 clases
  if (length(unique(df_rf$incumplimiento)) >= 2) {
    modelo_rf <- randomForest(
      incumplimiento ~ .,
      data = df_rf,
      ntree = 500,
      importance = TRUE
    )
    
    imp <- importance(modelo_rf)
    imp_df <- data.frame(
      variable = rownames(imp),
      importancia = imp[, 1]
    ) %>%
      dplyr::arrange(dplyr::desc(importancia))
    
    readr::write_csv(
      imp_df,
      here::here("outputs", "tablas", "04_importancia_rf.csv")
    )
    
    p_imp <- ggplot(imp_df,
                    aes(x = reorder(variable, importancia),
                        y = importancia)) +
      geom_col(fill = "#d0021b") +
      coord_flip() +
      labs(
        title = "Importancia predictiva preliminar (Random Forest)",
        x = "Variable",
        y = "Importancia"
      )
    
    guardar_plot(p_imp, "importancia_rf")
  } else {
    warning("Random Forest: solo hay una clase en incumplimiento después de drop_na().")
  }
} else {
  warning("El paquete 'randomForest' no está instalado; se omite esta sección.")
}

# ============================================================
# FIN DEL SCRIPT
# ============================================================
cat("==== Fin de 04_eda_bivariado_multivariado.R ====\n")
