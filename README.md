# Proyecto de Análisis y Modelado de Riesgo Crediticio  
### Maestría en Ciencia de Datos – Pontificia Universidad Javeriana Cali  
### Autores: Paula Andrea Tovar - Esteban Grajales Castaño

---

## 📘 1. Descripción general del proyecto

Este proyecto corresponde al desarrollo integral del caso aplicado de la asignatura **Modelos Estadísticos para la Toma de Decisiones**, dentro de la Maestría en Ciencia de Datos de la Pontificia Universidad Javeriana Cali.

El objetivo principal es **analizar el riesgo crediticio de los estudiantes de la Universidad Autónoma de Occidente (UAO)** utilizando múltiples fuentes de información (créditos, cartera, pagos, comportamiento histórico) y construir una base analítica confiable para futuros modelos predictivos de **probabilidad de incumplimiento**.

Para asegurar rigurosidad metodológica, el proyecto sigue la metodología **CRISP-DM**, estructurando el proceso en fases:  
1. Entendimiento del negocio  
2. Entendimiento de los datos  
3. Preparación de los datos  
4. Modelado (siguiente fase)  
5. Evaluación  
6. Despliegue

Actualmente, el repositorio contiene las fases 1 a 3 completamente desarrolladas.

---

## 🎯 2. Objetivo del análisis

Construir una base analítica consolidada a nivel de crédito que permita:

- Identificar el comportamiento financiero real de cada estudiante.
- Integrar los registros de cartera con los créditos otorgados.
- Detectar patrones de morosidad y desempeño histórico.
- Definir la variable objetivo **incumplimiento = 1 si el estudiante presenta mora > 30 días**.
- Preparar una base estructurada y validada para modelado estadístico.

---

## 📂 3. Estructura del repositorio

UAO_riesgo_credito/
│
├── R/ # Scripts del proyecto
│ ├── 00_setup_eda.R
│ ├── 01_eda_estructura.R
│ ├── 01a_eda_temporal.R
│ ├── 01b_clasificacion_variables.R
│ ├── 02_preparacion_datos.R
│ ├── 03_eda_descriptivo_base_creditos.R
│ ├── 04_eda_bivariado_multivariado.R
│
├── data/
│ ├── raw/ # Datos originales
│ ├── processed/ # Datos transformados (RDS)
│
├── outputs/
│ ├── tablas/ # Tablas, CSVs exportados
│ ├── graficos/ # Visualizaciones generadas
│
├── reports/
│ ├── 00_entendimiento_negocio.Rmd
│ ├── 00_entendimiento_negocio.md
│
└── UAO_riesgo_credito.Rproj # Proyecto de RStudio


---

## 🧪 4. Scripts y fase del proceso CRISP-DM

### ✔ **00_setup_eda.R**  
Configuración inicial, carga de paquetes, temas gráficos y opciones globales.

### ✔ **01_eda_estructura.R**  
Exploración estructural de cada dataset:  
- Tipos de variables  
- Conteo de faltantes  
- Detección de duplicados  
- Rangos y valores atípicos básicos

### ✔ **01a_eda_temporal.R**  
Análisis temporal de:  
- Desembolsos  
- Valor promedio por periodo  
- Tendencias y ciclos

### ✔ **01b_clasificacion_variables.R**  
Clasificación automática de variables por rol analítico:  
- Demográfica  
- Académica  
- Financiera  
- Temporal  
- Identificadora  
- Texto no modelable

### ✔ **02_preparacion_datos.R**  
Etapa más crítica del proyecto. Incluye:  
- Validaciones de negocio  
- Limpieza estructural (duplicados, rangos, fechas)  
- Integración datasets crédito + cartera  
- Normalización de llaves (`no_creditos`)  
- Consolidación a nivel de crédito  
- Cálculo de variables derivadas de comportamiento  
- Construcción de la variable objetivo

### ✔ **03_eda_descriptivo_base_creditos.R**  
Análisis descriptivo univariado:  
- Histogramas, densidades, boxplots  
- Distribuciones categóricas  
- Estadísticos descriptivos

### ✔ **04_eda_bivariado_multivariado.R**  
Exploración relacional entre variables:  
- Asimetrías entre incumplidos vs. no incumplidos  
- Gráficos bivariados  
- Matriz de correlación  
- Intento preliminar de PCA (descartado por NA/Inf)

---

## 📊 5. Resultados preliminares

### 🔹 Base analítica final  
**106.870 créditos únicos**, cada uno con más de **25 variables** entre demográficas, financieras y de comportamiento.

### 🔹 Variable objetivo  

Incumplimiento = 1 si max_dias_mora > 30


Distribución:
- **1 = 1.27%**
- **0 = 98.73%**

Dataset altamente desbalanceado.

### 🔹 Hallazgos iniciales del EDA  
- Existen patrones claros de comportamiento según línea de crédito.  
- La morosidad se concentra en ciertos periodos académicos.  
- Variables financieras muestran correlaciones útiles para modelado.  
- Se identificaron y corrigieron múltiples problemas de llaves y duplicados entre crédito y cartera.

---

## 🚀 6. Próximos pasos

Los siguientes pasos corresponden a la fase de **modelado** en CRISP-DM:

1. Selección de variables para modelado  
2. Tratamiento de desbalance (SMOTE, weighting, etc.)  
3. Modelos candidatos:  
   - Regresión logística  
   - Árboles de decisión  
   - Random Forest  
   - Gradient Boosting  
4. Comparación de métricas: ROC-AUC, KS, matriz de confusión  
5. Selección del mejor modelo  
6. Construcción de reporte final académico

---

## 📧 Contactos  
**Paula Andrea Tovar - Esteban Grajales**  
Maestría en Ciencia de Datos – PUJ Cali  


