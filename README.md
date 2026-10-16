# Goles por periodo y clasificación final en las cinco grandes ligas europeas (2016-2017)

Trabajo de **Diseños de Experimentos** (Facultad de Estadística, Universidad Santo Tomás, profesor Javier Mauricio Sierra).
Autores: Haider Rojas Salazar y Margarita Triana Ancinez.

El proyecto analiza, con ANOVA, prueba *t* y regresión lineal múltiple, si el momento del partido en que se marcan los goles se relaciona con la posición final de los equipos. Se basa en el artículo de González-García y Martínez-Martínez (2019) y usa datos de 98 equipos de las ligas alemana, española, francesa, inglesa e italiana.

## Tablero interactivo

**https://x74iyh-paula0margarita-triana0ancinez.shinyapps.io/diseo/**

El tablero (Shiny) permite explorar los resultados inferenciales y tiene cuatro pestañas:

| Pestaña | Contenido |
|---|---|
| KPIs y Heatmap | Número de equipos, ligas seleccionadas, rango de clasificaciones y mapa de calor de correlaciones entre las variables transformadas |
| ANOVA por liga | ANOVA de una vía (liga) para la muestra completa, el Top 4 y el Bottom 4 |
| Prueba T | Comparación Top 4 frente a Bottom 4 |
| Regresión lineal | Estadísticas globales y coeficientes del modelo |

Controles en la barra lateral: selección de ligas y resaltado de una variable en las tablas y el mapa de calor.

## Contenido del repositorio

| Archivo | Descripción |
|---|---|
| `app.R` | Script de análisis en R y aplicación Shiny (tablero) |
| `datos_ligas_europeas_16-17.xlsx` | Base de datos (hoja con 98 equipos; otra hoja con el diccionario) |
| Artículo original | Artículo de González-García y Martínez-Martínez (2019), incluido como referencia |
| `informe_goles_ligas_europeas.tex` | Informe en LaTeX (junto con las figuras `.png`) |

## Datos

Cada fila es un equipo de la temporada 2016-2017, ordenado por su posición final dentro de su liga.

| Variable | Descripción |
|---|---|
| `LG` | Liga (ALEMANA, ESPAÑOLA, FRANCESA, INGLESA, ITALIANA) |
| `EQ` | Equipo |
| `PFL` | Posición final en la liga |
| `P1` a `P6` | Goles en cada periodo de 15 minutos (1-15, 16-30, 31-45, 46-60, 61-75, 76-90) |
| `TG` | Total de goles |
| `TPP` | Goles en la primera parte (P1 + P2 + P3) |
| `TSP` | Goles en la segunda parte (P4 + P5 + P6) |

> **Nota:** según el diccionario de la base, estas variables corresponden a goles **anotados** por el equipo (no encajados).

## Metodología

1. Análisis descriptivo y gráfico (histogramas y diagramas de caja por liga).
2. Pruebas de supuestos por liga: Shapiro-Wilk (residuos), Levene y χ² de independencia (variables discretizadas en cuartiles).
3. Transformación logarítmica natural de las ocho variables (`ln_P1` … `ln_TSP`) y repetición de los supuestos.
4. ANOVA de una vía por liga: muestra completa, Top 4 y Bottom 4.
5. Prueba *t* (con corrección de Welch) entre Top 4 y Bottom 4.
6. Regresión lineal múltiple: `Clasificacion ~ ln_P1 + ... + ln_P6 + ln_TPP + ln_TSP`.

Definiciones: Top 4 son los puestos 1 a 4 de cada liga. En el script, Bottom 4 son los puestos mayores a 16, por lo que la liga alemana (18 equipos) aporta solo 2 equipos; en el tablero, en cambio, se toman los 4 últimos de cada liga.

## Resultados principales

- **Entre ligas:** solo el tercer periodo (P3) presenta diferencias significativas con la muestra completa (F = 2.925; p = 0.025). En el Top 4 y el Bottom 4 no hay diferencias.
- **Top 4 vs. Bottom 4:** los equipos de la parte alta anotan significativamente más en los ocho casos (p < 0.001).
- **Regresión:** R² = 0.7635 (ajustado 0.7423). Son significativos P4, P5 y P6 (segunda parte), con coeficientes negativos: más goles se asocian con una mejor posición.
- **Limitación:** TPP y TSP son sumas de los periodos y están muy correlacionados con ellos (multicolinealidad), por lo que no resultan significativos de forma individual.

## Cómo ejecutarlo

### Requisitos

R (≥ 4.1) y los siguientes paquetes:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "purrr", "ggplot2", "car", "psych", "gridExtra",
  "cluster", "factoextra", "broom", "knitr", "lmtest", "rstatix", "kableExtra",
  "readr", "shiny", "DT", "plotly", "bslib", "rsconnect"
))
```

### Pasos

1. Clona el repositorio y abre `app.R` en RStudio.
2. Ubica el archivo de Excel en la misma carpeta que `app.R`. El script lo lee con el nombre `datos ligas europeas 16-17.xlsx`; si tu archivo se llama `datos_ligas_europeas_16-17.xlsx`, ajusta la línea `read_excel(...)` al inicio del script.
3. Establece la carpeta del proyecto como directorio de trabajo (`setwd()` o abriendo el proyecto en RStudio).
4. Ejecuta el script completo. Primero corre el análisis (tablas y gráficos en consola y panel) y al final lanza el tablero con `shinyApp(ui, server)`.

### Publicación en shinyapps.io

El script incluye una llamada a `rsconnect::setAccountInfo(...)` con el token y el secret de la cuenta. **No publiques esas credenciales en el repositorio:**

- Elimina esa llamada de `app.R` y configura la cuenta una sola vez desde RStudio (*Tools → Global Options → Publishing*) o con variables de entorno.
- Si el token ya se subió a GitHub, genera uno nuevo en shinyapps.io y revoca el anterior.

## Referencia

González-García, H., & Martínez-Martínez, F. D. (2019). Análisis de la influencia de los goles encajados en la clasificación final de las principales ligas de fútbol europeas. *Journal of Sport and Health Research*, 11(3), 305-314.
