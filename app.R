# --- Librerías ---
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(psych)
library(gridExtra)
library(cluster)
library(factoextra)
library(broom)       
library(knitr)      
library(lmtest)
library(rstatix)
library(kableExtra) 
library(readr)

# --- 1. Cargar datos ---

datos <- read_excel("datos ligas europeas 16-17.xlsx", sheet = 1)

# --- 2. Variables numéricas a analizar ---
periodos <- c("P1", "P2", "P3", 
              "P4", "P5", "P6",
              "TPP", "TSP")

# =============================================================
# 3. TRANSFORMACIÓN LOGARÍTMICA
# =============================================================

for (var in periodos) {
  nueva_col <- paste0("ln_", var)
  datos[[nueva_col]] <- log(datos[[var]] + 1e-6)
}

print(head(datos %>% select(LG, starts_with("ln_"))))
variables_ln <- paste0("ln_", periodos)

# =============================================================
# 4. ANÁLISIS DESCRIPTIVO (DATOS ORIGINALES)
# =============================================================


resumen_descriptivo <- datos %>%
  group_by(LG) %>%
  summarise(across(all_of(periodos),
                   list(media = mean, sd = sd, min = min, max = max),
                   na.rm = TRUE))
print(resumen_descriptivo)

graficos_hist <- list()
graficos_box <- list()

for (var in periodos) {
  g1 <- ggplot(datos, aes(x = .data[[var]])) +
    geom_histogram(color = "black", fill = "skyblue", bins = 15) +
    labs(title = paste("Histograma -", var),
         x = var, y = "Frecuencia") +
    theme_minimal()
  graficos_hist[[var]] <- g1
  
  g2 <- ggplot(datos, aes(x = LG, y = .data[[var]], fill = LG)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot -", var), x = "Liga", y = var) +
    theme_minimal() +
    theme(legend.position = "none")
  graficos_box[[var]] <- g2
}

do.call("grid.arrange", c(graficos_hist[1:4], ncol = 2))
do.call("grid.arrange", c(graficos_hist[5:8], ncol = 2))
do.call("grid.arrange", c(graficos_box[1:4], ncol = 2))
do.call("grid.arrange", c(graficos_box[5:8], ncol = 2))

# =============================================================
# 5. SUPUESTOS (SHAPIRO, LEVENE, CHI-CUADRADO)
# =============================================================
resultados_shapiro <- data.frame()
resultados_levene <- data.frame()
resultados_chi <- data.frame()

for (var in periodos) {
  cat("\n\n##############################################################\n")
  cat("SUPUESTOS PARA:", var, "\n")
  cat("##############################################################\n")
  
  medias_liga <- datos %>%
    group_by(LG) %>%
    summarise(media_grupo = mean(.data[[var]], na.rm = TRUE))
  
  datos_res <- datos %>%
    left_join(medias_liga, by = "LG") %>%
    mutate(residuo_manual = .data[[var]] - media_grupo)
  
  datos[[paste0("residuos_", var)]] <- datos_res$residuo_manual
  
  shapiro_por_liga <- datos_res %>%
    group_by(LG) %>%
    summarise(p_value = tryCatch(shapiro.test(residuo_manual)$p.value, error = function(e) NA)) %>%
    mutate(variable = var)
  
  resultados_shapiro <- rbind(resultados_shapiro, shapiro_por_liga)
  
  lev <- leveneTest(as.formula(paste(var, "~ LG")), data = datos)
  resultados_levene <- rbind(resultados_levene,
                             data.frame(variable = var,
                                        estadistico = lev[1,1],
                                        p_value = lev[1,3]))
  
  nueva_col <- paste0(var, "_cat")
  datos[[nueva_col]] <- cut(
    datos[[var]],
    breaks = quantile(datos[[var]], probs = seq(0, 1, 0.25), na.rm = TRUE, type = 5),
    include.lowest = TRUE,
    labels = c("Bajo", "Medio", "Alto", "Muy_Alto")
  )
  
  cat_var <- paste0(var, "_cat")
  tabla_chi <- table(datos$LG, datos[[cat_var]])
  
  if (min(dim(tabla_chi)) > 1) { 
    chi <- chisq.test(tabla_chi)
    resultados_chi <- rbind(resultados_chi,
                            data.frame(variable = var,
                                       estadistico = chi$statistic,
                                       gl = chi$parameter,
                                       p_value = chi$p.value))
  } else {
    resultados_chi <- rbind(resultados_chi,
                            data.frame(variable = var,
                                       estadistico = NA, gl = NA, p_value = NA))
  }
}

# =============================================================
# 6. ANÁLISIS DESCRIPTIVO (DATOS LOG-TRANSFORMADOS)
# =============================================================

resumen_ln <- datos %>%
  group_by(LG) %>%
  summarise(across(starts_with("ln_"),
                   list(media = mean, sd = sd, min = min, max = max),
                   na.rm = TRUE))
print(resumen_ln)

# =============================================================
# 7. DISCRETIZACIÓN DE VARIABLES LOGARÍTMICAS
# =============================================================

for (var in periodos) {
  ln_var <- paste0("ln_", var)
  cat_var <- paste0(ln_var, "_cat")
  
  datos[[cat_var]] <- cut(
    datos[[ln_var]],
    breaks = quantile(datos[[ln_var]], probs = seq(0, 1, 0.25), na.rm = TRUE, type = 5),
    include.lowest = TRUE,
    labels = c("Bajo", "Medio", "Alto", "Muy_Alto")
  )
}

# =============================================================
# 8. SUPUESTOS (DATOS LOG-TRANSFORMADOS)
# =============================================================
resultados_shapirotrans <- data.frame()
resultados_levenetrans <- data.frame()
resultados_chitrans <- data.frame()

for (var in periodos) {
  ln_var <- paste0("ln_", var)
  ln_cat <- paste0(ln_var, "_cat")
  
  cat("\n\n##############################################################\n")
  cat("SUPUESTOS PARA:", ln_var, "\n")
  cat("##############################################################\n")
  
  medias_liga <- datos %>%
    group_by(LG) %>%
    summarise(media_grupo = mean(.data[[ln_var]], na.rm = TRUE))
  
  datos_resln <- datos %>%
    left_join(medias_liga, by = "LG") %>%
    mutate(residuo_manual = .data[[ln_var]] - media_grupo)
  
  datos[[paste0("residuos_", ln_var)]] <- datos_resln$residuo_manual
  
  shapiro_por_ligaln <- datos_resln %>%
    group_by(LG) %>%
    summarise(p_value = tryCatch(shapiro.test(residuo_manual)$p.value, error = function(e) NA)) %>%
    mutate(variable = ln_var)
  resultados_shapirotrans <- rbind(resultados_shapirotrans, shapiro_por_ligaln)
  
  levln <- leveneTest(as.formula(paste(ln_var, "~ LG")), data = datos)
  resultados_levenetrans <- rbind(resultados_levenetrans,
                                  data.frame(variable = ln_var,
                                             estadistico = levln[1,1],
                                             p_value = levln[1,3]))
  
  tabla_chiln <- table(datos$LG, datos[[ln_cat]])
  
  if (min(dim(tabla_chiln)) > 1) { 
    chiln <- chisq.test(tabla_chiln)
    resultados_chitrans <- rbind(resultados_chitrans,
                                 data.frame(variable = ln_var,
                                            estadistico = chiln$statistic,
                                            gl = chiln$parameter,
                                            p_value = chiln$p.value))
  } else {
    resultados_chitrans <- rbind(resultados_chitrans,
                                 data.frame(variable = ln_var,
                                            estadistico = NA, gl = NA, p_value = NA))
  }
}
##############################################################
# 9. ANOVA, PRUEBA T y REGRESIÓN LINEAL (DATOS TRANSFORMADOS)
##############################################################
# =============================================================
# PREPROCESAMIENTO: CLASIFICACIÓN Y SUBGRUPOS
# =============================================================

datos <- datos %>%
  group_by(LG) %>%
  mutate(Clasificacion = row_number()) %>% 
  ungroup() %>%
  mutate(
    Grupo_Rendimiento = ifelse(Clasificacion <= 4, "Top_4", "Bottom_4"),
    Grupo_Rendimiento = factor(Grupo_Rendimiento)
  )

datos_top_4 <- datos %>% filter(Clasificacion <= 4)                     
datos_bottom_4 <- datos %>% filter(Clasificacion > max(Clasificacion) - 4) 
datos_ttest <- bind_rows(datos_top_4, datos_bottom_4)                   

variables_analisis_ln <- variables_ln
lg_cols <- unique(datos$LG)

# --- 9.1. TABLA 1: ANOVA MUESTRA COMPLETA ---
cat("\n9.1. TABLA 1: ANOVA - Muestra Completa\n")

tabla_1_anova <- purrr::map_dfr(variables_analisis_ln, ~{
  
  # 1. Calcular ANOVA
  aov_model <- aov(as.formula(paste(.x, "~ LG")), data = datos)
  aov_res <- broom::tidy(aov_model) %>% filter(term == "LG")
  
  # 2. Calcular M±DT por liga y pivotear
  stats_res <- datos %>%
    group_by(LG) %>%
    summarise(
      Mean = round(mean(get(.x), na.rm = TRUE), 3),
      SD = round(sd(get(.x), na.rm = TRUE), 3)
    ) %>%
    mutate(
      LG_Stats = paste0(Mean, " ± ", SD)
    ) %>%
    select(LG, LG_Stats) %>%
    tidyr::pivot_wider(names_from = LG, values_from = LG_Stats)
  
  # 3. Combinar y Formatear
  data.frame(
    Variable = gsub("ln_", "", .x),
    stats_res,
    ANOVA = paste0("F=", round(aov_res$statistic, 3), 
                   "; p=", format.pval(aov_res$p.value, digits = 3, eps = 0.001, scientific = FALSE)),
    check.names = FALSE
  )
})

# Renombrar columnas LG_Stats
names(tabla_1_anova)[2:(1+length(lg_cols))] <- paste0(lg_cols, " (M±DT)")

print(tabla_1_anova %>% 
        kable(format = "html", 
              caption = "Tabla 1: ANOVA de una vía para ln(Goles por Periodo y Totales) entre ligas - Muestra Completa",
              align = 'c') %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 10))


# --- 9.2. TABLA 2: ANOVA TOP 4 CLASIFICADOS ---
cat("\n9.2. TABLA 2: ANOVA - Top 4 Clasificados\n")

tabla_2_anova <- purrr::map_dfr(variables_analisis_ln, ~{
  aov_model <- aov(as.formula(paste(.x, "~ LG")), data = datos_top_4)
  aov_res <- broom::tidy(aov_model) %>% filter(term == "LG")
  
  stats_res <- datos_top_4 %>%
    group_by(LG) %>%
    summarise(
      Mean = round(mean(get(.x), na.rm = TRUE), 3),
      SD = round(sd(get(.x), na.rm = TRUE), 3)
    ) %>%
    mutate(
      LG_Stats = paste0(Mean, " ± ", SD)
    ) %>%
    select(LG, LG_Stats) %>%
    tidyr::pivot_wider(names_from = LG, values_from = LG_Stats)
  
  data.frame(
    Variable = gsub("ln_", "", .x),
    stats_res,
    ANOVA = paste0("F=", round(aov_res$statistic, 3), 
                   "; p=", format.pval(aov_res$p.value, digits = 3, eps = 0.001, scientific = FALSE)),
    check.names = FALSE
  )
})

names(tabla_2_anova)[2:(1+length(lg_cols))] <- paste0(lg_cols, " (M±DT)")

print(tabla_2_anova %>% 
        kable(format = "html", 
              caption = "Tabla 2: ANOVA de una vía para ln(Goles por Periodo y Totales) entre ligas - Top 4 Clasificados",
              align = 'c') %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 10))

# --- 9.3. TABLA 3: ANOVA BOTTOM 4 CLASIFICADOS ---
cat("\n9.3. TABLA 3: ANOVA - Bottom 4 Clasificados\n")

tabla_3_anova <- purrr::map_dfr(variables_analisis_ln, ~{
  aov_model <- aov(as.formula(paste(.x, "~ LG")), data = datos_bottom_4)
  aov_res <- broom::tidy(aov_model) %>% filter(term == "LG")
  
  stats_res <- datos_bottom_4 %>%
    group_by(LG) %>%
    summarise(
      Mean = round(mean(get(.x), na.rm = TRUE), 3),
      SD = round(sd(get(.x), na.rm = TRUE), 3)
    ) %>%
    mutate(
      LG_Stats = paste0(Mean, " ± ", SD)
    ) %>%
    select(LG, LG_Stats) %>%
    tidyr::pivot_wider(names_from = LG, values_from = LG_Stats)
  
  data.frame(
    Variable = gsub("ln_", "", .x),
    stats_res,
    ANOVA = paste0("F=", round(aov_res$statistic, 3), 
                   "; p=", format.pval(aov_res$p.value, digits = 3, eps = 0.001, scientific = FALSE)),
    check.names = FALSE
  )
})

names(tabla_3_anova)[2:(1+length(lg_cols))] <- paste0(lg_cols, " (M±DT)")

print(tabla_3_anova %>% 
        kable(format = "html", 
              caption = "Tabla 3: ANOVA de una vía para ln(Goles por Periodo y Totales) entre ligas - Bottom 4 Clasificados",
              align = 'c') %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 10))


# --- 9.4. TABLA 4: PRUEBA T (TOP 4 vs. BOTTOM 4) ---
cat("\n9.4. TABLA 4: Prueba t de Student\n")

tabla_4_ttest <- purrr::map_dfr(variables_analisis_ln, ~{
  
  # 1. Calcular Prueba t
  ttest_res <- rstatix::t_test(data = datos_ttest, formula = as.formula(paste(.x, "~ Grupo_Rendimiento")))
  
  # 2. Calcular M±DT por grupo y pivotear
  stats_res <- datos_ttest %>%
    group_by(Grupo_Rendimiento) %>%
    summarise(
      Mean = round(mean(get(.x), na.rm = TRUE), 3),
      SD = round(sd(get(.x), na.rm = TRUE), 3)
    ) %>%
    mutate(
      Grupo_Stats = paste0(Mean, " ± ", SD)
    ) %>%
    select(Grupo_Rendimiento, Grupo_Stats) %>%
    tidyr::pivot_wider(names_from = Grupo_Rendimiento, values_from = Grupo_Stats)
  
  # 3. Combinar y Formatear
  data.frame(
    Variable = gsub("ln_", "", .x),
    stats_res,
    `Prueba t` = paste0("t=", round(ttest_res$statistic, 3), 
                        "; gl=", round(ttest_res$df, 0),
                        "; p=", format.pval(ttest_res$p, digits = 3, eps = 0.001, scientific = FALSE)),
    check.names = FALSE
  ) %>%
    select(Variable, Top_4, Bottom_4, `Prueba t`)
})

# Renombrar Top/Bottom para M±DT
names(tabla_4_ttest)[2:3] <- c("Top_4 (M±DT)", "Bottom_4 (M±DT)")

print(tabla_4_ttest %>% 
        kable(format = "html",
              caption = "Tabla 4: Prueba t de Student (Top 4 vs. Bottom 4 Clasificados)",
              align = 'c') %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 10))


# --- 9.5. TABLA 5: REGRESIÓN LINEAL MÚLTIPLE ---
cat("\n9.5. TABLA 5: Regresión Lineal Múltiple\n")

formula_regresion <- as.formula("Clasificacion ~ ln_P1 + ln_P2 + ln_P3 + ln_P4 + ln_P5 + ln_P6 +ln_TPP + ln_TSP")
regresion_model <- lm(formula_regresion, data = datos)

# 1. TABLA DE COEFICIENTES (broom::tidy)
regresion_summary <- broom::tidy(regresion_model) %>%
  mutate(
    term = gsub("ln_", "", term),
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = format.pval(p.value, digits = 3, eps = 0.001)
  ) %>%
  select(Término = term, `β` = estimate, `EE` = std.error, `t` = statistic, `p-valor` = p.value)

print(regresion_summary %>% 
        kable(format = "html",
              caption = "Tabla 5A: Coeficientes de la Regresión Lineal Múltiple",
              align = 'c') %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 10))

# 2. ESTADÍSTICAS GLOBALES (broom::glance)
glance_regresion <- broom::glance(regresion_model)

global_stats_tabla <- data.frame(
  "Estadística" = c("R-cuadrado", "R-cuadrado Ajustado", "F-statistic", "p-valor (Modelo)"),
  "Valor" = c(
    round(glance_regresion$r.squared, 4),
    round(glance_regresion$adj.r.squared, 4),
    round(glance_regresion$statistic, 3),
    format.pval(glance_regresion$p.value, digits = 3, eps = 0.001)
  )
)

print(global_stats_tabla %>% 
        kable(format = "html", align = 'l', 
              caption = "Tabla 5B: Estadísticas Globales del Modelo",
              col.names = c("Estadística", "Valor")) %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 10))



# Coloca aquí la versión del código Shiny con las interpretaciones integradas.
# Pega tu código y dime exactamente en qué secciones deseas insertar cada interpretación para que pueda modificarlo correctamente.
# =============================================================
#                DASHBOARD TOP 5 LIGAS EUROPEAS
# =============================================================

# --- LIBRERÍAS ---
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(readxl)
library(tidyr)
library(bslib)
library(ggplot2)
library(broom)
library(rstatix)

# =============================================================
# DATOS Y VARIABLES (Tus datos ya están cargados arriba)
# =============================================================
library(rsconnect)

rsconnect::setAccountInfo(
  name = "x74iyh-paula0margarita-triana0ancinez",
  token = "0084466DF9BC9CD657877E6544A4519F",
  secret = "SyXJWV7IaR1ZRDhB/wSDrRIpNAaDzTva1VHsAqDc"
)

datos <- datos
variables_ln <- variables_ln

# =============================================================
# FUNCIÓN: FORMATEAR TABLA ANOVA
# =============================================================

formatear_datatable_anova <- function(tabla, variable_resaltada) {
  
  if (is.null(tabla) || nrow(tabla) == 0) {
    return(DT::datatable(tabla, options = list(dom = "t", scrollX = TRUE)))
  }
  
  # Si NO se resalta nada → devolver tabla normal
  if (variable_resaltada == "Ninguna") {
    return(
      DT::datatable(
        tabla,
        rownames = FALSE,
        options = list(
          dom = "t",
          scrollX = TRUE,
          pageLength = nrow(tabla)
        )
      )
    )
  }
  
  # Buscar fila a resaltar
  idx_resaltar <- which(tabla$Variable == variable_resaltada)
  
  # Crear tabla
  dt <- DT::datatable(
    tabla,
    rownames = FALSE,
    options = list(
      dom = "t",
      scrollX = TRUE,
      pageLength = nrow(tabla),
      
      # --------- 🔥 FIX CRÍTICO: rowCallback para resaltar fila entera ---------
      rowCallback = JS(
        sprintf("
          function(row, data, index) {
            if (index + 1 === %d) {
              $('td', row).css({
                'background-color': '#d6eaff',
                'font-weight': 'bold'
              });
            }
          }
        ", idx_resaltar)
      )
      # -------------------------------------------------------------------------
      
    )
  )
  
  return(dt)
}

# =============================================================
# FUNCIÓN ANOVA
# =============================================================

calcular_anova_reactivo <- function(data_input) {
  req(data_input)
  
  if (nrow(data_input) == 0) {
    return(data.frame(
      Variable = gsub("ln_", "", variables_ln),
      Mensaje = "No hay datos", check.names = FALSE))
  }
  
  ligas_presentes <- unique(data_input$LG)
  
  if (length(ligas_presentes) < 2) {
    return(data.frame(
      Variable = gsub("ln_", "", variables_ln),
      Mensaje = "Filtro insuficiente (min 2 ligas)", check.names = FALSE))
  }
  
  purrr::map_dfr(variables_ln, ~{
    varname <- .x
    aov_model <- tryCatch(aov(as.formula(paste(varname, "~ LG")), data = data_input),
                          error = function(e) NULL)
    aov_res <- NULL
    
    if (!is.null(aov_model)) {
      tidy_aov <- broom::tidy(aov_model)
      aov_res <- tidy_aov %>% filter(term == "LG")
    }
    
    if (is.null(aov_res) || nrow(aov_res)==0 || is.na(aov_res$statistic)) {
      anova_text <- "ANOVA no disponible"
    } else {
      anova_text <- paste0("F=", round(aov_res$statistic,3),
                           "; p=", format.pval(aov_res$p.value, digits=3,
                                               eps=0.001))
    }
    
    stats_res <- tryCatch(
      data_input %>%
        group_by(LG) %>%
        summarise(
          Mean = round(mean(.data[[varname]], na.rm=TRUE), 3),
          SD = round(sd(.data[[varname]], na.rm=TRUE), 3)
        ) %>%
        mutate(LG_Stats = paste0(Mean, " ± ", SD)) %>%
        select(LG, LG_Stats) %>%
        pivot_wider(names_from = LG, values_from = LG_Stats),
      error=function(e) data.frame()
    )
    
    out <- data.frame(Variable=gsub("ln_","", varname))
    if (ncol(stats_res)>0) out <- cbind(out, stats_res)
    out$ANOVA <- anova_text
    out
  })
}

# =============================================================
# UI
# =============================================================

ui <- fluidPage(
  
  theme = bs_theme(bootswatch="minty",
                   base_font = font_google("Montserrat")),
  
  titlePanel("Dashboard de Análisis Inferencial – Top 5 Ligas Europeas"),
  
  div(
    style="padding:12px;background:#f7f7f7;border-radius:10px;margin-bottom:18px;",
    h4("Propósito del Tablero"),
    p("Este dashboard permite analizar estadísticamente el rendimiento de los equipos ",
      "de las Top 5 Ligas Europeas. Incluye KPIs, ANOVA, pruebas t, regresiones y ",
      "visualizaciones interactivas.")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Controles del Análisis"),
      
      selectInput(
        "ligas_seleccionadas", "Seleccionar Ligas:",
        choices = unique(datos$LG), selected = unique(datos$LG),
        multiple = TRUE
      ),
      
      selectInput(
        "variable_seleccionada", "Resaltar Variable:",
        choices = c("Ninguna", gsub("ln_", "", variables_ln)),
        selected = "Ninguna"
      ),
      
      hr()
    ),
    
    mainPanel(
      tabsetPanel(type="pills",
                  
                  tabPanel("📊 KPIs y Heatmap",
                           fluidRow(
                             column(4, uiOutput("kpi_equipos")),
                             column(4, uiOutput("kpi_ligas")),
                             column(4, uiOutput("kpi_rango"))
                           ),
                           hr(),
                           plotlyOutput("heatmap_cor", height="600px"),
                           hr(),
                           uiOutput("interpretacion_mapadecalor")
                  ),
                  
                  tabPanel("📈 ANOVA por Liga",
                           h3("ANOVA por Liga"),
                           dataTableOutput("tabla_anova_full_react"),
                           h4("Top 4"),
                           dataTableOutput("tabla_anova_top4_react"),
                           h4("Bottom 4"),
                           dataTableOutput("tabla_anova_bottom4_react"),
                           hr(),
                           uiOutput("interpretacion_anova")
                  ),
                  
                  tabPanel("🔍 Prueba T",
                           h3("Prueba T Top 4 vs Bottom 4"),
                           dataTableOutput("tabla_ttest_rendimiento_react"),
                           hr(),
                           uiOutput("interpretacion_ttest")
                  ),
                  
                  tabPanel("📉 Regresión Lineal",
                           fluidRow(
                             column(6, dataTableOutput("tabla_reg_global_react")),
                             column(6, dataTableOutput("tabla_reg_coefs_react"))
                           ),
                           hr(),
                           uiOutput("interpretacion_regresion")
                  )
      )
    )
  )
)

# =============================================================
# SERVER
# =============================================================

server <- function(input, output, session) {
  
  # ---------- Filtrado ----------
  datos_filtrados_react <- reactive({
    req(input$ligas_seleccionadas)
    
    data_filtered <- datos %>% filter(LG %in% input$ligas_seleccionadas)
    
    data_processed <- data_filtered %>%
      group_by(LG) %>% mutate(Clasificacion=row_number()) %>% ungroup()
    
    list(
      full = data_processed,
      top_4 = data_processed %>% filter(Clasificacion <= 4),
      bottom_4 = data_processed %>% 
        group_by(LG) %>% 
        filter(Clasificacion > max(Clasificacion) - 4) %>% 
        ungroup(),
      ttest = bind_rows(
        data_processed %>% filter(Clasificacion<=4) %>% mutate(Grupo_Rendimiento="Top_4"),
        data_processed %>% group_by(LG) %>% 
          filter(Clasificacion>max(Clasificacion)-4) %>%
          ungroup() %>% mutate(Grupo_Rendimiento="Bottom_4")
      )
    )
  })
  
  # ---------------- KPIs ----------------
  estilo_kpi <- "background:linear-gradient(135deg,#2980b9,#6dd5fa);
                 color:white;padding:22px;border-radius:14px;
                 text-align:center;box-shadow:0 4px 10px rgba(0,0,0,0.1);"
  
  output$kpi_equipos <- renderUI({
    n <- nrow(datos_filtrados_react()$full)
    div(style=estilo_kpi, h2(n), p("Equipos Analizados"))
  })
  
  output$kpi_ligas <- renderUI({
    n <- length(unique(datos_filtrados_react()$full$LG))
    div(style=gsub("#2980b9","#27ae60", estilo_kpi), 
        h2(n), p("Ligas Seleccionadas"))
  })
  
  output$kpi_rango <- renderUI({
    df <- datos_filtrados_react()$full
    div(style=gsub("#2980b9","#e67e22", estilo_kpi),
        h2(paste0(min(df$Clasificacion)," - ",max(df$Clasificacion))),
        p("Rango de Clasificaciones"))
  })
  
  # ---------------- HEATMAP ----------------
  output$heatmap_cor <- renderPlotly({
    # datos y variables
    df <- datos_filtrados_react()$full %>% select(all_of(variables_ln))
    vars_sin_ln <- gsub("ln_", "", variables_ln)
    var_sel <- input$variable_seleccionada
    
    # matriz de correlación
    cormat <- round(cor(df, use = "pairwise.complete.obs"), 2)
    n <- length(vars_sin_ln)
    
    # plotly heatmap usando coordenadas numéricas 1..n y etiquetas personalizadas
    p <- plot_ly(
      z = cormat,
      x = 1:n,
      y = 1:n,
      type = "heatmap",
      colors = colorRamp(c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")),
      zmin = -1, zmax = 1,
      hoverinfo = "text",
      text = apply(cormat, 1:2, function(val, i, j) {
        # generate hover text: value + labels
        # we'll override via a small wrapper below
        NULL
      })
    ) %>%
      layout(
        xaxis = list(
          tickmode = "array",
          tickvals = 1:n,
          ticktext = vars_sin_ln,
          tickangle = -45
        ),
        yaxis = list(
          tickmode = "array",
          tickvals = 1:n,
          ticktext = vars_sin_ln,
          autorange = "reversed"  # para tener la matriz orientada como matriz R/ggplot
        ),
        margin = list(l = 120, b = 120)
      )
    
    # Agregar marco rojo y subrayados usando COORDENADAS NUMÉRICAS
    if (!is.null(var_sel) && var_sel != "Ninguna" && var_sel %in% vars_sin_ln) {
      idx <- which(vars_sin_ln == var_sel)  # índice numérico
      
      # Línea vertical (columna) - marco rojo
      p <- p %>% add_trace(
        type = "scatter",
        mode = "lines",
        x = c(idx, idx),
        y = c(0.5, n + 0.5),
        line = list(color = "red", width = 4),
        hoverinfo = "none",
        showlegend = FALSE
      )
      
      # Línea horizontal (fila) - marco rojo
      p <- p %>% add_trace(
        type = "scatter",
        mode = "lines",
        x = c(0.5, n + 0.5),
        y = c(idx, idx),
        line = list(color = "red", width = 4),
        hoverinfo = "none",
        showlegend = FALSE
      )
      
      # Subrayado pequeño debajo de la etiqueta X (horizontal short line)
      p <- p %>% add_trace(
        type = "scatter",
        mode = "lines",
        x = c(max(0.5, idx - 0.3), min(n + 0.5, idx + 0.3)),
        y = rep(n + 0.7, 2),   # un poco por encima del heatmap (arriba) — ajustable
        line = list(color = "black", width = 6),
        hoverinfo = "none",
        showlegend = FALSE
      )
      
      # Subrayado pequeño a la derecha de la etiqueta Y (vertical short line)
      p <- p %>% add_trace(
        type = "scatter",
        mode = "lines",
        x = rep(n + 0.7, 2),
        y = c(max(0.5, idx - 0.3), min(n + 0.5, idx + 0.3)),
        line = list(color = "black", width = 6),
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
    p
  })
  
  
  output$interpretacion_mapadecalor <- renderUI({
    HTML("<h4>Interpretaciones del mapa de calor:</h4>
    </b> La matriz de correlación muestra una correlación positiva muy fuerte entre los goles anotados en los distintos periodos y los totales, lo que indica un rendimiento goleador consistente durante el juego. El hallazgo más notable es la alta correlación entre el Total de Goles en la primera parte (\\ln_TPP) y Goles en la segunda parte(\\ln_TSP), lo que sugiere que los equipos con ofensivas altas tienden a participar en partidos de alto puntaje donde su defensa resulta ser más abierta.</p>")
  })
  
  # ---------------- ANOVA ----------------
  
  tabla_anova_full_reactiva <- reactive({
    calcular_anova_reactivo(datos_filtrados_react()$full)
  })
  
  output$tabla_anova_full_react <- renderDataTable({
    formatear_datatable_anova(tabla_anova_full_reactiva(), input$variable_seleccionada)
  })
  
  tabla_anova_top4_reactiva <- reactive({
    calcular_anova_reactivo(datos_filtrados_react()$top_4)
  })
  
  output$tabla_anova_top4_react <- renderDataTable({
    formatear_datatable_anova(tabla_anova_top4_reactiva(), input$variable_seleccionada)
  })
  
  tabla_anova_bottom4_reactiva <- reactive({
    calcular_anova_reactivo(datos_filtrados_react()$bottom_4)
  })
  
  output$tabla_anova_bottom4_react <- renderDataTable({
    formatear_datatable_anova(tabla_anova_bottom4_reactiva(), input$variable_seleccionada)
  })
  
  output$interpretacion_anova <- renderUI({
    HTML("<h4>Interpretación ANOVA:</h4>
    <p><b>ANOVA 1 (Base completa):</b> Los valores p son mayores a 0.05 en casi todas las variables, por lo que no hay diferencias significativas entre ligas. Solo P3 muestra diferencia (p=0.025). En general, los promedios goleadores entre ligas son muy similares cuando se consideran todos los equipos.</p>
    <p><b>ANOVA 2 (Top 4):</b> Todos los valores p superan 0.05. No hay diferencias significativas entre ligas en los equipos de élite. Esto indica que los mejores equipos presentan un comportamiento ofensivo homogéneo sin importar la liga.</p>
    <p><b>ANOVA 3 (Bottom 4):</b> De nuevo, todos los p-valores son mayores a 0.05. Los equipos de bajo rendimiento también muestran niveles comparables de goles entre ligas. El patrón ofensivo se mantiene estable sin importar el campeonato.</p>")
  })
  
  # ---------------- PRUEBA T ----------------
  
  tabla_ttest_reactiva <- reactive({
    
    data_ttest <- datos_filtrados_react()$ttest
    
    if (nrow(data_ttest) < 4 ||
        length(unique(data_ttest$Grupo_Rendimiento)) < 2) {
      return(data.frame(
        Variable = gsub("ln_","",variables_ln),
        Prueba_t="Filtro insuficiente",
        check.names = FALSE))
    }
    
    purrr::map_dfr(variables_ln, ~{
      
      varname <- .x
      
      ttest_res <- tryCatch(
        rstatix::t_test(data=data_ttest,
                        formula=as.formula(paste(varname, "~ Grupo_Rendimiento"))),
        error=function(e) tibble(statistic=NA, df=NA, p=NA)
      )
      
      t_stat <- round(ttest_res$statistic[1], 3)
      t_df   <- round(ttest_res$df[1], 0)
      t_p    <- format.pval(ttest_res$p[1], digits=3, eps=0.001)
      
      stats_res <- data_ttest %>%
        group_by(Grupo_Rendimiento) %>%
        summarise(
          Mean=round(mean(.data[[varname]],na.rm=TRUE),3),
          SD=round(sd(.data[[varname]],na.rm=TRUE),3)
        ) %>%
        mutate(Grupo_Stats = paste0(Mean," ± ",SD)) %>%
        select(Grupo_Rendimiento, Grupo_Stats) %>%
        pivot_wider(names_from=Grupo_Rendimiento,
                    values_from=Grupo_Stats)
      
      out <- data.frame(Variable=gsub("ln_","",varname),
                        stringsAsFactors = FALSE)
      if (ncol(stats_res)>0) out <- cbind(out, stats_res)
      
      out$Prueba_t <- paste0("t=",t_stat,"; gl=",t_df,"; p=",t_p)
      
      # Renombrar columnas si existen
      current_names <- names(out)
      if ("Top_4" %in% current_names && "Bottom_4" %in% current_names) {
        names(out)[which(current_names == "Top_4")] <- "Top_4 (M±DT)"
        names(out)[which(current_names == "Bottom_4")] <- "Bottom_4 (M±DT)"
      }
      
      return(out)
    })
  })
  
  output$tabla_ttest_rendimiento_react <- renderDataTable({
    
    tabla <- tabla_ttest_reactiva()
    var_sel <- input$variable_seleccionada
    
    if (is.null(tabla) || nrow(tabla)==0)
      return(datatable(tabla, options=list(dom="t", scrollX=TRUE)))
    
    if (var_sel == "Ninguna")
      return(datatable(tabla, options=list(dom="t", scrollX=TRUE)))
    
    idx_resaltar <- which(tabla$Variable == var_sel)
    
    datatable(
      tabla,
      rownames = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE,
        pageLength = nrow(tabla),
        rowCallback = JS(
          sprintf("
          function(row, data, index) {
            if (index + 1 === %d) {
              $('td', row).css({
                'background-color': '#fff3cd',
                'font-weight': 'bold',
                'text-decoration': 'underline'
              });
            }
          }
        ", idx_resaltar)
        )
      )
    )
  })
  
  output$interpretacion_ttest <- renderUI({
    HTML("<h4>Interpretación Prueba t:</h4>
    <p>A diferencia del ANOVA, aquí todas las pruebas son altamente significativas (p < 0.001). Los equipos del Top 4 anotan mucho más que los Bottom 4 en todos los periodos. Dentro de cada liga sí existe una brecha clara en desempeño ofensivo.</p>")
  })
  
  # ---------------- REGRESIÓN ----------------
  
  regresion_model_reactivo <- reactive({
    
    data_full <- datos_filtrados_react()$full
    
    if (nrow(data_full) < (length(variables_ln) + 2)) return(NULL) 
    
    formula_regresion <- as.formula(
      paste("Clasificacion ~", paste(variables_ln, collapse=" + "))
    )
    
    tryCatch(lm(formula_regresion, data=data_full),
             error=function(e) NULL)
  })
  
  output$tabla_reg_global_react <- renderDataTable({
    model <- regresion_model_reactivo()
    
    if (is.null(model)) {
      return(datatable(data.frame("Modelo no disponible"), options=list(dom="t")))
    }
    
    glance_reg <- broom::glance(model)
    
    global_stats_tabla <- data.frame(
      Estadística = c("R²","R² Ajustado","F-statistic","p-valor"),
      Valor = c(
        round(glance_reg$r.squared,4),
        round(glance_reg$adj.r.squared,4),
        round(glance_reg$statistic,3),
        format.pval(glance_reg$p.value, digits=3, eps=0.001)
      )
    )
    
    datatable(global_stats_tabla, options=list(dom="t"))
  })
  
  output$tabla_reg_coefs_react <- renderDataTable({
    model <- regresion_model_reactivo()
    
    if (is.null(model)) {
      return(datatable(data.frame(Mensaje="No hay suficientes datos"),
                       options=list(dom="t")))
    }
    
    regresion_summary <- broom::tidy(model) %>%
      mutate(
        term = gsub("ln_","",term),
        estimate  = round(estimate, 3),
        std.error = round(std.error,3),
        statistic = round(statistic,3),
        p.value   = format.pval(p.value, digits=3, eps=0.001)
      ) %>%
      select(Término=term, β=estimate, EE=std.error, t=statistic, `p-valor`=p.value)
    
    datatable(regresion_summary, options=list(dom='t', scrollX=TRUE))
  })
  
  output$interpretacion_regresion <- renderUI({
    HTML("<h4>Interpretaciones de la Regresión:</h4>
    <p><b>Regresión – Estadísticas Globales:</b> El modelo explica un 76% de la variabilidad (R² = 0.7635). El R² ajustado (0.7423) confirma un buen ajuste. La F-global es altamente significativa (p < 0.001), lo que demuestra que el conjunto de predictores tiene capacidad explicativa real.</p>
    <p><b>Regresión – Coeficientes:</b> P4, P5 y P6 son predictores significativos (p < 0.05), con efectos negativos importantes sobre la clasificación (incrementos en estas variables empeoran el resultado). P1, P2, P3, TPP y TSP no resultan significativos individualmente.</p>")
  })
}

# =============================================================
# EJECUTAR APP
# =============================================================
shinyApp(ui = ui, server = server)



