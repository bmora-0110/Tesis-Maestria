# ============================================================
# Brayan Mora y Arturo Melo 
# Modelos VARX (1) triado
# y_t = activos_conocimiento
# u_t = graduados
# ============================================================
gc(); rm(list = ls())

require(pacman)
p_load(
  readr, dplyr, ggplot2, stringr, forecast, tibble, FinTS, tseries,
  tidyr, vars, readxl, writexl
)

root <- "C:/Users/braya/Documents/TESIS_MAESTRIA/"

#################################################
#             FUNCIONES AUXILIARES              #
#################################################

fmt_num <- function(x, digits = 3) {
  if (x >= 0) {
    paste0("+", format(round(x, digits), nsmall = digits))
  } else {
    format(round(x, digits), nsmall = digits)
  }
}

build_equation <- function(region_name, coef_df, regions_used, region_short_map) {
  
  intercept <- coef_df["const", "Estimate"]
  
  eq <- paste0(
    "ŷ_", region_short_map[[region_name]], ",t = ",
    format(round(intercept, 3), nsmall = 3)
  )
  
  # términos rezagados
  for (r in regions_used) {
    rn <- paste0(gsub(" ", ".", r), ".l1")
    beta <- coef_df[rn, "Estimate"]
    eq <- paste0(eq, " ", fmt_num(beta), region_short_map[[r]], "_{t-1}")
  }
  
  # términos exógenos
  for (r in regions_used) {
    rn <- gsub(" ", ".", r)
    beta <- coef_df[rn, "Estimate"]
    eq <- paste0(eq, " ", fmt_num(beta), "u_", region_short_map[[r]], ",t")
  }
  
  eq <- paste0(
    eq,
    " + ε_", region_short_map[[region_name]], ",t"
  )
  
  return(eq)
}

frobenius_ratio <- function(y, yhat) {
  ok <- complete.cases(y, yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  
  if (length(y) == 0) return(NA_real_)
  
  num <- sqrt(sum((y - yhat)^2))
  den <- sqrt(sum(y^2))
  
  if (!is.finite(den) || den == 0) return(NA_real_)
  num / den
}

calc_r2 <- function(y, yhat) {
  ok <- complete.cases(y, yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  
  if (length(y) == 0) return(NA_real_)
  
  sse <- sum((y - yhat)^2)
  sst <- sum((y - mean(y))^2)
  
  if (!is.finite(sst) || sst == 0) return(NA_real_)
  
  1 - (sse / sst)
}

#################################################
#         PROCESAMIENTO DE DATOS                #
#################################################

file_path <- paste0(root, "02_datos/02_resultados/df_regiones.xlsx")
selected_regions <- c("NARINO", "CAUCA", "VALLE DEL CAUCA")
sector <- "Pacifico_sur"  # Triangulo_desarrollo, Pacifico_sur 

region_short <- setNames(
  substr(gsub(" ", "", selected_regions), 1, 3),
  selected_regions
)

#################################################
#             MODELO VARX (1)                   #
#################################################

df <- read_excel(file_path)

df_tri <- df %>%
  filter(region %in% selected_regions) %>%
  dplyr::select(year, region, graduados, activos_conocimiento) %>%
  arrange(year, region)

df_tri$region <- factor(df_tri$region, levels = selected_regions)

Y <- df_tri %>%
  dplyr::select(year, region, activos_conocimiento) %>%
  pivot_wider(names_from = region, values_from = activos_conocimiento) %>%
  arrange(year)

U <- df_tri %>%
  dplyr::select(year, region, graduados) %>%
  pivot_wider(names_from = region, values_from = graduados) %>%
  arrange(year)

years <- Y$year

Y_mat <- Y %>%
  dplyr::select(-year) %>%
  as.matrix()

U_mat <- U %>%
  dplyr::select(-year) %>%
  as.matrix()

colnames(Y_mat) <- selected_regions
colnames(U_mat) <- selected_regions

varx_model <- VAR(
  y = Y_mat,
  p = 1,
  type = "const",
  exogen = U_mat
)

#################################################
#             ECUACIONES VARX (1)               #
#################################################

equations <- list()
coef_list <- coef(varx_model)

for (reg in selected_regions) {
  reg_model <- gsub(" ", ".", reg)
  
  if (!(reg_model %in% names(coef_list))) {
    stop(paste("No se encontró la ecuación para la región:", reg,
               "| nombre buscado en el modelo:", reg_model))
  }
  
  coef_table <- coef_list[[reg_model]]
  
  eq_text <- build_equation(
    region_name = reg,
    coef_df = coef_table,
    regions_used = selected_regions,
    region_short_map = region_short
  )
  
  equations[[reg]] <- eq_text
}

#################################################
#             ESTIMADOS VARX (1)                #
#################################################

Y_hat <- fitted(varx_model)

fitted_df <- as.data.frame(Y_hat)
fitted_df$year <- years[-1]

observed_df <- as.data.frame(Y_mat)
colnames(observed_df) <- selected_regions
observed_df$year <- years

observed_aligned <- observed_df %>%
  filter(year %in% fitted_df$year)

names(fitted_df) <- gsub("\\.", " ", names(fitted_df))

plot_df <- observed_aligned %>%
  pivot_longer(
    cols = all_of(selected_regions),
    names_to = "region",
    values_to = "observado"
  ) %>%
  left_join(
    fitted_df %>%
      pivot_longer(
        cols = all_of(selected_regions),
        names_to = "region",
        values_to = "estimado"
      ),
    by = c("year", "region")
  ) %>%
  mutate(region = factor(region, levels = selected_regions))

g <- ggplot(plot_df, aes(x = year)) +
  geom_line(aes(y = observado, color = "Observado"), linewidth = 0.9) +
  geom_line(aes(y = estimado, color = "Estimado"), linewidth = 0.9, linetype = "dashed") +
  facet_wrap(~ region, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Observado" = "#1f77b4", "Estimado" = "#d62728")) +
  labs(
    title = "VARX(1): Observados vs Estimados por departamento",
    x = "Año",
    y = "Activos de conocimiento",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = paste0(root, "04_resultados/graficos/modelo_VARX/VARX_1_Obs_vs_est_triado_", sector, ".png"),
  plot = g,
  dpi = 600,
  width = 8,
  height = 5
)

#################################################
# VALIDACION DE SUPUESTOS DEL VARX Y CRITERIOS  #
#################################################

#-----------------------------------------------
# 1. RESIDUOS DEL MODELO
#-----------------------------------------------
residuos_varx <- residuals(varx_model)
residuos_df <- as.data.frame(residuos_varx)
residuos_df$year <- years[-1]

#-----------------------------------------------
# 2. NORMALIDAD MULTIVARIADA DEL SISTEMA
#-----------------------------------------------
normalidad_varx <- vars::normality.test(varx_model, multivariate.only = TRUE)

#-----------------------------------------------
# 3. AUTOCORRELACION SERIAL
#-----------------------------------------------
serial_varx <- vars::serial.test(varx_model, lags.pt = 10, type = "PT.asymptotic")

#-----------------------------------------------
# 4. HETEROCEDASTICIDAD ARCH
#-----------------------------------------------
arch_varx <- vars::arch.test(varx_model, lags.multi = 5, multivariate.only = TRUE)

#-----------------------------------------------
# 5. ESTABILIDAD DEL SISTEMA
#-----------------------------------------------
raices_varx <- roots(varx_model)
estable_varx <- all(Mod(raices_varx) < 1)

#-----------------------------------------------
# 6. CRITERIOS DE INFORMACION: AIC, BIC, HQ
#-----------------------------------------------
lag_selection <- vars::VARselect(
  y = Y_mat,
  lag.max = 2,
  type = "const",
  exogen = U_mat
)

criterios_lag <- as.data.frame(lag_selection$criteria)
criterios_lag$criterio <- rownames(criterios_lag)
rownames(criterios_lag) <- NULL

# selección óptima sugerida por cada criterio
lag_AIC <- unname(lag_selection$selection["AIC(n)"])
lag_BIC <- unname(lag_selection$selection["SC(n)"])
lag_HQ  <- unname(lag_selection$selection["HQ(n)"])

#-----------------------------------------------
# 7. MATRICES DE COVARIANZA Y CORRELACION DE RESIDUOS
#    (útiles para documentar el enfoque SUR)
#-----------------------------------------------
cov_residuos <- cov(residuos_varx, use = "complete.obs")
cor_residuos <- cor(residuos_varx, use = "complete.obs")

cov_residuos_df <- as.data.frame(round(cov_residuos, 6), check.names = FALSE)
cor_residuos_df <- as.data.frame(round(cor_residuos, 6), check.names = FALSE)

#-----------------------------------------------
# 8. NOTA METODOLOGICA SOBRE SUR Y FIML
#-----------------------------------------------
nota_metodologica <- tibble(
  elemento = c("SUR", "FIML"),
  descripcion = c(
    "En un VARX con las mismas regresoras en cada ecuación, la estimación ecuación por ecuación por MCO es equivalente a SUR.",
    "Bajo normalidad gaussiana, la estimación del sistema es consistente con máxima verosimilitud del sistema (ML/FIML)."
  ),
  uso = c(
    "Justifica tratar el VARX como sistema de ecuaciones relacionadas.",
    "Respalda la inferencia paramétrica cuando los residuos son aproximadamente normales."
  )
)

#-----------------------------------------------
# 9. TABLA RESUMEN DE SUPUESTOS DEL SISTEMA
#-----------------------------------------------
diagnostico_sistema_varx <- tibble(
  prueba = c(
    "Normalidad multivariada",
    "Autocorrelación serial Portmanteau",
    "Heterocedasticidad ARCH multivariada",
    "Estabilidad por raíces",
    "Rezago óptimo por AIC",
    "Rezago óptimo por BIC",
    "Rezago óptimo por Hannan-Quinn"
  ),
  estadistico = c(
    round(as.numeric(normalidad_varx$jb.mul$JB$statistic), 3),
    round(as.numeric(serial_varx$serial$statistic), 3),
    round(as.numeric(arch_varx$arch.mul$statistic), 3),
    NA,
    NA,
    NA,
    NA
  ),
  p_value = c(
    round(as.numeric(normalidad_varx$jb.mul$JB$p.value), 3),
    round(as.numeric(serial_varx$serial$p.value), 3),
    round(as.numeric(arch_varx$arch.mul$p.value), 3),
    NA,
    NA,
    NA,
    NA
  ),
  resultado = c(
    ifelse(normalidad_varx$jb.mul$JB$p.value > 0.05, "No se rechaza normalidad", "Se rechaza normalidad"),
    ifelse(serial_varx$serial$p.value > 0.05, "No se rechaza autocorrelación nula", "Se detecta autocorrelación"),
    ifelse(arch_varx$arch.mul$p.value > 0.05, "No se rechaza homocedasticidad", "Se detecta efecto ARCH"),
    ifelse(estable_varx, "Sistema estable", "Sistema inestable"),
    paste0("p = ", lag_AIC),
    paste0("p = ", lag_BIC),
    paste0("p = ", lag_HQ)
  )
)

diagnostico_sistema_varx <- as.data.frame(diagnostico_sistema_varx)



#################################################
#     GRAFICOS DE DIAGNOSTICO DEL SISTEMA VARX  #
#################################################

#-----------------------------------------------
# 1. Grafico de p-values de las pruebas
#-----------------------------------------------
df_pvalues <- tibble(
  prueba = c(
    "Normalidad multivariada",
    "Autocorrelación serial",
    "Heterocedasticidad ARCH"
  ),
  p_value = c(
    as.numeric(normalidad_varx$jb.mul$JB$p.value),
    as.numeric(serial_varx$serial$p.value),
    as.numeric(arch_varx$arch.mul$p.value)
  ),
  decision = c(
    ifelse(normalidad_varx$jb.mul$JB$p.value > 0.05, "No rechaza H0", "Rechaza H0"),
    ifelse(serial_varx$serial$p.value > 0.05, "No rechaza H0", "Rechaza H0"),
    ifelse(arch_varx$arch.mul$p.value > 0.05, "No rechaza H0", "Rechaza H0")
  )
)

g_pvalues <- ggplot(df_pvalues, aes(x = reorder(prueba, p_value), y = p_value, fill = decision)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "P-values de las pruebas de diagnóstico del sistema VARX",
    subtitle = "La línea roja punteada representa el umbral de significancia de 0.05",
    x = "",
    y = "p-value",
    fill = "Decisión"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_pvalues_sistema_", sector, ".png"),
  plot = g_pvalues,
  dpi = 600,
  width = 9,
  height = 5
)

#-----------------------------------------------
# 2. Grafico de estabilidad por raíces
#-----------------------------------------------
df_roots <- tibble(
  raiz = seq_along(raices_varx),
  modulo = Mod(raices_varx),
  cumple = ifelse(Mod(raices_varx) < 1, "Dentro del círculo unitario", "Fuera del círculo unitario")
)

g_roots <- ggplot(df_roots, aes(x = raiz, y = modulo, color = cumple)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Módulo de las raíces del sistema VARX",
    subtitle = "El sistema es estable si todas las raíces tienen módulo menor que 1",
    x = "Raíz",
    y = "Módulo",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_estabilidad_raices_", sector, ".png"),
  plot = g_roots,
  dpi = 600,
  width = 8,
  height = 5
)

#-----------------------------------------------
# 3. Grafico de criterios de informacion por rezago
#-----------------------------------------------
criteria_mat <- lag_selection$criteria

df_criteria <- data.frame(
  lag = as.numeric(colnames(criteria_mat)),
  AIC = as.numeric(criteria_mat["AIC(n)", ]),
  BIC = as.numeric(criteria_mat["SC(n)", ]),
  HQ  = as.numeric(criteria_mat["HQ(n)", ])
) %>%
  pivot_longer(
    cols = c(AIC, BIC, HQ),
    names_to = "criterio",
    values_to = "valor"
  )

g_criteria <- ggplot(df_criteria, aes(x = lag, y = valor, color = criterio)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  labs(
    title = "Criterios de información por número de rezagos",
    subtitle = "El rezago óptimo corresponde al menor valor del criterio",
    x = "Número de rezagos",
    y = "Valor del criterio",
    color = "Criterio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_criterios_informacion_", sector, ".png"),
  plot = g_criteria,
  dpi = 600,
  width = 9,
  height = 5
)

#-----------------------------------------------
# 4. Exportar tablas auxiliares de los graficos
#-----------------------------------------------
write_xlsx(
  list(
    pvalues_sistema = df_pvalues,
    raices_sistema = df_roots,
    criterios_info = as.data.frame(criteria_mat)
  ),
  path = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_graficos_diagnostico_", sector, ".xlsx")
)







#-----------------------------------------------
# 10. GUARDAR DIAGNOSTICOS PREVIOS A METRICAS
#-----------------------------------------------
write_xlsx(
  list(
    residuos = residuos_df,
    diagnostico_sistema = diagnostico_sistema_varx,
    criterios_informacion = criterios_lag,
    covarianza_residuos = cov_residuos_df,
    correlacion_residuos = cor_residuos_df,
    nota_SUR_FIML = nota_metodologica
  ),
  path = paste0(root, "04_resultados/graficos/modelo_VARX/VARX_diagnostico_previo_", sector, ".xlsx")
)

########################################
##########  METRICAS Y GRAFICOS  #######
########################################

#-------------------------------
# 1. PRUEBAS DEL SISTEMA VARX
#-------------------------------
# Ya fueron calculadas arriba para dejar la validación
# antes de este bloque:
# serial_varx
# arch_varx
# raices_varx
# estable_varx
# lag_selection
# residuos_varx

for (reg in selected_regions) {
  
  reg_model <- gsub(" ", ".", reg)
  
  if (!(reg_model %in% colnames(residuos_varx))) {
    warning(paste("No existe la columna de residuos para:", reg, 
                  "| nombre buscado:", reg_model))
    next
  }
  
  res_reg <- as.numeric(residuos_varx[, reg_model])
  years_res <- years[-1]
  
  ### AUTOCORRELACION
  g_acf <- forecast::ggAcf(res_reg) +
    labs(
      title = paste0("Función de autocorrelación de los residuos: ", reg),
      subtitle = "Evaluación de autocorrelación",
      x = "Rezago",
      y = "Autocorrelación"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_1_acf_residuos_", reg, ".png"),
    plot = g_acf,
    dpi = 600,
    width = 8,
    height = 5
  )
  
  ### MEDIA CERO
  df_res <- data.frame(
    tiempo = years_res,
    residuo = res_reg
  )
  
  g_media <- ggplot(df_res, aes(x = tiempo, y = residuo)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste0("Comportamiento temporal de los residuos del modelo VARX(1): ", reg),
      subtitle = "Evaluación de media cero",
      x = "Tiempo",
      y = "Residuo"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_1_media_cero_residuos_", reg, ".png"),
    plot = g_media,
    dpi = 600,
    width = 8,
    height = 5
  )
  
  ### HOMOCEDASTICIDAD
  g_homo <- ggplot(df_res, aes(x = tiempo, y = residuo)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste0("Comportamiento temporal de los residuos del modelo VARX(1): ", reg),
      subtitle = "Evaluación visual de homocedasticidad",
      x = "Tiempo",
      y = "Residuo"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_1_homocedasticidad_residuos_", reg, ".png"),
    plot = g_homo,
    dpi = 600,
    width = 10,
    height = 5
  )
  
  ### NORMALIDAD
  g_qq <- ggplot(data.frame(residuo = res_reg), aes(sample = residuo)) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = paste0("Gráfico Q-Q de los residuos: ", reg),
      subtitle = "Evaluación de normalidad",
      x = "Cuantiles teóricos",
      y = "Cuantiles muestrales"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_1_normalidad_residuos_", reg, ".png"),
    plot = g_qq,
    dpi = 600,
    width = 8,
    height = 5
  )
}

#-------------------------------
# 4. GRAFICO DE RAICES
#-------------------------------
df_roots <- data.frame(
  raiz = seq_along(raices_varx),
  modulo = Mod(raices_varx)
)

g_roots <- ggplot(df_roots, aes(x = raiz, y = modulo)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Módulo de las raíces del sistema VARX",
    subtitle = "Evaluación de estabilidad del sistema",
    x = "Raíz",
    y = "Módulo"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = paste0(root, "04_resultados/graficos/validacion_supuestos/VARX/VARX_1_estabilidad_raices.png"),
  plot = g_roots,
  dpi = 600,
  width = 8,
  height = 5
)

#-------------------------------
# 5. TABLA FINAL
#-------------------------------
tabla_final_varx <- tibble(
  region = selected_regions,
  ecuacion = sapply(selected_regions, function(reg) equations[[reg]]),
  Frobenius = sapply(selected_regions, function(reg) {
    x <- observed_aligned[[reg]]
    xhat <- fitted_df[[reg]]
    frobenius_ratio(x, xhat)
  }),
  Rcuadrado = sapply(selected_regions, function(reg) {
    df_reg <- plot_df %>% filter(region == reg)
    calc_r2(df_reg$observado, df_reg$estimado)
  }),
  mean_res = sapply(selected_regions, function(reg) {
    reg_model <- gsub(" ", ".", reg)
    if (reg_model %in% colnames(residuos_varx)) {
      mean(residuos_varx[, reg_model], na.rm = TRUE)
    } else {
      NA_real_
    }
  }),
  shapiro = sapply(selected_regions, function(reg) {
    reg_model <- gsub(" ", ".", reg)
    if (reg_model %in% colnames(residuos_varx)) {
      shapiro.test(as.numeric(residuos_varx[, reg_model]))$p.value
    } else {
      NA_real_
    }
  })
) %>%
  mutate(
    Frobenius = round(Frobenius, 2),
    Rcuadrado = round(Rcuadrado, 2),
    mean_res = round(mean_res, 4),
    shapiro = round(shapiro, 4),
    Portmanteau = round(serial_varx$serial$p.value, 4),
    ARCH = round(arch_varx$arch.mul$p.value, 4),
    Normalidad_multivar = round(normalidad_varx$jb.mul$p.value, 4),
    Estable = estable_varx,
    AIC_lag = lag_AIC,
    BIC_lag = lag_BIC,
    HQ_lag = lag_HQ,
    SUR_equiv = "Sí",
    FIML_equiv_bajo_normalidad = "Sí"
  )

print(tabla_final_varx)

write_xlsx(
  tabla_final_varx,
  paste0(root, "04_resultados/graficos/modelo_VARX/VARX_", sector, ".xlsx")
)

#############################################################################
#################################################
#   MATRICES DE INFLUENCIA Y VECINDAD - VARX    #
#################################################

#---------------------------------------------
# 1. Matriz de influencia directa del VARX(1)
#---------------------------------------------
build_varx_influence_matrix <- function(coef_list, regions_used) {
  
  M <- matrix(
    NA_real_,
    nrow = length(regions_used),
    ncol = length(regions_used),
    dimnames = list(regions_used, regions_used)
  )
  
  for (target in regions_used) {
    target_model <- gsub(" ", ".", target)
    coef_df <- coef_list[[target_model]]
    
    for (source in regions_used) {
      coef_name <- paste0(gsub(" ", ".", source), ".l1")
      
      if (coef_name %in% rownames(coef_df)) {
        M[target, source] <- coef_df[coef_name, "Estimate"]
      } else {
        M[target, source] <- NA_real_
      }
    }
  }
  
  return(M)
}

matriz_influencia_varx <- build_varx_influence_matrix(
  coef_list = coef_list,
  regions_used = selected_regions
)

#---------------------------------------------
# 2. Matriz de influencia absoluta
#---------------------------------------------
matriz_influencia_abs <- abs(matriz_influencia_varx)

#---------------------------------------------
# 3. Matriz binaria de influencia
#---------------------------------------------
matriz_influencia_binaria <- ifelse(
  is.na(matriz_influencia_varx),
  NA,
  ifelse(matriz_influencia_varx != 0, 1, 0)
)

#---------------------------------------------
# 4. Resumen de influencia neta por región
#---------------------------------------------
resumen_influencia <- tibble(
  region = selected_regions,
  influencia_emitida = round(colSums(matriz_influencia_abs, na.rm = TRUE), 4),
  influencia_recibida = round(rowSums(matriz_influencia_abs, na.rm = TRUE), 4)
)

#---------------------------------------------
# 5. Guardar resultados en Excel
#---------------------------------------------
matriz_influencia_varx <- as.data.frame(round(matriz_influencia_varx, 4), check.names = FALSE)
matriz_influencia_abs  <- as.data.frame(round(matriz_influencia_abs, 4), check.names = FALSE)

write_xlsx(
  matriz_influencia_varx,
  paste0(root, "04_resultados/graficos/modelo_VARX/influencia_VARX_", sector, ".xlsx")
)

write_xlsx(
  matriz_influencia_abs,
  paste0(root, "04_resultados/graficos/modelo_VARX/matriz_influencia_abs_VARX_", sector, ".xlsx")
)

write_xlsx(
  resumen_influencia,
  paste0(root, "04_resultados/graficos/modelo_VARX/resumen_influencia_", sector, ".xlsx")
)

#################################################
#        FUNCION IMPULSO - RESPUESTA (IRF)      #
#################################################

#---------------------------------------------
# 1. Configuración
#---------------------------------------------
horizonte_irf <- 10

selected_regions_model <- gsub(" ", ".", selected_regions)

irf_result <- vars::irf(
  varx_model,
  impulse = selected_regions_model,
  response = selected_regions_model,
  n.ahead = horizonte_irf,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95
)

irf_to_df <- function(irf_obj) {
  
  impulses <- names(irf_obj$irf)
  out_list <- list()
  
  for (imp in impulses) {
    
    mat_irf   <- irf_obj$irf[[imp]]
    mat_lower <- irf_obj$Lower[[imp]]
    mat_upper <- irf_obj$Upper[[imp]]
    
    responses <- colnames(mat_irf)
    horizons  <- 0:(nrow(mat_irf) - 1)
    
    df_imp <- do.call(rbind, lapply(responses, function(resp) {
      data.frame(
        impulse   = imp,
        response  = resp,
        horizonte = horizons,
        irf       = mat_irf[, resp],
        lower     = mat_lower[, resp],
        upper     = mat_upper[, resp],
        stringsAsFactors = FALSE
      )
    }))
    
    out_list[[imp]] <- df_imp
  }
  
  df_final <- dplyr::bind_rows(out_list)
  df_final$impulse  <- gsub("\\.", " ", df_final$impulse)
  df_final$response <- gsub("\\.", " ", df_final$response)
  
  return(df_final)
}

df_irf <- irf_to_df(irf_result)

g_irf <- ggplot(df_irf, aes(x = horizonte, y = irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(response ~ impulse, scales = "free_y") +
  labs(
    title = "Funciones Impulso-Respuesta del modelo VARX",
    subtitle = "Respuesta de cada región ante shocks en las demás",
    x = "Horizonte",
    y = "Respuesta impulsiva"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(g_irf)

ggsave(
  filename = paste0(root, "04_resultados/graficos/modelo_VARX/Funcion_impulso_", sector, ".png"),
  plot = g_irf,
  width = 12,
  height = 10,
  dpi = 600
)