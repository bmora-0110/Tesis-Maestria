# ============================================================
# Brayan Mora y Arturo Melo 
# Modelos ARMAX(2,2)
# y_t = activos_conocimiento
# u_t = graduados
# ============================================================

gc();rm(list = ls())
require(pacman)
p_load(readr, dplyr, ggplot2, stringr, forecast, tibble,FinTS,tseries,writexl,readxl)

root <- "C:/Users/braya/Documents/TESIS_MAESTRIA/"

#################################################
#             FUNCIONES AUXILIARES              #
#################################################

calc_frobenius_rel <- function(y, yhat) {
  ok <- complete.cases(y, yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  
  if (length(y) == 0) return(NA_real_)
  
  num <- sqrt(sum((y - yhat)^2))
  den <- sqrt(sum(y^2))
  
  if (!is.finite(den) || den == 0) return(NA_real_)
  num / den
}

fmt_num <- function(x) sprintf("%.4f", x)

make_term <- function(value, label) {
  paste0(ifelse(value >= 0, " + ", " - "), fmt_num(abs(value)), " ", label)
}

safe_filename <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_+", "_") |>
    str_replace_all("^_|_$", "")
}

get_coef_safe <- function(coefs, name) {
  if (name %in% names(coefs)) as.numeric(coefs[name]) else 0
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

armax_tesis <- function(data, region_objetivo){
  
  df <- data %>%
    dplyr::filter(region == region_objetivo) %>%
    dplyr::arrange(year) %>%
    dplyr::select(year, region, graduados, activos_conocimiento)
  
  if (nrow(df) < 8) {
    stop("No hay suficientes datos para ajustar los modelos.")
  }
  
  #################################################
  #             MOdelo ARMAX (2,2)                #
  #################################################
  
  work_armax <- df %>%
    dplyr::mutate(
      y = activos_conocimiento,
      u_t = graduados,
      u_t_1 = dplyr::lag(graduados, 1),
      u_t_2 = dplyr::lag(graduados, 2)
    ) %>%
    dplyr::filter(complete.cases(y, u_t, u_t_1, u_t_2))
  
  y_work <- ts(work_armax$y, start = min(work_armax$year), frequency = 1)
  xreg_work <- as.matrix(work_armax[, c("u_t", "u_t_1", "u_t_2")])
  
  fit_armax <- forecast::Arima(
    y = y_work,
    order = c(2, 0, 2),
    xreg = xreg_work,
    include.mean = FALSE,
    method = "ML"
  )
  
  
  
  #################################################
  #             Ecuacion ARMAX (2,2)              #
  #################################################
  coefs_armax <- coef(fit_armax)
  
  phi1   <- get_coef_safe(coefs_armax, "ar1")
  phi2   <- get_coef_safe(coefs_armax, "ar2")
  theta1 <- get_coef_safe(coefs_armax, "ma1")
  theta2 <- get_coef_safe(coefs_armax, "ma2")
  
  b0 <- get_coef_safe(coefs_armax, "u_t")
  b1 <- get_coef_safe(coefs_armax, "u_t_1")
  b2 <- get_coef_safe(coefs_armax, "u_t_2")
  
  ecuacion_armax <- paste0(
    "y_t = ", fmt_num(phi1), " y_{t-1}",
    make_term(phi2, "y_{t-2}"),
    make_term(b0, "u_t"),
    make_term(b1, "u_{t-1}"),
    make_term(b2, "u_{t-2}"),
    " + e_t",
    make_term(theta1, "e_{t-1}"),
    make_term(theta2, "e_{t-2}")
  )
  
  cat("ECUACION ARMAX(2,2):\n")
  cat(ecuacion_armax, "\n\n")
  
  #################################################
  #             Estimados ARMAX (2,2)            #
  #################################################
  work_armax$estimado_armax <- as.numeric(fitted(fit_armax))
  
  df_plot_armax <- df %>%
    dplyr::left_join(
      work_armax %>% dplyr::select(year, estimado_armax),
      by = "year"
    ) %>%
    dplyr::mutate(
      observado = activos_conocimiento
    )
  
  
  
  #################################################
  #             GRAFICO ARMAX (2,2)               #
  #################################################
  g_armax <- ggplot(df_plot_armax, aes(x = year)) +
    geom_line(aes(y = observado, color = "Observado"), linewidth = 1) +
    geom_point(aes(y = observado, color = "Observado"), size = 2) +
    geom_line(
      aes(y = estimado_armax, color = "Estimado"),
      linewidth = 1,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    geom_point(
      aes(y = estimado_armax, color = "Estimado"),
      size = 2,
      na.rm = TRUE
    ) +
    labs(
      title = paste("ARMAX(2,2) -", region_objetivo),
      subtitle = "Activos de conocimiento: Observado vs Estimado",
      x = "Año",
      y = "Activos de conocimiento",
      color = "Serie"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  
  
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/modelo_ARMAX/ARMAX_2_Obs_vs_est_",region_objetivo,".png"),
    plot = g_armax,
    dpi = 600,
    width = 8,
    height = 5
  )
  
  ########################################
  ##########  METRICAS Y GRAFICOS  #######
  ########################################
  
  frob_armax <- calc_frobenius_rel(df_plot_armax$observado, df_plot_armax$estimado_armax)
  r2_armax <- calc_r2(df_plot_armax$observado, df_plot_armax$estimado_armax)
  
  ### AUROCORRELACION 
  ljung_box <- Box.test(residuals(fit_armax), lag = 10, type = "Ljung-Box")
  acf <- forecast::ggAcf(residuals(fit_armax)) +
    ggplot2::labs(
      title = paste0("Función de autocorrelación de los residuos: ", region_objetivo),
      subtitle = "Evaluación de autocorrelación",
      x = "Rezago",
      y = "Autocorrelación"
    )
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/ARMAX/ARMAX_2_acf_residuos_",region_objetivo,".png"),
    plot = acf,
    
    dpi = 600,
    width = 6,
    height = 3
  )
  
  ### MEdia cero 
  mean_res <- mean(residuals(fit_armax), na.rm = TRUE)
  
  
  g_residuos <- data.frame(
    tiempo = time(residuals(fit_armax)),
    residuo = residuals(fit_armax)
  )
  
  gg <- ggplot(g_residuos, aes(x = tiempo, y = residuo)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste0("Residuos del modelo ARMAX(2,2): ", region_objetivo),
      subtitle = "Evaluación de media cero",
      x = "Tiempo",
      y = "Residuo"
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/ARMAX/ARMAX_2_media_cero_residuos_",region_objetivo,".png"),
    plot = gg,
    dpi = 600,
    width = 6,
    height = 3
  )
  
  #####  HOMOCEDASTICIDAD 
  
  g_var <- data.frame(
    tiempo = time(residuals(fit_armax)),
    residuo = residuals(fit_armax)
  )
  
  homo <- ggplot(g_var, aes(x = tiempo, y = residuo)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste0("Residuos del modelo ARMAX(2,2):",region_objetivo),
      subtitle = "Evaluación de homocedasticidad",
      x = "Tiempo",
      y = "Residuo"
    ) +
    theme_minimal()
  
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/ARMAX/ARMAX_2_homocedasticidad_residuos_",region_objetivo,".png"),
    plot = homo,
    dpi = 600,
    width = 6,
    height = 3
  )
  
  
  arch=ArchTest(residuals(fit_armax))
  
  ################ Normalidad de los residuos
  
  sh <- shapiro.test(residuals(fit_armax))
  
  qq_data <- data.frame(
    residuo = residuals(fit_armax)
  )
  
  g_qq <- ggplot(qq_data, aes(sample = residuo)) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = paste0("Gráfico Q-Q de los residuos: ", region_objetivo),
      subtitle = "Evaluación de normalidad",
      x = "Cuantiles teóricos",
      y = "Cuantiles muestrales"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  
  
  ggsave(
    filename = paste0(root, "04_resultados/graficos/validacion_supuestos/ARMAX/ARMAX_2_normalidad_residuos_",region_objetivo,".png"),
    plot = g_qq,
    dpi = 600,
    width = 6,
    height = 3
  )
  
  
  
  tabla_final_armax <- data.frame(
    region = region_objetivo,
    ecuacion   = ecuacion_armax,
    Frobenius  = round(frob_armax, 2),
    Rcuadrado  = round(r2_armax, 2),
    AIC= round(AIC(fit_armax),2),
    Ljung_box =  ljung_box$p.value,
    mean_res = mean_res,
    ARCH = arch$p.value,
    shapiro = sh$p.value
  )
  
  return(tabla_final_armax)
  
}

df <- read_excel(paste0(root,"02_datos/02_resultados/df_regiones.xlsx"))
departamentos <- c("ANTIOQUIA", "BOGOTA D C","VALLE DEL CAUCA","CAUCA","NARINO")

res <- lapply(1: length(departamentos), function(i){
  cat(paste0("Procesando departamento : ",departamentos[i], "\n"))
  armax_tesis(data = df, region_objetivo = departamentos[i])
  
})

df <- do.call(rbind, res)
row.names(df) <- 1:nrow(df)
write_xlsx(df, paste0(root, "04_resultados/graficos/modelo_ARMAX/armax_2_2.xlsx"))


res <- read_excel("C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/modelo_ARMAX/armax_2_2.xlsx")

res <- as.data.frame(res)
colnames(res)

val_sup <- res[,c("region","Ljung_box", "mean_res","ARCH","shapiro")]
colnames(val_sup) <- c("departamento","Ljung_box", "mean_res","ARCH","shapiro_wilk")




