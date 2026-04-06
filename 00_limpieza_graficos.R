# ------------------------------------------------------------
# Limpieza de base: PI y graduados STEM por categoria 2.xlsx
# ------------------------------------------------------------

g=gc();rm(list=ls())
require(pacman)
p_load(readxl, dplyr,stringr ,stringi,writexl, tidyr,ggplot2 )

in_path  <- "C:/Users/braya/Documents/TESIS_MAESTRIA/02_datos/01_inputs/graduados_imputados_backfill.xlsx"
out_dir  <- "D:/OneDrive - CGIAR/Desktop/TESIS_MAESTRIA/02_datos/01_inputs"
out_path <- file.path(out_dir, "data_base_PI_gaduados_clean.xlsx")

df <- read_excel(in_path)
df<- df[, c("year","region","graduados", "activos_conocimiento","graduados_bfill" )]

df$graduados <- df$graduados_bfill 
df$graduados_bfill <- NULL


regions_keep <- c(
  "BOGOTA D C", "ANTIOQUIA",
  "NARINO", "VALLE DEL CAUCA", "CAUCA")

df_plot <- filter(df,region %in% regions_keep) 
gg <-ggplot(df_plot, aes(x = year, y = graduados, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Evolución de graduados en educación superior por departamento",
    x = "Año",
    y = "Graduados (total)",
    color = "Departamento"   
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

gg

ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/grafico_graduados.png",
  plot = gg,
  width = 8,
  height = 6,
  dpi = 600
)


gg1<-ggplot(df_plot, aes(x = year, y = activos_conocimiento, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Evolución de activos de conocimiento por departamento",
    x = "Año",
    y = "Activos de conocimiento (total)",
    color = "Departamento"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/grafico_activos.png",
  plot = gg1,
  width = 8,
  height = 6,
  dpi = 600
)



gg2 <- ggplot(df_plot, aes(x = graduados, y = activos_conocimiento, color = year)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  facet_wrap(~ region, scales = "free") +
  labs(
    title = "Relación graduados vs activos (coloreado por año)",
    x = "Graduados (total)",
    y = "Activos de conocimiento (total)",
    color = "Año"
  ) +
  theme_minimal()

ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/grafico_activos_vs_graduados.png",
  plot = gg2,
  width = 8,
  height = 6,
  dpi = 600
)


######################################################################################


df_plot <- df %>%
  filter(region %in% regions_keep) %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(
    graduados_idx = (graduados - min(graduados, na.rm=TRUE)) /
      (max(graduados, na.rm=TRUE) - min(graduados, na.rm=TRUE)),
    activos_idx   = (activos_conocimiento - min(activos_conocimiento, na.rm=TRUE)) /
      (max(activos_conocimiento, na.rm=TRUE) - min(activos_conocimiento, na.rm=TRUE))
  ) %>%
  ungroup() %>%
  dplyr::select(region, year, graduados_idx, activos_idx) %>%
  pivot_longer(cols = c(graduados_idx, activos_idx),
               names_to = "serie", values_to = "valor")

gg3 <- ggplot(df_plot, aes(x = year, y = valor, color = serie)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ region, scales = "free_x") +
  labs(
    title = "Graduados vs Activos de conocimiento (series normalizadas 0-1) por región",
    x = "Año",
    y = "Indice normalizado (0-1)",
    color = "Serie"
  ) +
  theme_minimal()



ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/grafico_series_normalizadas.png",
  plot = gg3,
  width = 10,
  height = 7,
  dpi = 600
)




# 8) Guardar resultado
writexl::write_xlsx(df,"C:/Users/braya/Documents/TESIS_MAESTRIA/02_datos/02_resultados/df_regiones.xlsx" )


