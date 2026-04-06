gc(); rm(list = ls())

# =========================
# 1. Cargar librerías
# =========================
library(geodata)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)

# =========================
# 2. Descargar shapefile de Colombia (departamentos)
# =========================
colombia_dep <- geodata::gadm(country = "COL", level = 1, path = tempdir())

# =========================
# 3. Convertir a formato sf
# =========================
colombia_sf <- st_as_sf(colombia_dep)

# Verificar nombres
print(unique(colombia_sf$NAME_1))

# =========================
# 4. Crear variable con orden en la leyenda
# =========================
colombia_sf <- colombia_sf %>%
  mutate(
    grupo = case_when(
      NAME_1 == "Bogotá D.C." ~ "Bogotá D.C.",
      NAME_1 == "Antioquia" ~ "Antioquia",
      NAME_1 == "Valle del Cauca" ~ "Valle del Cauca",
      NAME_1 == "Nariño" ~ "Nariño",
      NAME_1 == "Cauca" ~ "Cauca",
      TRUE ~ "Otros"
    ),
    grupo = factor(
      grupo,
      levels = c(
        "Bogotá D.C.",
        "Antioquia",
        "Valle del Cauca",
        "Nariño",
        "Cauca",
        "Otros"
      )
    )
  )

# =========================
# 5. Recortar el mapa para reducir espacios en blanco
#    (ajusta estos valores si quieres acercar o alejar más)
# =========================
bbox_crop <- st_bbox(c(
  xmin = -79.5,
  xmax = -66.5,
  ymin = -4.8,
  ymax = 13.8
), crs = st_crs(colombia_sf))

colombia_crop <- st_crop(colombia_sf, bbox_crop)

# =========================
# 6. Crear mapa mejorado
# =========================
mapa <- ggplot(colombia_crop) +
  geom_sf(aes(fill = grupo), color = "white", linewidth = 0.25) +
  scale_fill_manual(
    values = c(
      "Bogotá D.C." = "#E41A1C",
      "Antioquia" = "#377EB8",
      "Valle del Cauca" = "#4DAF4A",
      "Nariño" = "#984EA3",
      "Cauca" = "#FF7F00",
      "Otros" = "grey85"
    ),
    breaks = c(
      "Bogotá D.C.",
      "Antioquia",
      "Valle del Cauca",
      "Nariño",
      "Cauca",
      "Otros"
    ),
    name = "Departamentos"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Departamentos de interés para el estudio",
    subtitle = "Distribución geográfica de los departamentos analizados"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(8, 8, 8, 8)
  )

# =========================
# 7. Mostrar mapa
# =========================
print(mapa)

# =========================
# 8. Guardar imagen en alta calidad
# =========================
ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/descriptivas/mapa_departamentos_colombia.png",
  plot = mapa,
  width = 7.5,
  height = 6.5,
  units = "in",
  dpi = 600,
  bg = "white"
)
# =========================
# =========================
# =========================
# =========================
# =========================
# =========================

gc(); rm(list = ls())

# =========================
# 1. Cargar librerías
# =========================
# install.packages(c("geodata", "terra", "sf", "ggplot2", "dplyr", "cowplot"))

library(geodata)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(cowplot)

# =========================
# 2. Descargar shapefile de Colombia (departamentos)
# =========================
colombia_dep <- geodata::gadm(country = "COL", level = 1, path = tempdir())

# =========================
# 3. Convertir a formato sf
# =========================
colombia_sf <- st_as_sf(colombia_dep)

# Verificar nombres disponibles
print(unique(colombia_sf$NAME_1))

# =========================
# 4. Recortar el mapa para reducir espacios en blanco
# =========================
bbox_crop <- st_bbox(c(
  xmin = -79.5,
  xmax = -66.5,
  ymin = -4.8,
  ymax = 13.8
), crs = st_crs(colombia_sf))

colombia_crop <- st_crop(colombia_sf, bbox_crop)

# =========================
# 5. Crear bases para los dos mapas
# =========================
mapa_tri <- colombia_crop %>%
  mutate(
    region = case_when(
      NAME_1 == "Bogotá D.C." ~ "Bogotá D.C.",
      NAME_1 == "Antioquia" ~ "Antioquia",
      NAME_1 == "Valle del Cauca" ~ "Valle del Cauca",
      TRUE ~ "Otros"
    ),
    region = factor(
      region,
      levels = c(
        "Bogotá D.C.",
        "Antioquia",
        "Valle del Cauca",
        "Nariño",
        "Cauca",
        "Otros"
      )
    )
  )

mapa_pacifico <- colombia_crop %>%
  mutate(
    region = case_when(
      NAME_1 == "Nariño" ~ "Nariño",
      NAME_1 == "Valle del Cauca" ~ "Valle del Cauca",
      NAME_1 == "Cauca" ~ "Cauca",
      TRUE ~ "Otros"
    ),
    region = factor(
      region,
      levels = c(
        "Bogotá D.C.",
        "Antioquia",
        "Valle del Cauca",
        "Nariño",
        "Cauca",
        "Otros"
      )
    )
  )

# =========================
# 6. Paleta única
# =========================
colores_regiones <- c(
  "Bogotá D.C." = "#E41A1C",
  "Antioquia" = "#377EB8",
  "Valle del Cauca" = "#4DAF4A",
  "Nariño" = "#984EA3",
  "Cauca" = "#FF7F00",
  "Otros" = "grey85"
)

orden_leyenda <- c(
  "Bogotá D.C.",
  "Antioquia",
  "Valle del Cauca",
  "Nariño",
  "Cauca",
  "Otros"
)

# =========================
# 7. Crear mapa 1 SIN leyenda
# =========================
p1 <- ggplot(mapa_tri) +
  geom_sf(aes(fill = region), color = "white", linewidth = 0.25) +
  scale_fill_manual(
    values = colores_regiones,
    breaks = orden_leyenda,
    drop = FALSE
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Triángulo de Desarrollo",
    subtitle = "Bogotá D.C., Antioquia y Valle del Cauca"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(8, 8, 8, 8)
  )

# =========================
# 8. Crear mapa 2 SIN leyenda
# =========================
p2 <- ggplot(mapa_pacifico) +
  geom_sf(aes(fill = region), color = "white", linewidth = 0.25) +
  scale_fill_manual(
    values = colores_regiones,
    breaks = orden_leyenda,
    drop = FALSE
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Región Pacífico Sur",
    subtitle = "Nariño, Valle del Cauca y Cauca"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(8, 8, 8, 8)
  )

# =========================
# 9. Crear una leyenda bonita aparte
# =========================
plot_leyenda <- ggplot(
  data.frame(region = factor(orden_leyenda, levels = orden_leyenda)),
  aes(x = region, y = 1, fill = region)
) +
  geom_tile() +
  scale_fill_manual(
    values = colores_regiones,
    breaks = orden_leyenda,
    name = "Departamentos:",
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 13),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.x = unit(0.4, "cm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left",
      title.hjust = 0.5
    )
  )

leyenda_unica <- cowplot::get_legend(plot_leyenda)

# =========================
# 10. Unir los dos mapas
# =========================
mapas_superiores <- cowplot::plot_grid(
  p1, p2,
  ncol = 2,
  align = "h",
  rel_widths = c(1, 1)
)

# =========================
# 11. Figura final: mapas + una sola leyenda
# =========================
mapa_final <- cowplot::plot_grid(
  mapas_superiores,
  leyenda_unica,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

# Mostrar resultado
print(mapa_final)

# =========================
# 12. Guardar en PNG
# =========================
ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/descriptivas/mapas_regiones_colombia.png",
  plot = mapa_final,
  width = 14,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

# =========================
# 13. Guardar en TIFF
# =========================
ggsave(
  filename = "C:/Users/braya/Documents/TESIS_MAESTRIA/04_resultados/graficos/descriptivas/mapas_regiones_colombia.tiff",
  plot = mapa_final,
  width = 14,
  height = 8,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

