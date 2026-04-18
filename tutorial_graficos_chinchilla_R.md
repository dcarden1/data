# Tutorial: Gráficos e Imágenes en R para el Informe Final
## Estudio Ecológico de Guaridas de Chinchilla de Cola Corta — Proyecto SADDN

> Este tutorial explica paso a paso cómo generar **todas las figuras y gráficos** que acompañan los resultados del informe `informe_final_v2_SADDN.docx` y la presentación `Presentación1_resultados_chinchilla_1.pdf`, usando R y los datos del repositorio. Incluye **23 figuras** más variantes y un panel resumen.

---

## 0. Configuración inicial

### 0.1 Paquetes necesarios

```r
# Instalar si es necesario
install.packages(c(
  "ggplot2",        # Gráficos principales
  "dplyr",          # Manipulación de datos
  "tidyr",          # Pivotar datos
  "sf",             # Datos espaciales
  "terra",          # Rasters y DEM
  "elevatr",        # Descarga de DEM
  "unmarked",       # Modelos Royle-Nichols
  "AICcmodavg",     # Selección de modelos
  "viridis",        # Paletas de color accesibles
  "ggrepel",        # Etiquetas sin solapamiento
  "patchwork",      # Combinar paneles de gráficos
  "scales",         # Formateo de ejes
  "corrplot",       # Matriz de correlaciones
  "gt",             # Tablas formateadas de alta calidad
  "cluster",        # Análisis de clusters (silhouette)
  "factoextra",     # Visualización de clusters y método del codo
  "rosm",           # Basemaps satelitales (tiles OpenStreetMap/Stamen)
  "ggspatial"       # Anotaciones espaciales (escala, norte)
))

# Cargar todos
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(elevatr)
library(unmarked)
library(AICcmodavg)
library(viridis)
library(ggrepel)
library(patchwork)
library(scales)
library(corrplot)
library(gt)
library(cluster)
library(factoextra)
library(rosm)
library(ggspatial)
```

### 0.2 Tema gráfico personalizado para el informe

```r
# Tema limpio estilo publicación científica
theme_informe <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Colores consistentes para los 3 tipos de guarida
colores_tipo <- c(
  "arte_norte" = "#2166AC",   # azul
  "arte_sur"   = "#B2182B",   # rojo
  "natural"    = "#4DAF4A"    # verde
)

# Etiquetas bonitas para los tipos
etiquetas_tipo <- c(
  "arte_norte" = "Artificial Norte (+)",
  "arte_sur"   = "Artificial Sur (−)",
  "natural"    = "Natural (ID)"
)
```

### 0.3 Cargar los datos

```r
# Datos geomorfológicos procesados (versión 2, desde DEM)
datos <- read.csv2(
  "Datos_Chinchilla_SADDN_Geomorfologicos_v2.csv",
  stringsAsFactors = FALSE
)

# Convertir columnas numéricas (separador decimal = coma)
cols_num <- c("tasa_ocupacion", "delta_t_media", "elevacion_dem",
              "pendiente_dem", "rugosidad_dem", "tpi_dem",
              "radiacion_dem", "aspecto_dem", "UTM_Este", "UTM_Norte")
for (col in cols_num) {
  datos[[col]] <- as.numeric(gsub(",", ".", datos[[col]]))
}

# Tasa de ocupación como proporción (0–1)
datos$tasa_ocup <- datos$tasa_ocupacion / 100

# Factor ordenado para tipo
datos$tipo <- factor(datos$tipo,
                     levels = c("arte_norte", "arte_sur", "natural"))

# Datos crudos (presencia semanal + temperatura)
datos_raw <- read.csv2("Datos Chinchilla SADDN.csv",
                       stringsAsFactors = FALSE)
```

---

## 1. Figura 1 — Boxplot de tasa de ocupación por tipo de guarida

> **→ Resultado del informe:** Sección 4.1, Tabla 4-1 — "Las tasas de ocupación difirieron significativamente entre los tres tipos (H = 30,955; p < 0,001)"

```r
fig1 <- ggplot(datos, aes(x = tipo, y = tasa_ocup, fill = tipo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, width = 0.6) +
  geom_jitter(aes(color = tipo), width = 0.15, alpha = 0.5, size = 2) +
  scale_fill_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_color_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_x_discrete(labels = etiquetas_tipo) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Tasa de ocupación por tipo de guarida",
    subtitle = "Kruskal-Wallis: H = 30,955; p < 0,001 | n = 103 guaridas, 34 semanas",
    x = "Tipo de guarida",
    y = "Tasa de ocupación (%)",
    fill = "Tipo", color = "Tipo"
  ) +
  # Añadir líneas de significancia
  annotate("segment", x = 1, xend = 3, y = 0.97, yend = 0.97) +
  annotate("text", x = 2, y = 0.99, label = "*** p < 0.001", size = 3.5) +
  annotate("segment", x = 2, xend = 3, y = 0.92, yend = 0.92) +
  annotate("text", x = 2.5, y = 0.94, label = "*** p < 0.001", size = 3.5) +
  annotate("segment", x = 1, xend = 2, y = 0.87, yend = 0.87) +
  annotate("text", x = 1.5, y = 0.89, label = "n.s. p = 0.269", size = 3.5) +
  theme_informe

# Guardar
ggsave("fig1_boxplot_ocupacion.png", fig1, width = 8, height = 6, dpi = 300)
```

### Variante: Violin plot (muestra mejor la distribución)

```r
fig1b <- ggplot(datos, aes(x = tipo, y = tasa_ocup, fill = tipo)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_x_discrete(labels = etiquetas_tipo) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribución de tasa de ocupación por tipo de guarida",
    x = "Tipo de guarida",
    y = "Tasa de ocupación (%)"
  ) +
  theme_informe + theme(legend.position = "none")

ggsave("fig1b_violin_ocupacion.png", fig1b, width = 8, height = 6, dpi = 300)
```

---

## 2. Figura 2 — Variación estacional de la ocupación por tipo

> **→ Resultado del informe:** Sección 4.2, Tabla 4-3 — "ocupación mínima en agosto (norte: 0,426) y máxima en enero (norte: 0,795)"

### 2.1 Preparar datos mensuales

```r
# Columnas de presencia por mes
meses <- c("AGO", "SEP", "OCT", "NOV", "DIC", "ENE", "FEB", "MAR")

# Asignar tipo a datos_raw
datos_raw$tipo <- ifelse(grepl("^ID", datos_raw$ID_Guarida), "natural",
                  ifelse(grepl("\\+", datos_raw$ID_Guarida), "arte_norte",
                         "arte_sur"))

# Calcular ocupación mensual por guarida
for (mes in meses) {
  cols_mes <- grep(paste0("^", mes, "_S.*_Pres|^", mes, "_.*_Pres"),
                   names(datos_raw), value = TRUE)
  datos_raw[[paste0(mes, "_ocup")]] <- rowMeans(
    datos_raw[, cols_mes, drop = FALSE], na.rm = TRUE
  )
}

# Tabla resumen por tipo y mes
ocup_mensual <- datos_raw %>%
  select(tipo, ends_with("_ocup")) %>%
  pivot_longer(-tipo, names_to = "mes", values_to = "ocupacion") %>%
  mutate(mes = gsub("_ocup", "", mes),
         mes = factor(mes, levels = meses)) %>%
  group_by(tipo, mes) %>%
  summarise(
    media = mean(ocupacion, na.rm = TRUE),
    se = sd(ocupacion, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ocup_mensual$tipo <- factor(ocup_mensual$tipo,
                             levels = c("arte_norte", "arte_sur", "natural"))
```

### 2.2 Gráfico de líneas con barras de error

```r
fig2 <- ggplot(ocup_mensual, aes(x = mes, y = media,
                                  color = tipo, group = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, linewidth = 0.6) +
  scale_color_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Variación estacional de la tasa de ocupación",
    subtitle = "Media ± error estándar por tipo de guarida | agosto 2025 – marzo 2026",
    x = "Mes",
    y = "Tasa de ocupación promedio",
    color = "Tipo de guarida"
  ) +
  # Añadir etiquetas de estación
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 1,
           alpha = 0.05, fill = "blue") +
  annotate("rect", xmin = 4.5, xmax = 7.5, ymin = 0, ymax = 1,
           alpha = 0.05, fill = "red") +
  annotate("text", x = 1.5, y = 0.02, label = "Invierno",
           size = 3, color = "blue", fontface = "italic") +
  annotate("text", x = 6, y = 0.02, label = "Verano",
           size = 3, color = "red", fontface = "italic") +
  theme_informe

ggsave("fig2_estacionalidad_ocupacion.png", fig2, width = 10, height = 6, dpi = 300)
```

---

## 3. Figura 3 — Diferencial térmico (DeltaT) vs. tasa de ocupación

> **→ Resultado del informe:** Sección 4.3 — "rs = −0,372 (p < 0,001) — guaridas con mayor diferencial tendieron a ser más ocupadas"

```r
# Excluir guaridas sin temperatura
datos_temp <- datos %>% filter(!is.na(delta_t_media))

fig3 <- ggplot(datos_temp, aes(x = delta_t_media, y = tasa_ocup)) +
  geom_point(aes(color = tipo, shape = tipo), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "gray30",
              linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_shape_manual(values = c(16, 17, 15), labels = etiquetas_tipo) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Diferencial térmico vs. tasa de ocupación",
    subtitle = expression(paste("Spearman: ", r[s], " = −0,372; p < 0,001 | n = 103 guaridas")),
    x = "ΔT medio (°C) — Interior − Exterior",
    y = "Tasa de ocupación (%)",
    color = "Tipo", shape = "Tipo"
  ) +
  # Línea vertical en DeltaT promedio de naturales
  geom_vline(xintercept = -15.0, linetype = "dotted", color = "#4DAF4A") +
  annotate("text", x = -14.5, y = 0.95, label = "Natural\nmedia",
           size = 2.8, color = "#4DAF4A", hjust = 0) +
  theme_informe

ggsave("fig3_deltaT_vs_ocupacion.png", fig3, width = 9, height = 6, dpi = 300)
```

---

## 4. Figura 4 — Temperatura interna, externa y DeltaT por tipo

> **→ Resultado del informe:** Tabla 4-4 — "Art. norte: −18,2 ± 2,2°C; Art. sur: −17,3 ± 2,1°C; Naturales: −15,0 ± 2,8°C"

```r
# Preparar datos de temperatura por mes y tipo
cols_tint <- grep("T_Int$", names(datos_raw), value = TRUE)
cols_text <- grep("T_Ext$", names(datos_raw), value = TRUE)
cols_dt   <- grep("DeltaT$", names(datos_raw), value = TRUE)

temp_resumen <- datos_raw %>%
  mutate(
    T_Int_media  = rowMeans(across(all_of(cols_tint)), na.rm = TRUE),
    T_Ext_media  = rowMeans(across(all_of(cols_text)), na.rm = TRUE),
    DeltaT_media = rowMeans(across(all_of(cols_dt)), na.rm = TRUE)
  ) %>%
  select(ID_Guarida, tipo, T_Int_media, T_Ext_media, DeltaT_media) %>%
  filter(!is.na(DeltaT_media)) %>%
  pivot_longer(cols = c(T_Int_media, T_Ext_media, DeltaT_media),
               names_to = "variable", values_to = "valor")

temp_resumen$tipo <- factor(temp_resumen$tipo,
                             levels = c("arte_norte", "arte_sur", "natural"))

temp_resumen$variable <- factor(temp_resumen$variable,
  levels = c("T_Ext_media", "T_Int_media", "DeltaT_media"),
  labels = c("T Externa (°C)", "T Interna (°C)", "ΔT (°C)")
)

fig4 <- ggplot(temp_resumen, aes(x = tipo, y = valor, fill = tipo)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_x_discrete(labels = c("Norte (+)", "Sur (−)", "Natural")) +
  labs(
    title = "Temperatura y diferencial térmico por tipo de guarida",
    subtitle = "Promedios mensuales agosto 2025 – marzo 2026",
    x = "Tipo de guarida",
    y = "Temperatura (°C)"
  ) +
  theme_informe + theme(legend.position = "none")

ggsave("fig4_temperatura_por_tipo.png", fig4, width = 12, height = 5, dpi = 300)
```

---

## 5. Figura 5 — Barplot de ocupación por categoría (alta / media / baja)

> **→ Resultado del informe:** Tabla 4-1 — "Alta: 41% (norte), 29% (sur), 7% (natural); Baja: 7%, 11%, 78%"

```r
datos$categoria <- factor(
  ifelse(datos$tasa_ocup > 0.75, "Alta (>75%)",
  ifelse(datos$tasa_ocup >= 0.50, "Media (50–75%)",
         "Baja (<50%)")),
  levels = c("Baja (<50%)", "Media (50–75%)", "Alta (>75%)")
)

conteos <- datos %>%
  count(tipo, categoria) %>%
  group_by(tipo) %>%
  mutate(pct = n / sum(n))

fig5 <- ggplot(conteos, aes(x = tipo, y = pct, fill = categoria)) +
  geom_col(position = "stack", width = 0.6, color = "white") +
  scale_fill_manual(values = c("Baja (<50%)" = "#FEE08B",
                                "Media (50–75%)" = "#FDAE61",
                                "Alta (>75%)" = "#D73027")) +
  scale_x_discrete(labels = etiquetas_tipo) +
  scale_y_continuous(labels = percent_format()) +
  geom_text(aes(label = paste0(round(pct*100), "%")),
            position = position_stack(vjust = 0.5), size = 3.5,
            fontface = "bold") +
  labs(
    title = "Distribución de categorías de ocupación por tipo",
    subtitle = "Alta > 75% | Media 50–75% | Baja < 50%",
    x = "Tipo de guarida",
    y = "Proporción de guaridas",
    fill = "Categoría"
  ) +
  theme_informe

ggsave("fig5_categorias_ocupacion.png", fig5, width = 8, height = 6, dpi = 300)
```

---

## 6. Figura 6 — Análisis intra-cluster (natural vs. artificial)

> **→ Resultado del informe:** Sección 4.4 — "En el 85% de los clusters, las artificiales superaron a la natural de referencia"

```r
# Asignar cluster (número base)
datos$cluster <- gsub("[+-].*$", "", gsub("^G|^ID", "", datos$ID_Guarida))

intra <- datos %>%
  group_by(cluster) %>%
  summarise(
    ocup_natural = mean(tasa_ocup[tipo == "natural"], na.rm = TRUE),
    ocup_artif   = mean(tasa_ocup[tipo != "natural"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(ocup_natural) & !is.na(ocup_artif))

fig6 <- ggplot(intra, aes(x = ocup_natural, y = ocup_artif)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50") +
  geom_point(aes(size = ocup_artif - ocup_natural),
             color = "#2166AC", alpha = 0.7) +
  geom_text_repel(aes(label = paste0("G", cluster)),
                  size = 3, max.overlaps = 20) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  labs(
    title = "Comparación intra-cluster: Natural vs. Artificial",
    subtitle = "Puntos sobre la diagonal = artificial > natural | 85% de los clusters",
    x = "Tasa de ocupación — Guarida natural (ID)",
    y = "Tasa de ocupación promedio — Guaridas artificiales (G)"
  ) +
  annotate("text", x = 0.8, y = 0.2, label = "Natural > Artificial",
           color = "gray60", size = 3.5, fontface = "italic") +
  annotate("text", x = 0.2, y = 0.8, label = "Artificial > Natural",
           color = "#2166AC", size = 3.5, fontface = "italic") +
  theme_informe

ggsave("fig6_intra_cluster.png", fig6, width = 8, height = 7, dpi = 300)
```

---

## 7. Figura 7 — Tabla de selección de modelos AICc (gráfico de pesos)

> **→ Resultado del informe:** Tabla 4-5 — "Mejor modelo: N(tipo+tpi_z), AICc = 468,50, peso = 0,58"

```r
# Datos de la tabla de selección de modelos
modelos_df <- data.frame(
  modelo = c("N(tipo+tpi_z)", "N(tipo+delta_z)", "N(tipo)",
             "N(tipo+rad_z)", "N(tipo+delta_z+rad_z)",
             "N(tipo+rug_z)", "N(tipo+delta_z+rug_z)",
             "N(tipo+expo)", "N(delta_z)", "N(.)"),
  AICc = c(468.50, 472.40, 472.59, 473.10, 473.22,
           473.95, 474.09, 474.24, 486.59, 498.57),
  peso = c(0.58, 0.08, 0.07, 0.06, 0.05,
           0.04, 0.04, 0.03, 0.00, 0.00)
)

modelos_df$deltaAICc <- modelos_df$AICc - min(modelos_df$AICc)
modelos_df$modelo <- factor(modelos_df$modelo,
                             levels = rev(modelos_df$modelo))

fig7 <- ggplot(modelos_df, aes(x = modelo, y = peso)) +
  geom_col(aes(fill = deltaAICc), width = 0.7) +
  geom_text(aes(label = paste0(round(peso*100, 1), "%")),
            hjust = -0.2, size = 3.5) +
  scale_fill_viridis(option = "D", direction = -1, name = "ΔAICc") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.75)) +
  coord_flip() +
  labs(
    title = "Selección de modelos de abundancia Royle-Nichols",
    subtitle = "Peso de evidencia AICc | 21 modelos evaluados",
    x = "Modelo",
    y = "Peso de evidencia AICc"
  ) +
  theme_informe

ggsave("fig7_seleccion_modelos.png", fig7, width = 10, height = 6, dpi = 300)
```

---

## 8. Figura 8 — Coeficientes del mejor modelo (forest plot)

> **→ Resultado del informe:** Tabla 4-6 — Coeficientes N(tipo+tpi_z)

```r
coef_df <- data.frame(
  parametro = c("Intercepto\n(Arte Norte)", "Tipo:\nArte Sur",
                "Tipo:\nNatural", "TPI (z)"),
  estimado = c(2.567, -0.242, -0.874, -0.153),
  se = c(0.193, 0.138, 0.157, 0.062),
  ic_low = c(2.189, -0.512, -1.182, -0.274),
  ic_high = c(2.945, 0.027, -0.567, -0.032),
  significativo = c(TRUE, FALSE, TRUE, TRUE)
)

fig8 <- ggplot(coef_df, aes(x = parametro, y = estimado)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = ic_low, ymax = ic_high,
                       color = significativo),
                  size = 1, linewidth = 1) +
  scale_color_manual(values = c("TRUE" = "#D73027", "FALSE" = "gray50"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "n.s."),
                     name = "Significancia") +
  labs(
    title = "Coeficientes del mejor modelo N(tipo + tpi_z)",
    subtitle = "Abundancia en escala log | Barras = IC 95%",
    x = "Parámetro",
    y = "Coeficiente estimado (log-escala)"
  ) +
  theme_informe

ggsave("fig8_coeficientes_modelo.png", fig8, width = 8, height = 5, dpi = 300)
```

---

## 9. Figura 9 — Abundancia estimada por tipo (barras con IC)

> **→ Resultado del informe:** Tabla 4-7 — "Arte norte: 13,0 ind. (8,9–19,0); Arte sur: 10,2 (5,3–19,5); Natural: 5,4 (2,7–10,8)"

```r
abund_df <- data.frame(
  tipo = factor(c("arte_norte", "arte_sur", "natural"),
                levels = c("arte_norte", "arte_sur", "natural")),
  N_media = c(13.0, 10.2, 5.4),
  ic_low = c(8.9, 5.3, 2.7),
  ic_high = c(19.0, 19.5, 10.8),
  N_total = c(560, 376, 133)
)

fig9 <- ggplot(abund_df, aes(x = tipo, y = N_media, fill = tipo)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = paste0(N_media, " ind.")),
            vjust = -0.5, fontface = "bold", size = 4) +
  geom_text(aes(y = 1, label = paste0("N total = ", N_total)),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_x_discrete(labels = etiquetas_tipo) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Abundancia estimada por tipo de guarida",
    subtitle = "Modelo Royle-Nichols N(tipo + tpi_z) | Barras = IC 95%",
    x = "Tipo de guarida",
    y = "Abundancia media (individuos / guarida)"
  ) +
  theme_informe + theme(legend.position = "none")

ggsave("fig9_abundancia_por_tipo.png", fig9, width = 8, height = 6, dpi = 300)
```

---

## 10. Figura 10 — Efecto del TPI sobre la abundancia

> **→ Resultado del informe:** Sección 4.6 — "TPI tuvo efecto negativo significativo (β = −0,153; p = 0,013)"

```r
# Calcular abundancia predicha en función de TPI (para arte_norte)
tpi_seq <- seq(-2, 2, length.out = 100)
# log(N) = 2.567 + (-0.153)*tpi_z
N_pred <- exp(2.567 + (-0.153) * tpi_seq)
N_low  <- exp(2.189 + (-0.274) * tpi_seq)
N_high <- exp(2.945 + (-0.032) * tpi_seq)

pred_df <- data.frame(tpi_z = tpi_seq, N = N_pred,
                       N_low = N_low, N_high = N_high)

fig10 <- ggplot(pred_df, aes(x = tpi_z, y = N)) +
  geom_ribbon(aes(ymin = N_low, ymax = N_high),
              alpha = 0.2, fill = "#2166AC") +
  geom_line(color = "#2166AC", linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  # Añadir anotaciones de interpretación
  annotate("text", x = -1.5, y = max(N_pred) * 0.9,
           label = "← Quebrada\n(TPI negativo)",
           color = "#2166AC", size = 3.5, fontface = "italic") +
  annotate("text", x = 1.5, y = max(N_pred) * 0.9,
           label = "Cresta/Ladera →\n(TPI positivo)",
           color = "#B2182B", size = 3.5, fontface = "italic") +
  labs(
    title = "Efecto de la posición topográfica (TPI) sobre la abundancia",
    subtitle = "Modelo N(tipo + tpi_z) | Referencia: Artificial Norte | Banda = IC 95%",
    x = "TPI estandarizado (z-score)",
    y = "Abundancia estimada (individuos / guarida)"
  ) +
  theme_informe

ggsave("fig10_efecto_tpi.png", fig10, width = 9, height = 6, dpi = 300)
```

---

## 11. Figura 11 — Mapa espacial de las guaridas

> **→ Resultado del informe:** Sección 3.1 — "106 guaridas distribuidas en 30 clusters espaciales"

```r
# Usar coordenadas WGS84 (lon, lat)
fig11 <- ggplot(datos, aes(x = UTM_Este, y = UTM_Norte)) +
  geom_point(aes(color = tipo, size = tasa_ocup), alpha = 0.7) +
  scale_color_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_size_continuous(range = c(1.5, 6),
                        name = "Tasa ocupación",
                        labels = percent_format()) +
  labs(
    title = "Distribución espacial de guaridas y tasa de ocupación",
    subtitle = "Tamaño del punto = tasa de ocupación | Color = tipo de guarida",
    x = "Longitud (°W)",
    y = "Latitud (°S)",
    color = "Tipo"
  ) +
  coord_fixed(ratio = 1) +
  theme_informe

ggsave("fig11_mapa_guaridas.png", fig11, width = 10, height = 7, dpi = 300)
```

### 11.2 Mapa con DEM de fondo (versión avanzada)

```r
# Descargar DEM
puntos_sf <- st_as_sf(datos, coords = c("UTM_Este", "UTM_Norte"), crs = 4326)
dem <- get_elev_raster(puntos_sf, z = 12, clip = "bbox", expand = 0.01)

# Convertir DEM a dataframe para ggplot
dem_rast <- rast(dem)
dem_df <- as.data.frame(dem_rast, xy = TRUE)
names(dem_df)[3] <- "elevacion"

fig11b <- ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevacion)) +
  scale_fill_viridis(option = "E", name = "Elevación (m)") +
  geom_point(data = datos,
             aes(x = UTM_Este, y = UTM_Norte,
                 color = tipo, size = tasa_ocup),
             alpha = 0.8) +
  scale_color_manual(values = c("#FFFFFF", "#FF6666", "#66FF66"),
                     labels = etiquetas_tipo) +
  scale_size_continuous(range = c(2, 6), name = "Ocupación",
                        labels = percent_format()) +
  labs(
    title = "Guaridas sobre Modelo Digital de Elevación (DEM)",
    subtitle = "DEM SRTM z=12 (~17 m resolución) | elevatr + terra",
    x = "Longitud", y = "Latitud",
    color = "Tipo"
  ) +
  coord_fixed() +
  theme_informe

ggsave("fig11b_mapa_dem.png", fig11b, width = 11, height = 8, dpi = 300)
```

---

## 12. Figura 12 — Matriz de correlaciones entre variables

> **→ Resultado del informe:** Sección 3.2.3 — "todas las correlaciones entre delta_z, rug_z, tpi_z y rad_z fueron menores a r = 0,21"

```r
# Estandarizar variables
datos$delta_z <- scale(datos$delta_t_media)
datos$rug_z   <- scale(datos$rugosidad_dem)
datos$tpi_z   <- scale(datos$tpi_dem)
datos$rad_z   <- scale(datos$radiacion_dem)
datos$pend_z  <- scale(datos$pendiente_dem)
datos$elev_z  <- scale(datos$elevacion_dem)

# Matriz de correlación
vars_modelo <- datos[, c("delta_z", "rug_z", "tpi_z",
                          "rad_z", "pend_z", "elev_z")]
vars_modelo <- vars_modelo[complete.cases(vars_modelo), ]
cor_matrix <- cor(vars_modelo, use = "complete.obs")

# Nombres bonitos
colnames(cor_matrix) <- rownames(cor_matrix) <-
  c("ΔT", "Rugosidad", "TPI", "Radiación", "Pendiente", "Elevación")

# Guardar como PNG
png("fig12_correlaciones.png", width = 8, height = 7, units = "in", res = 300)
corrplot(cor_matrix,
         method = "ellipse",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.9,
         tl.col = "black",
         tl.cex = 1,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlaciones entre variables predictoras",
         mar = c(0, 0, 2, 0))
dev.off()
```

---

## 13. Figura 13 — Histograma de distancias entre guaridas (conectividad)

> **→ Resultado del informe:** Sección 4.7 — "176 pares a menos de 80 metros, distancia mediana: 30 m"

```r
# Calcular distancias (UTM)
puntos_utm <- st_transform(puntos_sf, crs = 32719)
dist_matrix <- as.matrix(st_distance(puntos_utm))

# Extraer triángulo superior (pares únicos)
dist_vec <- dist_matrix[upper.tri(dist_matrix)]

# Filtrar pares < 200 m para visualización
dist_cercanos <- dist_vec[dist_vec < 200 & dist_vec > 0]

fig13 <- ggplot(data.frame(distancia = dist_cercanos),
                aes(x = distancia)) +
  geom_histogram(binwidth = 10, fill = "#2166AC", color = "white",
                 alpha = 0.8) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "#D73027",
             linewidth = 1) +
  annotate("text", x = 85, y = Inf, label = "Umbral 80 m",
           color = "#D73027", vjust = 2, hjust = 0, size = 4,
           fontface = "bold") +
  geom_vline(xintercept = 30, linetype = "dotted", color = "#4DAF4A",
             linewidth = 0.8) +
  annotate("text", x = 35, y = Inf, label = "Mediana 30 m",
           color = "#4DAF4A", vjust = 3.5, hjust = 0, size = 3.5) +
  labs(
    title = "Distribución de distancias entre pares de guaridas",
    subtitle = "176 pares < 80 m | Distancia mediana = 30 m",
    x = "Distancia (metros)",
    y = "Número de pares"
  ) +
  theme_informe

ggsave("fig13_distancias_pares.png", fig13, width = 9, height = 5, dpi = 300)
```

---

## 14. Figura 14 — Heatmap de co-ocurrencia entre guaridas cercanas

> **→ Resultado del informe:** Sección 4.7 — "co-ocurrencia mediana: 0,62; máximos: G7+3–G7+4 (0,969)"

```r
# Identificar pares < 80 m y calcular co-ocurrencia
cols_pres <- grep("_Pres$", names(datos_raw), value = TRUE)
pares_idx <- which(dist_matrix < 80 & dist_matrix > 0, arr.ind = TRUE)
pares_idx <- pares_idx[pares_idx[,1] < pares_idx[,2], ]

co_ocu <- data.frame()
for (i in seq_len(nrow(pares_idx))) {
  g1_idx <- pares_idx[i, 1]
  g2_idx <- pares_idx[i, 2]
  g1 <- as.numeric(datos_raw[g1_idx, cols_pres])
  g2 <- as.numeric(datos_raw[g2_idx, cols_pres])
  ambas  <- sum(g1 == 1 & g2 == 1, na.rm = TRUE)
  alguna <- sum(g1 == 1 | g2 == 1, na.rm = TRUE)
  idx_co <- ifelse(alguna > 0, ambas / alguna, NA)
  co_ocu <- rbind(co_ocu, data.frame(
    guarida1 = datos$ID_Guarida[g1_idx],
    guarida2 = datos$ID_Guarida[g2_idx],
    distancia = dist_matrix[g1_idx, g2_idx],
    co_ocurrencia = idx_co
  ))
}

# Top 20 pares con mayor co-ocurrencia
top_pares <- co_ocu %>%
  arrange(desc(co_ocurrencia)) %>%
  head(20) %>%
  mutate(par = paste(guarida1, "–", guarida2))

fig14 <- ggplot(top_pares, aes(x = reorder(par, co_ocurrencia),
                                y = co_ocurrencia)) +
  geom_col(aes(fill = co_ocurrencia), width = 0.7) +
  geom_text(aes(label = paste0(round(distancia), " m")),
            hjust = -0.1, size = 3, color = "gray40") +
  scale_fill_viridis(option = "C", direction = -1, name = "Co-ocurrencia") +
  scale_y_continuous(limits = c(0, 1.15)) +
  coord_flip() +
  labs(
    title = "Top 20 pares de guaridas por co-ocurrencia",
    subtitle = "Índice = semanas simultáneas / semanas con ≥1 ocupada | Número = distancia (m)",
    x = "Par de guaridas",
    y = "Índice de co-ocurrencia"
  ) +
  theme_informe

ggsave("fig14_co_ocurrencia.png", fig14, width = 10, height = 7, dpi = 300)
```

---

## 15. Figura 15 — Estimación de individuos únicos (triangulación)

> **→ Resultado del informe:** Tabla 4-8 — "Rango más plausible: 35–64 individuos únicos"

```r
estimaciones <- data.frame(
  fuente = c("Modelo\nRoyle-Nichols", "Individuo\nmás móvil",
             "Promedio\nmarcados", "Co-ocurrencia\nespacial",
             "Visual\ncampo"),
  individuos = c(NA, 35, 64, 315, 30),
  tipo = c("Estadístico", "Foto-ID", "Foto-ID",
           "Espacial", "Observación"),
  confianza = c("Alta", "Alta", "Moderada", "Referencia", "Baja")
)

# Rango plausible
fig15 <- ggplot(estimaciones %>% filter(!is.na(individuos)),
                aes(x = reorder(fuente, individuos), y = individuos)) +
  # Banda del rango plausible
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 35, ymax = 64,
           alpha = 0.15, fill = "#2166AC") +
  annotate("text", x = 4.5, y = 49.5,
           label = "Rango plausible\n35 – 64 ind.",
           color = "#2166AC", size = 3.5, fontface = "bold") +
  geom_col(aes(fill = confianza), width = 0.6, alpha = 0.8) +
  geom_text(aes(label = individuos), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Alta" = "#2166AC", "Moderada" = "#FDAE61",
                                "Referencia" = "gray70", "Baja" = "#FEE08B")) +
  labs(
    title = "Triangulación de estimaciones de individuos únicos",
    subtitle = "Banda azul = rango más plausible (35–64 individuos)",
    x = "Método de estimación",
    y = "Número estimado de individuos"
  ) +
  theme_informe

ggsave("fig15_triangulacion_individuos.png", fig15, width = 9, height = 6, dpi = 300)
```

---

## 16. Figura 16 — Variables geomorfológicas por tipo de guarida (panel)

> **→ Resultado del informe:** Sección 3.2.3 — Variables del DEM extraídas con terra

```r
vars_panel <- datos %>%
  select(tipo, elevacion_dem, rugosidad_dem, tpi_dem, radiacion_dem) %>%
  pivot_longer(-tipo, names_to = "variable", values_to = "valor")

vars_panel$variable <- factor(vars_panel$variable,
  levels = c("elevacion_dem", "rugosidad_dem", "tpi_dem", "radiacion_dem"),
  labels = c("Elevación (m s.n.m.)", "Rugosidad (índice)",
             "TPI (posición topográfica)", "Radiación solar (0–1)")
)

fig16 <- ggplot(vars_panel, aes(x = tipo, y = valor, fill = tipo)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = colores_tipo, labels = etiquetas_tipo) +
  scale_x_discrete(labels = c("Norte", "Sur", "Natural")) +
  labs(
    title = "Variables geomorfológicas por tipo de guarida",
    subtitle = "Extraídas del DEM SRTM z=12 con terra",
    x = "Tipo de guarida",
    y = "Valor"
  ) +
  theme_informe + theme(legend.position = "bottom")

ggsave("fig16_variables_geomorfologicas.png", fig16, width = 10, height = 8, dpi = 300)
```

---

## 17. Figura 17 — Exposición de ladera (gráfico circular)

> **→ Resultado del informe:** Sección 3.2.3 — "Exposición de ladera derivada del aspecto"

```r
expo_conteo <- datos %>%
  count(tipo, exposicion_dem) %>%
  group_by(tipo) %>%
  mutate(pct = n / sum(n))

expo_conteo$tipo <- factor(expo_conteo$tipo,
                            levels = c("arte_norte", "arte_sur", "natural"))

fig17 <- ggplot(expo_conteo,
                aes(x = "", y = pct, fill = exposicion_dem)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~tipo, labeller = labeller(tipo = etiquetas_tipo)) +
  scale_fill_manual(values = c("Norte" = "#D73027", "Sur" = "#2166AC",
                                "Este" = "#FEE08B", "Oeste" = "#4DAF4A"),
                    name = "Exposición") +
  geom_text(aes(label = ifelse(pct > 0.08, paste0(round(pct*100), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Distribución de exposición de ladera por tipo de guarida",
    subtitle = "Norte (315°–45°) | Este (45°–135°) | Sur (135°–225°) | Oeste (225°–315°)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("fig17_exposicion_ladera.png", fig17, width = 11, height = 5, dpi = 300)
```

---

## 18. Figura 18 — Tabla 3-1 formateada (distribución de guaridas por tipo)

> **→ Presentación:** Diapositiva 2 — Tabla resumen con n° de guaridas, clusters e identificadores por tipo

```r
# Tabla 3-1 del informe: distribución de guaridas
tabla_3_1 <- data.frame(
  Tipo = c("Naturales (control)", "Artificiales norte (+)",
           "Artificiales sur (−)", "TOTAL"),
  N_guaridas = c("27*", "44", "35", "106"),
  N_clusters = c("27", "16", "14", "30"),
  Identificacion = c("ID1 – ID30", "G[n]+[1..6]",
                      "G[n]−[1..6]", "—")
)

fig18 <- gt(tabla_3_1) %>%
  tab_header(
    title = md("**Tabla 3-1.** Distribución de guaridas por tipo y posición respecto a la traza del Proyecto SADDN.")
  ) %>%
  cols_label(
    Tipo = "Tipo",
    N_guaridas = "N° guaridas",
    N_clusters = "N° clusters",
    Identificacion = "Identificación"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#4DAF4A"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Tipo == "TOTAL")
  ) %>%
  tab_footnote(
    footnote = "* 3 guaridas sin monitoreo por impacto directo de la traza (ID10, ID20, ID24). 3 guaridas desmanteladas con cámara pero sin temperatura (ID9, ID13, ID19), excluidas de modelos con delta_t.",
    locations = cells_body(columns = N_guaridas, rows = 1)
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.align = "left",
    column_labels.border.bottom.color = "#4DAF4A",
    column_labels.border.bottom.width = px(2),
    table_body.hlines.color = "#E8E8E8"
  )

# Guardar como PNG
gtsave(fig18, "fig18_tabla_3_1.png", vwidth = 800, vheight = 350)
```

---

## 19. Figura 19 — Mapa con basemap satelital ("¿Dónde?")

> **→ Presentación:** Diapositiva 13 — Mapa satelital/aéreo mostrando la ubicación del área de estudio y las guaridas

```r
# Convertir a sf con CRS WGS84
puntos_sf <- st_as_sf(datos, coords = c("UTM_Este", "UTM_Norte"), crs = 4326)

# Calcular bounding box con margen
bbox <- st_bbox(puntos_sf)
margen <- 0.005  # ~500 m
bbox_exp <- c(
  xmin = bbox["xmin"] - margen, ymin = bbox["ymin"] - margen,
  xmax = bbox["xmax"] + margen, ymax = bbox["ymax"] + margen
)

fig19 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 15, cachedir = tempdir()) +
  geom_sf(data = puntos_sf,
          aes(color = tipo, size = tasa_ocup),
          alpha = 0.85) +
  scale_color_manual(values = colores_tipo, labels = etiquetas_tipo,
                     name = "Tipo de guarida") +
  scale_size_continuous(range = c(2, 6), name = "Tasa ocupación",
                        labels = percent_format()) +
  annotation_scale(location = "bl", width_hint = 0.25,
                   style = "ticks") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1.2, "cm"),
                         width = unit(1.2, "cm")) +
  coord_sf(xlim = c(bbox_exp["xmin"], bbox_exp["xmax"]),
           ylim = c(bbox_exp["ymin"], bbox_exp["ymax"])) +
  labs(
    title = "¿Dónde? — Ubicación del área de estudio",
    subtitle = "106 guaridas sobre imagen satelital | Proyecto SADDN",
    x = "Longitud", y = "Latitud"
  ) +
  theme_informe

ggsave("fig19_mapa_satelital.png", fig19, width = 11, height = 9, dpi = 300)
```

---

## 20. Figura 20 — Método del codo (Elbow Method / WSS)

> **→ Presentación:** Diapositiva 15 — "Elbow Method: Suma de los Cuadrados dentro del Cluster (WSS)" para determinar el número óptimo de clusters espaciales

```r
# Preparar coordenadas UTM para clustering
puntos_utm <- st_transform(puntos_sf, crs = 32719)
coords_utm <- st_coordinates(puntos_utm)

# Método del codo: calcular WSS para k = 1 a 15
set.seed(42)
wss <- sapply(1:15, function(k) {
  kmeans(coords_utm, centers = k, nstart = 25)$tot.withinss
})

wss_df <- data.frame(k = 1:15, WSS = wss)

fig20 <- ggplot(wss_df, aes(x = k, y = WSS)) +
  geom_line(color = "#2166AC", linewidth = 1.2) +
  geom_point(color = "#2166AC", size = 3) +
  # Marcar el codo óptimo (visual: alrededor de k = 5–7)
  geom_vline(xintercept = 6, linetype = "dashed", color = "#D73027",
             linewidth = 0.8) +
  annotate("text", x = 6.5, y = max(wss) * 0.7,
           label = "k óptimo ≈ 6",
           color = "#D73027", size = 4, fontface = "bold", hjust = 0) +
  scale_x_continuous(breaks = 1:15) +
  labs(
    title = "Método del codo (Elbow Method)",
    subtitle = "Suma de cuadrados intra-cluster (WSS) vs. número de clusters k",
    x = "Número de clusters (k)",
    y = "Suma total de cuadrados\nintra-cluster (WSS)"
  ) +
  theme_informe

ggsave("fig20_elbow_wss.png", fig20, width = 9, height = 6, dpi = 300)
```

---

## 21. Figura 21 — Clusters espaciales de guaridas (k-means en mapa)

> **→ Presentación:** Diapositiva 14 y 16 — Asignación de clusters espaciales sobre el mapa del área de estudio

```r
# Ejecutar k-means con el k óptimo
set.seed(42)
k_optimo <- 6
km_result <- kmeans(coords_utm, centers = k_optimo, nstart = 25)

# Añadir asignación de cluster a los datos
datos$cluster_km <- factor(km_result$cluster)

# Centroides de cada cluster
centroides <- as.data.frame(km_result$centers)
names(centroides) <- c("X", "Y")
centroides$cluster <- factor(1:k_optimo)
centroides_sf <- st_as_sf(centroides, coords = c("X", "Y"), crs = 32719)
centroides_wgs <- st_transform(centroides_sf, crs = 4326)
centroides_coords <- cbind(
  st_coordinates(centroides_wgs),
  cluster = centroides$cluster
) %>% as.data.frame()
centroides_coords$X <- as.numeric(centroides_coords$X)
centroides_coords$Y <- as.numeric(centroides_coords$Y)
centroides_coords$cluster <- factor(centroides_coords$cluster)

# Paleta de colores para clusters
colores_cluster <- c("#E41A1C", "#377EB8", "#4DAF4A",
                     "#984EA3", "#FF7F00", "#A65628")

fig21 <- ggplot(datos, aes(x = UTM_Este, y = UTM_Norte)) +
  # Puntos coloreados por cluster
  geom_point(aes(color = cluster_km, shape = tipo),
             size = 3.5, alpha = 0.8) +
  # Centroides
  geom_point(data = centroides_coords, aes(x = X, y = Y),
             shape = 4, size = 6, stroke = 2, color = "black") +
  # Etiquetas de cluster
  geom_label_repel(data = centroides_coords,
                   aes(x = X, y = Y, label = paste("Cluster", cluster)),
                   size = 3, fontface = "bold",
                   fill = "white", alpha = 0.8,
                   max.overlaps = 20) +
  scale_color_manual(values = colores_cluster, name = "Cluster") +
  scale_shape_manual(values = c("arte_norte" = 16, "arte_sur" = 17,
                                "natural" = 15),
                     labels = etiquetas_tipo, name = "Tipo") +
  labs(
    title = "Análisis espacial — Clusters de guaridas (k-means)",
    subtitle = paste0("k = ", k_optimo,
                      " clusters | × = centroide | Forma = tipo de guarida"),
    x = "Longitud (°W)", y = "Latitud (°S)"
  ) +
  coord_fixed(ratio = 1) +
  theme_informe

ggsave("fig21_clusters_mapa.png", fig21, width = 11, height = 8, dpi = 300)
```

---

## 22. Figura 22 — Silhouette plot (calidad de los clusters)

> **→ Presentación:** Diapositiva 16 — Evaluación visual de la calidad de la asignación de clusters

```r
# Calcular coeficiente silhouette
sil <- silhouette(km_result$cluster, dist(coords_utm))

# Convertir a dataframe para ggplot
sil_df <- data.frame(
  guarida = 1:nrow(sil),
  cluster = factor(sil[, "cluster"]),
  sil_width = sil[, "sil_width"]
)

# Ordenar por cluster y ancho de silueta
sil_df <- sil_df %>%
  arrange(cluster, desc(sil_width)) %>%
  mutate(orden = row_number())

# Silueta promedio
sil_promedio <- mean(sil_df$sil_width)

fig22 <- ggplot(sil_df, aes(x = orden, y = sil_width, fill = cluster)) +
  geom_col(width = 1) +
  geom_hline(yintercept = sil_promedio, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  annotate("text", x = nrow(sil_df) * 0.85, y = sil_promedio + 0.05,
           label = paste0("Promedio = ", round(sil_promedio, 3)),
           color = "red", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = colores_cluster, name = "Cluster") +
  scale_y_continuous(limits = c(-0.2, 1)) +
  labs(
    title = "Silhouette plot — Calidad de asignación a clusters",
    subtitle = paste0("k = ", k_optimo,
                      " | Valores altos = buena asignación | Negativos = posible error"),
    x = "Guaridas (ordenadas por cluster)",
    y = "Ancho de silueta (silhouette width)"
  ) +
  theme_informe

ggsave("fig22_silhouette.png", fig22, width = 10, height = 6, dpi = 300)
```

---

## 23. Figura 23 — Resumen de ocupación por cluster espacial

> **→ Presentación:** Diapositiva 17 — Comparación de tasas de ocupación entre clusters espaciales, combinando tipo y ubicación

```r
# Resumen de ocupación por cluster
resumen_cluster <- datos %>%
  group_by(cluster_km) %>%
  summarise(
    n_guaridas = n(),
    ocup_media = mean(tasa_ocup, na.rm = TRUE),
    ocup_sd = sd(tasa_ocup, na.rm = TRUE),
    pct_artificial = mean(tipo != "natural") * 100,
    n_natural = sum(tipo == "natural"),
    n_artif = sum(tipo != "natural"),
    .groups = "drop"
  ) %>%
  mutate(etiqueta = paste0("Cluster ", cluster_km,
                           "\n(n=", n_guaridas, ")"))

fig23 <- ggplot(resumen_cluster,
                aes(x = reorder(etiqueta, -ocup_media),
                    y = ocup_media, fill = pct_artificial)) +
  geom_col(width = 0.65, alpha = 0.85) +
  geom_errorbar(aes(ymin = pmax(0, ocup_media - ocup_sd),
                    ymax = pmin(1, ocup_media + ocup_sd)),
                width = 0.2, linewidth = 0.6) +
  geom_text(aes(label = paste0(round(ocup_media * 100, 1), "%")),
            vjust = -0.8, fontface = "bold", size = 3.5) +
  geom_text(aes(y = 0.02,
                label = paste0(n_artif, " art / ", n_natural, " nat")),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#4DAF4A", high = "#2166AC",
                      name = "% Artificiales",
                      labels = function(x) paste0(round(x), "%")) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Tasa de ocupación media por cluster espacial",
    subtitle = "Barras = media ± DE | Color = proporción de guaridas artificiales | Texto = composición",
    x = "Cluster espacial",
    y = "Tasa de ocupación media"
  ) +
  theme_informe

ggsave("fig23_ocupacion_por_cluster.png", fig23, width = 10, height = 6, dpi = 300)
```

---

## 24. Panel combinado — Resumen ejecutivo (4 gráficos en 1)

```r
# Combinar figuras clave usando patchwork
panel_final <- (fig1 + fig2) / (fig9 + fig10) +
  plot_annotation(
    title = "Resumen visual — Estudio Chinchilla SADDN",
    subtitle = "106 guaridas | 34 semanas | Modelo Royle-Nichols N(tipo + tpi_z)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray40")
    )
  )

ggsave("panel_resumen_ejecutivo.png", panel_final,
       width = 16, height = 12, dpi = 300)
```

---

## 25. Resumen: Lista completa de figuras generadas

| # | Archivo | Contenido | Sección / Presentación |
|---|---------|-----------|------------------------|
| 1 | `fig1_boxplot_ocupacion.png` | Boxplot ocupación por tipo + significancia | §4.1 / Tabla 4-1 |
| 1b | `fig1b_violin_ocupacion.png` | Violin plot (variante) | §4.1 |
| 2 | `fig2_estacionalidad_ocupacion.png` | Variación mensual con barras de error | §4.2 / Tabla 4-3 |
| 3 | `fig3_deltaT_vs_ocupacion.png` | Scatter DeltaT vs. ocupación + Spearman | §4.3 |
| 4 | `fig4_temperatura_por_tipo.png` | Boxplots T interna, externa, DeltaT | §4.3 / Tabla 4-4 |
| 5 | `fig5_categorias_ocupacion.png` | Barplot apilado alta/media/baja | §4.1 / Tabla 4-1 |
| 6 | `fig6_intra_cluster.png` | Scatter natural vs. artificial por cluster | §4.4 |
| 7 | `fig7_seleccion_modelos.png` | Barplot horizontal de pesos AICc | §4.5 / Tabla 4-5 |
| 8 | `fig8_coeficientes_modelo.png` | Forest plot de coeficientes ± IC 95% | §4.6 / Tabla 4-6 |
| 9 | `fig9_abundancia_por_tipo.png` | Barplot abundancia estimada + IC | §4.6 / Tabla 4-7 |
| 10 | `fig10_efecto_tpi.png` | Curva de abundancia vs. TPI + banda IC | §4.6 |
| 11 | `fig11_mapa_guaridas.png` | Mapa de puntos por ubicación y ocupación | §3.1 |
| 11b | `fig11b_mapa_dem.png` | Mapa con DEM de fondo | §3.2.3 |
| 12 | `fig12_correlaciones.png` | Matriz de correlaciones entre variables | §3.2.3 |
| 13 | `fig13_distancias_pares.png` | Histograma de distancias entre pares | §4.7 |
| 14 | `fig14_co_ocurrencia.png` | Top 20 pares por co-ocurrencia | §4.7 |
| 15 | `fig15_triangulacion_individuos.png` | Barplot estimaciones de individuos | §4.7 / Tabla 4-8 |
| 16 | `fig16_variables_geomorfologicas.png` | Panel de variables DEM por tipo | §3.2.3 |
| 17 | `fig17_exposicion_ladera.png` | Gráficos circulares de exposición | §3.2.3 |
| **18** | **`fig18_tabla_3_1.png`** | **Tabla 3-1 formateada (distribución guaridas)** | **Pres. diap. 2 / §3.1** |
| **19** | **`fig19_mapa_satelital.png`** | **Mapa con basemap satelital (¿Dónde?)** | **Pres. diap. 13** |
| **20** | **`fig20_elbow_wss.png`** | **Método del codo (Elbow/WSS)** | **Pres. diap. 15** |
| **21** | **`fig21_clusters_mapa.png`** | **Clusters espaciales k-means en mapa** | **Pres. diap. 14, 16** |
| **22** | **`fig22_silhouette.png`** | **Silhouette plot — calidad de clusters** | **Pres. diap. 16** |
| **23** | **`fig23_ocupacion_por_cluster.png`** | **Ocupación media por cluster espacial** | **Pres. diap. 17** |
| — | `panel_resumen_ejecutivo.png` | Panel combinado 4 figuras | Resumen |

> 📌 **Figuras 18–23** (en negrita) corresponden a gráficos presentes en la presentación `Presentación1_resultados_chinchilla_1.pdf` que no estaban incluidos en la versión anterior del tutorial.

---

## 26. Cómo reproducir todas las figuras

1. **Abrir R** (versión ≥ 4.3.2) con RStudio
2. **Instalar paquetes** (sección 0.1)
3. **Establecer directorio de trabajo** al directorio con los CSV
4. **Ejecutar secciones 0.2 y 0.3** (tema y datos)
5. **Ejecutar cada figura** en orden — cada una genera un PNG en el directorio de trabajo
6. **Tamaño recomendado** para el informe Word: insertar a 15 cm de ancho

> ⚠️ **Nota**: Las figuras 11b (mapa DEM), 13–14 (distancias y co-ocurrencia), 19 (mapa satelital) y 20–22 (clusters) requieren conexión a internet la primera vez para descargar el DEM o los tiles del mapa. Las demás figuras funcionan offline con los CSV. La figura 18 (tabla formateada) requiere el paquete `gt` y `chromote` para exportar como PNG.

> 💡 **Tip**: Para cambiar a formato PDF en vez de PNG, simplemente cambie `ggsave("nombre.png", ...)` por `ggsave("nombre.pdf", ...)`.

---

*Tutorial de gráficos generado a partir del informe_final_v2_SADDN.docx, la presentación Presentación1_resultados_chinchilla_1.pdf y los datos del repositorio dcarden1/data (rama Chinchillas). Figuras 18–23 agregadas desde la presentación.*
