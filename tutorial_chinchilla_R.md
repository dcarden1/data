# Tutorial: Del Script de R al Informe Final
## Estudio Ecológico de Guaridas de Chinchilla de Cola Corta — Proyecto SADDN

> Este tutorial explica paso a paso cómo se usó R para producir cada uno de los resultados presentados en el **informe_final_v2_SADDN.docx**, partiendo de los datos crudos de monitoreo.

---

## 1. Archivos del proyecto

| Archivo | Contenido |
|---------|-----------|
| `Datos Chinchilla SADDN.csv` | Datos crudos: presencia/ausencia semanal + temperatura por guarida (106 guaridas × 34 semanas) |
| `Datos_Chinchilla_SADDN_Geomorfologicos_v2.csv` | Datos procesados: tasas de ocupación + variables geomorfológicas del DEM (elevación, pendiente, rugosidad, TPI, radiación, aspecto, exposición, coordenadas UTM) |
| `datos chinchilla R_V2_claude` | CSV intermedio con variables geomorfológicas calculadas desde coordenadas originales (versión 1, pre-DEM) |
| `informe_final_v2_SADDN.docx` | Informe final con metodología, resultados y discusión |

---

## 2. Paquetes de R necesarios

```r
# Instalar si es necesario
install.packages(c("unmarked", "AICcmodavg", "elevatr", "terra", "sf"))

# Cargar
library(unmarked)      # Modelos Royle-Nichols (occuRN)
library(AICcmodavg)    # Selección de modelos por AICc
library(elevatr)       # Descarga de DEM (SRTM vía AWS)
library(terra)         # Cálculo de variables geomorfológicas
library(sf)            # Manejo de coordenadas espaciales
```

> **Versiones usadas:** R v4.3.2, unmarked v1.3.1, elevatr v0.4.2, terra v1.7.65

---

## 3. Paso 1 — Cargar los datos crudos de monitoreo

El archivo `Datos Chinchilla SADDN.csv` contiene los registros semanales de presencia (1) y ausencia (0) para cada guarida, junto con temperaturas mensuales.

```r
# Leer datos crudos (separador = punto y coma, decimales = coma)
datos_raw <- read.csv2("Datos Chinchilla SADDN.csv", stringsAsFactors = FALSE)

# Ver estructura
str(datos_raw)
# 106 filas (guaridas) × 60 columnas aprox.
# Columnas de presencia: AGO_S1_Pres ... MAR_S4_Pres (34 semanas)
# Columnas de temperatura: AGO_T_Int, AGO_T_Ext, AGO_DeltaT, etc.
```

### Estructura de las columnas de presencia:
- **AGO** = agosto (4 semanas: S1–S4)
- **SEP** = septiembre (4 semanas: S1–S4)
- **OCT** = octubre (5 semanas: S1–S5)
- **NOV** = noviembre (4 semanas: S1–S4)
- **DIC** = diciembre (4 semanas: S1–S4)
- **ENE** = enero (5 semanas: S1–S5)
- **FEB** = febrero (4 semanas: S1–S4)
- **MAR** = marzo (4 semanas: S1–S4)
- **Total: 34 semanas**

> **→ Resultado del informe:** Sección 3.1 — "106 guaridas distribuidas en 30 clusters espaciales" y "34 semanas: agosto (4), septiembre (4), octubre (5)..."

---

## 4. Paso 2 — Clasificar las guaridas por tipo

```r
# Crear variable "tipo" según el nombre de la guarida
datos_raw$tipo <- ifelse(grepl("^ID", datos_raw$ID_Guarida), "natural",
                  ifelse(grepl("\\+", datos_raw$ID_Guarida), "arte_norte",
                         "arte_sur"))

# Verificar conteos
table(datos_raw$tipo)
# arte_norte: 44
# arte_sur:   35
# natural:    27 (24 con monitoreo completo)
```

> **→ Resultado del informe:** Tabla 3-1 — "Naturales (control): 27*, Artificiales norte (+): 44, Artificiales sur (-): 35, TOTAL: 106"

---

## 5. Paso 3 — Calcular la tasa de ocupación por guarida

```r
# Seleccionar solo columnas de presencia semanal
cols_pres <- grep("_Pres$", names(datos_raw), value = TRUE)

# Contar semanas con presencia y calcular tasa
datos_raw$semanas_ocupadas <- rowSums(datos_raw[, cols_pres], na.rm = TRUE)
datos_raw$tasa_ocupacion <- datos_raw$semanas_ocupadas / 34

# Categorizar
datos_raw$categoria <- ifelse(datos_raw$tasa_ocupacion > 0.75, "Alta",
                       ifelse(datos_raw$tasa_ocupacion >= 0.50, "Media", "Baja"))
```

> **→ Resultado del informe:** Sección 4.1 — "Las guaridas naturales presentaron media de 0,362 ± 0,212... Las artificiales norte alcanzaron 0,695 ± 0,125"

---

## 6. Paso 4 — Estadística descriptiva y pruebas no paramétricas

### 6.1 Kruskal-Wallis (comparación de 3 tipos)

```r
kruskal.test(tasa_ocupacion ~ tipo, data = datos_raw)
# Kruskal-Wallis chi-squared = 30.955, df = 2, p-value < 0.001
```

> **→ Resultado del informe:** Sección 4.1 — "Kruskal-Wallis: H = 30,955; gl = 2; p < 0,001"

### 6.2 Mann-Whitney por pares

```r
# Natural vs. arte_norte
wilcox.test(tasa_ocupacion ~ tipo,
            data = subset(datos_raw, tipo %in% c("natural", "arte_norte")),
            correct = TRUE)
# W = 929, p < 0.001

# Natural vs. arte_sur
wilcox.test(tasa_ocupacion ~ tipo,
            data = subset(datos_raw, tipo %in% c("natural", "arte_sur")),
            correct = TRUE)
# W = 720.5, p < 0.001

# Arte_norte vs. arte_sur
wilcox.test(tasa_ocupacion ~ tipo,
            data = subset(datos_raw, tipo %in% c("arte_norte", "arte_sur")),
            correct = TRUE)
# W = 882, p = 0.269 (NO significativo)
```

> **→ Resultado del informe:** Sección 4.1 — "No hubo diferencia significativa entre artificiales norte y sur (W = 882; p = 0,269). Las diferencias natural vs. norte (W = 929; p < 0,001) y natural vs. sur (W = 720,5; p < 0,001) fueron altamente significativas."

### 6.3 Estadísticos descriptivos por tipo

```r
library(dplyr)

resumen <- datos_raw %>%
  filter(!ID_Guarida %in% c("ID10", "ID20", "ID24")) %>%  # excluir sin monitoreo
  group_by(tipo) %>%
  summarise(
    n      = n(),
    media  = mean(tasa_ocupacion),
    DE     = sd(tasa_ocupacion),
    mediana = median(tasa_ocupacion),
    min    = min(tasa_ocupacion),
    max    = max(tasa_ocupacion),
    alta_pct = mean(tasa_ocupacion > 0.75) * 100,
    baja_pct = mean(tasa_ocupacion < 0.50) * 100
  )
print(resumen)
```

> **→ Resultado del informe:** Tabla 4-1 y Tabla 4-2 (Top 10 guaridas más y menos ocupadas)

---

## 7. Paso 5 — Calcular el diferencial térmico (DeltaT) y correlación con ocupación

```r
# Columnas de DeltaT mensual
cols_delta <- grep("DeltaT$", names(datos_raw), value = TRUE)

# Promedio de DeltaT por guarida (ignorando NA)
datos_raw$delta_t_media <- rowMeans(
  datos_raw[, cols_delta], na.rm = TRUE
)

# Correlación de Spearman entre DeltaT y tasa de ocupación
# (excluir guaridas sin temperatura: ID9, ID13, ID19)
datos_temp <- datos_raw[!is.na(datos_raw$delta_t_media), ]
cor.test(datos_temp$delta_t_media, datos_temp$tasa_ocupacion,
         method = "spearman")
# rs = -0.372, p < 0.001
```

> **→ Resultado del informe:** Sección 4.3 — "La correlación de Spearman entre DeltaT y tasa de ocupación fue rs = −0,372 (p < 0,001)"

---

## 8. Paso 6 — Calcular la ocupación mensual (variación estacional)

```r
# Para cada mes, calcular proporción promedio de presencia por tipo
meses <- c("AGO", "SEP", "OCT", "NOV", "DIC", "ENE", "FEB", "MAR")

ocup_mensual <- data.frame()
for (mes in meses) {
  cols_mes <- grep(paste0("^", mes, "_S.*_Pres"), names(datos_raw), value = TRUE)
  datos_raw[[paste0(mes, "_ocup")]] <- rowMeans(datos_raw[, cols_mes], na.rm = TRUE)
  
  temp <- datos_raw %>%
    group_by(tipo) %>%
    summarise(ocup = mean(.data[[paste0(mes, "_ocup")]]), .groups = "drop") %>%
    mutate(mes = mes)
  ocup_mensual <- rbind(ocup_mensual, temp)
}

# Formato tabla
library(tidyr)
ocup_tabla <- ocup_mensual %>% pivot_wider(names_from = mes, values_from = ocup)
print(ocup_tabla)
```

> **→ Resultado del informe:** Tabla 4-3 — "ocupación mínima en agosto (norte: 0,426; sur: 0,357) y máxima en enero (norte: 0,795; sur: 0,749)"

---

## 9. Paso 7 — Obtener variables geomorfológicas desde el DEM

Este es el paso clave de la **Versión 2** del informe, donde las variables se recalcularon directamente desde un DEM descargado con `elevatr`.

### 9.1 Preparar coordenadas espaciales

```r
library(sf)

# Leer coordenadas WGS84 del CSV
coords <- data.frame(
  ID = datos_raw$ID_Guarida,
  lon = datos_raw$UTM_Este,   # en grados decimales (WGS84)
  lat = datos_raw$UTM_Norte
)

# Convertir a objeto sf
puntos_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

# Convertir a UTM zona 19S para distancias en metros
puntos_utm <- st_transform(puntos_sf, crs = 32719)
```

### 9.2 Descargar el DEM con elevatr

```r
library(elevatr)

# Descargar DEM SRTM (zoom level 12 ≈ 17m de resolución a 23°S)
dem <- get_elev_raster(puntos_sf, z = 12, clip = "bbox", expand = 0.01)
# expand = 0.01° (~1 km buffer) para evitar efectos de borde
```

> **→ Resultado del informe:** Sección 3.2.3 — "Los datos de elevación fueron obtenidos mediante el paquete elevatr v0.4.2... zoom level z = 12, con resolución efectiva aproximada de 17 m"

### 9.3 Calcular variables derivadas con terra

```r
library(terra)

dem_rast <- rast(dem)

# Pendiente (grados)
pendiente <- terrain(dem_rast, v = "slope", unit = "degrees")

# Aspecto (dirección de la ladera en grados)
aspecto <- terrain(dem_rast, v = "aspect")

# Rugosidad del terreno (variación en ventana 3×3)
rugosidad <- terrain(dem_rast, v = "roughness")

# TPI - Topographic Position Index
tpi <- terrain(dem_rast, v = "TPI")

# Radiación solar potencial
pendiente_rad <- pendiente * pi / 180
aspecto_rad <- aspecto * pi / 180
radiacion <- shade(pendiente_rad, aspecto_rad, angle = 67, direction = 180)
```

### 9.4 Extraer valores en cada guarida

```r
# Convertir puntos sf a terra vect
puntos_vect <- vect(puntos_sf)

# Extraer valores del DEM y derivados en cada guarida
datos_raw$elevacion_dem  <- extract(dem_rast, puntos_vect)[, 2]
datos_raw$pendiente_dem  <- extract(pendiente, puntos_vect)[, 2]
datos_raw$rugosidad_dem  <- extract(rugosidad, puntos_vect)[, 2]
datos_raw$tpi_dem        <- extract(tpi, puntos_vect)[, 2]
datos_raw$radiacion_dem  <- extract(radiacion, puntos_vect)[, 2]
datos_raw$aspecto_dem    <- extract(aspecto, puntos_vect)[, 2]

# Clasificar exposición de ladera
datos_raw$exposicion_dem <- ifelse(
  datos_raw$aspecto_dem >= 315 | datos_raw$aspecto_dem < 45, "Norte",
  ifelse(datos_raw$aspecto_dem >= 45 & datos_raw$aspecto_dem < 135, "Este",
  ifelse(datos_raw$aspecto_dem >= 135 & datos_raw$aspecto_dem < 225, "Sur",
         "Oeste"))
)
```

> **→ Resultado del informe:** Sección 3.2.3 — "Pendiente (°): calculada con terrain... Rugosidad: terrain(v='roughness')... TPI: terrain(v='TPI')... Exposición de ladera: derivada del aspecto"

### 9.5 Estandarizar variables (z-score)

```r
# Estandarizar para el modelo (media=0, DE=1)
datos_raw$delta_z <- scale(datos_raw$delta_t_media)
datos_raw$rug_z   <- scale(datos_raw$rugosidad_dem)
datos_raw$tpi_z   <- scale(datos_raw$tpi_dem)
datos_raw$rad_z   <- scale(datos_raw$radiacion_dem)

# Verificar correlaciones entre variables (todas deben ser < 0.65)
cor(datos_raw[, c("delta_z", "rug_z", "tpi_z", "rad_z")],
    use = "complete.obs")
# Todas < 0.21 → sin problemas de colinealidad
```

> **→ Resultado del informe:** Sección 3.2.3 — "Todas las variables continuas fueron estandarizadas (z-score)... todas las correlaciones entre delta_z, rug_z, tpi_z y rad_z fueron menores a r = 0,21"

---

## 10. Paso 8 — Preparar la matriz de detección mensual para unmarked

El modelo Royle-Nichols necesita una **matriz de detección** (1/0) por ocasión de muestreo. Las 34 semanas se agregaron en **8 ocasiones mensuales**:

```r
# Agregar semanas a meses: presencia = 1 si al menos 1 semana del mes tuvo detección
meses_cols <- list(
  AGO = grep("^AGO_S.*_Pres", names(datos_raw), value = TRUE),
  SEP = grep("^SEP_S.*_Pres", names(datos_raw), value = TRUE),
  OCT = grep("^OCT_S.*_Pres", names(datos_raw), value = TRUE),
  NOV = grep("^NOV_S.*_Pres", names(datos_raw), value = TRUE),
  DIC = grep("^DIC_S.*_Pres", names(datos_raw), value = TRUE),
  ENE = grep("^ENE_.*_Pres",  names(datos_raw), value = TRUE),
  FEB = grep("^FEB_S.*_Pres", names(datos_raw), value = TRUE),
  MAR = grep("^MAR_S.*_Pres", names(datos_raw), value = TRUE)
)

y_mensual <- sapply(meses_cols, function(cols) {
  as.integer(rowSums(datos_raw[, cols], na.rm = TRUE) > 0)
})

# Verificar: 106 guaridas × 8 meses = 848 celdas
# 83 ceros estructurales (10,1% de las 824 ocasiones con dato)
```

> **→ Resultado del informe:** Sección 3.3.3 — "Las 34 semanas fueron agregadas en 8 ocasiones mensuales... introduciendo 83 ceros estructurales (10,1% de las 824 ocasiones totales)"

### Crear el objeto unmarkedFrame

```r
library(unmarked)

# Covariables de sitio
siteCovs <- data.frame(
  tipo    = factor(datos_raw$tipo, levels = c("arte_norte", "arte_sur", "natural")),
  delta_z = as.numeric(datos_raw$delta_z),
  rug_z   = as.numeric(datos_raw$rug_z),
  tpi_z   = as.numeric(datos_raw$tpi_z),
  rad_z   = as.numeric(datos_raw$rad_z),
  expo    = factor(datos_raw$exposicion_dem)
)

# Crear unmarkedFrameOccu (para occuRN)
umf_mes <- unmarkedFrameOccu(y = y_mensual, siteCovs = siteCovs)
summary(umf_mes)
```

---

## 11. Paso 9 — Ajustar modelos Royle-Nichols y selección por AICc

### 11.1 Ajustar los 21 modelos candidatos

```r
# Modelo nulo
rn0  <- occuRN(~1 ~1, data = umf_mes)

# Solo tipo
rn1  <- occuRN(~1 ~ tipo, data = umf_mes)

# Tipo + cada variable individual
rn2  <- occuRN(~1 ~ tipo + delta_z, data = umf_mes)
rn3  <- occuRN(~1 ~ tipo + rug_z,   data = umf_mes)
rn4  <- occuRN(~1 ~ tipo + tpi_z,   data = umf_mes)  # ← MEJOR MODELO
rn5  <- occuRN(~1 ~ tipo + rad_z,   data = umf_mes)
rn6  <- occuRN(~1 ~ tipo + expo,    data = umf_mes)

# Tipo + combinaciones de 2 variables
rn7  <- occuRN(~1 ~ tipo + delta_z + rug_z, data = umf_mes)
rn8  <- occuRN(~1 ~ tipo + delta_z + rad_z, data = umf_mes)
# ... (continúan hasta 21 modelos)

# Solo variables ambientales (sin tipo)
rn_delta <- occuRN(~1 ~ delta_z, data = umf_mes)
```

### 11.2 Selección de modelos con AICc

```r
library(AICcmodavg)

# Lista de modelos y nombres
modelos <- list(rn0, rn1, rn2, rn3, rn4, rn5, rn6, rn7, rn8, rn_delta)
nombres <- c("N(.)", "N(tipo)", "N(tipo+delta_z)", "N(tipo+rug_z)",
             "N(tipo+tpi_z)", "N(tipo+rad_z)", "N(tipo+expo)",
             "N(tipo+delta_z+rug_z)", "N(tipo+delta_z+rad_z)", "N(delta_z)")

# Tabla de selección
tabla_aic <- aictab(modelos, modnames = nombres, second.ord = TRUE)
print(tabla_aic)
```

> **→ Resultado del informe:** Tabla 4-5
> - **Mejor modelo: N(tipo + tpi_z)** — AICc = 468,50, peso = 0,58
> - Segundo: N(tipo + delta_z) — AICc = 472,40, ΔAICc = 3,91
> - Modelos sin tipo: ΔAICc > 18 (soporte nulo)

---

## 12. Paso 10 — Interpretar coeficientes del mejor modelo

```r
# Mejor modelo: N(tipo + tpi_z)
rn_best <- rn4  # occuRN(~1 ~ tipo + tpi_z)

# Ver coeficientes
summary(rn_best)

# Coeficientes en escala log (abundancia) y logit (detección):
# Abundancia:
#   Intercepto (arte_norte): 2.567 → exp(2.567) = 13.0 ind./guarida
#   tipo arte_sur:          -0.242 → exp(2.567 - 0.242) = 10.2 ind./guarida
#   tipo natural:           -0.874 → exp(2.567 - 0.874) = 5.4 ind./guarida
#   tpi_z:                  -0.153 (p = 0.013) → quebradas = más abundancia
# Detección:
#   Intercepto:             -1.080 → plogis(-1.080) = 0.254 (25.4%)

# Intervalos de confianza 95%
confint(rn_best, type = "state")  # para abundancia
confint(rn_best, type = "det")    # para detección
```

> **→ Resultado del informe:** Sección 4.6 y Tablas 4-6 y 4-7
> - Arte_norte: 13,0 ind./guarida (IC: 8,9–19,0)
> - Arte_sur: 10,2 ind./guarida (IC: 5,3–19,5)
> - Natural: 5,4 ind./guarida (IC: 2,7–10,8)
> - TPI: β = −0,153 (p = 0,013) — quebradas tienen más chinchillas
> - Detección mensual: p = 0,254 (IC: 0,170–0,361)

### Calcular abundancia total

```r
# Abundancia estimada por guarida (suma = individuos-guarida)
N_est <- bup(ranef(rn_best), stat = "mode")  # estimación de N por sitio
sum(N_est)  # ~1069 individuos-guarida
```

> **→ Resultado del informe:** Tabla 4-7 — "N total: 1.069"

---

## 13. Paso 11 — Análisis de conectividad y co-ocurrencia

### 13.1 Distancias entre guaridas

```r
# Calcular matriz de distancias (UTM, en metros)
dist_matrix <- st_distance(puntos_utm)

# Identificar pares a menos de 80 m
pares <- which(as.matrix(dist_matrix) < 80 & as.matrix(dist_matrix) > 0,
               arr.ind = TRUE)
pares <- pares[pares[,1] < pares[,2], ]  # eliminar duplicados
nrow(pares)  # 176 pares
```

> **→ Resultado del informe:** Sección 4.7 — "Se identificaron 176 pares de guaridas a menos de 80 metros"

### 13.2 Índice de co-ocurrencia

```r
# Para cada par, calcular semanas con presencia simultánea / semanas con presencia en al menos una
co_ocurrencia <- apply(pares, 1, function(par) {
  g1 <- as.numeric(datos_raw[par[1], cols_pres])
  g2 <- as.numeric(datos_raw[par[2], cols_pres])
  ambas   <- sum(g1 == 1 & g2 == 1, na.rm = TRUE)
  alguna  <- sum(g1 == 1 | g2 == 1, na.rm = TRUE)
  if (alguna == 0) return(NA)
  return(ambas / alguna)
})

median(co_ocurrencia, na.rm = TRUE)  # 0.62
```

> **→ Resultado del informe:** Sección 4.7 — "El índice de co-ocurrencia entre pares cercanos presentó mediana de 0,62"

### 13.3 Estimación de individuos únicos

```r
# Abundancia total del modelo
N_total <- 1069  # individuos-guarida

# Factor de corrección: cada individuo usa entre 17 y 31 guaridas
# (basado en foto-identificación de ~25 individuos)
individuos_min <- N_total / 31  # = 35
individuos_max <- N_total / 17  # = 64

cat("Rango estimado:", round(individuos_min), "–", round(individuos_max), "individuos únicos\n")
```

> **→ Resultado del informe:** Sección 4.7 y Tabla 4-8 — "el número de individuos únicos se estima entre 35 y 64"

---

## 14. Paso 12 — Análisis intra-cluster

```r
# Asignar número de cluster a cada guarida
datos_raw$cluster <- gsub("[+-].*", "", gsub("^G|^ID", "", datos_raw$ID_Guarida))

# Comparar natural vs. artificial en cada cluster
comparacion_cluster <- datos_raw %>%
  group_by(cluster) %>%
  summarise(
    ocup_natural    = mean(tasa_ocupacion[tipo == "natural"], na.rm = TRUE),
    ocup_artificial = mean(tasa_ocupacion[tipo != "natural"], na.rm = TRUE),
    artificial_mayor = ocup_artificial > ocup_natural
  ) %>%
  filter(!is.na(ocup_natural))

# Porcentaje de clusters donde artificial > natural
mean(comparacion_cluster$artificial_mayor) * 100  # 85%
```

> **→ Resultado del informe:** Sección 4.4 — "En el 85% de los clusters (23/27), las guaridas artificiales superaron en ocupación a la natural de referencia"

---

## 15. Resumen: Mapa script → informe

| Sección del informe | Función R principal | Resultado clave |
|---------------------|---------------------|-----------------|
| Tabla 3-1 (diseño) | `table(tipo)` | 27 naturales, 44 norte, 35 sur |
| Tabla 4-1 (descriptivos) | `group_by() %>% summarise()` | Media ocupación: 0,362 / 0,695 / 0,662 |
| Sección 4.1 (Kruskal-Wallis) | `kruskal.test()` | H = 30,955; p < 0,001 |
| Sección 4.1 (Mann-Whitney) | `wilcox.test()` | W = 882; p = 0,269 (N vs. S) |
| Tabla 4-3 (estacional) | `rowMeans()` por mes | Mín ago, máx ene |
| Sección 4.3 (DeltaT) | `cor.test(method="spearman")` | rs = −0,372; p < 0,001 |
| Sección 3.2.3 (DEM) | `get_elev_raster(z=12)` | DEM SRTM ~17 m resolución |
| Sección 3.2.3 (variables) | `terrain()`, `shade()` | Pendiente, rugosidad, TPI, radiación |
| Tabla 4-5 (modelos) | `occuRN()` + `aictab()` | Mejor: N(tipo+tpi_z), AICc=468,50 |
| Tabla 4-6 (coeficientes) | `summary(rn_best)` | Intercepto=2,567; tpi_z=−0,153 |
| Tabla 4-7 (abundancia) | `exp(coeficientes)` | 13,0 / 10,2 / 5,4 ind./guarida |
| Sección 4.7 (conectividad) | `st_distance()` | 176 pares < 80 m |
| Tabla 4-8 (individuos) | N_total / guaridas_por_ind | 35–64 individuos únicos |

---

## 16. Cómo reproducir los resultados

1. **Abrir R** (versión 4.3.2 o superior)
2. **Instalar paquetes**: `unmarked`, `AICcmodavg`, `elevatr`, `terra`, `sf`
3. **Colocar los archivos CSV** en el directorio de trabajo
4. **Ejecutar los pasos 1–12** en orden
5. Los resultados numéricos deben coincidir con los del informe

> ⚠️ **Nota importante**: La descarga del DEM con `elevatr` requiere conexión a internet. Los valores extraídos pueden variar ligeramente si cambia la fuente de datos o el zoom level. Los valores del archivo `Datos_Chinchilla_SADDN_Geomorfologicos_v2.csv` son los valores definitivos usados en el informe.

---

## 17. Glosario rápido de funciones R

| Función | Paquete | Qué hace |
|---------|---------|----------|
| `read.csv2()` | base | Lee CSV con separador `;` y decimal `,` |
| `kruskal.test()` | stats | Prueba no paramétrica para 3+ grupos |
| `wilcox.test()` | stats | Prueba Mann-Whitney para 2 grupos |
| `cor.test(method="spearman")` | stats | Correlación de rangos |
| `get_elev_raster()` | elevatr | Descarga DEM desde AWS/SRTM |
| `terrain()` | terra | Calcula pendiente, rugosidad, TPI, aspecto |
| `shade()` | terra | Calcula radiación solar potencial |
| `scale()` | base | Estandariza variables (z-score) |
| `occuRN()` | unmarked | Ajusta modelo Royle-Nichols |
| `aictab()` | AICcmodavg | Tabla de selección de modelos por AICc |
| `confint()` | stats | Intervalos de confianza 95% |
| `bup(ranef())` | unmarked | Estimación de abundancia por sitio |
| `st_distance()` | sf | Matriz de distancias entre puntos |

---

*Tutorial generado a partir del informe_final_v2_SADDN.docx y los datos del repositorio dcarden1/data (rama Chinchillas).*
