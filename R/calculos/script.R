# =====================================================================
# 1. CARGA DE DATOS Y EXTRACCIÓN DE LA MUESTRA
# =====================================================================
poblacion <- read.csv("datos_cve_completos.csv")

set.seed(42) 
muestra <- poblacion[sample(nrow(poblacion), 500), ]
datos <- na.omit(muestra$Base_Score)

# =====================================================================
# 2. CREACIÓN DE LA TABLA DE FRECUENCIAS
# =====================================================================
# Cortes desde el mínimo al máximo real: evita intervalos en cero
cortes <- floor(min(datos)):ceiling(max(datos))
intervalos <- cut(datos, breaks = cortes, right = FALSE, include.lowest = TRUE)

frec_abs <- table(intervalos)
frec_rel <- prop.table(frec_abs)
marcas_clase <- cortes[-length(cortes)] + 0.5 

tabla_frecuencias <- data.frame(
  Intervalo = names(frec_abs),
  Marca_Clase = marcas_clase,
  Frec_Absoluta = as.numeric(frec_abs),
  Frec_Relativa = round(as.numeric(frec_rel), 4),
  Frec_Abs_Acumulada = cumsum(as.numeric(frec_abs)),
  Frec_Rel_Acumulada = round(cumsum(as.numeric(frec_rel)), 4)
)

# Quitar intervalos con frecuencia cero
tabla_frecuencias <- tabla_frecuencias[tabla_frecuencias$Frec_Absoluta > 0, ]

# IMPRIMIR EN LA CONSOLA:
cat("\n========================================================\n")
cat("   TABLA DE FRECUENCIAS (MUESTRA n=500) - Base Score\n")
cat("========================================================\n")
print(tabla_frecuencias)
cat("Suma de frecuencias:", sum(tabla_frecuencias$Frec_Absoluta),
    " | n =", length(datos), "\n")
cat("========================================================\n\n")

# =====================================================================
# ¡NUEVO!: GUARDAR LA TABLA COMO ARCHIVO PARA ABRIR EN EXCEL
# =====================================================================
write.csv(tabla_frecuencias, "tabla_frecuencias.csv", row.names = FALSE)
cat("¡Éxito! Tu tabla también se guardó como 'tabla_frecuencias.csv' en tu carpeta.\n")


# =====================================================================
# 3. HISTOGRAMA Y POLÍGONO DE FRECUENCIAS RELATIVAS
# =====================================================================
h <- hist(datos, breaks = cortes, right = FALSE, include.lowest = TRUE,
          plot = FALSE)
h$counts <- h$counts / sum(h$counts)

# Histograma (gráfica sola)
plot(h, freq = TRUE,
     col = "steelblue", border = "black",
     main = "Histograma del Base Score",
     xlab = "Puntaje CVSS",
     ylab = "Frecuencia relativa")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# Polígono de frecuencias (gráfica aparte, sin histograma)
plot(h$mids, h$counts, type = "b", pch = 19, lwd = 2.5,
     col = "darkred",
     main = "Polígono de frecuencias relativas",
     xlab = "Puntaje CVSS",
     ylab = "Frecuencia relativa")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")


# =====================================================================
# 4. TLC: 30 MUESTREOS ALEATORIOS Y SUS MEDIAS
# =====================================================================
# Cada muestra es aleatoria simple sin reemplazo, de tamaño n = 60.
set.seed(42)
medias30 <- replicate(30, mean(sample(poblacion$Base_Score, 60, replace = FALSE)))

mean(medias30)                      # media de las medias
sd(medias30)                        # desviación de las medias
sd(poblacion$Base_Score) / sqrt(60) # error estándar teórico

# Con 30 medias el histograma sale irregular. Se usan 1000
# muestreos solo para que la campana del TLC se aprecie bien.
set.seed(42)
medias <- replicate(1000, mean(sample(poblacion$Base_Score, 60)))

hist(medias, breaks = 20, freq = FALSE, col = "lightgreen",
     main = "TLC: 1000 medias muestrales (n = 60)",
     xlab = "Media muestral", ylab = "Densidad")
curve(dnorm(x, mean(medias), sd(medias)), add = TRUE, col = "red", lwd = 2)