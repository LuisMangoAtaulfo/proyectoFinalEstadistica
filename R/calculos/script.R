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
cortes <- seq(0, 10, by = 1)
intervalos <- cut(datos, breaks = cortes, right = FALSE)

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

# IMPRIMIR EN LA CONSOLA:
cat("\n========================================================\n")
cat("   TABLA DE FRECUENCIAS (MUESTRA n=500) - Base Score\n")
cat("========================================================\n")
print(tabla_frecuencias)
cat("========================================================\n\n")

# =====================================================================
# ¡NUEVO!: GUARDAR LA TABLA COMO ARCHIVO PARA ABRIR EN EXCEL
# =====================================================================
write.csv(tabla_frecuencias, "tabla_frecuencias.csv", row.names = FALSE)
cat("¡Éxito! Tu tabla también se guardó como 'tabla_frecuencias.csv' en tu carpeta.\n")


# =====================================================================
# 3. HISTOGRAMA Y POLÍGONO DE FRECUENCIAS RELATIVAS
# =====================================================================
h <- hist(datos, breaks = cortes, right = FALSE, plot = FALSE)
h$counts <- h$counts / sum(h$counts)

plot(h, freq = TRUE, 
     col = "steelblue", border = "black",
     main = "Distribución de la Severidad (Base Score)",
     xlab = "Puntaje CVSS (0.0 a 10.0)",
     ylab = "Frecuencia Relativa",
     ylim = c(0, max(h$counts) * 1.2), 
     axes = FALSE) 

axis(1, at = cortes) 
axis(2) 

x_pol <- c(cortes[1] - 0.5, h$mids, cortes[length(cortes)] + 0.5)
y_pol <- c(0, h$counts, 0)
lines(x_pol, y_pol, col = "darkred", lwd = 2.5, type = "b", pch = 19)

legend("topright", legend = c("Histograma", "Polígono de Frec. Relativa"), 
       fill = c("steelblue", NA), border = c("black", NA),
       col = c(NA, "darkred"), lwd = c(NA, 2.5), pch = c(NA, 19), bty = "n")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")