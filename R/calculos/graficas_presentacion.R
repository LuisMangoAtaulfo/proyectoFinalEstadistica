# ============================================================
# GRÁFICAS DE LA PRESENTACIÓN FINAL
# Análisis de vulnerabilidades CVE
#
# Reproduce las 8 gráficas de la presentación usando la
# población de N = 1,969 CVE (1 al 23 de enero de 2024),
# que es la que aparece en las diapositivas.
#
# Solo R base. Ejecutar desde R/calculos:
#   Rscript graficas_presentacion.R
#
# Las imágenes se guardan en la carpeta:
#   graficas_presentacion/
# ============================================================


# ------------------------------------------------------------
# 1. CARGAR DATOS Y CREAR LA MUESTRA
# ------------------------------------------------------------
# En RStudio: usar la carpeta donde está este script
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  archivo <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(archivo)) setwd(dirname(archivo))
}

# Buscar el CSV de la presentación (N = 1,969)
ruta <- "datos_cve_1969.csv"
if (!file.exists(ruta)) ruta <- "R/calculos/datos_cve_1969.csv"
if (!file.exists(ruta)) ruta <- "../R/calculos/datos_cve_1969.csv"
if (!file.exists(ruta)) stop("No se encontró datos_cve_1969.csv")

poblacion <- read.csv(ruta)
poblacion$Fecha_Publicacion <- as.Date(poblacion$Fecha_Publicacion)

set.seed(42)                                  # misma muestra que la presentación
muestra <- poblacion[sample(nrow(poblacion), 60), ]

x <- muestra$Base_Score                       # variable de estudio
n <- length(x)                                # n = 60
N <- nrow(poblacion)                          # N = 1,969

# Estadísticos que se usan en las gráficas
media_m <- mean(x)
mediana_m <- median(x)
var_m <- var(x)
sd_m <- sd(x)

media_p <- mean(poblacion$Base_Score)
var_p <- mean((poblacion$Base_Score - media_p)^2)
sd_p <- sqrt(var_p)

# Clases para el histograma y el polígono
rango <- max(x) - min(x)
k <- ceiling(1 + 3.322 * log10(n))            # 7 clases
A <- rango / k                                # amplitud
cortes <- seq(min(x), max(x), length.out = k + 1)

# Carpeta de salida, junto al CSV
carpeta <- file.path(dirname(ruta), "graficas_presentacion")
dir.create(carpeta, showWarnings = FALSE)

cat("Población: N =", N, " media =", round(media_p, 4),
    " varianza =", round(var_p, 4), "\n")
cat("Muestra:   n =", n, " media =", round(media_m, 4),
    " varianza =", round(var_m, 4), " mediana =", mediana_m, "\n")
cat("Cinco números:", paste(round(fivenum(x), 2), collapse = ", "), "\n\n")


# ------------------------------------------------------------
# GRÁFICA 1. CVE publicadas por día (población)
# ------------------------------------------------------------
conteo_dia <- table(poblacion$Fecha_Publicacion)

# Etiquetas: solo 1, 8, 15 y 22 de enero, como en la presentación
etiquetas <- paste(as.integer(format(as.Date(names(conteo_dia)), "%d")), "ene")
etiquetas[-c(1, 8, 15, 22)] <- ""

png(file.path(carpeta, "01_cve_por_dia.png"),
    width = 1440, height = 768, res = 120)

barplot(conteo_dia,
        col = "#0E8AA3", border = "white",
        ylim = c(0, 250),
        names.arg = etiquetas,
        main = "CVE publicadas por día (población)",
        xlab = "", ylab = "CVE", las = 1)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 2. Composición por severidad: población vs. muestra
# ------------------------------------------------------------
niveles <- c("LOW", "MEDIUM", "HIGH", "CRITICAL")

# Se excluye NONE (1 registro) para comparar las 4 severidades
pob4 <- subset(poblacion, Severidad != "NONE")
f_pob <- table(factor(pob4$Severidad, levels = niveles))
f_mue <- table(factor(muestra$Severidad, levels = niveles))

# Porcentajes de cada grupo
mat <- rbind(100 * f_pob / sum(f_pob),
             100 * f_mue / sum(f_mue))
colnames(mat) <- c("Baja", "Media", "Alta", "Crítica")

png(file.path(carpeta, "02_severidad.png"),
    width = 1440, height = 768, res = 120)

barplot(mat, beside = TRUE,
        col = c("#004AAD", "#0E8AA3"), border = NA,
        ylim = c(0, 60),
        main = "Composición por severidad",
        ylab = "Porcentaje (%)",
        legend.text = c(paste0("Población (N = ", N, ")"),
                        paste0("Muestra (n = ", n, ")")),
        args.legend = list(bty = "n", x = "topright"))

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 3. Histograma de frecuencias relativas
# ------------------------------------------------------------
h <- hist(x, breaks = cortes, right = FALSE, include.lowest = TRUE,
          plot = FALSE)
h$counts <- h$counts / n                      # frecuencias relativas

png(file.path(carpeta, "03_histograma.png"),
    width = 1440, height = 768, res = 120)

plot(h, col = "#A8D8E3", border = "#0E8AA3", axes = FALSE,
     ylim = c(0, 0.25),
     main = "Histograma de frecuencias relativas",
     xlab = "Base Score (límites de clase)",
     ylab = "Frecuencia relativa")
axis(1, at = cortes, labels = sprintf("%.2f", cortes), cex.axis = 0.8)
axis(2, las = 1)

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 4. Polígono de frecuencias relativas
# ------------------------------------------------------------
# Se agregan dos puntos en cero en los extremos, como en la presentación
x_pol <- c(h$mids[1] - A, h$mids, h$mids[length(h$mids)] + A)
y_pol <- c(0, h$counts, 0)

png(file.path(carpeta, "04_poligono.png"),
    width = 1440, height = 768, res = 120)

plot(x_pol, y_pol, type = "b", pch = 19, lwd = 2, col = "#0E8AA3",
     xlim = range(x_pol), ylim = c(0, 0.25), axes = FALSE,
     main = "Polígono de frecuencias relativas",
     xlab = "Marca de clase (Base Score)",
     ylab = "Frecuencia relativa")
axis(1, at = x_pol, labels = sprintf("%.2f", x_pol), cex.axis = 0.8)
axis(2, las = 1)

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 5. Diagrama de caja y bigotes
# ------------------------------------------------------------
st <- fivenum(x)                              # mín, Q1, mediana, Q3, máx

png(file.path(carpeta, "05_caja_bigotes.png"),
    width = 1440, height = 768, res = 120)

boxplot(x, horizontal = TRUE,
        col = "#A8D8E3", border = "#0E8AA3",
        main = "Diagrama de caja y bigotes del Base Score",
        xlab = "Base Score", ylim = c(2, 10))
points(mean(x), 1, pch = 18, cex = 2, col = "red")
title(sub = paste0("Mín = ", st[1], "   Q1 = ", st[2],
                   "   Mediana = ", st[3], "   Q3 = ", st[4],
                   "   Máx = ", st[5], "   Media = ", round(mean(x), 2)),
      cex.sub = 0.8)

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 6. Intervalo de confianza para la media (95 %)
# ------------------------------------------------------------
ic_media <- t.test(x, conf.level = 0.95)$conf.int

png(file.path(carpeta, "06_ic_media.png"),
    width = 1440, height = 768, res = 120)

plot(NA, xlim = c(6.0, 7.2), ylim = c(0, 1), yaxt = "n",
     xlab = "Base Score", ylab = "",
     main = "Intervalo de confianza para la media (95%)")

segments(ic_media[1], 0.5, ic_media[2], 0.5, lwd = 10, col = "#0E8AA3")
segments(ic_media[1], 0.42, ic_media[1], 0.58, lwd = 2, col = "#0E8AA3")
segments(ic_media[2], 0.42, ic_media[2], 0.58, lwd = 2, col = "#0E8AA3")

points(media_m, 0.5, pch = 19, cex = 2, col = "#004AAD")
points(media_p, 0.5, pch = 18, cex = 2, col = "red")

text(ic_media[1], 0.68, sprintf("%.2f", ic_media[1]), cex = 0.9)
text(ic_media[2], 0.68, sprintf("%.2f", ic_media[2]), cex = 0.9)
text(media_p, 0.72, sprintf("Población = %.2f", media_p), cex = 0.9)
text(media_m, 0.28, sprintf("Muestra = %.2f", media_m), cex = 0.9)

legend("topright",
       legend = c("IC 95%", "Media muestral", "Media poblacional"),
       lwd = c(10, NA, NA), pch = c(NA, 19, 18),
       col = c("#0E8AA3", "#004AAD", "red"), bty = "n")

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 7. Intervalo de confianza para la varianza (95 %)
# ------------------------------------------------------------
ic_var <- c((n - 1) * var(x) / qchisq(0.975, n - 1),
            (n - 1) * var(x) / qchisq(0.025, n - 1))

png(file.path(carpeta, "07_ic_varianza.png"),
    width = 1440, height = 768, res = 120)

plot(NA, xlim = c(2, 5), ylim = c(0, 1), yaxt = "n",
     xlab = "Varianza", ylab = "",
     main = "Intervalo de confianza para la varianza (95%)")

segments(ic_var[1], 0.5, ic_var[2], 0.5, lwd = 10, col = "#A8D8E3")
segments(ic_var[1], 0.42, ic_var[1], 0.58, lwd = 2, col = "#0E8AA3")
segments(ic_var[2], 0.42, ic_var[2], 0.58, lwd = 2, col = "#0E8AA3")

points(var_m, 0.5, pch = 19, cex = 2, col = "#004AAD")
points(var_p, 0.5, pch = 18, cex = 2, col = "red")

text(ic_var[1], 0.68, sprintf("%.2f", ic_var[1]), cex = 0.9)
text(ic_var[2], 0.68, sprintf("%.2f", ic_var[2]), cex = 0.9)
text(var_p, 0.72, sprintf("Población = %.2f", var_p), cex = 0.9, adj = c(1, 0.5))
text(var_m, 0.28, sprintf("Muestra = %.2f", var_m), cex = 0.9, adj = c(0, 0.5))

legend("topright",
       legend = c("IC 95%", "Varianza muestral", "Varianza poblacional"),
       lwd = c(10, NA, NA), pch = c(NA, 19, 18),
       col = c("#A8D8E3", "#004AAD", "red"), bty = "n")

invisible(dev.off())


# ------------------------------------------------------------
# GRÁFICA 8. Regresión lineal con banda de confianza al 95 %
# ------------------------------------------------------------
modelo <- lm(Base_Score ~ Impact_Score, data = muestra)

nuevo <- data.frame(
  Impact_Score = seq(min(muestra$Impact_Score),
                     max(muestra$Impact_Score),
                     length.out = 100)
)
banda <- predict(modelo, nuevo, interval = "confidence", level = 0.95)

png(file.path(carpeta, "08_regresion.png"),
    width = 1440, height = 768, res = 120)

plot(muestra$Impact_Score, muestra$Base_Score, type = "n",
     xlim = c(1, 6), ylim = c(2, 10),
     xlab = "Impacto (subpuntaje CVSS)", ylab = "Base Score",
     main = "Regresión lineal: impacto y severidad")

# Banda de confianza (primero, para que quede detrás)
polygon(c(nuevo$Impact_Score, rev(nuevo$Impact_Score)),
        c(banda[, "lwr"], rev(banda[, "upr"])),
        col = rgb(0, 0, 0, 0.12), border = NA)

points(muestra$Impact_Score, muestra$Base_Score,
       pch = 19, col = "#0E8AA3")
abline(modelo, col = "black", lwd = 2)

legend("topleft",
       legend = c("CVE de la muestra", "Recta estimada",
                  "Banda de confianza al 95%"),
       pch = c(19, NA, NA), lty = c(NA, 1, 1), lwd = c(NA, 2, 8),
       col = c("#0E8AA3", "black", rgb(0, 0, 0, 0.2)), bty = "n")

text(1.2, 2.6,
     sprintf("Base = %.2f + %.2f * Impacto",
             coef(modelo)[1], coef(modelo)[2]),
     adj = 0, cex = 0.9)
text(1.2, 2.0,
     sprintf("r = %.2f    R2 = %.2f",
             cor(muestra$Base_Score, muestra$Impact_Score),
             summary(modelo)$r.squared),
     adj = 0, cex = 0.9)

invisible(dev.off())


# ------------------------------------------------------------
# RESUMEN EN CONSOLA
# ------------------------------------------------------------
cat("IC 95% para la media:   ", round(ic_media, 4), "\n")
cat("IC 95% para la varianza:", round(ic_var, 4), "\n")
cat("Regresión: Base =", round(coef(modelo)[1], 4), "+",
    round(coef(modelo)[2], 4), "* Impacto\n")
cat("r =", round(cor(muestra$Base_Score, muestra$Impact_Score), 4),
    " R2 =", round(summary(modelo)$r.squared, 4),
    " Error estándar =", round(summary(modelo)$sigma, 4), "\n")
cat("\nGráficas guardadas en:", carpeta, "\n")
