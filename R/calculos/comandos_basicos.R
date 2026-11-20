# ============================================================
# COMANDOS BÁSICOS DE R — PROYECTO FINAL DE ESTADÍSTICA
# Variable de estudio: Base_Score (puntaje CVSS de un CVE)
#
# Solo se usa R base: nada de paquetes ni funciones propias.
#
# Se puede ejecutar desde RStudio sin importar la carpeta de trabajo:
#   - desde R/calculos:  Rscript comandos_basicos.R
#   - desde la raíz del repo, o abriendo el archivo en RStudio.
# ============================================================


# ------------------------------------------------------------
# PUNTO 2. POBLACIÓN Y MUESTRA
# ------------------------------------------------------------
# En RStudio: usar la carpeta donde está este script
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  archivo <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(archivo)) setwd(dirname(archivo))
}

# Buscar el CSV en las ubicaciones normales del proyecto
ruta <- "datos_cve_completos.csv"                       # si estás en R/calculos
if (!file.exists(ruta)) ruta <- "R/calculos/datos_cve_completos.csv"      # si estás en la raíz
if (!file.exists(ruta)) ruta <- "../R/calculos/datos_cve_completos.csv"   # si estás en informe/
if (!file.exists(ruta)) stop("No se encontró datos_cve_completos.csv")

poblacion <- read.csv(ruta)
carpeta_datos <- dirname(ruta)   # para guardar las salidas junto al CSV

nrow(poblacion)              # N = tamaño de la población
names(poblacion)             # variables del archivo
summary(poblacion)           # resumen rápido de todas las variables

set.seed(42)                 # semilla: la muestra siempre es la misma
muestra <- poblacion[sample(nrow(poblacion), 60), ]   # n = 60

x <- muestra$Base_Score      # variable cuantitativa que vamos a estudiar
length(x)                    # n de la muestra


# ------------------------------------------------------------
# PUNTO 3. RECOPILACIÓN Y REVISIÓN DE DATOS
# (los datos ya se descargaron de la API del NIST en Python)
# ------------------------------------------------------------
head(poblacion)              # primeras filas
dim(poblacion)               # filas y columnas
sum(is.na(poblacion$Base_Score))   # datos faltantes en la variable

# Si algún día se descarga directo desde una URL:
# datos <- read.csv("https://.../archivo.csv")


# ------------------------------------------------------------
# PUNTO 4. TABLA DE FRECUENCIAS, HISTOGRAMA Y POLÍGONO
# ------------------------------------------------------------
cortes <- 0:10               # intervalos [0,1), [1,2), ... [9,10]
clases <- cut(x, breaks = cortes, right = FALSE, include.lowest = TRUE)
frec_abs <- table(clases)

tabla <- data.frame(
  Intervalo        = names(frec_abs),
  Marca_Clase      = cortes[-length(cortes)] + 0.5,
  Frec_Absoluta    = as.numeric(frec_abs),
  Frec_Relativa    = round(as.numeric(frec_abs) / length(x), 4),
  Frec_Acumulada   = cumsum(as.numeric(frec_abs)),
  Frec_Rel_Acum    = round(cumsum(as.numeric(frec_abs)) / length(x), 4)
)
print(tabla)
write.csv(tabla, file.path(carpeta_datos, "tabla_frecuencias_basica.csv"),
          row.names = FALSE)

# Histograma de frecuencias relativas + polígono
h <- hist(x, breaks = cortes, right = FALSE, plot = FALSE)
h$counts <- h$counts / length(x)     # convertir a frecuencias relativas

plot(h, col = "steelblue", main = "Base Score (muestra n = 60)",
     xlab = "Puntaje CVSS", ylab = "Frecuencia relativa")
lines(h$mids, h$counts, type = "b", col = "red", lwd = 2, pch = 19)


# ------------------------------------------------------------
# PUNTO 5. PARÁMETROS DESCRIPTIVOS DE LA MUESTRA
# ------------------------------------------------------------
mean(x)                      # media
median(x)                    # mediana
names(which.max(table(x)))   # moda (valor que más se repite)
var(x)                       # varianza muestral (divide entre n-1)
sd(x)                        # desviación estándar muestral

# Sesgo y curtosis (se calculan con los momentos)
m2 <- mean((x - mean(x))^2)
m3 <- mean((x - mean(x))^3)
m4 <- mean((x - mean(x))^4)
m3 / m2^1.5                  # coeficiente de sesgo
m4 / m2^2                    # coeficiente de curtosis (sin restar 3)

# Moda con la fórmula de datos agrupados (opcional, usa la tabla)
i  <- which.max(frec_abs)    # intervalo con más frecuencia
A  <- 1                      # amplitud de cada intervalo
fa <- as.numeric(frec_abs)[i - 1]
fp <- as.numeric(frec_abs)[i + 1]
cortes[i] + (frec_abs[i] - fa) /
  ((frec_abs[i] - fa) + (frec_abs[i] - fp)) * A


# ------------------------------------------------------------
# PUNTO 6. PARÁMETROS POBLACIONALES VS. MUESTRALES
# ------------------------------------------------------------
p <- poblacion$Base_Score

media_p <- mean(p)                        # media poblacional (mu)
var_p   <- mean((p - media_p)^2)          # varianza poblacional (divide entre N)
sd_p    <- sqrt(var_p)                    # desviación estándar poblacional
media_p
var_p
sd_p

# Comparación
mean(x)                                   # media muestral
var(x)                                    # varianza muestral
sd(x)                                     # desviación estándar muestral


# ------------------------------------------------------------
# PUNTO 7. CINCO EXPERIMENTOS CON DISTINTAS DISTRIBUCIONES
# ------------------------------------------------------------
N <- nrow(poblacion)
p_crit <- mean(poblacion$Severidad == "CRITICAL")   # proporción de CRITICAL
K_red  <- sum(poblacion$Vector_Ataque == "NETWORK") # CVEs con vector NETWORK
lambda <- sum(poblacion$Severidad == "CRITICAL") / 91  # CRITICAL por día

# 1) BINOMIAL: X = nº de CRITICAL entre 10 CVEs
dbinom(2, size = 10, prob = p_crit)        # P(X = 2)
1 - dbinom(0, size = 10, prob = p_crit)    # P(X >= 1)

# 2) POISSON: X = nº de CRITICAL publicadas en un día
dpois(0, lambda)                           # P(X = 0)
1 - ppois(1, lambda)                       # P(X >= 2)

# 3) GEOMÉTRICA: X = nº de CVEs revisados hasta hallar el primero CRITICAL
# R cuenta fracasos: X = 6 equivale a dgeom(5, p)
dgeom(5, prob = p_crit)                    # P(X = 6)
pgeom(3, prob = p_crit)                    # P(X <= 4)

# 4) HIPERGEOMÉTRICA: X = nº de NETWORK en 20 CVEs elegidos sin reemplazo
dhyper(20, m = K_red, n = N - K_red, k = 20)       # P(X = 20)
1 - phyper(17, m = K_red, n = N - K_red, k = 20)   # P(X >= 18)

# 5) NORMAL: X = Base Score de un CVE al azar
pnorm(8, mean(x), sd(x)) - pnorm(6, mean(x), sd(x))  # P(6 <= X <= 8)
1 - pnorm(9, mean(x), sd(x))                         # P(X > 9)
pnorm(5, mean(x), sd(x))                             # P(X < 5)

# Gráfica rápida de una distribución (ejemplo binomial)
plot(0:10, dbinom(0:10, 10, p_crit), type = "h", lwd = 3,
     xlab = "x", ylab = "P(X = x)", main = "Binomial(10, p)")


# ------------------------------------------------------------
# PUNTO 8. DOS INTERVALOS DE CONFIANZA
# ------------------------------------------------------------
# (a) Intervalo de confianza para la MEDIA (95 %)
t.test(x, conf.level = 0.95)$conf.int

# (b) Intervalo de confianza para la VARIANZA (95 %)
n <- length(x)
c((n - 1) * var(x) / qchisq(0.975, n - 1),
  (n - 1) * var(x) / qchisq(0.025, n - 1))

# Comparación con la población: el parámetro poblacional debe caer dentro
media_p          # comparar con el IC de la media
var_p            # comparar con el IC de la varianza

# (Alternativa) IC para la DIFERENCIA DE MEDIAS:
# NETWORK vs. LOCAL
red   <- muestra$Base_Score[muestra$Vector_Ataque == "NETWORK"]
local <- muestra$Base_Score[muestra$Vector_Ataque == "LOCAL"]
t.test(red, local, conf.level = 0.95)$conf.int


# ------------------------------------------------------------
# PUNTO 9. REGRESIÓN LINEAL
# ------------------------------------------------------------
plot(muestra$Impact_Score, muestra$Base_Score,
     xlab = "Impact Score", ylab = "Base Score",
     main = "Regresión lineal")

modelo <- lm(Base_Score ~ Impact_Score, data = muestra)
abline(modelo, col = "red", lwd = 2)
summary(modelo)              # ecuación, R cuadrada, p-valor
cor(muestra$Impact_Score, muestra$Base_Score)   # correlación
