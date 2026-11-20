# =====================================================================
# parametros_kiss.R
# Puntos 5, 6 y 7 del Proyecto Final de Estadística — versión KISS.
#
# Mismo flujo y mismas macros de salida que parametros.R, pero:
#   * Sin la tabla de Walpole embebida: las probabilidades de la normal
#     estándar se calculan con pnorm() (función base de R).
#   * Funciones auxiliares pequeñas, cada una con su comentario de qué
#     hace y qué parámetros recibe.
#
# Fórmulas: todas del formulario del curso (J. D. Fuentes Mora).
# Variable: X = Base_Score (CVSS v3.1) de vulnerabilidades CVE.
# Salida: imprime en consola y escribe "resultados.tex" (macros LaTeX
#         que informe.tex importa con \input).
#
# Uso:  cd R/calculos && Rscript parametros_kiss.R
# =====================================================================

poblacion <- read.csv("datos_cve_completos.csv")

# Muestra aleatoria simple sin reemplazo, n = 60.
# set.seed(42) fija la semilla del generador para que el muestreo sea
# reproducible: siempre se obtiene la misma muestra.
set.seed(42)
muestra <- poblacion[sample(nrow(poblacion), 60), ]
datos   <- na.omit(muestra$Base_Score)   # limpia filas sin Base_Score
n       <- length(datos)                 # tamaño muestral (60)
N       <- nrow(poblacion)               # tamaño poblacional (1969)

# Formato numérico: fija cuántos decimales se muestran/imprimen.
fmt <- function(x, d = 4) formatC(x, format = "f", digits = d)

# Acumulador de líneas para el archivo de macros LaTeX.
R_tex <- character(0)
add   <- function(...) R_tex <<- c(R_tex, ...)

# =====================================================================
# FUNCIONES AUXILIARES
# =====================================================================

# momento_central(x, r)
#   Calcula el momento central muestral  m_r = (1/n) · Σ (x_i − x̄)^r
#   Parámetros:
#     x : vector numérico con los datos de la muestra
#     r : orden del momento, entero >= 1
#         (r = 2 varianza, r = 3 sesgo, r = 4 curtosis)
#   Retorna: valor numérico de m_r
momento_central <- function(x, r) {
  mean((x - mean(x))^r)
}

# moda_agrupada(cortes, frecs)
#   Moda por la fórmula de datos agrupados del formulario:
#     Mod = L_inf + [d1/(d1+d2)]·A ,  d1 = f_mod − f_ant ,  d2 = f_mod − f_pos
#   Parámetros:
#     cortes : vector con los límites de los intervalos, longitud k + 1
#              (ej: c(2.4, 3.49, ..., 10.0))
#     frecs  : frecuencias absolutas por intervalo, longitud k
#   Retorna: lista con el valor de la moda y sus componentes
#            (mod, Linf, A, fmod, fant, fpos, d1, d2)
moda_agrupada <- function(cortes, frecs) {
  i   <- which.max(frecs)                    # índice del intervalo modal
  fmo <- frecs[i]                            # f_mod
  fan <- if (i > 1) frecs[i - 1] else 0      # f_ant (0 si es el primero)
  fpo <- if (i < length(frecs)) frecs[i + 1] else 0   # f_pos (0 si es el último)
  d1  <- fmo - fan                           # d1
  d2  <- fmo - fpo                           # d2
  A   <- cortes[2] - cortes[1]               # amplitud (intervalos equiespaciados)
  list(mod  = cortes[i] + (d1 / (d1 + d2)) * A,
       Linf = cortes[i], A = A, fmod = fmo, fant = fan, fpos = fpo,
       d1 = d1, d2 = d2)
}

# errores(estadistico, parametro)
#   Compara un estadístico muestral contra su parámetro poblacional.
#   Parámetros:
#     estadistico : valor muestral (x̄, s², s o CV de la muestra)
#     parametro   : valor poblacional correspondiente (µ, σ², σ o CV)
#   Retorna: vector c(error_absoluto, error_relativo_en_porcentaje)
errores <- function(estadistico, parametro) {
  eabs <- abs(estadistico - parametro)
  c(eabs, 100 * eabs / abs(parametro))
}

# estandarizar(x, media, desv)
#   Convierte un valor de X a su puntaje z (normal estándar):
#     z = (x − µ)/σ ,  con Z ~ N(0,1)
#   Parámetros:
#     x     : valor de la variable original a estandarizar
#     media : µ de la distribución normal
#     desv  : σ de la distribución normal
#   Retorna: valor z (numérico)
estandarizar <- function(x, media, desv) {
  (x - media) / desv
}

# p_entre(a, b, media, desv)
#   Probabilidad P(a ≤ X ≤ b) para X ~ N(media, desv²).
#   Parámetros:
#     a     : límite inferior del intervalo
#     b     : límite superior del intervalo
#     media : µ de la distribución
#     desv  : σ de la distribución
#   Retorna: probabilidad numérica (usa pnorm de R)
p_entre <- function(a, b, media, desv) {
  pnorm(b, media, desv) - pnorm(a, media, desv)
}

# p_mayor(a, media, desv)
#   Probabilidad P(X > a) para X ~ N(media, desv²).
#   Parámetros:
#     a     : valor umbral
#     media : µ de la distribución
#     desv  : σ de la distribución
#   Retorna: probabilidad de la cola superior (lower.tail = FALSE)
p_mayor <- function(a, media, desv) {
  pnorm(a, media, desv, lower.tail = FALSE)
}

# p_menor(a, media, desv)
#   Probabilidad P(X < a) para X ~ N(media, desv²).
#   Parámetros:
#     a     : valor umbral
#     media : µ de la distribución
#     desv  : σ de la distribución
#   Retorna: probabilidad de la cola inferior
p_menor <- function(a, media, desv) {
  pnorm(a, media, desv)
}

# cola_estandar(z)
#   Probabilidad P(Z > z) de la normal estándar Z ~ N(0,1).
#   Equivale al área en cola derecha que entregaba la Tabla 4.
#   Parámetros:
#     z : puntaje z (puede ser negativo; pnorm maneja ambos casos)
#   Retorna: probabilidad numérica
cola_estandar <- function(z) {
  pnorm(z, lower.tail = FALSE)
}

# =====================================================================
# PUNTO 5 — PARÁMETROS DESCRIPTIVOS DE LA MUESTRA
# =====================================================================

# [F] Media: x̄ = (1/n)·Σ x_i
media_m <- mean(datos)

# [F] Mediana con n par: Med = ( x_(n/2) + x_(n/2+1) ) / 2
ordenados <- sort(datos)
mediana_m <- (ordenados[n / 2] + ordenados[n / 2 + 1]) / 2

# [F] Amplitud de intervalos (datos continuos):
#     R = x_max − x_min ;  k = 1 + 3.322·log10(n) ;  A = R/k ;  m_i = (L_i+L_s)/2
xmin_d <- min(datos)
xmax_d <- max(datos)
R_d    <- xmax_d - xmin_d
k_d    <- ceiling(1 + 3.322 * log10(n))     # redondeo hacia arriba: número de clases
A_d    <- R_d / k_d
cortes <- seq(xmin_d, xmax_d, length.out = k_d + 1)
marcas <- (cortes[-length(cortes)] + cortes[-1]) / 2
frecs  <- as.numeric(table(cut(datos, breaks = cortes, right = FALSE,
                               include.lowest = TRUE)))

# [F] Moda agrupada (usa la función moda_agrupada definida arriba)
moda <- moda_agrupada(cortes, frecs)

# [F] Media agrupada de control: x̄_agr = Σ f_i·m_i / n
media_agr <- sum(frecs * marcas) / n

# [F] Varianza muestral: s² = Σ (x_i − x̄)² / (n−1)   [var() de R usa n−1]
var_m <- var(datos)
# [F] Desviación estándar: s = √(s²)
sd_m  <- sqrt(var_m)
# [F] Coeficiente de variación: CV = (s/x̄)·100%
cv_m  <- 100 * sd_m / media_m

# [F] Momentos centrales m2, m3, m4 y coeficientes de forma
m2 <- momento_central(datos, 2)
m3 <- momento_central(datos, 3)
m4 <- momento_central(datos, 4)
sesgo_m     <- m3 / m2^1.5          # sesgo: m3 / m2^(3/2)
curtosis_m  <- m4 / m2^2            # curtosis: m4 / m2²
curt_exceso <- curtosis_m - 3       # exceso de curtosis

cat("\n==================== PUNTO 5 (n =", n, ") ====================\n")
cat("Media x̄                    :", fmt(media_m), "\n")
cat("Mediana (n par)            :", fmt(mediana_m), "\n")
cat("R / k / A                  :", fmt(R_d), "/", k_d, "clases /", fmt(A_d), "\n")
cat("Frecuencias por intervalo  :", paste(frecs, collapse = ", "), "\n")
cat("Media agrupada (control)   :", fmt(media_agr), "\n")
cat("Moda agrupada              :", fmt(moda$mod), "\n")
cat("Varianza s² / Desv. s      :", fmt(var_m), "/", fmt(sd_m), "\n")
cat("CV                         :", fmt(cv_m, 2), "%\n")
cat("Sesgo / Curtosis / Exceso  :", fmt(sesgo_m), "/", fmt(curtosis_m), "/",
    fmt(curt_exceso), "\n")
cat("===============================================================\n\n")

add("% ---- Punto 5 ----")
add(paste0("\\def\\FilasTablaAgr{", paste(sprintf(
  "$[%.2f,\\,%.2f%s$ & $%s$ & $%d$ & $%.4f$ & $%.4f$ \\\\",
  cortes[-length(cortes)], cortes[-1],
  ifelse(seq_along(frecs) == length(frecs), "]", ")"),  # el último cierra en x_max
  ifelse(seq_along(frecs) == which.max(frecs),
         paste0("\\mathbf{", fmt(marcas, 2), "}"), fmt(marcas, 2)),
  frecs, frecs / n, cumsum(frecs) / n), collapse = " "), "}"))
add(paste0("\\def\\nMuestra{", n, "}"))
add(paste0("\\def\\mediaM{", fmt(media_m), "}"))
add(paste0("\\def\\medianaM{", fmt(mediana_m), "}"))
add(paste0("\\def\\xDatosMin{", fmt(xmin_d, 1), "}"))
add(paste0("\\def\\xDatosMax{", fmt(xmax_d, 1), "}"))
add(paste0("\\def\\RTotal{", fmt(R_d), "}"))
add(paste0("\\def\\kClases{", k_d, "}"))
add(paste0("\\def\\AClases{", fmt(A_d), "}"))
add(paste0("\\def\\mediaAgrup{", fmt(media_agr), "}"))
add(paste0("\\def\\modaM{", fmt(moda$mod), "}"))
add(paste0("\\def\\modaLinf{", fmt(moda$Linf, 2), "}"))
add(paste0("\\def\\modaLsup{", fmt(moda$Linf + moda$A, 2), "}"))
add(paste0("\\def\\modaFmod{", moda$fmod, "}"))
add(paste0("\\def\\modaFant{", moda$fant, "}"))
add(paste0("\\def\\modaFpos{", moda$fpos, "}"))
add(paste0("\\def\\modaDuno{", moda$d1, "}"))
add(paste0("\\def\\modaDdos{", moda$d2, "}"))
add(paste0("\\def\\varM{", fmt(var_m), "}"))
add(paste0("\\def\\sdM{", fmt(sd_m), "}"))
add(paste0("\\def\\cvM{", fmt(cv_m, 2), "}"))
add(paste0("\\def\\sesgoM{", fmt(sesgo_m), "}"))
add(paste0("\\def\\curtM{", fmt(curtosis_m), "}"))
add(paste0("\\def\\curtExcesoM{", fmt(curt_exceso), "}"))

# =====================================================================
# PUNTO 6 — PARÁMETROS POBLACIONALES VS. ESTADÍSTICOS MUESTRALES
# =====================================================================

base_pob <- poblacion$Base_Score

# [F] Media poblacional: µ = (1/N)·Σ x_i
media_p <- mean(base_pob)
# [F] Varianza poblacional: σ² = Σ (x_i − µ)² / N   (divisor N, no N−1)
var_p <- mean((base_pob - media_p)^2)
# [F] Desviación poblacional: σ = √(σ²)
sd_p  <- sqrt(var_p)
# [F] CV poblacional: CV = (σ/µ)·100%
cv_p  <- 100 * sd_p / media_p

# Errores de comparación (estadístico muestral vs. parámetro poblacional)
err_media <- errores(media_m, media_p)
err_var   <- errores(var_m,   var_p)
err_sd    <- errores(sd_m,    sd_p)
err_cv    <- errores(cv_m,    cv_p)

# [F] Varianza de la media muestral. El formulario solo aplica la
#     corrección por población finita si n/N > 0.05 (muestreo sin reemplazo):
#       con corrección: Var(X̄) = (σ²/n)·[(N−n)/(N−1)]
#       sin corrección: Var(X̄) = σ²/n
frac     <- n / N
var_xbar <- if (frac > 0.05) {
  (var_p / n) * ((N - n) / (N - 1))
} else {
  var_p / n
}
# Error estándar de X̄: √Var(X̄)
se_xbar <- sqrt(var_xbar)

cat("==================== PUNTO 6 (N =", N, ") ====================\n")
cat(sprintf("%-22s %10s %10s %10s %9s\n", "Estadístico", "Poblac.",
            "Muestral", "Err.abs", "Err.%"))
cat(sprintf("%-22s %10s %10s %10s %8.2f%%\n", "Media",
            fmt(media_p), fmt(media_m), fmt(err_media[1]), err_media[2]))
cat(sprintf("%-22s %10s %10s %10s %8.2f%%\n", "Varianza",
            fmt(var_p), fmt(var_m), fmt(err_var[1]), err_var[2]))
cat(sprintf("%-22s %10s %10s %10s %8.2f%%\n", "Desv. estándar",
            fmt(sd_p), fmt(sd_m), fmt(err_sd[1]), err_sd[2]))
cat(sprintf("%-22s %10s %10s %10s %8.2f%%\n", "CV (%)",
            fmt(cv_p, 2), fmt(cv_m, 2), fmt(err_cv[1], 2), err_cv[2]))
cat("n/N =", fmt(frac), if (frac > 0.05) "-> aplica corrección\n"
     else "-> NO aplica corrección (n/N <= 0.05)\n")
cat("Var(X̄) =", fmt(var_xbar), " ; Error estándar =", fmt(se_xbar), "\n")
cat("|x̄ − µ| =", fmt(err_media[1]), "(≈", fmt(err_media[1] / se_xbar, 2),
    " errores estándar)\n")
cat("===============================================================\n\n")

add("% ---- Punto 6 ----")
add(paste0("\\def\\nPob{", N, "}"))
add(paste0("\\def\\mediaP{", fmt(media_p), "}"))
add(paste0("\\def\\varP{", fmt(var_p), "}"))
add(paste0("\\def\\sdP{", fmt(sd_p), "}"))
add(paste0("\\def\\cvP{", fmt(cv_p, 2), "}"))
add(paste0("\\def\\errAbsMedia{", fmt(err_media[1]), "}"))
add(paste0("\\def\\errRelMedia{", fmt(err_media[2], 2), "}"))
add(paste0("\\def\\errAbsVar{", fmt(err_var[1]), "}"))
add(paste0("\\def\\errRelVar{", fmt(err_var[2], 2), "}"))
add(paste0("\\def\\errAbsSD{", fmt(err_sd[1]), "}"))
add(paste0("\\def\\errRelSD{", fmt(err_sd[2], 2), "}"))
add(paste0("\\def\\errAbsCV{", fmt(err_cv[1], 2), "}"))
add(paste0("\\def\\errRelCV{", fmt(err_cv[2], 2), "}"))
add(paste0("\\def\\fracMuestral{", fmt(frac), "}"))
add(paste0("\\def\\correccionFPC{", fmt((N - n) / (N - 1)), "}"))
add(paste0("\\def\\varXbarMedia{", fmt(var_xbar), "}"))
add(paste0("\\def\\seXbarMedia{", fmt(se_xbar), "}"))
add(paste0("\\def\\seVsObs{", fmt(err_media[1] / se_xbar, 2), "}"))
if (frac > 0.05) {
  add(paste0("\\newcommand{\\CondicionFPC}{se cumple la condición del formulario\n",
             "  ($n/N = \\fracMuestral > 0{,}05$), por lo que se aplica el\n",
             "  \\textbf{factor de corrección por población finita}}"))
  add(paste0("\\newcommand{\\FormulaVarXbar}{\\operatorname{Var}(\\bar{X}) =\n",
             "  \\frac{\\sigma^{2}}{\\nMuestra}\\cdot\\frac{\\nPob-\\nMuestra}{\\nPob-1}\n",
             "  = \\frac{\\varP}{\\nMuestra}\\cdot\\correccionFPC = \\varXbarMedia}"))
} else {
  add(paste0("\\newcommand{\\CondicionFPC}{\\textbf{no} se cumple la condición del\n",
             "  formulario ($n/N = \\fracMuestral \\leq 0{,}05$), por lo que la\n",
             "  corrección por población finita \\textbf{no aplica} y se usa\n",
             "  directamente la fórmula de distribuciones muestrales}"))
  add(paste0("\\newcommand{\\FormulaVarXbar}{\\operatorname{Var}(\\bar{X}) =\n",
             "  \\frac{\\sigma^{2}}{\\nMuestra} = \\frac{\\varP}{\\nMuestra} = \\varXbarMedia}"))
}

# =====================================================================
# PUNTO 7 — CINCO EXPERIMENTOS (fórmulas de la tabla de distribuciones)
# =====================================================================

# Parámetros poblacionales usados por los experimentos:
# p = proporción de severidad CRITICAL (ensayos de Bernoulli)
p_crit <- mean(poblacion$Severidad == "CRITICAL")
# K = número de vulnerabilidades con vector de ataque NETWORK
K_red <- sum(poblacion$Vector_Ataque == "NETWORK")
# λ = promedio de vulnerabilidades CRÍTICAS por día (91 días: ene 1 – abr 1 2024)
dias       <- as.integer(as.Date("2024-04-01") - as.Date("2024-01-01"))
lambda_dia <- sum(poblacion$Severidad == "CRITICAL") / dias

cat("==================== PUNTO 7 ====================\n")
cat("p (CRITICAL) =", fmt(p_crit), " ; K (NETWORK) =", K_red,
    " ; λ (crít./día) =", fmt(lambda_dia), "\n\n")

# --- Experimento 1: BINOMIAL ---------------------------------------
# [F] p(x) = C(n,x)·p^x·(1−p)^(n−x) ; E = np ; Var = np(1−p)
nb       <- 10
bin_esp  <- nb * p_crit
bin_var  <- nb * p_crit * (1 - p_crit)
bin_p2   <- dbinom(2, nb, p_crit)         # P(X = 2)
bin_pge1 <- 1 - dbinom(0, nb, p_crit)     # P(X ≥ 1) = 1 − P(X = 0)
cat("--- Exp. 1: Bin(10,", fmt(p_crit), ") ---\n")
cat("E =", fmt(bin_esp), "; Var =", fmt(bin_var),
    "; P(X=2) =", fmt(bin_p2), "; P(X≥1) =", fmt(bin_pge1), "\n\n")

add("% ---- Exp. 1: Binomial ----")
add(paste0("\\def\\expPbin{", fmt(p_crit), "}"))
add(paste0("\\def\\binPdos{", fmt(bin_p2), "}"))
add(paste0("\\def\\binPgeuno{", fmt(bin_pge1), "}"))
add(paste0("\\def\\binE{", fmt(bin_esp), "}"))
add(paste0("\\def\\binV{", fmt(bin_var), "}"))

# --- Experimento 2: POISSON ----------------------------------------
# [F] p(x) = e^(−λ)·λ^x / x! ; E = Var = λ
poi_p0   <- dpois(0, lambda_dia)          # P(X = 0)
poi_pge2 <- 1 - ppois(1, lambda_dia)      # P(X ≥ 2) = 1 − P(X ≤ 1)
cat("--- Exp. 2: Poisson(", fmt(lambda_dia), ") ---\n")
cat("E = Var = λ =", fmt(lambda_dia),
    "; P(X=0) =", fmt(poi_p0), "; P(X≥2) =", fmt(poi_pge2), "\n\n")

add("% ---- Exp. 2: Poisson ----")
add(paste0("\\def\\poiLam{", fmt(lambda_dia), "}"))
add(paste0("\\def\\poiPcero{", fmt(poi_p0), "}"))
add(paste0("\\def\\poiPgeDos{", fmt(poi_pge2), "}"))

# --- Experimento 3: GEOMÉTRICA -------------------------------------
# [F] p(x) = (1−p)^(x−1)·p ; E = 1/p ; Var = (1−p)/p²
kg       <- 6
geo_esp  <- 1 / p_crit
geo_var  <- (1 - p_crit) / p_crit^2
geo_p6   <- (1 - p_crit)^(kg - 1) * p_crit  # P(X = 6)
# pgeom cuenta FRACASOS antes del primer éxito: X <= 4 revisions
# equivale a <= 3 fracasos, por eso pgeom(3, p) y no pgeom(kg - 2, p)
geo_ple4 <- pgeom(3, p_crit)                # P(X ≤ 4)
cat("--- Exp. 3: Geom(", fmt(p_crit), ") ---\n")
cat("E =", fmt(geo_esp), "; Var =", fmt(geo_var),
    "; P(X=6) =", fmt(geo_p6), "; P(X≤4) =", fmt(geo_ple4), "\n\n")

add("% ---- Exp. 3: Geométrica ----")
add(paste0("\\def\\geoPseis{", fmt(geo_p6), "}"))
add(paste0("\\def\\geoPlecuatro{", fmt(geo_ple4), "}"))
add(paste0("\\def\\geoE{", fmt(geo_esp), "}"))
add(paste0("\\def\\geoV{", fmt(geo_var), "}"))

# --- Experimento 4: HIPERGEOMÉTRICA ---------------------------------
# [F] p(x) = [C(K,x)·C(N−K, n−x)] / C(N,n)
#     E = nK/N ; Var = n·(K/N)·(1−K/N)·[(N−n)/(N−1)]
n4        <- 20
hip_esp   <- n4 * K_red / N
hip_var   <- n4 * (K_red / N) * (1 - K_red / N) * (N - n4) / (N - 1)
hip_pge18 <- 1 - phyper(17, K_red, N - K_red, n4)   # P(X ≥ 18)
hip_p20   <- dhyper(20, K_red, N - K_red, n4)       # P(X = 20)
cat("--- Exp. 4: Hiper(N=", N, ", K=", K_red, ", n=", n4, ") ---\n")
cat("E =", fmt(hip_esp), "; Var =", fmt(hip_var),
    "; P(X≥18) =", fmt(hip_pge18), "; P(X=20) =", fmt(hip_p20), "\n\n")

add("% ---- Exp. 4: Hipergeométrica ----")
add(paste0("\\def\\hipN{", N, "}"))
add(paste0("\\def\\hipK{", K_red, "}"))
add(paste0("\\def\\hipn{", n4, "}"))
add(paste0("\\def\\hipPgeDieciocho{", fmt(hip_pge18), "}"))
add(paste0("\\def\\hipPveinte{", fmt(hip_p20), "}"))
add(paste0("\\def\\hipE{", fmt(hip_esp), "}"))
add(paste0("\\def\\hipV{", fmt(hip_var), "}"))

# --- Experimento 5: NORMAL ------------------------------------------
# [F] f(x) = [1/(σ√(2π))]·e^{−(x−µ)²/(2σ²)} ; Z = (X−µ)/σ ~ N(0,1)
#     µ y σ se estiman con la muestra; probabilidades con pnorm().
mu_n <- media_m
sd_n <- sd_m

# Puntajes z de los valores límite (para mostrar en el documento)
z6 <- estandarizar(6, mu_n, sd_n)
z8 <- estandarizar(8, mu_n, sd_n)
z9 <- estandarizar(9, mu_n, sd_n)
z5 <- estandarizar(5, mu_n, sd_n)

# Probabilidades pedidas (equivalentes a la Tabla 4, pero exactas)
norm_p68  <- p_entre(6, 8, mu_n, sd_n)    # P(6 ≤ X ≤ 8)
norm_p9   <- p_mayor(9, mu_n, sd_n)       # P(X > 9)
norm_p5   <- p_menor(5, mu_n, sd_n)       # P(X < 5)

cat("--- Exp. 5: N(", fmt(mu_n), ",", fmt(sd_n), "^2) ---\n")
cat("z(6) =", fmt(z6), "; z(8) =", fmt(z8),
    "; z(9) =", fmt(z9), "; z(5) =", fmt(z5), "\n")
cat("P(6≤X≤8) =", fmt(norm_p68),
    "; P(X>9) =", fmt(norm_p9),
    "; P(X<5) =", fmt(norm_p5), "\n\n")

# Macros del Exp. 5: z a 2 decimales y sus colas P(Z > z), que es lo que
# informe.tex muestra al resolver con la estandarización.
add("% ---- Exp. 5: Normal (probabilidades exactas con pnorm) ----")
add(paste0("\\def\\normZseis{", fmt(round(z6, 2), 2), "}"))
add(paste0("\\def\\normZseisAbs{", fmt(abs(round(z6, 2)), 2), "}"))
add(paste0("\\def\\normZocho{", fmt(round(z8, 2), 2), "}"))
add(paste0("\\def\\normZnueve{", fmt(round(z9, 2), 2), "}"))
add(paste0("\\def\\normZcinco{", fmt(round(z5, 2), 2), "}"))
add(paste0("\\def\\normZcincoAbs{", fmt(abs(round(z5, 2)), 2), "}"))
add(paste0("\\def\\normColaZseis{", fmt(cola_estandar(abs(round(z6, 2))), 4), "}"))
add(paste0("\\def\\normColaZocho{", fmt(cola_estandar(round(z8, 2)), 4), "}"))
add(paste0("\\def\\normColaZnueve{", fmt(cola_estandar(round(z9, 2)), 4), "}"))
add(paste0("\\def\\normColaZcinco{", fmt(cola_estandar(abs(round(z5, 2))), 4), "}"))
add(paste0("\\def\\normPseisOcho{", fmt(norm_p68), "}"))
add(paste0("\\def\\probNormMayorNueve{", fmt(norm_p9), "}"))
add(paste0("\\def\\probNormMenorCinco{", fmt(norm_p5), "}"))

# =====================================================================
# EXPORTAR MACROS PARA LATEX
# =====================================================================
writeLines(R_tex, "resultados.tex")
cat("Archivo 'resultados.tex' generado con", length(R_tex), "macros.\n")
