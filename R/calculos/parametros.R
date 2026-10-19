# =====================================================================
# PUNTOS 5, 6 Y 7 DEL PROYECTO FINAL DE ESTADÍSTICA
# Variable de análisis: Base_Score (CVSS v3.1).
#
# TODAS las fórmulas usadas provienen EXCLUSIVAMENTE de:
#   [F] "Formulario de Probabilidad y Estadística" (J. D. Fuentes Mora)
#   [T] "Tabla 4: Áreas de curva normal" (R. E. Walpole) -> P(Z > z)
#
# Muestra aleatoria simple sin reemplazo de n = 60 (set.seed(42)).
# (script.R usa una muestra distinta de n = 500 para el histograma.)
#
# Salida: imprime en consola y exporta "resultados.tex" con macros
# numéricas para el documento LaTeX.
# =====================================================================

poblacion <- read.csv("datos_cve_completos.csv")

set.seed(42)
muestra <- poblacion[sample(nrow(poblacion), 60), ]
datos   <- na.omit(muestra$Base_Score)
n       <- length(datos)
N       <- nrow(poblacion)

fmt <- function(x, d = 4) formatC(x, format = "f", digits = d)
R   <- character(0)
add <- function(...) R <<- c(R, ...)

# =====================================================================
# PUNTO 5. PARÁMETROS DESCRIPTIVOS DE LA MUESTRA
# Fórmulas [F]: x̄, Med (n par), s², s, CV, Moda (datos agrupados)
#               y momentos centrales.
# =====================================================================

# [F] Media (Datos No Agrupados): x̄ = (1/n) Σ xi
media_m <- mean(datos)

# [F] Mediana (n par): Med = ( x_(n/2) + x_(n/2+1) ) / 2
ordenados <- sort(datos)
x_inf_med <- ordenados[n / 2]
x_sup_med <- ordenados[n / 2 + 1]
mediana_m <- (x_inf_med + x_sup_med) / 2

# [F] Amplitud de Intervalos (datos continuos): R = x_max - x_min
#     k = 1 + 3.322 log10(n)   ;   A = R/k   ;   m_i = (Li + Ls)/2
xmin_d <- min(datos)
xmax_d <- max(datos)
R_d    <- xmax_d - xmin_d                 # amplitud total
k_d    <- ceiling(1 + 3.322 * log10(n))   # número de clases
A_d    <- R_d / k_d                       # amplitud de intervalo

# Cortes equiespaciados desde x_min hasta x_max
cortes          <- seq(xmin_d, xmax_d, length.out = k_d + 1)
frec_intervalos <- table(cut(datos, breaks = cortes, right = FALSE,
                             include.lowest = TRUE))
marcas_clase    <- (cortes[-length(cortes)] + cortes[-1]) / 2   # [F] m_i = (Li+Ls)/2

# [F] Moda (Datos Agrupados): Mod = L_inf + [d1/(d1+d2)]·A
#     d1 = f_mod - f_ant ;  d2 = f_mod - f_pos
i_mod <- which.max(as.numeric(frec_intervalos))
fmod <- as.numeric(frec_intervalos[i_mod])
fant <- if (i_mod > 1) as.numeric(frec_intervalos[i_mod - 1]) else 0
fpos <- if (i_mod < length(frec_intervalos)) as.numeric(frec_intervalos[i_mod + 1]) else 0
Linf_mod <- cortes[i_mod]
d1       <- fmod - fant
d2       <- fmod - fpos
moda_m   <- Linf_mod + (d1 / (d1 + d2)) * A_d

# [F] Media agrupada (control): x̄_agr = Σ f_i m_i / n
media_agr <- sum(as.numeric(frec_intervalos) * marcas_clase) / n

# Exportar filas de la tabla de frecuencias agrupadas como macro para LaTeX
freqs <- as.numeric(frec_intervalos)
filas_tabla_agr <- character(length(freqs))
for (i in seq_along(freqs)) {
  marca <- if (i == i_mod)
    paste0("\\mathbf{", fmt(marcas_clase[i], 2), "}") else fmt(marcas_clase[i], 2)
  # El último intervalo es cerrado: cut(..., include.lowest = TRUE) incluye x_max
  cierre <- if (i == length(freqs)) "]" else ")"
  filas_tabla_agr[i] <- sprintf(
    "$[%.2f,\\,%.2f%s$ & $%s$ & $%d$ & $%.4f$ & $%.4f$ \\\\",
    cortes[i], cortes[i + 1], cierre, marca, freqs[i],
    freqs[i] / n, cumsum(freqs)[i] / n)
}

# [F] Varianza muestral (Datos No Agrupados): s² = [1/(n-1)] Σ (xi - x̄)²
var_m <- var(datos)
sd_m  <- sqrt(var_m)                        # [F] s = √(s²)

# [F] Coeficiente de variación: CV = (s/x̄) × 100%
cv_m <- 100 * sd_m / media_m

# [F] Momentos centrales (muestra): mk = (1/n) Σ (xi - x̄)^k
m2 <- mean((datos - media_m)^2)
m3 <- mean((datos - media_m)^3)
m4 <- mean((datos - media_m)^4)

# Coeficientes construidos sobre los momentos del formulario:
#   sesgo (Pearson) = m3 / m2^(3/2) ;  curtosis = m4 / m2² ;  exceso = curtosis - 3
sesgo_m     <- m3 / m2^1.5
curtosis_m  <- m4 / m2^2
curt_exceso <- curtosis_m - 3

cat("\n===============================================================\n")
cat("   PUNTO 5 - ESTADÍSTICOS DESCRIPTIVOS DE LA MUESTRA (n =", n, ")\n")
cat("   Fórmulas: Formulario Fuentes Mora\n")
cat("===============================================================\n")
cat("Media  x̄ = (1/n)Σxi           :", fmt(media_m), "\n")
cat("Mediana (n par)               :", fmt(mediana_m), "\n")
cat("--- Intervalos según el formulario (datos continuos) ---\n")
cat("R = xmax - xmin               :", fmt(R_d, 4), " (xmin =", fmt(xmin_d, 1),
    ", xmax =", fmt(xmax_d, 1), ")\n")
cat("k = 1 + 3.322 log10(n)        :", k_d, "clases\n")
cat("A = R/k                       :", fmt(A_d, 4), "\n")
cat("Marcas de clase m_i           :", paste(fmt(marcas_clase, 2), collapse = ", "), "\n")
cat("Frecuencias por intervalo     :", paste(as.numeric(frec_intervalos), collapse = ", "), "\n")
cat("Media agrupada Σf_i·m_i/n     :", fmt(media_agr), "(control de x̄)\n")
cat("Moda (datos agrupados)        :", fmt(moda_m, 4), "\n")
cat("   intervalo modal [", fmt(Linf_mod, 2), ",", fmt(Linf_mod + A_d, 2),
    ")  fmod =", fmod, ", fant =", fant, ", fpos =", fpos, "\n")
cat("   d1 =", d1, ", d2 =", d2, "\n")
cat("Varianza muestral s²          :", fmt(var_m), "\n")
cat("Desv. estándar s = √s²        :", fmt(sd_m), "\n")
cat("CV = (s/x̄)×100%              :", fmt(cv_m, 2), "%\n")
cat("Momentos  m2 / m3 / m4        :", fmt(m2), "/", fmt(m3), "/", fmt(m4), "\n")
cat("Sesgo  m3/m2^(3/2)            :", fmt(sesgo_m), "\n")
cat("Curtosis  m4/m2²              :", fmt(curtosis_m), "\n")
cat("Exceso de curtosis            :", fmt(curt_exceso), "\n")
cat("===============================================================\n\n")

add("% ---- Punto 5 (fórmulas del Formulario Fuentes Mora) ----")
add(paste0("\\def\\FilasTablaAgr{", paste(filas_tabla_agr, collapse = " "), "}"))
add(paste0("\\def\\nMuestra{", n, "}"))
add(paste0("\\def\\mediaM{", fmt(media_m), "}"))
add(paste0("\\def\\medianaM{", fmt(mediana_m), "}"))
add(paste0("\\def\\xDatosMin{", fmt(xmin_d, 1), "}"))
add(paste0("\\def\\xDatosMax{", fmt(xmax_d, 1), "}"))
add(paste0("\\def\\RTotal{", fmt(R_d, 4), "}"))
add(paste0("\\def\\kClases{", k_d, "}"))
add(paste0("\\def\\AClases{", fmt(A_d, 4), "}"))
add(paste0("\\def\\mediaAgrup{", fmt(media_agr), "}"))
add(paste0("\\def\\modaM{", fmt(moda_m, 4), "}"))
add(paste0("\\def\\modaLinf{", fmt(Linf_mod, 2), "}"))
add(paste0("\\def\\modaLsup{", fmt(Linf_mod + A_d, 2), "}"))
add(paste0("\\def\\modaFmod{", fmod, "}"))
add(paste0("\\def\\modaFant{", fant, "}"))
add(paste0("\\def\\modaFpos{", fpos, "}"))
add(paste0("\\def\\modaDuno{", d1, "}"))
add(paste0("\\def\\modaDdos{", d2, "}"))
add(paste0("\\def\\varM{", fmt(var_m), "}"))
add(paste0("\\def\\sdM{", fmt(sd_m), "}"))
add(paste0("\\def\\cvM{", fmt(cv_m, 2), "}"))
add(paste0("\\def\\sesgoM{", fmt(sesgo_m), "}"))
add(paste0("\\def\\curtM{", fmt(curtosis_m), "}"))
add(paste0("\\def\\curtExcesoM{", fmt(curt_exceso), "}"))

# =====================================================================
# PUNTO 6. PARÁMETROS POBLACIONALES VS. ESTADÍSTICOS MUESTRALES
# Fórmulas [F]: µ, σ² = (1/N)Σ(xi-µ)², σ = √σ², CV = (σ/µ)×100%
#               y FACTOR DE CORRECCIÓN POR POBLACIÓN FINITA.
# =====================================================================

base_pob <- poblacion$Base_Score

# [F] Media poblacional: µ = (1/N) Σ xi
media_p <- mean(base_pob)
# [F] Varianza poblacional: σ² = (1/N) Σ (xi - µ)²   (divisor N)
var_p <- mean((base_pob - media_p)^2)
sd_p  <- sqrt(var_p)                       # [F] σ = √(σ²)
# [F] CV poblacional
cv_p <- 100 * sd_p / media_p

# Errores de comparación (estadístico vs. parámetro)
err_abs_media <- abs(media_m - media_p); err_rel_media <- 100 * err_abs_media / media_p
err_abs_var   <- abs(var_m   - var_p);   err_rel_var   <- 100 * err_abs_var   / var_p
err_abs_sd    <- abs(sd_m    - sd_p);    err_rel_sd    <- 100 * err_abs_sd    / sd_p
err_abs_cv    <- abs(cv_m    - cv_p);    err_rel_cv    <- 100 * err_abs_cv    / cv_p

# [F] Distribuciones muestrales: E[X̄] = µ, Var(X̄) = σ²/n
# [F] Factor de corrección por población finita: Var(X̄) = (σ²/n)·[(N-n)/(N-1)]
#     Se aplica SOLO si n/N > 0.05 (muestreo sin reemplazo).
frac_muestral   <- n / N
if (frac_muestral > 0.05) {
  correccion_fpc  <- (N - n) / (N - 1)
  var_xbar        <- (var_p / n) * correccion_fpc
  aplica_fpc      <- TRUE
} else {
  correccion_fpc  <- (N - n) / (N - 1)   # se reporta como referencia
  var_xbar        <- var_p / n           # sin corrección: n/N <= 0.05
  aplica_fpc      <- FALSE
}
se_xbar <- sqrt(var_xbar)

cat("===============================================================\n")
cat("   PUNTO 6 - POBLACIÓN (N =", N, ") VS. MUESTRA (n =", n, ")\n")
cat("   Fórmulas: Formulario Fuentes Mora\n")
cat("===============================================================\n")
cat(sprintf("%-24s %12s %12s %12s %10s\n",
            "Estadístico", "Poblacional", "Muestral", "Error abs.", "Error %"))
cat(sprintf("%-24s %12s %12s %12s %9.2f%%\n", "Media (µ / x̄)",
            fmt(media_p), fmt(media_m), fmt(err_abs_media), err_rel_media))
cat(sprintf("%-24s %12s %12s %12s %9.2f%%\n", "Varianza (σ² / s²)",
            fmt(var_p), fmt(var_m), fmt(err_abs_var), err_rel_var))
cat(sprintf("%-24s %12s %12s %12s %9.2f%%\n", "Desv. estándar",
            fmt(sd_p), fmt(sd_m), fmt(err_abs_sd), err_rel_sd))
cat(sprintf("%-24s %12s %12s %12s %9.2f%%\n", "CV (%)",
            fmt(cv_p, 2), fmt(cv_m, 2), fmt(err_abs_cv, 2), err_rel_cv))
cat("---------------------------------------------------------------\n")
cat("Condición del formulario para corrección por población finita:\n")
cat("  n/N =", fmt(frac_muestral), if (aplica_fpc) "> 0.05 -> SÍ aplica"
       else "<= 0.05 -> NO aplica\n")
if (aplica_fpc) {
  cat("  Var(X̄) = (σ²/n)·[(N-n)/(N-1)] =", fmt(var_xbar), "\n")
} else {
  cat("  Var(X̄) = σ²/n (Distribuciones muestrales, sin corrección) =",
      fmt(var_xbar), "\n")
}
cat("  Error estándar de X̄  √Var(X̄) =", fmt(se_xbar), "\n")
cat("  |x̄ - µ| =", fmt(err_abs_media), " (≈",
    fmt(err_abs_media / se_xbar, 2), " errores estándar)\n")
cat("===============================================================\n\n")

add("% ---- Punto 6 (fórmulas del Formulario Fuentes Mora) ----")
add(paste0("\\def\\nPob{", N, "}"))
add(paste0("\\def\\mediaP{", fmt(media_p), "}"))
add(paste0("\\def\\varP{", fmt(var_p), "}"))
add(paste0("\\def\\sdP{", fmt(sd_p), "}"))
add(paste0("\\def\\cvP{", fmt(cv_p, 2), "}"))
add(paste0("\\def\\errAbsMedia{", fmt(err_abs_media), "}"))
add(paste0("\\def\\errRelMedia{", fmt(err_rel_media, 2), "}"))
add(paste0("\\def\\errAbsVar{", fmt(err_abs_var), "}"))
add(paste0("\\def\\errRelVar{", fmt(err_rel_var, 2), "}"))
add(paste0("\\def\\errAbsSD{", fmt(err_abs_sd), "}"))
add(paste0("\\def\\errRelSD{", fmt(err_rel_sd, 2), "}"))
add(paste0("\\def\\errAbsCV{", fmt(err_abs_cv, 2), "}"))
add(paste0("\\def\\errRelCV{", fmt(err_rel_cv, 2), "}"))
add(paste0("\\def\\fracMuestral{", fmt(frac_muestral, 4), "}"))
add(paste0("\\def\\correccionFPC{", fmt(correccion_fpc, 4), "}"))
add(paste0("\\def\\varXbarMedia{", fmt(var_xbar), "}"))
add(paste0("\\def\\seXbarMedia{", fmt(se_xbar), "}"))
add(paste0("\\def\\seVsObs{", fmt(err_abs_media / se_xbar, 2), "}"))
if (aplica_fpc) {
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
# TABLA NORMAL [T]: Walpole, Tabla 4 - áreas en COLA DERECHA P(Z > z).
# Valores transcritos del PDF provisto. Para z < 0 se usa simetría.
# =====================================================================
cola_z <- matrix(c(
  .5000,.4960,.4920,.4880,.4840,.4801,.4761,.4721,.4681,.4641,
  .4602,.4562,.4522,.4483,.4443,.4404,.4364,.4325,.4286,.4247,
  .4207,.4168,.4129,.4090,.4052,.4013,.3974,.3936,.3897,.3859,
  .3821,.3783,.3745,.3707,.3669,.3632,.3594,.3557,.3520,.3483,
  .3446,.3409,.3372,.3336,.3300,.3264,.3228,.3192,.3156,.3121,
  .3085,.3050,.3015,.2981,.2946,.2912,.2877,.2843,.2810,.2776,
  .2743,.2709,.2676,.2643,.2611,.2578,.2546,.2514,.2483,.2451,
  .2420,.2389,.2358,.2327,.2296,.2266,.2236,.2206,.2177,.2148,
  .2119,.2090,.2061,.2033,.2005,.1977,.1949,.1922,.1894,.1867,
  .1841,.1814,.1788,.1762,.1736,.1711,.1685,.1660,.1635,.1611,
  .1587,.1562,.1539,.1515,.1492,.1469,.1446,.1423,.1401,.1379,
  .1357,.1335,.1314,.1292,.1271,.1251,.1230,.1210,.1190,.1170,
  .1151,.1131,.1112,.1093,.1075,.1056,.1038,.1020,.1003,.0985,
  .0968,.0951,.0934,.0918,.0901,.0885,.0869,.0853,.0838,.0823,
  .0808,.0793,.0778,.0764,.0749,.0735,.0722,.0708,.0694,.0681,
  .0668,.0655,.0643,.0630,.0618,.0606,.0594,.0582,.0571,.0559,
  .0548,.0537,.0526,.0516,.0505,.0495,.0485,.0475,.0465,.0455,
  .0446,.0436,.0427,.0418,.0409,.0401,.0392,.0384,.0375,.0367,
  .0359,.0352,.0344,.0336,.0329,.0322,.0314,.0307,.0301,.0294,
  .0287,.0281,.0274,.0268,.0262,.0256,.0250,.0244,.0239,.0233,
  .0228,.0222,.0217,.0212,.0207,.0202,.0197,.0192,.0188,.0183,
  .0179,.0174,.0170,.0166,.0162,.0158,.0154,.0150,.0146,.0143,
  .0139,.0136,.0132,.0129,.0125,.0122,.0119,.0116,.0113,.0110,
  .0107,.0104,.0102,.0099,.0096,.0094,.0091,.0089,.0087,.0084,
  .0082,.0080,.0078,.0075,.0073,.0071,.0069,.0068,.0066,.0064,
  .0062,.0060,.0059,.0057,.0055,.0054,.0052,.0051,.0049,.0048,
  .0047,.0045,.0044,.0043,.0041,.0040,.0039,.0038,.0037,.0036,
  .0035,.0034,.0033,.0032,.0031,.0030,.0029,.0028,.0027,.0026,
  .0026,.0025,.0024,.0023,.0023,.0022,.0021,.0021,.0020,.0019,
  .0019,.0018,.0017,.0017,.0016,.0016,.0015,.0015,.0014,.0014),
  nrow = 30, byrow = TRUE)
esp_z  <- c(3.0, 3.5, 4.0, 4.5, 5.0)
esp_v  <- c(.00135, .000233, .0000317, .00000340, .000000287)

# [T] Área en cola derecha P(Z > z) para z >= 0
cola_derecha <- function(z) {
  az <- abs(round(z, 2))
  if (az <= 2.99) {
    cc   <- round(az * 100)
    fila <- cc %/% 10
    col  <- cc %% 10
    return(cola_z[fila + 1, col + 1])
  }
  esp_v[max(which(esp_z <= az))]
}
# [F] Normal estándar: Φ(z) = P(Z ≤ z);  por simetría P(Z ≤ -a) = P(Z > a)
Phi_tabla <- function(z) if (z < 0) cola_derecha(z) else 1 - cola_derecha(z)

# =====================================================================
# PUNTO 7. CINCO EXPERIMENTOS (fórmulas [F] de la tabla de
# distribuciones + estandarización [F] y tabla [T] para la Normal).
# =====================================================================

fechas   <- as.Date(poblacion$Fecha_Publicacion)
dias     <- as.integer(as.Date("2024-04-01") - as.Date("2024-01-01"))  # 91 días
p_crit   <- mean(poblacion$Severidad == "CRITICAL")
K_red    <- sum(poblacion$Vector_Ataque == "NETWORK")
lambda_dia <- sum(poblacion$Severidad == "CRITICAL") / dias

cat("===============================================================\n")
cat("   PUNTO 7 - PARÁMETROS POBLACIONALES PARA LOS EXPERIMENTOS\n")
cat("===============================================================\n")
cat("Proporción de severidad CRITICAL      p =", fmt(p_crit), "\n")
cat("Nº de vulnerabilidades con vector NETWORK  K =", K_red, "\n")
cat("Días del periodo (ene 1 - abr 1 2024) :", dias, "\n")
cat("Promedio de CRITICAL por día      lambda =", fmt(lambda_dia), "\n\n")

# --- Experimento 1: BINOMIAL ---------------------------------------
# [F] p(x) = C(n,x) p^x (1-p)^(n-x) ; X ~ Bin(n,p) ; E = np ; Var = np(1-p)
nb      <- 10
bin_p2  <- dbinom(2, nb, p_crit)      # P(X = 2)
bin_pge1<- 1 - dbinom(0, nb, p_crit)  # P(X >= 1)
bin_esp <- nb * p_crit
bin_var <- nb * p_crit * (1 - p_crit)
cat("--- Exp. 1: BINOMIAL  X ~ Bin(", nb, ",", fmt(p_crit), ") ---\n")
cat("E(X) = np =", fmt(bin_esp), " ; Var(X) = np(1-p) =", fmt(bin_var), "\n")
cat("P(X = 2) =", fmt(bin_p2), " ; P(X >= 1) =", fmt(bin_pge1), "\n\n")

add("% ---- Punto 7 - Exp. 1: Binomial ----")
add(paste0("\\def\\expPbin{", fmt(p_crit), "}"))
add(paste0("\\def\\binPdos{", fmt(bin_p2), "}"))
add(paste0("\\def\\binPgeuno{", fmt(bin_pge1), "}"))
add(paste0("\\def\\binE{", fmt(bin_esp), "}"))
add(paste0("\\def\\binV{", fmt(bin_var), "}"))
add(paste0("\\def\\binPgeunoPct{", fmt(100 * bin_pge1, 2), "}"))   # en %

# --- Experimento 2: POISSON ----------------------------------------
# [F] p(x) = e^(-λ) λ^x / x! ; X ~ Poisson(λ) ; E = Var = λ
poi_p0   <- dpois(0, lambda_dia)      # P(X = 0)
poi_pge2 <- 1 - ppois(1, lambda_dia)  # P(X >= 2)
cat("--- Exp. 2: POISSON  X ~ Poisson(", fmt(lambda_dia), ") ---\n")
cat("E(X) = Var(X) = λ =", fmt(lambda_dia), "\n")
cat("P(X = 0) =", fmt(poi_p0), " ; P(X >= 2) =", fmt(poi_pge2), "\n\n")

add("% ---- Punto 7 - Exp. 2: Poisson ----")
add(paste0("\\def\\poiLam{", fmt(lambda_dia), "}"))
add(paste0("\\def\\poiPcero{", fmt(poi_p0), "}"))
add(paste0("\\def\\poiPgeDos{", fmt(poi_pge2), "}"))

# --- Experimento 3: GEOMÉTRICA -------------------------------------
# [F] p(x) = (1-p)^(x-1) p ; X ~ Geom(p) ; E = 1/p ; Var = (1-p)/p²
kg       <- 6
geo_p6   <- (1 - p_crit)^(kg - 1) * p_crit   # P(X = 6)
# P(X <= 4) = 1 - (1-p)^4. En R, pgeom() cuenta FRACASOS antes del primer
# éxito, así que X <= 4 pruebas equivale a <= 3 fracasos -> pgeom(3, p).
geo_ple4 <- pgeom(3, p_crit)                 # P(X <= 4)
geo_esp  <- 1 / p_crit
geo_var  <- (1 - p_crit) / p_crit^2
cat("--- Exp. 3: GEOMÉTRICA  X ~ Geom(", fmt(p_crit), ") ---\n")
cat("E(X) = 1/p =", fmt(geo_esp), " ; Var(X) = (1-p)/p² =", fmt(geo_var), "\n")
cat("P(X = 6) =", fmt(geo_p6), " ; P(X <= 4) =", fmt(geo_ple4), "\n\n")

add("% ---- Punto 7 - Exp. 3: Geométrica ----")
add(paste0("\\def\\geoPseis{", fmt(geo_p6), "}"))
add(paste0("\\def\\geoPlecuatro{", fmt(geo_ple4), "}"))
add(paste0("\\def\\geoE{", fmt(geo_esp), "}"))
add(paste0("\\def\\geoV{", fmt(geo_var), "}"))

# --- Experimento 4: HIPERGEOMÉTRICA ---------------------------------
# [F] p(x) = [C(K,x)·C(N-K, n-x)] / C(N,n)
#     E = nK/N ; Var = n(K/N)(1-K/N)((N-n)/(N-1))
n4        <- 20
hip_pge18 <- 1 - phyper(17, K_red, N - K_red, n4)  # P(X >= 18)
hip_p20   <- dhyper(20, K_red, N - K_red, n4)      # P(X = 20)
hip_esp   <- n4 * K_red / N
hip_var   <- n4 * (K_red / N) * (1 - K_red / N) * (N - n4) / (N - 1)
cat("--- Exp. 4: HIPERGEOMÉTRICA  X ~ Hiper(", N, ",", K_red, ",", n4, ") ---\n")
cat("E(X) = nK/N =", fmt(hip_esp), "\n")
cat("Var(X) =", fmt(hip_var), "\n")
cat("P(X >= 18) =", fmt(hip_pge18), " ; P(X = 20) =", fmt(hip_p20), "\n\n")

add("% ---- Punto 7 - Exp. 4: Hipergeométrica ----")
add(paste0("\\def\\hipN{", N, "}"))
add(paste0("\\def\\hipK{", K_red, "}"))
add(paste0("\\def\\hipn{", n4, "}"))
add(paste0("\\def\\hipPgeDieciocho{", fmt(hip_pge18), "}"))
add(paste0("\\def\\hipPveinte{", fmt(hip_p20), "}"))
add(paste0("\\def\\hipE{", fmt(hip_esp), "}"))
add(paste0("\\def\\hipV{", fmt(hip_var), "}"))

# --- Experimento 5: NORMAL ------------------------------------------
# [F] f(x) = [1/(σ√(2π))] e^{-(x-µ)²/(2σ²)} ; X ~ N(µ,σ²) con µ = x̄, σ = s
# [F] Z = (X-µ)/σ ~ N(0,1) ; Φ(z) = P(Z ≤ z)
# [T] Tabla 4 (Walpole): valores de P(Z > z); simetría para z < 0.
z_6  <- (6 - media_m) / sd_m
z_8  <- (8 - media_m) / sd_m
z_9  <- (9 - media_m) / sd_m
z_5  <- (5 - media_m) / sd_m

norm_p68_tab <- Phi_tabla(z_8) - Phi_tabla(z_6)   # P(6 <= X <= 8)
norm_p9_tab  <- cola_derecha(z_9)                 # P(X > 9) = P(Z > z9)
norm_p5_tab  <- Phi_tabla(z_5)                    # P(X < 5)

# Validación en consola con los valores exactos (no se exportan al .tex)
norm_p68_exact <- pnorm(8, media_m, sd_m) - pnorm(6, media_m, sd_m)
norm_p9_exact  <- 1 - pnorm(9, media_m, sd_m)
norm_p5_exact  <- pnorm(5, media_m, sd_m)

cat("--- Exp. 5: NORMAL  X ~ N(", fmt(media_m), ",", fmt(sd_m), "^2) ---\n")
cat("[T] Tabla 4: P(Z > z), áreas en cola derecha; simetría para z < 0\n")
cat("z6 =", fmt(z_6), "-> redondeado a 2 dec. =", fmt(round(z_6, 2), 2),
    " ; Φ(z6) = P(Z >", fmt(abs(round(z_6, 2)), 2), ") =", fmt(Phi_tabla(z_6)), "\n")
cat("z8 =", fmt(z_8), "-> redondeado a 2 dec. =", fmt(round(z_8, 2), 2),
    " ; Φ(z8) = 1 - P(Z >", fmt(round(z_8, 2), 2), ") =", fmt(Phi_tabla(z_8)), "\n")
cat("P(6 <= X <= 8) [tabla] =", fmt(norm_p68_tab), " (exacto:", fmt(norm_p68_exact), ")\n")
cat("z9 =", fmt(z_9), "-> redondeado a 2 dec. =", fmt(round(z_9, 2), 2),
    " ; P(X > 9) = P(Z >", fmt(round(z_9, 2), 2), ") =", fmt(norm_p9_tab),
    " (exacto:", fmt(norm_p9_exact), ")\n")
cat("z5 =", fmt(z_5), "-> redondeado a 2 dec. =", fmt(round(z_5, 2), 2),
    " ; P(X < 5) = P(Z >", fmt(abs(round(z_5, 2)), 2), ") =", fmt(norm_p5_tab),
    " (exacto:", fmt(norm_p5_exact), ")\n\n")

add("% ---- Punto 7 - Exp. 5: Normal (valores de la Tabla 4 de Walpole) ----")
add(paste0("\\def\\normZseis{", fmt(round(z_6, 2), 2), "}"))
add(paste0("\\def\\normZseisAbs{", fmt(abs(round(z_6, 2)), 2), "}"))
add(paste0("\\def\\normZocho{", fmt(round(z_8, 2), 2), "}"))
add(paste0("\\def\\normZnueve{", fmt(round(z_9, 2), 2), "}"))
add(paste0("\\def\\normZcinco{", fmt(round(z_5, 2), 2), "}"))
add(paste0("\\def\\normZcincoAbs{", fmt(abs(round(z_5, 2)), 2), "}"))
add(paste0("\\def\\normColaZseis{", fmt(cola_derecha(abs(round(z_6, 2))), 4), "}"))
add(paste0("\\def\\normColaZocho{", fmt(cola_derecha(z_8), 4), "}"))
add(paste0("\\def\\normColaZnueve{", fmt(cola_derecha(z_9), 4), "}"))
add(paste0("\\def\\normColaZcinco{", fmt(cola_derecha(abs(round(z_5, 2))), 4), "}"))
add(paste0("\\def\\normPseisOcho{", fmt(norm_p68_tab), "}"))
add(paste0("\\def\\probNormMayorNueve{", fmt(norm_p9_tab), "}"))
add(paste0("\\def\\probNormMenorCinco{", fmt(norm_p5_tab), "}"))
# Mismas probabilidades expresadas en porcentaje para la interpretación
add(paste0("\\def\\normPseisOchoPct{", fmt(100 * norm_p68_tab, 2), "}"))
add(paste0("\\def\\probNormMayorNuevePct{", fmt(100 * norm_p9_tab, 2), "}"))
add(paste0("\\def\\probNormMenorCincoPct{", fmt(100 * norm_p5_tab, 2), "}"))

# =====================================================================
# EXPORTAR MACROS PARA LATEX
# =====================================================================
writeLines(R, "resultados.tex")
cat("===============================================================\n")
cat("Archivo 'resultados.tex' generado con", length(R), "macros.\n")
cat("===============================================================\n")
