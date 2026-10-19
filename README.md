# 📊 Proyecto Final de Estadística — Análisis de Vulnerabilidades (CVE)

Proyecto final de la materia de Estadística. Se analiza un conjunto de datos real de
vulnerabilidades de seguridad informática (CVE), obtenido de la API oficial del
**NIST NVD** (*National Vulnerability Database*), utilizando **Python** para la
extracción y preparación de datos y **R** para los cálculos estadísticos y la
visualización.

---

## 🎯 Objetivo

Aplicar los temas vistos en clase de estadística descriptiva a un dataset real:

- Extracción y limpieza de datos desde una API REST.
- Definición de variables cuantitativas (continuas y discretas) y cualitativas.
- Construcción de una **tabla de frecuencias** (absolutas, relativas y acumuladas).
- **Histograma** y **polígono de frecuencias** de la severidad de las vulnerabilidades.

---

## 📁 Estructura del proyecto

```
proyectoFinalEstadistica/
├── informe/
│   ├── informe.tex             # Documento LaTeX (puntos 5, 6 y 7)
│   └── informe.pdf             # Documento compilado
├── python/
│   ├── script.py               # Descarga la población completa (CVE) desde la API del NIST
│   ├── script2.py              # Descarga descripciones de ataques (es/en) → CSV
│   ├── script3.py              # Descarga descripciones en inglés → JSON
│   ├── datos_cve_completos.csv # Dataset principal (~1,969 registros)
│   ├── contexto_ataques.csv    # Descripciones de ataques (español/inglés)
│   └── contexto_ataques.json   # Descripciones de ataques en formato JSON
├── R/
│   └── calculos/
│       ├── script.R            # Muestreo, tabla de frecuencias, histograma y polígono
│       ├── parametros.R        # Puntos 5, 6 y 7: parámetros y distribuciones
│       ├── resultados.tex      # Macros numéricas exportadas por parametros.R
│       ├── datos_cve_completos.csv  # Copia del dataset para trabajar en R
│       └── tabla_frecuencias.csv    # Tabla de frecuencias exportada
└── README.md
```

---

## 🛠️ Tecnologías y dependencias

| Área | Lenguaje | Librerías |
|------|----------|-----------|
| Extracción de datos | Python 3 | `requests`, `pandas` |
| Análisis estadístico | R | Base R (`stats`, `graphics`) |

### Instalación (Python)

```bash
pip install requests pandas
```

### Instalación (R)

No se requieren paquetes adicionales; el script usa funciones base de R.

---

## 🚀 Cómo ejecutar el proyecto

### 1. Generar los datos con Python

```bash
cd python

# Descarga la población completa (~1,969 CVEs publicados entre enero y abril de 2024)
python script.py

# Descripciones de ataques en español/inglés (CSV)
python script2.py

# Descripciones de ataques en inglés (JSON)
python script3.py
```

> ⚠️ La API del NIST es gratuita pero limita la cantidad de peticiones.
> Si obtienes un error `403` o `429`, espera unos minutos y vuelve a intentarlo.
> También puedes obtener una clave API en [https://nvd.nist.gov/developers/request-an-api-key](https://nvd.nist.gov/developers/request-an-api-key).

### 2. Realizar los cálculos con R

```bash
cd R/calculos
Rscript script.R      # Tabla de frecuencias, histograma y polígono
Rscript parametros.R  # Puntos 5, 6 y 7 (exporta resultados.tex)
```

### 3. Compilar el informe LaTeX

```bash
# Requiere un motor LaTeX (tectonic, pdflatex o similar)
cd informe
tectonic informe.tex     # o: pdflatex informe.tex
```

> El informe importa automáticamente las cifras calculadas por `parametros.R`
> desde `R/calculos/resultados.tex`, por lo que siempre refleja los resultados
> más recientes del script de R.

---

## 📈 Metodología estadística

### Variables del dataset (`datos_cve_completos.csv`)

| Variable | Tipo | Descripción |
|----------|------|-------------|
| `CVE_ID` | Cualitativa | Identificador único de la vulnerabilidad |
| `Fecha_Publicacion` | Cualitativa (temporal) | Fecha de publicación del CVE |
| `Tipo_Falla_CWE` | Cualitativa | Tipo de debilidad (CWE), ej. CWE-79 (XSS) |
| `Dias_Para_Actualizacion` | Cuantitativa discreta | Días entre publicación y última modificación |
| `Num_Referencias` | Cuantitativa discreta | Número de referencias/enlaces del reporte |
| `Base_Score` | **Cuantitativa continua** | Puntaje CVSS v3.1 (0.0 a 10.0) — *variable de análisis principal* |
| `Exploitability_Score` | Cuantitativa continua | Puntaje de explotabilidad |
| `Impact_Score` | Cuantitativa continua | Puntaje de impacto |
| `Severidad` | Cualitativa | Nivel de severidad (LOW, MEDIUM, HIGH, CRITICAL) |
| `Vector_Ataque` | Cualitativa | Vector de ataque (NETWORK, LOCAL, etc.) |
| `Privilegios_Requeridos` | Cualitativa | Privilegios necesarios para explotarla |
| `Interaccion_Usuario` | Cualitativa | Si requiere interacción del usuario |

### Análisis en R (`R/calculos/script.R`)

1. **Muestreo:** Se extrae una muestra aleatoria simple de **n = 500** registros de la
   población (con `set.seed(42)` para que el resultado sea reproducible).
2. **Tabla de frecuencias:** Se construye la distribución del `Base Score` en
   intervalos de anchura 1 ( `[0,1)`, `[1,2)`, … `[9,10)` ), calculando:
   - Frecuencia absoluta y relativa.
   - Frecuencias absoluta y relativa acumuladas.
   - Marca de clase de cada intervalo.
3. **Gráficos:** Se genera un histograma de frecuencias relativas con el polígono
   de frecuencias superpuesto.

### Análisis en R (`R/calculos/parametros.R` → `informe/informe.pdf`)

El script `parametros.R` calcula los puntos 5, 6 y 7 del proyecto con una
**muestra aleatoria simple sin reemplazo de n = 60** (`set.seed(42)`), y
exporta todas las cifras a `resultados.tex` para que el documento LaTeX las
importe automáticamente.
**Todas las fórmulas provienen exclusivamente del formulario del curso**
(*Formulario de Probabilidad y Estadística*, J. D. Fuentes Mora) **y de la
Tabla 4 de áreas de curva normal** (R. E. Walpole, que entrega `P(Z > z)`):

- **Punto 5 — Parámetros descriptivos de la muestra:** media, mediana,
  intervalos según `R = x_max − x_min`, `k = 1 + 3,322·log₁₀(n)`, `A = R/k`,
  moda (fórmula de datos agrupados), varianza, desviación estándar, coeficiente
  de variación, y sesgo/curtosis construidos sobre los momentos del formulario.
- **Punto 6 — Parámetros poblacionales vs. muestrales:** media, varianza,
  desviación estándar y CV poblacionales (divisor N) comparados con sus
  estadísticos muestrales (divisor n−1), y verificación de la condición
  `n/N > 0.05` del formulario para el factor de corrección por población finita
  (con n = 60 no aplica, se usa `Var(X̄) = σ²/n`).
- **Punto 7 — Cinco experimentos con distribuciones de probabilidad** (todas
  distintas, con contexto del proyecto y fórmulas de la tabla de
  distribuciones del formulario):
  1. **Binomial** — nº de vulnerabilidades CRÍTICAS entre 10 revisadas.
  2. **Poisson** — nº de vulnerabilidades CRÍTICAS publicadas en un día.
  3. **Geométrica** — nº de CVEs a revisar hasta encontrar el primero CRÍTICO.
  4. **Hipergeométrica** — nº de ataques NETWORK en 20 CVEs sin reemplazo.
  5. **Normal** — Base Score de una vulnerabilidad al azar; probabilidades
     resueltas con estandarización `Z = (X−µ)/σ` y la **Tabla 4** (áreas en
     cola derecha, con simetría para z negativos).

### Resultado

`script.R` genera en pantalla el histograma con el polígono de frecuencias superpuesto.
Se observa que la mayoría de las vulnerabilidades publicadas en el periodo analizado
se concentran en severidades **MEDIUM** y **HIGH** (puntajes entre 4.0 y 8.0),
comportamiento típico de los datos CVSS.

---

## 📦 Datasets incluidos

| Archivo | Registros | Formato | Contenido |
|---------|-----------|---------|-----------|
| `python/datos_cve_completos.csv` | ~1,969 | CSV | Dataset principal con todas las variables |
| `python/contexto_ataques.csv` | 50 | CSV | Descripciones de ataques (español si existe, si no inglés) |
| `python/contexto_ataques.json` | 50 | JSON | Descripciones de ataques solo en inglés |
| `R/calculos/tabla_frecuencias.csv` | 10 intervalos | CSV | Tabla de frecuencias del Base Score |

---

## 🔗 Fuentes

- [National Vulnerability Database (NIST NVD)](https://nvd.nist.gov/)
- [NVD API 2.0 Documentation](https://nvd.nist.gov/developers/vulnerabilities)
- [CVSS v3.1 Specification](https://www.first.org/cvss/v3.1/specification-document)

---

## 👥 Autores

- **Luis Mango Ataulfo** — [GitHub](https://github.com/LuisMangoAtaulfo)

Proyecto final — Materia de Estadística, 7.º semestre.
