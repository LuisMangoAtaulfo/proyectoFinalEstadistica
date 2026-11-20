import csv
import time
from datetime import datetime

import requests

URL = "https://services.nvd.nist.gov/rest/json/cves/2.0"

# Rango de fechas del proyecto: enero-abril 2024 (91 dias).
# La API del NIST no acepta rangos mayores a 120 dias por peticion.
FECHA_INICIO = "2024-01-01T00:00:00.000Z"
FECHA_FIN = "2024-04-01T00:00:00.000Z"

# Maximo de resultados por pagina que permite la API (tope duro del NIST)
TAMANIO_PAGINA = 2000

# Sin clave API el NIST limita las peticiones, esperamos un poco entre paginas
ESPERA_ENTRE_PAGINAS = 7


def descargar_pagina(start_index):
    """Descarga una pagina de la API, reintentando si nos bloquean (403/429)."""
    params = {
        "pubStartDate": FECHA_INICIO,
        "pubEndDate": FECHA_FIN,
        "resultsPerPage": TAMANIO_PAGINA,
        "startIndex": start_index
    }

    for intento in range(5):
        respuesta = requests.get(URL, params=params, timeout=60)

        if respuesta.status_code == 200:
            return respuesta.json()

        if respuesta.status_code in (403, 429):
            espera = 15 * (intento + 1)
            print(f"  Limite de peticiones alcanzado ({respuesta.status_code}). "
                  f"Esperando {espera} segundos...")
            time.sleep(espera)
            continue

        respuesta.raise_for_status()

    raise RuntimeError("La API del NIST no respondio tras varios intentos.")


def descargar_todas_las_cves():
    """Recorre TODAS las paginas del rango hasta juntar todos los CVEs.

    La API devuelve como maximo 2000 resultados por pagina (ordenados por
    fecha de publicacion ascendente), asi que hay que avanzar el startIndex
    hasta cubrir el total reportado por la API.
    """
    vulnerabilidades = []
    total_esperado = None
    indice = 0

    while True:
        datos = descargar_pagina(indice)

        if total_esperado is None:
            total_esperado = datos.get("totalResults", 0)
            print(f"La API reporta {total_esperado} CVEs publicados en el periodo.")

        lote = datos.get("vulnerabilities", [])
        if not lote:
            break

        vulnerabilidades.extend(lote)
        indice += len(lote)
        print(f"  Descargados {indice}/{total_esperado}...")

        if indice >= total_esperado:
            break

        time.sleep(ESPERA_ENTRE_PAGINAS)

    return vulnerabilidades


print("Descargando datos oficiales del NIST... (tardara unos minutos)")
vulnerabilidades = descargar_todas_las_cves()

registros_limpios = []

for item in vulnerabilidades:
    cve = item.get("cve", {})
    cve_id = cve.get("id")

    # 1. Fechas y calculo de dias (Variable Cuantitativa nueva)
    published = cve.get("published")
    last_modified = cve.get("lastModified")
    dias_actualizacion = None

    if published and last_modified:
        # Recortamos los primeros 10 caracteres (YYYY-MM-DD) para facilitar el calculo
        d1 = datetime.strptime(published[:10], "%Y-%m-%d")
        d2 = datetime.strptime(last_modified[:10], "%Y-%m-%d")
        dias_actualizacion = (d2 - d1).days

    # 2. Numero de referencias (Variable Cuantitativa Discreta)
    referencias = len(cve.get("references", []))

    # 3. Tipo de falla / Debilidad (CWE) (Variable Cualitativa para la Moda)
    cwe_id = "Desconocido"
    weaknesses = cve.get("weaknesses", [])
    if weaknesses:
        desc = weaknesses[0].get("description", [])
        if desc:
            cwe_id = desc[0].get("value", "Desconocido")

    # 4. Metricas CVSS v3.1
    metrics = cve.get("metrics", {})
    cvss_v31 = metrics.get("cvssMetricV31", [])

    if cvss_v31:
        datos_cvss = cvss_v31[0].get("cvssData", {})

        # Cuantitativas continuas
        base_score = datos_cvss.get("baseScore")
        exploitability_score = cvss_v31[0].get("exploitabilityScore")
        impact_score = cvss_v31[0].get("impactScore")

        # Cualitativas de agrupacion (Excelentes para separar datos)
        severity = datos_cvss.get("baseSeverity")
        attack_vector = datos_cvss.get("attackVector")
        privileges = datos_cvss.get("privilegesRequired")
        user_interaction = datos_cvss.get("userInteraction")
    else:
        continue  # Saltamos las que no tienen datos numericos para mantener el dataset limpio

    fila = {
        "CVE_ID": cve_id,
        "Fecha_Publicacion": published[:10] if published else None,
        "Tipo_Falla_CWE": cwe_id,
        "Dias_Para_Actualizacion": dias_actualizacion,
        "Num_Referencias": referencias,
        "Base_Score": base_score,
        "Exploitability_Score": exploitability_score,
        "Impact_Score": impact_score,
        "Severidad": severity,
        "Vector_Ataque": attack_vector,
        "Privilegios_Requeridos": privileges,
        "Interaccion_Usuario": user_interaction
    }

    # Eliminar filas con datos incompletos (equivale al dropna de pandas)
    if any(valor is None for valor in fila.values()):
        continue

    registros_limpios.append(fila)

# Columnas en el mismo orden que el dataset original
campos = [
    "CVE_ID", "Fecha_Publicacion", "Tipo_Falla_CWE", "Dias_Para_Actualizacion",
    "Num_Referencias", "Base_Score", "Exploitability_Score", "Impact_Score",
    "Severidad", "Vector_Ataque", "Privilegios_Requeridos", "Interaccion_Usuario"
]

# Exportar a CSV
nombre_archivo = "datos_cve_completos.csv"
with open(nombre_archivo, "w", newline="", encoding="utf-8") as archivo:
    escritor = csv.DictWriter(archivo, fieldnames=campos)
    escritor.writeheader()
    escritor.writerows(registros_limpios)

print(f"¡Listo! Se guardaron {len(registros_limpios)} registros en '{nombre_archivo}'.")
print(f"Ahora tienes {len(campos)} variables (columnas) para jugar con tu estadística.")
