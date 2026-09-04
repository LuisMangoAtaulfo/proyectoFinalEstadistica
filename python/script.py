import requests
import pandas as pd
from datetime import datetime

url = "https://services.nvd.nist.gov/rest/json/cves/2.0"
# Ampliamos a 2000 resultados
params = {
    "pubStartDate": "2024-01-01T00:00:00.000",
    "pubEndDate": "2024-04-01T00:00:00.000",
    "resultsPerPage": 2000,
    "startIndex": 0
}

print("Descargando datos oficiales del NIST... (Tardará unos segundos)")
response = requests.get(url, params=params)

if response.status_code == 200:
    data = response.json()
    vulnerabilidades = data.get("vulnerabilities", [])
    registros_limpios = []

    for item in vulnerabilidades:
        cve = item.get("cve", {})
        cve_id = cve.get("id")

        # 1. Fechas y cálculo de días (Variable Cuantitativa nueva)
        published = cve.get("published")
        last_modified = cve.get("lastModified")
        dias_actualizacion = None

        if published and last_modified:
            # Recortamos los primeros 10 caracteres (YYYY-MM-DD) para facilitar el cálculo
            d1 = datetime.strptime(published[:10], "%Y-%m-%d")
            d2 = datetime.strptime(last_modified[:10], "%Y-%m-%d")
            dias_actualizacion = (d2 - d1).days

        # 2. Número de referencias (Variable Cuantitativa Discreta)
        referencias = len(cve.get("references", []))

        # 3. Tipo de falla / Debilidad (CWE) (Variable Cualitativa para la Moda)
        cwe_id = "Desconocido"
        weaknesses = cve.get("weaknesses", [])
        if weaknesses:
            desc = weaknesses[0].get("description", [])
            if desc:
                cwe_id = desc[0].get("value", "Desconocido")

        # 4. Métricas CVSS v3.1
        metrics = cve.get("metrics", {})
        cvss_v31 = metrics.get("cvssMetricV31", [])

        if cvss_v31:
            datos_cvss = cvss_v31[0].get("cvssData", {})

            # Cuantitativas continuas
            base_score = datos_cvss.get("baseScore")
            exploitability_score = cvss_v31[0].get("exploitabilityScore")
            impact_score = cvss_v31[0].get("impactScore")

            # Cualitativas de agrupación (Excelentes para separar datos)
            severity = datos_cvss.get("baseSeverity")
            attack_vector = datos_cvss.get("attackVector")
            privileges = datos_cvss.get("privilegesRequired")
            user_interaction = datos_cvss.get("userInteraction")
        else:
            continue  # Saltamos las que no tienen datos numéricos para mantener el dataset limpio

        # Guardar la fila completa
        registros_limpios.append({
            "CVE_ID": cve_id,
            "Fecha_Publicacion": published[:10],
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
        })

    # Convertir a tabla de Pandas
    df = pd.DataFrame(registros_limpios)

    # Eliminar filas con datos incompletos
    df = df.dropna()

    # Exportar a CSV
    nombre_archivo = "datos_cve_completos.csv"
    df.to_csv(nombre_archivo, index=False)

    print(f"¡Listo! Se guardaron {len(df)} registros en '{nombre_archivo}'.")
    print(f"Ahora tienes {len(df.columns)} variables (columnas) para jugar con tu estadística.")

else:
    print(f"Error {response.status_code}")