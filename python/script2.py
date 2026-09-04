import requests
import pandas as pd

# 1. Configuración de la petición a la API
url = "https://services.nvd.nist.gov/rest/json/cves/2.0"

# Pedimos un lote pequeño (50 resultados) de principios de año para analizar el contexto
params = {
    "pubStartDate": "2024-01-01T00:00:00.000",
    "pubEndDate": "2024-01-15T00:00:00.000",
    "resultsPerPage": 50,
    "startIndex": 0
}

print("Descargando descripciones de los ataques... \n")
response = requests.get(url, params=params)

if response.status_code == 200:
    data = response.json()
    vulnerabilidades = data.get("vulnerabilities", [])
    contexto_ataques = []

    for item in vulnerabilidades:
        cve = item.get("cve", {})
        cve_id = cve.get("id")

        # 2. Búsqueda inteligente de descripciones
        descripciones = cve.get("descriptions", [])
        texto_espanol = None
        texto_ingles = "Sin descripción."

        for desc in descripciones:
            idioma = desc.get("lang")
            texto = desc.get("value")

            if idioma == "es":
                texto_espanol = texto
            elif idioma == "en":
                texto_ingles = texto

        # Si hay español, lo usamos. Si no, usamos el inglés.
        descripcion_final = texto_espanol if texto_espanol else texto_ingles
        idioma_detectado = "Español" if texto_espanol else "Inglés"

        contexto_ataques.append({
            "CVE_ID": cve_id,
            "Idioma": idioma_detectado,
            "Descripcion": descripcion_final
        })

    # 3. Guardar en un DataFrame
    df_descripciones = pd.DataFrame(contexto_ataques)

    # 4. Mostrar en pantalla los primeros 3 para que los leas ahora mismo
    print("--- MUESTRA DE 3 ATAQUES ---")
    for index, row in df_descripciones.head(3).iterrows():
        print(f"\nID del Ataque: {row['CVE_ID']}")
        print(f"Idioma: {row['Idioma']}")
        print(f"Descripción: {row['Descripcion']}")
        print("-" * 50)

    # 5. Exportar todo a CSV
    nombre_archivo = "contexto_ataques.csv"
    df_descripciones.to_csv(nombre_archivo, index=False, encoding='utf-8')
    print(f"\n¡Listo! Se guardaron {len(df_descripciones)} descripciones en '{nombre_archivo}'.")

else:
    print(f"Error {response.status_code}: No se pudieron obtener los datos.")