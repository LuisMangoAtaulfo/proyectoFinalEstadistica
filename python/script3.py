import requests
import json

# 1. Configuración de la petición a la API del NIST
url = "https://services.nvd.nist.gov/rest/json/cves/2.0"

# Pedimos 50 resultados para contexto
params = {
    "pubStartDate": "2024-01-01T00:00:00.000",
    "pubEndDate": "2024-01-15T00:00:00.000",
    "resultsPerPage": 50,
    "startIndex": 0
}

print("Descargando descripciones en inglés... \n")
response = requests.get(url, params=params)

if response.status_code == 200:
    data = response.json()
    vulnerabilidades = data.get("vulnerabilities", [])
    contexto_ataques_json = []

    for item in vulnerabilidades:
        cve = item.get("cve", {})
        cve_id = cve.get("id")

        # 2. Extracción estricta de la descripción en inglés
        descripciones = cve.get("descriptions", [])
        texto_ingles = "No English description available."

        for desc in descripciones:
            if desc.get("lang") == "en":
                texto_ingles = desc.get("value")
                break  # Si encontramos la de inglés, dejamos de buscar

        # 3. Construir la estructura de diccionario para el JSON
        contexto_ataques_json.append({
            "CVE_ID": cve_id,
            "Description_EN": texto_ingles
        })

    # 4. Mostrar en pantalla una muestra en formato JSON (los primeros 2)
    print("--- MUESTRA JSON DE 2 ATAQUES ---")
    muestra_pantalla = contexto_ataques_json[:2]
    print(json.dumps(muestra_pantalla, indent=4))

    # 5. Exportar toda la lista a un archivo .json
    nombre_archivo = "contexto_ataques.json"

    with open(nombre_archivo, 'w', encoding='utf-8') as archivo_json:
        # json.dump escribe el diccionario directamente al archivo con sangría de 4 espacios
        json.dump(contexto_ataques_json, archivo_json, indent=4, ensure_ascii=False)

    print(
        f"\n¡Listo! Se ha guardado el archivo '{nombre_archivo}' con {len(contexto_ataques_json)} registros en formato JSON.")

else:
    print(f"Error {response.status_code}: No se pudieron obtener los datos.")