# 🚲 Cyclistic Bike-Share — Análisis de Comportamiento de Usuarios

Proyecto de análisis de datos desarrollado como parte **Google Data Analytics Capstone Project**. El objetivo es identificar diferencias de comportamiento entre usuarios miembros y casuales del servicio de bicicletas compartidas Cyclistic (Chicago, 2024), con el fin de diseñar una estrategia de marketing orientada a convertir usuarios casuales en miembros anuales.

> Análisis realizado íntegramente en **R**.

---

## 📸 Vista previa

![Horas pico](https://github.com/diegovillavicencio1208-ops/Google-Data-Analytics-Capstone-Complete-a-Case-Study/blob/deb0d7f842362c61b9af9492219a1fce4a8b4d70/Insights_final/7.2.%20Horas%20pico%202024_Miembros%20vs.%20Usuarios%20casuales.png)

<!-- Reemplaza la URL anterior con la ruta correcta a tu imagen en el repositorio -->

---

## 📌 Descripción del proyecto

Cyclistic es un servicio ficticio de bicicletas compartidas con más de 5.800 bicicletas y 600 estaciones en Chicago. La directora de marketing plantea la hipótesis de que maximizar el número de membresías anuales es clave para el crecimiento sostenible de la empresa.

Este análisis responde a la pregunta: **¿En qué se diferencia el uso de las bicicletas entre miembros anuales y usuarios casuales?**

---

## 💡 Insights principales

### 1. Patrón de uso: transporte vs. recreación
Los miembros premium utilizan el servicio de forma consistente durante los días hábiles, con picos marcados en las horas de entrada y salida del trabajo, lo que indica un uso orientado a la **movilidad urbana diaria**. Los usuarios casuales, en contraste, concentran sus viajes en fines de semana y en las últimas horas del día, con duraciones promedio significativamente mayores, lo que refleja un uso predominantemente **recreativo y de ocio**.

### 2. Concentración geográfica de usuarios casuales
Las estaciones con mayor afluencia de usuarios casuales — encabezadas por *Streeter Dr & Grand Ave* (2.955 viajes), *DuSable Lake Shore Dr & Monroe St* (1.934) y *DuSable Lake Shore Dr & North Blvd* (1.667) — se ubican en zonas costeras, parques y puntos turísticos del lago Michigan. Esta concentración geográfica permite **focalizar la estrategia de marketing** en puntos de alta visibilidad y afluencia de público objetivo.

### 3. Oportunidad de conversión mediante salud y bienestar
Los usuarios casuales que ya utilizan el servicio de forma regular presentan un perfil receptivo a mensajes orientados a los beneficios para la salud cardiovascular y el bienestar físico. Una campaña que posicione la membresía anual como una herramienta de **rutina de ejercicio accesible y urbana** — combinada con el ahorro económico frente al pago por viaje — representa una palanca de conversión con alto potencial.

---

## 🛠️ Stack tecnológico

| Herramienta | Uso |
|---|---|
| **R** | Lenguaje principal del análisis |
| **tidyverse** | Manipulación y transformación de datos |
| **ggplot2** | Visualización de datos |
| **lubridate** | Manejo de fechas y tiempos |
| **dplyr** | Agrupación y resumen estadístico |
| **readr** | Carga de archivos CSV |

---

## ⚙️ Requisitos previos

- R 4.x o superior
- RStudio (recomendado)
- Paquetes: `tidyverse`, `lubridate`, `ggplot2`, `scales`, `dplyr`, `readr`

Instalación rápida:
```r
install.packages(c("tidyverse", "lubridate", "ggplot2", "scales"))
```

---

## 🚀 Instrucciones de uso

1. Descarga los datos públicos de Cyclistic desde [divvybikes.com/system-data](https://divvybikes.com/system-data).
2. Coloca los archivos CSV mensuales en la carpeta `/data`.
3. Ejecuta el script principal de análisis en RStudio.
4. Los gráficos se exportan automáticamente a la carpeta `/output`.

---

## 📄 Fuente de datos

Motivate International Inc. — **Datos públicos de Divvy Bikes**  
Licencia de uso: [https://divvybikes.com/data-license-agreement](https://divvybikes.com/data-license-agreement)

Los datos han sido anonimizados. No es posible vincular viajes con usuarios individuales.

---

## 👤 Autor

<!-- Agrega tu nombre, usuario de GitHub o enlace a LinkedIn aquí -->
