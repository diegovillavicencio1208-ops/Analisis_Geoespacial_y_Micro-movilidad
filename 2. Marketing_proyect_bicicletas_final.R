# 1. Configuación inicial-------------------------------------------------------
  # Script: Cyclistic_Marketing_Proyect
  # Autor: Diego L. Villavicencio Merino
  # Fecha: 08-07-2025
  # 1.1. Descripcion-----------------------------------------------------------------
  # La directora de marketing cree que el éxito futuro de la empresa depende de 
  # maximizar el número de membresías anuales. Tu equipo quiere entender cómo los 
  # ciclistas ocasionales y los miembros anuales utilizan las bicicletas de 
  # Cyclistic de manera diferente.
  # 1.2. Requisitos--------------------------------------------------------------
install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("DescTools")
install.packages("paletteer")
install.packages("colorBlindness")
install.packages("sysfonts")
install.packages("showtext")
install.packages("ggh4x")
install.packages("viridis")
install.packages("treemapify")
install.packages("arrow")
  # 1.3. Librerias---------------------------------------------------------------
# Siempre en uso
library(tidyverse)
library(dplyr)
# Limpieza
library(janitor)
library(skimr)
library(lubridate)
library(DescTools)
# Graficos
library(ggplot2)
library(treemapify)
library(paletteer)
library(showtext)
library(sysfonts)
library(systemfonts)
library(forcats)
library(ggh4x)
library(viridis)
# extraer
library(arrow)
# Activar uso de fuentes 
showtext_auto()
# 2. Datos----------------------------------------------------------------------
  # 2.1. Cargar data sets---------------------------------------------------------
    # 2020--------------------------------------------------------------------------
Q1_2020 <- read.csv("1. Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")
abril_2020 <- read.csv("2. 202004-divvy-tripdata.csv")
mayo_2020 <- read.csv("3. 202005-divvy-tripdata.csv")
junio_2020 <- read.csv("4. 202006-divvy-tripdata.csv")
julio_2020 <- read.csv("5. 202007-divvy-tripdata.csv")
agosto_2020 <- read.csv("6. 202008-divvy-tripdata.csv")
septiembre_2020 <- read.csv("7. 202009-divvy-tripdata.csv")
octubre_2020 <- read.csv("8 .202010-divvy-tripdata.csv")
noviembre_2020 <- read.csv("9. 202011-divvy-tripdata.csv")
diciembre_2020 <- read.csv("10. 202012-divvy-tripdata.csv")
    # 2021-------------------------------------------------------------------
enero_2021 <- read.csv("202101-divvy-tripdata.csv")
febrero_2021 <- read.csv("202102-divvy-tripdata.csv")
marzo_2021 <- read.csv("202103-divvy-tripdata.csv")
abril_2021 <- read.csv("202104-divvy-tripdata.csv")
mayo_2021 <- read.csv("202105-divvy-tripdata.csv")
junio_2021 <- read.csv("202106-divvy-tripdata.csv")
julio_2021 <- read.csv("202107-divvy-tripdata.csv")
agosto_2021 <- read.csv("202108-divvy-tripdata.csv")
septiembre_2021 <- read.csv("202109-divvy-tripdata.csv")
octubre_2021 <- read.csv("202110-divvy-tripdata.csv")
noviembre_2021 <- read.csv("202111-divvy-tripdata.csv")
diciembre_2021 <- read.csv("202112-divvy-tripdata.csv")
    # 2022-------------------------------------------------------------------
enero_2022 <- read.csv("202201-divvy-tripdata.csv")
febrero_2022 <- read.csv("202202-divvy-tripdata.csv")
marzo_2022 <- read.csv("202203-divvy-tripdata.csv")
abril_2022 <- read.csv("202204-divvy-tripdata.csv")
mayo_2022 <- read.csv("202205-divvy-tripdata.csv")
junio_2022 <- read.csv("202206-divvy-tripdata.csv")
julio_2022 <- read.csv("202207-divvy-tripdata.csv")
agosto_2022 <- read.csv("202208-divvy-tripdata.csv")
septiembre_2022 <- read.csv("202209-divvy-tripdata.csv")
octubre_2022 <- read.csv("202210-divvy-tripdata.csv")
noviembre_2022 <- read.csv("202211-divvy-tripdata.csv") 
diciembre_2022 <- read.csv("202212-divvy-tripdata.csv")
    # 2023-------------------------------------------------------------------
enero_2023 <- read.csv("202301-divvy-tripdata.csv")
febrero_2023 <- read.csv("202302-divvy-tripdata.csv")
marzo_2023 <- read.csv("202303-divvy-tripdata.csv")
abril_2023 <- read.csv("202304-divvy-tripdata.csv")
mayo_2023 <- read.csv("202305-divvy-tripdata.csv")
junio_2023 <- read.csv("202306-divvy-tripdata.csv")
julio_2023 <- read.csv("202307-divvy-tripdata.csv")
agosto_2023 <- read.csv("202308-divvy-tripdata.csv")
septiembre_2023 <- read.csv("202309-divvy-tripdata.csv")
octubre_2023 <- read.csv("202310-divvy-tripdata.csv")
noviembre_2023 <- read.csv("202311-divvy-tripdata.csv")
diciembre_2023 <- read.csv("202312-divvy-tripdata.csv")
    # 2024-------------------------------------------------------------------
enero_2024 <- read.csv("202401-divvy-tripdata.csv")
febrero_2024 <- read.csv("202402-divvy-tripdata.csv")
marzo_2024 <- read.csv("202403-divvy-tripdata.csv")
abril_2024 <-read.csv("202404-divvy-tripdata.csv")
mayo_2024 <- read.csv("202405-divvy-tripdata.csv")
junio_2024 <- read.csv("202406-divvy-tripdata.csv")
julio_2024 <- read.csv("202407-divvy-tripdata.csv")
agosto_2024 <- read.csv("202408-divvy-tripdata.csv")
septiembre_2024 <- read.csv("202409-divvy-tripdata.csv")
octubre_2024 <- read.csv("202410-divvy-tripdata.csv")
noviembre_2024 <- read.csv("202411-divvy-tripdata.csv")
diciembre_2024 <-read.csv("202412-divvy-tripdata.csv")
  # 2.2. Muestreo y union de los data.sets 10%--------------------------------------
    # 2020--------------------------------------------------------------------------
# Enero
Q1_2020 <- Q1_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# Abril
abril_2020 <- abril_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# Mayo
mayo_2020 <- mayo_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# junio
junio_2020 <- junio_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# julio
julio_2020 <- julio_2020 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# agosto 
agosto_2020 <- agosto_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# septiembre
septiembre_2020 <- septiembre_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# octubre
octubre_2020 <- octubre_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# noviembre
noviembre_2020 <- noviembre_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
# diciembre
diciembre_2020 <- diciembre_2020 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
    # 2020_raw----------------------------------------------------------------------
raw_2020 <- bind_rows(Q1_2020, abril_2020, mayo_2020,
                      junio_2020, julio_2020, agosto_2020, septiembre_2020, 
                      octubre_2020, noviembre_2020, diciembre_2020)
remove(Q1_2020, abril_2020, mayo_2020,
       junio_2020, julio_2020, agosto_2020, septiembre_2020, 
       octubre_2020, noviembre_2020, diciembre_2020)
    # 2021--------------------------------------------------------------------------
# Enero
enero_2021 <- enero_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Febrero
febrero_2021 <- febrero_2021 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )

# Marzo
marzo_2021 <- marzo_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Abril
abril_2021 <- abril_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Mayo
mayo_2021 <- mayo_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# junio
junio_2021 <- junio_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# julio
julio_2021 <- julio_2021 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# agosto 
agosto_2021 <- agosto_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# septiembre
septiembre_2021 <- septiembre_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# octubre
octubre_2021 <- octubre_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# noviembre
noviembre_2021 <- noviembre_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# diciembre
diciembre_2021 <- diciembre_2021 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
    # 2021_raw----------------------------------------------------------------------
raw_2021 <- bind_rows(enero_2021, febrero_2021, marzo_2021, abril_2021, mayo_2021,
                      junio_2021, julio_2021, agosto_2021, septiembre_2021, 
                      octubre_2021, noviembre_2021, diciembre_2021)
remove(enero_2021, febrero_2021, marzo_2021, abril_2021, mayo_2021,
       junio_2021, julio_2021, agosto_2021, septiembre_2021, 
       octubre_2021, noviembre_2021, diciembre_2021)
    # 2022--------------------------------------------------------------------------
# Enero
enero_2022 <- enero_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Febrero
febrero_2022 <- febrero_2022 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )

# Marzo
marzo_2022 <- marzo_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Abril
abril_2022 <- abril_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Mayo
mayo_2022 <- mayo_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# junio
junio_2022 <- junio_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# julio
julio_2022 <- julio_2022 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# agosto 
agosto_2022 <- agosto_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# septiembre
septiembre_2022 <- septiembre_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# octubre
octubre_2022 <- octubre_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# noviembre
noviembre_2022 <- noviembre_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# diciembre
diciembre_2022 <- diciembre_2022 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
    # 2022_raw----------------------------------------------------------------------
raw_2022 <- bind_rows(enero_2022, febrero_2022, marzo_2022, abril_2022, mayo_2022,
                      junio_2022, julio_2022, agosto_2022, septiembre_2022, 
                      octubre_2022, noviembre_2022, diciembre_2022)
remove(enero_2022, febrero_2022, marzo_2022, abril_2022, mayo_2022,
       junio_2022, julio_2022, agosto_2022, septiembre_2022, 
       octubre_2022, noviembre_2022, diciembre_2022)
    # 2023--------------------------------------------------------------------------
# Enero
enero_2023 <- enero_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Febrero
febrero_2023 <- febrero_2023 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )

# Marzo
marzo_2023 <- marzo_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Abril
abril_2023 <- abril_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Mayo
mayo_2023 <- mayo_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# junio
junio_2023 <- junio_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# julio
julio_2023 <- julio_2023 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# agosto 
agosto_2023 <- agosto_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# septiembre
septiembre_2023 <- septiembre_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# octubre
octubre_2023 <- octubre_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# noviembre
noviembre_2023 <- noviembre_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# diciembre
diciembre_2023 <- diciembre_2023 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
    # 2023_raw----------------------------------------------------------------------
raw_2023 <- bind_rows(enero_2023, febrero_2023, marzo_2023, abril_2023, mayo_2023,
                      junio_2023, julio_2023, agosto_2023, septiembre_2023, 
                      octubre_2023, noviembre_2023, diciembre_2023)
remove(enero_2023, febrero_2023, marzo_2023, abril_2023, mayo_2023,
       junio_2023, julio_2023, agosto_2023, septiembre_2023, 
       octubre_2023, noviembre_2023, diciembre_2023)
    # 2024--------------------------------------------------------------------------
# Enero
enero_2024 <- enero_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Febrero
febrero_2024 <- febrero_2024 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )

# Marzo
marzo_2024 <- marzo_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Abril
abril_2024 <- abril_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# Mayo
mayo_2024 <- mayo_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# junio
junio_2024 <- junio_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# julio
julio_2024 <- julio_2024 %>%
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# agosto 
agosto_2024 <- agosto_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# septiembre
septiembre_2024 <- septiembre_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# octubre
octubre_2024 <- octubre_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# noviembre
noviembre_2024 <- noviembre_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
# diciembre
diciembre_2024 <- diciembre_2024 %>% 
  na.omit(member_casual) %>% 
  sample_frac(0.1) %>% 
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
  )
    # 2024_raw----------------------------------------------------------------------
raw_2024 <- bind_rows(enero_2024, febrero_2024, marzo_2024, abril_2024, mayo_2024,
                                   junio_2024, julio_2024, agosto_2024, septiembre_2024, 
                                   octubre_2024, noviembre_2024, diciembre_2024)
remove(enero_2024, febrero_2024, marzo_2024, abril_2024, mayo_2024,
       junio_2024, julio_2024, agosto_2024, septiembre_2024, 
       octubre_2024, noviembre_2024, diciembre_2024)
    # consolidada_raw-------------------------------------------------------------
# Unir años en un solo data.set 
raw_consolidada <- bind_rows(
  raw_2020,
  raw_2021,
  raw_2022,
  raw_2023,
  raw_2024
)
# Numero de observaciones antes de limpieza 2707909
# Numero de varaibles 13 antes de limpieza
# 3. Exploración inicial de los datos-------------------------------------------
glimpse(raw_consolidada)
skim(raw_consolidada)
  # 3.1. Seleccion de variables prioritarias------------------------------------
    # Member_casual: tipos de usarios, casuales y miembros
    # rideable_type: tipo de bicicleta usada 
    # started_at y end_at: Fecha y hora del incio y fun de un viaje
    # start_lat, end_lat, start_lng y endi_lng: distancia recorrida en un viaje
  # 3.2. Variables no prioridaterias pero relevantes----------------------------
    # start_station_name y end_station_name: Nombres de las estacionesn, incio y fin 
    # start_station_id y end_station_id: Id de las estaciones, inicio y fin
  # 3.3. Varaibles no relevantes--------------------------------------------------
    # ride_id
# 4. Limpieza de datos----------------------------------------------------------
limpia_consolidada <- raw_consolidada %>%  
  # 4.1. Corregir tipos de datos------------------------------------------------
mutate(
  started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
  ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
  # 4.2. Eliminar espacios en blanco--------------------------------------------
  # Eliminar todos los espacios 
  member_casual = str_replace_all(member_casual," ",""), 
  ride_id = str_replace_all(ride_id," ",""),
  rideable_type = str_replace_all(rideable_type," ",""),
  start_station_id = str_replace_all(start_station_id," ",""),
  end_station_id = str_replace_all(end_station_id," ",""),
  # Eliminar los espacios de los extremos y limita a los espacios entre palabras a 1
  start_station_name = str_squish(start_station_name),
  end_station_name = str_squish(end_station_name),
  # 4.3. Eliminar decimales de variables----------------------------------------
  start_station_id = sub("\\..*$", "", as.character(start_station_id)),
  end_station_id   = sub("\\..*$", "", as.character(end_station_id)),
  # 4.4. Renombrar cateogiras de variables--------------------------------------
  rideable_type = fct_recode(rideable_type,
                             "Scooter" = "electric_scooter",
                             "Electrica" = "electric_bike",
                             "Clasica" = "classic_bike"),
  member_casual = fct_recode(member_casual,
                             "Miembro" = "member",
                             "Casual" = "casual"),
    # 4.1.1. Cambiar el tipo de datos-------------------------------------------
  member_casual = as.character(member_casual),
  rideable_type = as.character(rideable_type)
) %>%
  # 4.5. Eliminar NA y celdas en blanco de las variables relevantes-------------
filter(
  # NA
  !is.na(member_casual),
  !is.na(rideable_type),
  !is.na(started_at),
  !is.na(ended_at),
  !is.na(start_lat),
  !is.na(end_lat),
  !is.na(start_lng),
  !is.na(end_lng),
  # Celdas en blanco
  nzchar(member_casual),
  nzchar(rideable_type),
  nzchar(started_at),
  nzchar(ended_at),
  nzchar(start_lat),
  nzchar(end_lat),
  nzchar(start_lng),
  nzchar(end_lng)
) %>% 
  # 4.7. Eliminar duplicados----------------------------------------------------
distinct() %>%
  # 4.8. Crear variables para el analisis------------------------------------------------
mutate(
  # Distancia en km del viaje
  distance_km = round(haversine_distance(start_lat, start_lng, end_lat, end_lng),2),
  # duracion del viaje en min y horas
  duration_min = round(as.numeric(difftime(ended_at, started_at, units = "min")), 2),
  duration_hours = round(duration_min / 60, 2),
  # Año
  año = year(started_at),
  # Mes
  mes = month(started_at, label = TRUE),
  # dia
  dia_semana = wday(started_at, label = TRUE, week_start = 1),
  # horas
  hora = hour(started_at)
) %>% 
  # 4.9. Eliminar valores imposibles--------------------------------------------
# Viajes absurdamente pequeños
filter(
  duration_min > 0,
  # Viajes superiores a una dia 
  duration_min <= 1440,
  # diastancias imposibles 
  distance_km > 0,
  distance_km <= 100
) 
  # 4.10. Eliminar valores atipicos----------------------------------------------
limpia_consolidada <- limpia_consolidada %>% 
  filter(
    duration_min >= limites_duracion[1] & duration_min <= limites_duracion[2],
    distance_km >= limites_distancia[1] & distance_km <= limites_distancia[2]
  )
  # 4.11. Cambiar Levels (categorias) de la variable MES y DIA------------------
levels(limpia_consolidada$mes) <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
levels(limpia_consolidada$dia_semana) <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
  # 4.12. Extraer año 2024--------------------------------------------------------
limpia_2024 <- limpia_consolidada %>%
  filter(year(started_at) == 2024)
# 5. Complementos para limpieza-------------------------------------------------
  # 5.1. Revisar formato correcto de start_station_id y end_station_id-------------
registros_imputables_opt <- raw_2024 %>%
  filter(
    nzchar(start_station_id),        # Equivale a !is.na() & != ""
    nzchar(end_station_id),
    start_station_id == end_station_id,
    !nzchar(start_station_name),     # Equivale a is.na() | == ""
    nzchar(end_station_name)
  )
# Conclusion no es posible realizar imputacion ya que las celdas no cumplen las 
# condicones necesariar para realizar el procedimiento, sin embargo no se eliminaran 
# las celdas vacias de la variable start_station_name, ya que esta variable no 
# es trancendental para el analisis y de eliminar celdas en blanco se perderian
# 18% del tota del data set lo cual no hara perder informacion que si es relevante 
# para el analisis. 
  # 5.2. Contar valores faltantes---------------------------------------------------
raw_consolidada %>%
  summarise(across(everything(), ~ sum(is.na(.))))
  # 5.3. Función Haversine para distancia en km------------------------------------
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Radio de la Tierra en km
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- R * c
  return(distance)
}
  # 5.4. Visualizacion grafica, atipicos ANTES de IQR---------------------------
# 1. Boxplot para duration_min
limpia_consolidada %>% 
  ggplot(mapping = aes(y = duration_min)) +
  geom_boxplot() +
  facet_wrap(~año) +
  labs(title = "Duracion en Minutos",
       subtitle = "Valores atipicos con el fintro empresarial")  
# 2. Boxplot para distance_km
limpia_consolidada %>% 
  ggplot(mapping = aes(y = distance_km)) +
  geom_boxplot() +
  facet_wrap(~año) +
  labs(title = "Distancia km",
       subtitle = "Valores atipicos con el fintro empresarial")
# 3. Histograma duration_minutos
limpia_consolidada %>% 
  ggplot(mapping = aes(x = duration_hours, fill = member_casual)) +
  facet_wrap(~año) +
  geom_histogram(bins = 30) +
  labs(title = "Duracion en Minutos",
       subtitle = "Valores atípicos con el filtro empresarial")
# 4. Histograma distance_km
limpia_consolidada %>% 
  ggplot(mapping = aes(x = distance_km, fill = member_casual)) +
  facet_wrap(~año) +
  geom_histogram(bins = 30) +
  labs(title = "Distancia en Kilómetros",
       subtitle = "Valores atípicos con el filtro empresarial")

  # 5.5. Función para detectar valores atípicos dentro de un rango validado por reglas de negocio----------
calcular_limites_iqr <- function(variable) {
  q1 <- quantile(variable, 0.25)
  q3 <- quantile(variable, 0.75)
  iqr <- q3 - q1
  lower_bound <- max(min(variable), q1 - 1.5 * iqr)  # No menor que el mínimo real
  upper_bound <- min(max(variable), q3 + 1.5 * iqr)  # No mayor que el máximo real
  return(c(lower_bound, upper_bound))
}
  # 5.6. Creacion de limites de las variables duration_hours y distance_km 2024----------
limites_duracion <- calcular_limites_iqr(limpia_consolidada$duration_min)
limites_distancia <- calcular_limites_iqr(limpia_consolidada$distance_km)
  # 5.7. Visualizacion grafica, atipicos DESPUEST DE IQR------------------------
# 1.1. Boxplot para duration_hours, Limpia
limpia_consolidada %>% 
  ggplot(mapping = aes(y = duration_min)) +
  geom_boxplot() +
  facet_grid(~año) +
  labs(title = "Duracion en Horas",
       subtitle = "IQR + Estandar empresarial")  
# 2.1. Boxplot para distance_km, Limpia
limpia_consolidada %>% 
  ggplot(mapping = aes(y = distance_km)) +
  geom_boxplot() +
  facet_wrap(~año) +
  labs(title = "Distancia km",
       subtitle = "IQR + Estandar empresarial")
# 3.1. Histograma duration_min, Limpia
limpia_consolidada %>% 
  ggplot(mapping = aes(x = duration_min, fill = member_casual)) +
  geom_histogram(bins = 30) +
  facet_grid(~año) +
  labs(title = "Duracion en Minutos",
       subtitle = "IQR + Estandar empresarial")
# 4.1. Histograma distance_km, Limpia
limpia_consolidada %>% 
  ggplot(mapping = aes(x = distance_km, fill = member_casual)) +
  geom_histogram(bins = 30) +
  facet_wrap(~año) +
  labs(title = "Distancia en Kilómetros",
       subtitle = "IQR + Estandar empresarial")
# 6. Insight--------------------------------------------------------------------
  # 6.1. Distribucion porcentual, 2024------------------------------------------
    # Filtro del data set-----------------------------------------------------------
limpia_2024 %>% 
  count(member_casual) %>% 
  mutate(percentage = n/sum(n) * 100) %>% 
    # Ordenar species por porcentaje (de menor a mayor para grafico ASC)#-----------
mutate(member_casual = fct_rev(fct_reorder(member_casual, percentage))) %>%
    # Grafico-----------------------------------------------------------------------
ggplot(aes(x = member_casual, y = percentage, fill = member_casual)) +
  geom_col() +
    # Etiquetas del grafico--------------------------------------------------------
geom_text(
  aes(label = paste0(round(percentage, 1), "%")),  # Formato: "XX.X%"
  position = position_dodge(width = 0.9), # Centrar en la barra
  vjust = -0.5,
  color = "black",  # Color del texto
  size = 5,       # Tamaño del texto
  fontface = "bold", # Negrita
  family = "Petrona Light"
) +
  # Ajustar eje Y: expande un 10% más allá del máximo (scale_y_continuous)
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10)),  # 0% abajo, 10% arriba
    limits = c(0, NA)) +
  # Nota se puede usar  "coord_flip()" para cambiar la presentacion de las barras
  # Nota se usa "fct_rev(fct_reorder()) para orden DESC
    # Leyendas y notaciones---------------------------------------------------------
labs(
  title = "Distribución de Tipos de Usuario en 2024",
  subtitle = "Porcentaje de la muestra: Miembros vs. Usuarios Casual",
  x = NULL,
  y = "Porcentaje (%)",
  caption = "Fuente: Datos de Cyclistic, 2024 | Autor: Diego L. Villavicencio Merino",
  fill = "Tipos de Usuarios"
) +
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 12, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-------------------------------------------------------------------------
scale_fill_paletteer_d("fishualize::Aluterus_scriptus")
  # 6.2. Evolución del uso por tipo de usuario (2020-2024)-----------------------
    # Filtro del data set-----------------------------------------------------------
a <- limpia_consolidada %>%       
  count(año, member_casual) %>%             # Contar por año y tipo de usuario
  group_by(año) %>%
  mutate(percentage = n / sum(n) * 100) %>%   # Calcular porcentaje por año
    # Grafico-----------------------------------------------------------------------
ggplot(aes(x = año, y = percentage, color = member_casual)) +
  geom_line (linewidth = 0.5, lineend = "round") +
  geom_point(size = 1) +
    # Etiquetas del grafico--------------------------------------------------------
geom_text(
  aes(label = paste0(round(percentage, 2), "%")),  # Formato: "XX.X%"
  position = position_dodge(width = 0.9), # Centrar en la barra
  vjust = -0.5,
  color = "black",  # Color del texto
  size = 5,       # Tamaño del texto
  fontface = "bold", # Negrita
  family = "Petrona Light"
) +
  # Ajuste de escala eje Y
  scale_y_continuous(limits = c(20, 80)) +
    # Leyendas y notaciones---------------------------------------------------------
labs(
  title = "Evolución anual del porcentaje de viajes por tipo de usuario (2020-2024)",
  subtitle = NULL,
  x = NULL,
  y = "Porcentaje (%)",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  color = "Tipo de Subscripción"
) +
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-------------------------------------------------------------------------
scale_color_manual(
  values = c(
    "Miembro" = "#011C40FF",
    "Casual" = "#035AA6FF"),
  # Ordenar el Orden de presentacion del Tipo de subscripción
  breaks = c("Miembro", "Casual")) 
  # 6.3. Tipos de bicicletas x Tipo de usuario 2024------------------------------------
    # Filtro del data set-----------------------------------------------------------
limpia_2024 %>% 
  count(rideable_type, member_casual) %>% 
  mutate(percentage = n/sum(n) * 100) %>% 
    # Ordenar species por porcentaje (de menor a mayor para grafico ASC)#-----------
mutate(rideable_type = fct_rev(fct_reorder(rideable_type, percentage))) %>%
    # Grafico-----------------------------------------------------------------------
ggplot(aes(x = rideable_type, y = percentage, fill = rideable_type)) +
  geom_col() +  # geom_col() es equivalente a geom_bar(stat = "identity")
  facet_wrap(~member_casual) +
    # Etiquetas del grafico--------------------------------------------------------
geom_text(
  aes(label = paste0(round(percentage, 2), "%")),  # Formato: "XX.X%"
  position = position_dodge(width = 0.9), # Centrar en la barra
  vjust = -0.5,
  color = "black",  # Color del texto
  size = 5,       # Tamaño del texto
  fontface = "bold", # Negrita
  family = "Petrona Light"
) +
    # Ajustar eje Y: expande un 10% más allá del máximo (scale_y_continuous)----
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10)),  # 0% abajo, 10% arriba
    limits = c(0, NA)) +
    # Leyendas y notaciones---------------------------------------------------------
labs(
  title = "Distribución de Tipos de Bicicletas por Tipo de Usuario, 2024",
  subtitle = "Porcentaje de uso según categoría (Miembro vs. Casual)",
  x = "Tipo de Bicicleta",
  y = "Porcentaje (%)",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = "Tipo de Bicicleta"
) +
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 12, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-------------------------------------------------------------------------
scale_fill_paletteer_d("fishualize::Aluterus_scriptus")
  # 6.4. Duración promedio de viajes por tipo de bicicleta, 2024-----------------
    # Filtro del data set-----------------------------------------------------------
limpia_2024 %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(promedio_min = mean(duration_min)) %>%
    # Ordenar species por porcentaje (de menor a mayor para grafico ASC)-----------
mutate(
  rideable_type = fct_rev(fct_reorder(rideable_type, promedio_min))
) %>%
    # Grafico---------------------------------------------------------------------
ggplot(aes(x = rideable_type, y = promedio_min, fill = rideable_type)) +
  geom_col() +
  facet_wrap(~member_casual) + 
    # Etiquetas del grafico--------------------------------------------------------
geom_text(
  aes(label = paste0(round(promedio_min, 2))),  # Formato: "XX.X%"
  position = position_dodge(width = 0.9), # Centrar en la barra
  vjust = -0.5,
  color = "black",  # Color del texto
  size = 5,       # Tamaño del texto
  fontface = "bold", # Negrita
  family = "Petrona Light"
) +
    # Ajustar eje Y: expande un 10% más allá del máximo (scale_y_continuous)------
scale_y_continuous(
  expand = expansion(mult = c(0, 0.10)),  # 0% abajo, 10% arriba
  limits = c(0, NA)) +
    # Leyendas y notaciones---------------------------------------------------------
labs(
  title = "Duración promedio de viajes por tipo de bicicleta",
  subtitle = "Promedio en minutos por tipo de usuario y bicicleta usada durante 2024",
  x = "Tipo de Bicicleta",
  y = "Promedio en minutos",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = "Tipo de Bicicleta"
) +
  # Opcional: Cambiar también la leyenda (si fill=rideable_type)
  
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-------------------------------------------------------------------------
scale_fill_paletteer_d("fishualize::Acanthurus_leucosternon")
  # 6.5. Duración promedio de viajes por tipo de usuario (2020-2024)-------------
    # Filtro del data set-----------------------------------------------------------
c <- limpia_consolidada %>%
  group_by(member_casual, año,.groups = "drop") %>% 
  summarise(
    promedio_min = mean(duration_min)) %>% 
    # Grafico---------------------------------------------------------------------
ggplot(aes(x = año, y = promedio_min, fill = member_casual)) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
    # Etiquetas del grafico--------------------------------------------------------
geom_text(
  aes(label = round(promedio_min, 2), group = member_casual),
  position = position_dodge(width = 0.9),
  hjust = -0.2,  # Ajuste horizontal por el flip
  color = "black",
  size = 5,
  fontface = "bold",
  family = "Petrona Light"
) +
    # Ajustar eje Y: expande un 10% más allá del máximo (scale_y_continuous)------
scale_y_continuous(
  expand = expansion(mult = c(0, 0.10)),  # 0% abajo, 10% arriba
  limits = c(0, NA)) +
    # Leyendas y notaciones---------------------------------------------------------
labs(
  title = "Duración promedio de viajes por tipo de usuario (2020-2024)",
  subtitle = "Minutos promedio empleados por tipo de usuario",
  x = NULL,
  y = "Promedio en minutos",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = "Tipo de Usuarios"
) +
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-------------------------------------------------------------------------
scale_fill_manual(
  values = c(
    "Miembro" = "#011C40FF",
    "Casual" = "#035AA6FF"),
  # Ordenar el Orden de presentacion del Tipo de subscripción
  breaks = c("Miembro", "Casual")) 
  # 6.6. Duración promedio por día y tipo de usuario, 2024-----------------------
    # Filtro del data set-----------------------------------------------------------
limpia_2024 %>%
  group_by(member_casual, dia_semana) %>% 
  summarise(promedio_min = mean(duration_min), .groups = "drop") %>% 
    # Grafico---------------------------------------------------
ggplot(aes(x = dia_semana, y = member_casual, fill = promedio_min)) +
  geom_tile() +
    # Leyendas y notaciones-----------------------------------------------------
labs(
  title = "Minutos promedio de uso por día y tipo de usuario, 2024",
  subtitle = NULL,
  x = NULL,
  y = NULL,
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = "Minutos promedio"
) +
    # Configuracion del fondo del tema----------------------------------------
theme_minimal() +  
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light", hjust = 0.028, vjust = -2),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light", hjust = 0.028, vjust = -2),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-----------------------------------------------------------------------
scale_fill_gradientn(colors = c(
  "#9EC8FFFF", 
  "#207CF5FF",
  "#0E34B2FF",
  "#071140FF"
))
  
  # 6.7. Distribución porcentual de viajes por tipo de usuario 2024--------------
    # Filtro del data set---------------------------------------------------------
limpia_2024 %>% 
  count(mes, member_casual) %>% 
  group_by(mes) %>% 
  mutate(percentage = n/sum(n) * 100) %>%
  ungroup() %>%
    # Grafico----------------------------------------------------------------------
ggplot(aes(x = mes, y = percentage, fill = member_casual)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
    # Leyendas--------------------------------------------------------------------
labs(
  title = "Distribución porcentual de viajes por tipo de usuario 2024",
  subtitle = "Cada barra representa el 100% de viajes en el mes",
  x = NULL, y = NULL, fill = "Tipo de usuario",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino"
) +
    # Configuracion del fondo del tema----------------------------------------
theme_minimal() +  
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 12, family = "Abhaya Libre"),
    legend.title = element_text(size = 13, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-----------------------------------------------------------------------
scale_fill_manual(
  values = c(
    "Miembro" = "#011C40FF",
    "Casual" = "#035AA6FF"))

  # 6.8. Tasa de crecimiento anual Cyclistic 2020-2024---------------------------------------------------------------------
    # Filtro del data set--------------------------------------------------------- 
x <- limpia_consolidada %>%
  count(año, member_casual) %>%
  arrange(member_casual, año) %>%
  group_by(member_casual) %>%
  mutate(crecimiento = (n / lag(n) - 1) * 100) %>%
  ungroup() %>%
  filter(!is.na(crecimiento))
    # Grafico---------------------------------------------------------------------
b <- ggplot(x, aes(x = año, y = crecimiento, fill = crecimiento < 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~member_casual) + 
    # Etiquetas del grafico--------------------------------------------------------
geom_text(
  aes(label = paste0(round(crecimiento, 1), "%")),
  vjust = ifelse(x$crecimiento < 0, 1.5, -0.5),
  size = 5,
  family = "Petrona Light"
) +
    # Leyendas y notaciones------------------------------------------------------
labs(
  title = "Tasa de crecimiento anual Cyclistic",
  subtitle = "Se utilizaon los años desde el 2020-2024",
  x = NULL,
  y = "Crecimiento (%)",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = NULL
) +
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-----------------------------------------------------------------------
scale_fill_manual(
  values = c("TRUE" = "#69000C", "FALSE" = "#011C40FF"),
  guide = "none" # Elimina la leyenda
)
  # 6.9. Distancia en km recorrida por tipo de usuario 2020-2024----------------
    # Filtro del data set----------------------------------------------------------
# Calcular el promedio de distancia por tipo de usuario y año
promedio_distancia <- limpia_consolidada %>%
  group_by(member_casual, año) %>%
  summarise(
    distancia_promedio =  round(mean(distance_km),2),
    .groups = 'drop'
  )
    # Grafico----------------------------------------------------------------------
d <- ggplot(promedio_distancia, aes(x = año, y = distancia_promedio, color = member_casual)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    limits = c(1.5, max(promedio_distancia$distancia_promedio) * 1.1),
    expand = expansion(mult = c(0, 0.05))  # 5% de espacio superior, 0% en inferior
  ) +
    # Etiquetas del grafico---------------------------------------------------------
  geom_text(
    aes(label = distancia_promedio), 
    vjust = -1,                       
    size = 4,
    family = "Trirong Light",
    show.legend = FALSE
  ) +
    # Leyendas y notaciones---------------------------------------------------------
labs(
  title = "Promedio de km recorridos por tipo de usuario (2020-2024)",
  x = NULL,
  y = "Distancia promedio (km)",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  color = "Tipo de usuario"
) +
    # Tema usado--------------------------------------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-------------------------------------------------------------------------
scale_color_manual(
  values = c("Miembro" = "#011C40FF", 
             "Casual" = "#035AA6FF")
  )
# 7. Recomendaciones------------------------------------------------------------
  # 7.1. Treemap 2024, estaciones mas usadas Casuales
    # Filtro del data set---------------------------------------------------------
      # Top 10 estaciones para usuarios 'Casual'
top_casual <- limpia_2024 %>%
  filter(member_casual == "Casual",
         nzchar(start_station_name)) %>%
  count(start_station_name, sort = TRUE) %>%
  head(10) %>%
  mutate(member_casual = "Casual")
    # Grafico-----------------------------------------------------------------------
ggplot(top_casual,
       mapping = aes(area = n, fill = n, label = paste(start_station_name, "\n", n, ""))) +
  geom_treemap(
    size = 3,
    colour = "white"
  ) +
  geom_treemap_text(
    color = "white",                     # Color del texto
    place = "topright",                  # Centrar el texto
    reflow = TRUE,                       # Ajustar texto si es largo
    size = 14,                           # Tamaño del texto
    family = "Trirong Light"
  ) +
    # Leyendas y notaciones-----------------------------------------------------
labs(
  title = "Top 10, Estaciones más utilizadas por usuarios casuales en 2024",
  subtitle = NULL,
  x = NULL,
  y = NULL,
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = "Número de viajes"
) +
    # Configuracion del fondo del tema----------------------------------------
theme_minimal() +  
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 14, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-----------------------------------------------------------------------
scale_fill_gradientn(colors = c(
  "#9EC8FFFF", 
  "#207CF5FF",
  "#0E34B2FF",
  "#071140FF"
))
  # 7.2. Horas pico 2024: Miembros vs. Usuarios casuales-----------------------------------------------------------------------
    # Filtro del data set---------------------------------------------------------
limpia_2024 %>% 
  count(hora, member_casual) %>% 
  group_by(hora) %>% 
  ungroup() %>%
    # Grafico----------------------------------------------------------------------
ggplot(aes(x = hora, y = n, fill = member_casual)) +
  geom_col() +
    # Líneas verticales para resaltar horas pico (15-20)--------------------------
geom_vline(xintercept = 15, linetype = 2, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 20, linetype = 2, color = "black", linewidth = 0.5) +
  # Relleno entre las líneas
  annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax = Inf,
           alpha = 0.1, fill = "#207CF5FF") +
    # Líneas ESPECÍFICAS para Miembros (6-10)-------------------------------------
geom_vline(
  data = ~ filter(.x, member_casual == "Miembro"),  # Filtro para solo Miembros
  aes(xintercept = 6), 
  linetype = 2, 
  color = "black", 
  linewidth = 0.5
) +
  geom_vline(
    data = ~ filter(.x, member_casual == "Miembro"),
    aes(xintercept = 10), 
    linetype = 2, 
    color = "black", 
    linewidth = 0.5
  ) +
  # Relleno específico para Miembros (6-10)
  geom_rect(
    aes(xmin = 6, xmax = 10, ymin = 0, ymax = Inf),
    alpha = 0.1, fill = "red",
    data = data.frame(member_casual = "Miembro"),
    inherit.aes = FALSE
  ) +
    # facet-----------------------------------------------------------------------
facet_wrap(~member_casual) +
    # Leyendas--------------------------------------------------------------------
labs(
  title = "Horas pico 2024: Miembros vs. Usuarios casuales",
  subtitle = "En ambos grupos, las horas pico en el uso del servicio fueron entre 15 y 20 horas",
  x = "Hora",
  y = "Número de viajes",
  caption = "Fuente: Datos de Cyclistic | Autor: Diego L. Villavicencio Merino",
  fill = NULL
) +
    # Configuracion del fondo del tema----------------------------------------
theme_bw() +
  # Configuracion del fondo del tema 
  theme(
    # Quitar cuadrícula
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # Fondo blanco en facetas
    strip.background = element_rect(fill = "white"),
    # Título principal
    plot.title = element_text(size = 18, family = "Trirong Light"),
    # Subtítulo
    plot.subtitle = element_text(size = 14, family = "Petrona Light"),
    # Texto de los ejes
    axis.title = element_text(size = 13, family = "Abhaya Libre"),
    axis.text = element_text(size = 12, family = "Abhaya Libre"),
    # Etiquetas de las categorías (si las hay)
    legend.text = element_text(size = 11, family = "Abhaya Libre"),
    legend.title = element_text(size = 12, family = "Petrona Light"),
    # Pie de página (caption)
    plot.caption = element_text(size = 10, family = "Abhaya Libre"),
    plot.caption.position = "plot",
    # Texto de las facetas ("female", "male")
    strip.text = element_text(size = 14, family = "Abhaya Libre")
  ) +
    # Color-----------------------------------------------------------------------
scale_fill_manual(
  values = c(
    "Miembro" = "#011C40FF",
    "Casual" = "#035AA6FF"),
  guide = "none")

# 8. Extraer data set para powerBi------------------------------------------------
  # Eliminar variables irrelevantes
  limpia_consolidada <- limpia_consolidada %>% 
  select(
    -ride_id,
    -started_at,
    -ended_at,
    -start_station_id,
    -end_station_name,
    -end_station_id,
    -start_lat,
    -start_lng,
    -end_lat,
    -end_lng,
  )

marketing_data <- limpia_consolidada %>%
  mutate(across(where(is.character), ~na_if(trimws(.), ""))) %>%  # columnas de texto: espacios vacíos -> NA
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))   # columnas numéricas: NaN -> NA




# Notas------------------------------------------------------------------------
# En R, el símbolo " ! " es el operador de negación lógica, es decir, "NO" o "lo contrario".
