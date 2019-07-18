# Nombre Programa: 5_test_muestreo_promedio
# Ubicacion: GitHub/Tesis_RT
# Autor: Monica Flores
# Fecha Creacion: 15/03/2019
# Proyecto: Tesis Doctorado Ricardo Truffello
# Objetivo: Comparar tamaño del error zonas homogeneas vs. muestreo aleatorio simple y muestreo estratificado
# Output: 
# Notas: Esta comparacion utiliza el promedio ISMT


library(tidyverse)
library(glue)


# Definiciones iniciales --------------------------------------------------


# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# Definir floor
floor <- 10000 

# Leer datos
data <- readRDS(glue("Output/Geoda_Stgo_cluster_{floor}.Rds"))
head(data)

### Distribución total ###
prom_ismt_pers <- data$ISMT_pers %>% mean(na.rm = TRUE) # Promedio ISMT persona

# Seleccionar una muestra similar a las distribución por zona homogenea
n_abc1 <- 0.209
n_c2 <- 0.148
n_c3 <- 0.283
n_d <- 0.300
n_e <- 0.0605


# 1. DEFINICIÓN TAMAÑOS MUESTRALES ----------------------------------------


#### Definir distintos tamaños de muestra para gráfico final
#### Luego el for loop itera por cada tamaño muestral
#### 

# Muestreo aleatorio POR manzana
sample_al <- c(1, 10, 20, 30, 50, 100, 300, 500, 625, 750) # Tomando 4 personas por manzana
# Muestreo estratificado SIMPLE
sample_size <- c(1, 2, 5, 10, 20, 30, 50, 100, 150, 200) # Muestra mas chica - tomando 4 personas por manzana, por GSE
# Muestreo espacializado por zona censal 
sample_zc <- c(1, 10, 50, 100, 500, 1000, 1500, 2000) # Muestra tomando 4 personas por zona censal
# Muestreo espacializado por distrito censal 
sample_dc <- c(1, 10, 50, 100, 500, 1000, 1500, 2000) # Muestra tomando 4 personas por distrito censal
# Muestreo espacializado por barrios
sample_br <- c(1, 10, 50, 100, 500, 1000, 1500, 2000) # Muestra tomando 4 personas por barrio
# Muestreo espacializado por ZONA HOMOGENEA
sample_zh <- c(1, 10, 50, 100, 500, 1000, 1500, 2000) # Muestra tomando 4 personas por zona homogenea



# 2. DEFINICION FUNCIONES MUESTREO -------------------------------------------

### CADA FUNCIÓN AQUÍ DEFINIDA TOMA UNA MUESTRA Y LUEGO CALCULA EL ERROR 
### EL ERROR SE CALCULA SACANDO EL PROMEDIO ISMT DE LA MUESTRA Y LUEGO LA 
### DIFERENCIA PORCENTUAL CON CON EL PROMEDIO TOTAL ISMT 

# Muestreo aleatorio POR manzana ------------------------------------------

# Función para sacar % error dada una muestra aleatoria de manzanas tomando 4 personas por manzana

montecarlo_al <-  function(data) {
  
  sample_mzn <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>% 
    group_by(manzent) %>% 
    summarise(n_ = n()) %>% 
    filter(n_>=4) %>% # Filtro manzanas con más de 4 personas
    select(manzent) %>% 
    sample_n(n) # Selección muestra 96 manzanas
  
  sample_data <- data %>%
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(sample_mzn, by = "manzent") %>% 
    group_by(manzent) %>% 
    sample_n(4) %>% # 4 PERSONAS POR MANZANA
    ungroup() %>% 
    summarise(
      mean_ISMT = mean(ISMT_pers, na.rm=TRUE)
    ) %>% 
    transmute(
      pct_error = abs(mean_ISMT - prom_ismt_pers)/prom_ismt_pers
    )
  
  # Output: error
  sample_data$pct_error
}


# Muestreo estratificado --------------------------------------------------


# Función para sacar % error dado una muestra estratificada
m_estratif <-  function(data)   {
  
  # Seleccionar manzanas con menos de cuatro hogares
  mzn_excl <- data %>% 
    filter(!is.na(GSE_ISMT_mzn) & GSE_ISMT_mzn != "E") %>% # Filtrar obs sin GSE ISMT mzn o es E
    group_by(manzent) %>% 
    summarise(n_ = n()) %>% 
    filter(n_<4) %>% # en Santiago hay 46 manzanas con menos de 4 hogares
    select(manzent)
  
  # Seleccionar manzanas aleatoriamente por GSE (excepto GSE E)
  data_mzn <- data %>%  
    filter(!is.na(GSE_ISMT_mzn) & GSE_ISMT_mzn != "E") %>% # Hay solo 3 manzanas E en Temuco, 65 en Stgo
    anti_join(mzn_excl, by = "manzent") %>% # Excluir mzn con menos de 4 hogares
    select(manzent, GSE_ISMT_mzn) %>% 
    unique() %>% 
    group_by(GSE_ISMT_mzn) %>% 
    sample_n(n) %>% # Muestras por GSE (el n total es por GSE)
    ungroup() %>% 
    select(manzent)

  # Manzanas E 
  mzn_e <- data %>%
    filter(GSE_ISMT_mzn == "E")  %>%
    select(manzent) %>%
    unique() %>% 
    sample_n(if_else(n<55, n, 55)) # Hay solo 55 manzanas E. Tomar muestra con n menor a 55.
  
  # Unir Manzanas E a muestreo total
  data_mzn <- bind_rows(data_mzn, mzn_e)
  
  # Seleccionar 4 hogares aleatoriamente por manzana
  sample_estrat <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_mzn, by = "manzent") %>% 
    group_by(manzent) %>% 
    sample_n(4) %>% # 4 PERSONAS POR MANZANA
    ungroup() %>% 
    summarise(
      mean_ISMT = mean(ISMT_pers, na.rm=TRUE)
    ) %>% 
    transmute(
      pct_error = abs(mean_ISMT - prom_ismt_pers)/prom_ismt_pers
    )

  # Output: error
  sample_estrat$pct_error
}

# Muestreo espacializado por zona censal ----------------------------------

m_zc <-  function(data)   {
  
  # Construir GSE E con el 6% más bajo (No hay zonas censales E) - da 81 zonas E 
  data_zc <- data %>% 
    filter(!is.na(GSE_ISMT_zc) & !is.na(GSE_ISMT_pers)) %>% 
    select(geocode, GSE_ISMT_zc, ISMTptj_zc) %>%
    unique() %>% 
    mutate(
      rank = percent_rank(ISMTptj_zc),
      GSE_proxy = case_when(
        rank < 0.06 | GSE_ISMT_zc == "E" ~ "E",
        rank >= 0.06 & GSE_ISMT_zc == "D" ~ "D",
        TRUE ~ NA_character_)
    )
  
  # Tomar una muestra aleatoria de ZC según GSE, sin exceder el máximo de zonas censales de cada GSE
  data_abc1 <- data_zc %>% filter(GSE_ISMT_zc == "ABC1") %>% sample_n(if_else(n_abc1*n<233, as.integer(n_abc1*n), 233L)) 
  data_c2 <- data_zc %>% filter(GSE_ISMT_zc == "C2") %>% sample_n(if_else(n_c2*n<144, as.integer(n_c2*n), 144L)) 
  data_c3 <- data_zc %>% filter(GSE_ISMT_zc == "C3") %>% sample_n(if_else(n_c3*n<460, as.integer(n_c3*n), 460L)) 
  data_d <- data_zc %>% filter(GSE_proxy == "D") %>% sample_n(if_else(n_d*n<420, as.integer(n_d*n), 420L)) 
  data_e <- data_zc %>% filter(GSE_proxy == "E") %>% sample_n(if_else(n_e*n<81, as.integer(n_e*n), 81L)) 
  
  # Unir muestra grupal
  data_zc <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(geocode)
  
  # Calculo del número efectivo de zonas censales muestreadas
  n_zc <- data_zc %>% summarise(n()) %>% as.numeric()
  
  sample_zc <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_zc, by = "geocode") %>% 
    group_by(geocode) %>% 
    sample_n(1) %>% # Sacar 1 persona por manzana
    ungroup() %>% 
    summarise(
      mean_ISMT = mean(ISMT_pers, na.rm=TRUE)
    ) %>% 
    transmute(
      pct_error = abs(mean_ISMT - prom_ismt_pers)/prom_ismt_pers
    )
  
  # Output: error
  sample_zc$pct_error
}

# Muestreo espacializado por distrito ----------------------------------

m_dc <-  function(data)   {
  
  # Construir GSE E con el 6% más bajo (No hay distritos E) 
  data_dc <- data %>% 
    filter(!is.na(GSE_dc) & !is.na(GSE_ISMT_pers)) %>% 
    select(comuna, distrito, GSE_dc, ISMT_dc) %>%
    unique() %>% 
    mutate(
      rank = percent_rank(ISMT_dc),
      GSE_proxy = case_when(
        rank < 0.06 | GSE_dc == "E" ~ "E",
        rank >= 0.06 & GSE_dc == "D" ~ "D",
        TRUE ~ NA_character_)
    )  
  
  # Tomar una muestra aleatoria de distritos según GSE, sin exceder el máximo de distritos de cada GSE
  data_abc1 <- data_dc %>% filter(GSE_dc == "ABC1") %>% sample_n(if_else(n_abc1*n<37, as.integer(n_abc1*n), 37L)) 
  data_c2 <- data_dc %>% filter(GSE_dc == "C2") %>% sample_n(if_else(n_c2*n<30, as.integer(n_c2*n), 30L)) 
  data_c3 <- data_dc %>% filter(GSE_dc == "C3") %>% sample_n(if_else(n_c3*n<116, as.integer(n_c3*n), 116L)) 
  data_d <- data_dc %>% filter(GSE_proxy == "D") %>% sample_n(if_else(n_d*n<94, as.integer(n_d*n), 94L)) 
  data_e <- data_dc %>% filter(GSE_proxy == "E") %>% sample_n(if_else(n_e*n<19, as.integer(n_e*n), 19L)) 
  
  # Unir muestra GSE
  data_dc <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(comuna, distrito)
  
  # Tamaño muestra efectiva
  n_dc <- data_dc %>% summarise(n()) %>% as.numeric()
  
  # Cálculo error
  sample_dc <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_dc, by = c("comuna", "distrito")) %>% 
    group_by(geocode) %>% 
    sample_n(1) %>% # Sacar 1 persona por manzana
    ungroup() %>% 
    summarise(
      mean_ISMT = mean(ISMT_pers, na.rm=TRUE)
    ) %>% 
    transmute(
      pct_error = abs(mean_ISMT - prom_ismt_pers)/prom_ismt_pers
    )
  
  # Output: error
  sample_dc$pct_error
}

# Muestreo espacializado por barrio ----------------------------------


m_br <-  function(data)   {
  
  # Construir GSE E con el 6% más bajo - da 81 E
  data_br <- data %>% 
    filter(!is.na(GSE_barrio) & !is.na(GSE_ISMT_pers)) %>% 
    select(id_barrio, GSE_barrio, ISMT_barrio) %>%
    unique() %>% 
    # group_by(GSE_barrio) %>% summarise(n())
    mutate(
      rank = percent_rank(ISMT_barrio),
      GSE_proxy = case_when(
        rank < 0.06 | GSE_barrio == "E" ~ "E",
        rank >= 0.06 & GSE_barrio == "D" ~ "D",
        TRUE ~ NA_character_)
    ) # %>% group_by(GSE_proxy) %>% summarise(n())
  
  data_abc1 <- data_br %>% filter(GSE_barrio == "ABC1") %>% sample_n(if_else(n_abc1*n<139, as.integer(n_abc1*n), 139L)) 
  data_c2 <- data_br %>% filter(GSE_barrio == "C2") %>% sample_n(if_else(n_c2*n<96, as.integer(n_c2*n), 96L)) 
  data_c3 <- data_br %>% filter(GSE_barrio == "C3") %>% sample_n(if_else(n_c3*n<322, as.integer(n_c3*n), 322L)) 
  data_d <- data_br %>% filter(GSE_proxy == "D") %>% sample_n(if_else(n_d*n<223, as.integer(n_d*n), 223L)) 
  data_e <- data_br %>% filter(GSE_proxy == "E") %>% sample_n(if_else(n_e*n<50, as.integer(n_e*n), 50L)) 
  
  data_br <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(id_barrio)
  
  n_dc <- data_br %>% summarise(n()) %>% as.numeric()
  
  sample_dc <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_br, by = "id_barrio") %>% 
    group_by(id_barrio) %>% 
    sample_n(1) %>% # Sacar 1 persona por manzana
    ungroup() %>% 
    summarise(
      mean_ISMT = mean(ISMT_pers, na.rm=TRUE)
    ) %>% 
    transmute(
      pct_error = abs(mean_ISMT - prom_ismt_pers)/prom_ismt_pers
    )
  
  # Output: error
  sample_dc$pct_error
}

# Muestreo espacializado zonas homogeneas ---------------------------------

m_zh <-  function(data)   {
  
  data_zh <- data %>% 
    select(cluster, GSE_ISMT_zh, med_ISMT_zh) %>%
    unique() %>% 
    # group_by(GSE_ISMT_zh) %>% summarise(n())
    mutate(
      rank = percent_rank(med_ISMT_zh),
      GSE_proxy = case_when( # Construir GSE E con el 6% más bajo
        rank < 0.06 ~ "E", 
        rank >= 0.06 & GSE_ISMT_zh == "D" ~ "D",
        TRUE ~ NA_character_)
    ) # %>% group_by(GSE_proxy) %>% summarise(n())
  
  data_abc1 <- data_zh %>% filter(GSE_ISMT_zh == "ABC1") %>% sample_n(if_else(n_abc1*n<max_abc1, as.integer(n_abc1*n), max_abc1)) 
  data_c2 <- data_zh %>% filter(GSE_ISMT_zh == "C2") %>% sample_n(if_else(n_c2*n<max_c2, as.integer(n_c2*n), max_c2))
  data_c3 <- data_zh %>% filter(GSE_ISMT_zh == "C3") %>% sample_n(if_else(n_c3*n<max_c3, as.integer(n_c3*n), max_c3))
  data_d <- data_zh %>% filter(GSE_proxy == "D") %>% sample_n(if_else(n_d*n<max_d, as.integer(n_d*n), max_d)) 
  data_e <- data_zh %>% filter(GSE_proxy == "E") %>% sample_n(if_else(n_e*n<max_e, as.integer(n_e*n), max_e)) 
  
  data_zh <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(cluster)
  
  n_zh <- data_zh %>% summarise(n()) %>% as.numeric
  
  sample_zh <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_zh, by = "cluster") %>% 
    group_by(cluster) %>%
    sample_n(1) %>%
    ungroup() %>%
    summarise(
      mean_ISMT = mean(ISMT_pers, na.rm=TRUE)
    ) %>% 
    transmute(
      pct_error = abs(mean_ISMT - prom_ismt_pers)/prom_ismt_pers
    )
  
  # Output: error
  sample_zh$pct_error
  
}


# 3RA parte ---------------------------------------------------------------

### LUEGO DE DEFINIR LAS FUNCIONES PARA CADA MUESTREO Y CADA ESCALA DEL MUESTREO ESPACIALIZADO
### SE ITERA 50 VECES POR CADA UNA (MONTECARLO), PARA ESTIMAR UN ERROR PROMEDIO DE CADA MUESTREO
### PARA CADA TAMAÑO MUESTRAL DEFINIDO (EN EL PASO N°1)

# 3.1 Montecarlo para cada función ----------------------------------------

### MUESTREO ALEATORIO, ESTRATIFICADO Y ESPACIALIZADO POR GEOGRAFIAS CENSALES Y BARRIOS

# Muestreo aleatorio 
m_aleatorio <-  data.frame()

for (n in sample_al) {
  m_aleatorio_result <- replicate(50, montecarlo_al(data)) 
  error <- mean(m_aleatorio_result)
  sample <- n * 4
  tipo <- "Aleatorio"
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_aleatorio <- bind_rows(m_aleatorio, data_merge)
}


# Muestreo estratificado
m_estrat <-  data.frame()

for (n in sample_size) {
  m_estrat_result <- replicate(50, m_estratif(data)) 
  error <- mean(m_estrat_result)
  sample <- if_else(n<55, (n * 4 * 5), (n * 4 * 4) + 55)
  tipo <- "Estratificado"
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_estrat <- bind_rows(m_estrat, data_merge)
}


# Zona Censal 
m_esp_zc <-  data.frame()

for (n in sample_zc) {
  m_esp_result <- replicate(50, m_zc(data)) 
  error <- mean(m_esp_result)
  sample <- (if_else(n_abc1*n<233, as.integer(n_abc1*n), 233L)) + (if_else(n_c2*n<144, as.integer(n_c2*n), 144L)) +
    (if_else(n_c3*n<460, as.integer(n_c3*n), 460L)) + (if_else(n_d*n<420, as.integer(n_d*n), 420L)) + (if_else(n_e*n<81, as.integer(n_e*n), 81L)) 
  tipo <- "Espacializado - ZC"
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_zc <- bind_rows(m_esp_zc, data_merge)
}

# Distrito Censal 
m_esp_dc <-  data.frame()

for (n in sample_dc) {
  m_esp_result <- replicate(50, m_dc(data)) 
  error <- mean(m_esp_result)
  sample <- (if_else(n_abc1*n<37, as.integer(n_abc1*n), 37L)) + (if_else(n_c2*n<30, as.integer(n_c2*n), 30L)) +
    (if_else(n_c3*n<116, as.integer(n_c3*n), 116L)) + (if_else(n_d*n<94, as.integer(n_d*n), 94L)) + (if_else(n_e*n<19, as.integer(n_e*n), 19L)) 
  tipo <- "Espacializado - Distrito"
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_dc <- bind_rows(m_esp_dc, data_merge)
}

# Distrito Censal 
m_esp_barrio <-  data.frame()

for (n in sample_br) {
  m_esp_result <- replicate(50, m_br(data)) 
  error <- mean(m_esp_result)
  sample <- (if_else(n_abc1*n<139, as.integer(n_abc1*n), 139L)) + (if_else(n_c2*n<96, as.integer(n_c2*n), 96L)) +
    (if_else(n_c3*n<322, as.integer(n_c3*n), 322L)) + (if_else(n_d*n<223, as.integer(n_d*n), 223L)) + (if_else(n_e*n<50, as.integer(n_e*n), 50L)) 
  tipo <- "Espacializado - Barrio"
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_barrio <- bind_rows(m_esp_barrio, data_merge)
}

# 3.2 Montecarlo para función ZONAS HOMOGÉNEAS ----------------------------------------

### Itera por la zonificación de cada floor
### tomando una muestra aleatoria (50 veces) y encontrando el error promedio

### FLOOR 1500

# Definir floor y leer datos
floor <- 1500 
data <- readRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))

# Definir maximo obs (cambia según muestra)
max_abc1 <- 312L
max_c2 <- 200L
max_c3 <- 843L
max_d <- 654L
max_e <- 131L

# Zona Homogenea - floor 1500
m_esp_zh_1500 <-  data.frame()

for (n in sample_zh) {
  m_esp_result <- replicate(50, m_zh(data)) 
  error <- mean(m_esp_result)
  sample <- if_else(n_abc1*n<max_abc1, as.integer(n_abc1*n), max_abc1) + if_else(n_c2*n<max_c2, as.integer(n_c2*n), max_c2) +
    if_else(n_c3*n<max_c3, as.integer(n_c3*n), max_c3) + if_else(n_d*n<max_d, as.integer(n_d*n), max_d) +
    if_else(n_e*n<max_e, as.integer(n_e*n), max_e)
  tipo <- "Espacializado - ZH f1500" # Cambiar según floor
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_zh_1500 <- bind_rows(m_esp_zh_1500, data_merge)
}


### FLOOR 2000

# Definir floor y leer datos
floor <- 2000 
data <- readRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))

# Definir maximo obs (cambia según muestra)
max_abc1 <- 231L
max_c2 <- 144L
max_c3 <- 644L
max_d <- 493L
max_e <- 100L

# Zona Homogenea - floor 2000
m_esp_zh_2000 <-  data.frame()

for (n in sample_zh) {
  m_esp_result <- replicate(50, m_zh(data)) 
  error <- mean(m_esp_result)
  sample <- if_else(n_abc1*n<max_abc1, as.integer(n_abc1*n), max_abc1) + if_else(n_c2*n<max_c2, as.integer(n_c2*n), max_c2) +
    if_else(n_c3*n<max_c3, as.integer(n_c3*n), max_c3) + if_else(n_d*n<max_d, as.integer(n_d*n), max_d) +
    if_else(n_e*n<max_e, as.integer(n_e*n), max_e)
  tipo <- "Espacializado - ZH f2000" # Cambiar según floor
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_zh_2000 <- bind_rows(m_esp_zh_2000, data_merge)
}


### FLOOR 2500

# Definir floor y leer datos
floor <- 2500 
data <- readRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))

# Definir maximo obs (cambia según muestra)
max_abc1 <- 174L
max_c2 <- 122L
max_c3 <- 516L
max_d <- 392L
max_e <- 77L

# Zona Homogenea - floor 2500  
m_esp_zh_2500 <-  data.frame()

for (n in sample_zh) {
  m_esp_result <- replicate(50, m_zh(data)) 
  error <- mean(m_esp_result)
  sample <- if_else(n_abc1*n<max_abc1, as.integer(n_abc1*n), max_abc1) + if_else(n_c2*n<max_c2, as.integer(n_c2*n), max_c2) +
    if_else(n_c3*n<max_c3, as.integer(n_c3*n), max_c3) + if_else(n_d*n<max_d, as.integer(n_d*n), max_d) +
    if_else(n_e*n<max_e, as.integer(n_e*n), max_e)
  tipo <- "Espacializado - ZH f2500" # Cambiar según floor
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_zh_2500 <- bind_rows(m_esp_zh_2500, data_merge)
}


### FLOOR 10000

# Definir floor y leer datos
floor <- 10000 
data <- readRDS(glue("Output/Geoda_Stgo_cluster_{floor}.Rds"))

# Definir maximo obs (cambia según muestra)
max_abc1 <- 44L
max_c2 <- 31L
max_c3 <- 112L
max_d <- 98L
max_e <- 20L

# Zona Homogenea - floor 2000  
m_esp_zh_10000 <-  data.frame()

for (n in sample_zh) {
  m_esp_result <- replicate(50, m_zh(data)) 
  error <- mean(m_esp_result)
  sample <- if_else(n_abc1*n<max_abc1, as.integer(n_abc1*n), max_abc1) + if_else(n_c2*n<max_c2, as.integer(n_c2*n), max_c2) +
    if_else(n_c3*n<max_c3, as.integer(n_c3*n), max_c3) + if_else(n_d*n<max_d, as.integer(n_d*n), max_d) +
    if_else(n_e*n<max_e, as.integer(n_e*n), max_e)
  tipo <- "Espacializado - ZH f10000" # Cambiar según floor
  
  data_merge <- data.frame(sample, error, tipo)
  
  m_esp_zh_10000 <- bind_rows(m_esp_zh_10000, data_merge)
}

# 4. Grafico -----------------------------------------------------------------

### UNIR Y GRAFICAR LOS RESULTADOS DEL MONTECARLO PARA CADA TAMAÑO MUESTRAL Y CADA METODO DE MUESTREO

# Unir resultados montecarlo
muestreo <-  bind_rows(
                       m_aleatorio,
                       m_estrat,
                       m_esp_zc,
                       m_esp_dc,
                       m_esp_barrio,
                       m_esp_zh_1500, 
                       m_esp_zh_2000, 
                       m_esp_zh_2500, 
                       m_esp_zh_10000
                       ) %>% filter(!is.na(error)) %>% 
  mutate(
    tipo2 = if_else(tipo %in% c("Espacializado - ZH f1500", "Espacializado - ZH f2000", "Espacializado - ZH f2500", "Espacializado - ZH f10000"), "A", "B")
  )

# Guardar csv
muestreo %>% filter(sample<3000) %>% arrange(tipo, sample) %>% write.csv2("Montecarlo/muestreo.csv", row.names = FALSE)
test <- read.csv2("Montecarlo/muestreo.csv")

# Definir colores gráfico
paleta <- c(
            "Aleatorio" = "#FF0000", 
            "Estratificado" = "#CC0066", 
            "Espacializado - Distrito" = "#FF8C00",
            "Espacializado - Barrio" = "#FFD700",
            "Espacializado - ZC" = "firebrick1",
            "Espacializado - ZH f1500" = "#000080", 
            "Espacializado - ZH f2000" ="#800080", 
            "Espacializado - ZH f2500" = "#00CED1", 
            "Espacializado - ZH f10000" = "#008080")

# Definir etiquetas gráfico
etiquetas <- c(
  "Aleatorio" = "Random", 
  "Estratificado" = "Stratified", 
  "Espacializado - Distrito" = "Spatial - Census District",
  "Espacializado - Barrio" = "Spatial - Neighborhood",
  "Espacializado - ZC" = "Spatial - Census Zone",
  "Espacializado - ZH f1500" = "Spatial - Zoning floor 1500", 
  "Espacializado - ZH f2000" ="Spatial - Zoning floor 2000", 
  "Espacializado - ZH f2500" = "Spatial - Zoning floor 2500", 
  "Espacializado - ZH f10000" = "Spatial - Zoning floor 10000"
)


# Plotear curva de tendencia
muestreo %>% filter(sample<3000) %>% 
  ggplot(aes(sample, error, color = tipo)) + #linetype=tipo2
  # geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1), expand = c(0,0)) +
  geom_smooth(method = "loess", se = FALSE, formula = y ~ log(x), lwd=1.25) +
  labs( x = "Sample", y = "Error",
        title ="Mean error by sampling method",
        subtitle = NULL,
        color = NULL) +
  # scale_linetype_manual(values=c("solid", "dotdash")) +
  scale_color_manual(
    values = paleta,
    labels = etiquetas
  ) + 
  theme(
    panel.border = element_blank(),  # Remove panel border
    panel.grid.major = element_line(colour = "grey90"),# Remove panel grid lines
    panel.grid.minor = element_line(colour = "grey90"), # Remove panel background
    panel.background = element_blank(),# Add axis line
    axis.line = element_line(colour = "grey90"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 18),
    legend.title=element_text(size=14), 
    legend.text=element_text(size=14),
    legend.position="bottom"
  )

# Plotear valores montecarlo (valores reales en lugar de curva de tendencia)
muestreo %>% filter(sample<3000) %>% 
  ggplot(aes(sample, error, color = tipo)) +
  geom_line(lwd = 1 ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  labs( x = "Sample", y = "Error",
        title ="Mean error by sampling method",
        subtitle = NULL,
        color = "Method") 


