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

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# Definir floor
floor <- 1500 
# Leer datos
data <- readRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))
head(data)


# Distribución total ------------------------------------------------------

# Promedio ISMT persona
prom_ismt_pers <- data$ISMT_pers %>% mean(na.rm = TRUE)

# Muestreo aleatorio POR manzana ------------------------------------------

# sample_al <- 96 # 384/4 
sample_al <- c(1, 10, 20, 30, 50, 100, 300, 500, 625, 750)
sample_mz <- 4

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
    sample_n(sample_mz) %>% # 4
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

# sample_estr <- seq(1, 750, 10) # Muestra mas chica - tomando 4 personas por manzana
sample_size <- c(1, 2, 5, 10, 20, 30, 50, 100, 150, 200) # Muestra mas chica - tomando 4 personas por manzana, por grupo
sample_manz <- 4 # Personas por manzana

# Función para sacar % error dado una muestra estratificada
m_estratif <-  function(data)   {
  
  # Seleccionar manzanas con menos de cuatro hogares
  mzn_excl <- data %>% 
    filter(!is.na(GSE_ISMT_mzn) & GSE_ISMT_mzn != "E") %>% # Filtrar obs sin GSE ISMT mzn o es E
    group_by(manzent) %>% 
    summarise(n_ = n()) %>% 
    filter(n_<4) %>% # en Santiago hay 46 manzanas con menos de 4 hogares
    select(manzent)
  
  # Seleccionar manzanas aleatoriamente por GSE
  data_mzn <- data %>%  
    filter(!is.na(GSE_ISMT_mzn) & GSE_ISMT_mzn != "E") %>% # Hay solo 3 manzanas E en Temuco, 65 en Stgo
    anti_join(mzn_excl, by = "manzent") %>% # Excluir mzn con menos de 4 hogares
    select(manzent, GSE_ISMT_mzn) %>% 
    unique() %>% 
    group_by(GSE_ISMT_mzn) %>% 
    sample_n(n) %>% # Muestras por GSE (el n total es por GSE)
    ungroup() %>% 
    select(manzent)

  # Manzanas E (todas tienen 4 o más hogares)
  mzn_e <- data %>%
    filter(GSE_ISMT_mzn == "E")  %>%
    select(manzent) %>%
    unique() %>% 
    sample_n(if_else(n<55, n, 55)) # Hay solo 55 manzanas E. Sample aleatorio para n menor a 50.
  
  # Unir Manzanas E
  data_mzn <- bind_rows(data_mzn, mzn_e)
  
  # Seleccionar 4 hogares aleatoriamente por manzana
  sample_estrat <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_mzn, by = "manzent") %>% 
    group_by(manzent) %>% 
    sample_n(sample_manz) %>% # 4
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

sample_zc <- c(1, 10, 50, 100, 500, 1000, 1500, 2000) # Muestra tomando 4 personas por zona censal

# Seleccionar una muestra similar a las distribución por zona homogenea
n_abc1 <- 0.209
n_c2 <- 0.148
n_c3 <- 0.283
n_d <- 0.300
n_e <- 0.0605

m_zc <-  function(data)   {
  
  # Construir GSE E con el 6% más bajo - da 81 E
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
  
  data_abc1 <- data_zc %>% filter(GSE_ISMT_zc == "ABC1") %>% sample_n(if_else(n_abc1*n<233, as.integer(n_abc1*n), 233L)) 
  data_c2 <- data_zc %>% filter(GSE_ISMT_zc == "C2") %>% sample_n(if_else(n_c2*n<144, as.integer(n_c2*n), 144L)) 
  data_c3 <- data_zc %>% filter(GSE_ISMT_zc == "C3") %>% sample_n(if_else(n_c3*n<460, as.integer(n_c3*n), 460L)) 
  data_d <- data_zc %>% filter(GSE_proxy == "D") %>% sample_n(if_else(n_d*n<420, as.integer(n_d*n), 420L)) 
  data_e <- data_zc %>% filter(GSE_proxy == "E") %>% sample_n(if_else(n_e*n<81, as.integer(n_e*n), 81L)) 
  
  data_zc <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(geocode)
  
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



# Muestreo espacializado zonas homogeneas ---------------------------------

sample_zh <- c(1, 10, 50, 100, 500, 1000, 1500, 2000) # Muestra tomando 4 personas por zona homogenea

# Seleccionar una muestra similar a las distribución por zona homogenea
n_abc1 <- 0.209
n_c2 <- 0.148
n_c3 <- 0.283
n_d <- 0.300
n_e <- 0.0605

m_zh <-  function(data)   {
  
  data_zh <- data %>% 
    select(cluster, GSE_ISMT_zh, med_ISMT_zh) %>%
    unique() %>% 
    # Construir GSE E con el 6% más bajo
    mutate(
      rank = percent_rank(med_ISMT_zh),
      GSE_proxy = case_when(
        rank < 0.06 ~ "E", 
        rank >= 0.06 & GSE_ISMT_zh == "D" ~ "D",
        TRUE ~ NA_character_)
    ) 
  
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


# Montecarlo para cada función --------------------------------------------

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

# Definir floor y leer datos
floor <- 2500 
data <- readRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))

# Definir maximo obs (cambia según muestra)
max_abc1 <- 174L
max_c2 <- 122L
max_c3 <- 516L
max_d <- 392L
max_e <- 77L

# Zona Homogenea - floor 2000  
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

# Grafico -----------------------------------------------------------------

muestreo <-  bind_rows(m_aleatorio, m_estrat, m_esp_zc, m_esp_zh_1500, m_esp_zh_2000, m_esp_zh_2500) %>% filter(!is.na(error))

# Guardar csv
muestreo %>% filter(sample<3000) %>% arrange(tipo, sample) %>% write.csv2("Montecarlo/muestreo.csv", row.names = FALSE)

# Plotear curva de tendencia
muestreo %>% filter(sample<3000) %>% 
  ggplot(aes(sample, error, color = tipo)) +
  # geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_smooth(method = "loess", se = FALSE, formula = y ~ log(x)) +
  labs( x = "Muestra", y = "Error",
        title ="Error promedio por método de muestreo",
        subtitle = NULL,
        color = "Muestreo") 

# Plotear valores montecarlo
muestreo %>% filter(sample<3000) %>% 
  ggplot(aes(sample, error, color = tipo)) +
  geom_line(lwd = 1 ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  #geom_smooth(method = "loess", se = FALSE, formula = y ~ log(x)) +
  labs( x = "Muestra", y = "Error",
        title ="Error promedio por método de muestreo",
        subtitle = NULL,
        color = "Muestreo") 


