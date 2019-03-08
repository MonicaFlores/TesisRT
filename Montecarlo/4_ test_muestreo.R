# Nombre Programa: 4_test_muestreo
# Ubicacion: GitHub/Tesis_RT
# Autor: Monica Flores
# Fecha Creacion: 28/02/2018
# Proyecto: Tesis Doctorado Ricardo Truffello
# Objetivo: Comparar tamaño del error zonas homogeneas vs. muestreo aleatorio simple y muestreo estratificado
# Output: 
# Notas:

library(tidyverse)

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# Leer datos
data <- readRDS("Output/Data_Stgo_cluster_ismt_1.Rds")
# data <- readRDS("Output/Data_Temuco_cluster_mediana_ismt_8.Rds")
# data <- readRDS("Output/Data_Stgo_ismt.Rds") 
head(data)


# Distribución total ------------------------------------------------------

tot_hog <- data %>% filter(!is.na(GSE_ISMT_pers)) %>% summarise(n()) %>% as.numeric()

data_gse <- data %>% 
  filter(!is.na(GSE_ISMT_pers)) %>% 
  group_by(GSE_ISMT_pers) %>% 
  summarise(
    n_hog = n(),
    pct_hog = n_hog / tot_hog
  ) %>% 
  select(-n_hog)

# Temuco
# GSE_ISMT_pers n_hog pct_hog
#  ABC1          16570  0.207 
#  C2            12105  0.152 
#  C3            22652  0.284 
#  D             23722  0.297 
#  E              4846  0.0607 
  
# # Santiago   
# GSE_ISMT_pers pct_hog
#    ABC1           0.209 
#    C2             0.148 
#    C3             0.283 
#    D              0.300 
#    E              0.0605

# Muestreo aleatorio ---------------------------------------

sample_size <- 384 # 384 para error .05 población entre 100.000 - 2.000.000 https://es.slideshare.net/DanielMorales63/tamao-muestra-15307553 p12

m_aleatorio <-  function(data) {
    
  sample_data <- data %>%
    filter(!is.na(GSE_ISMT_pers)) %>%
    sample_n(sample_size) %>% 
    group_by(GSE_ISMT_pers) %>% 
    summarise(
      n_hog_s = n(),
      pct_hog_s = n_hog_s / sample_size 
    ) %>% 
    ungroup() %>% 
    left_join(data_gse, by = "GSE_ISMT_pers") %>% 
    mutate(
      pct_error =  abs(pct_hog - pct_hog_s)
    )
  
  sample_data %>% summarise(error = sum(pct_error)) %>% as.numeric()

}

# Replicar 100 veces
result <- replicate(100, m_aleatorio(data))
mean(result)
# 0.08461054 - 0.07674775 error Temuco
# 0.07829189 error Santiago


# Muestreo aleatorio por manzana ------------------------------------------

sample_al <- 96 # 384/4 
sample_mz <- 4

m_aleatorio_mzn <-  function(data) {
  
  sample_mzn <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>% 
    group_by(manzent) %>% 
    summarise(n = n()) %>% 
    filter(n>=4) %>% # Filtro manzanas con más de 4 personas
    select(manzent) %>% 
    sample_n(sample_al) # Selección muestra 96 manzanas
  
  sample_data <- data %>%
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(sample_mzn, by = "manzent") %>% 
    group_by(manzent) %>% 
    sample_n(sample_mz) %>% 
    ungroup() %>% 
    group_by(GSE_ISMT_pers) %>% 
    summarise(
      n_hog_s = n(),
      pct_hog_s = n_hog_s/384 
    ) %>% 
    left_join(data_gse, by = "GSE_ISMT_pers") %>% 
    mutate(
      pct_error =  abs(pct_hog - pct_hog_s)
    )
  
  sample_data %>% summarise(error = sum(pct_error)) %>% as.numeric()
  
}

# Replicar 100 veces
result <- replicate(100, m_aleatorio_mzn(data))
mean(result)
# 0.1777729 Santiago
# 0.1046598 Temuco

# Seleccionar muestra estratificada por manzana ---------------------------------------

sample_estr <- 96 # 384/4 = 96 - tomando 4 personas por manzana
sample_manz <- 4 # Personas por manzana

m_estratif <-  function(data)   {
  
  # Seleccionar manzanas con menos de cuatro hogares
  mzn_excl <- data %>% 
    filter(!is.na(GSE_ISMT_mzn) & GSE_ISMT_mzn != "E") %>% # Filtrar obs sin GSE ISMT mzn o es E
    group_by(manzent) %>% 
    summarise(n = n()) %>% 
    filter(n<4) %>% # en Santiago hay 46 manzanas con menos de 4 hogares
    select(manzent)
  
  # Seleccionar manzanas aleatoriamente por GSE
  data_mzn <- data %>%  
    filter(!is.na(GSE_ISMT_mzn) & GSE_ISMT_mzn != "E") %>% # Hay solo 3 manzanas E en Temuco, 65 en Stgo
    anti_join(mzn_excl, by = "manzent") %>% # Excluir mzn con menos de 4 hogares
    select(manzent, GSE_ISMT_mzn) %>% 
    unique() %>% 
    group_by(GSE_ISMT_mzn) %>% 
    sample_n(sample_estr) %>% 
    ungroup() %>% 
    select(manzent)
  
  # Manzanas E (todas tienen 4 o más hogares)
  mzn_e <- data %>%
    filter(GSE_ISMT_mzn == "E")  %>%
    select(manzent) %>%
    unique()

  # Unir Manzanas E
  data_mzn <- bind_rows(data_mzn, mzn_e)
  
  # Calcular total muestra manzanas
  tot_mzn <- data_mzn %>% summarise(n()) %>% as.numeric()
  
  # Seleccionar 4 hogares aleatoriamente por manzana
  sample_estrat <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_mzn, by = "manzent") %>% 
    group_by(manzent) %>% 
    # summarise(n()) %>% 
    sample_n(sample_manz) %>% 
    ungroup() %>% 
    group_by(GSE_ISMT_pers) %>% 
    summarise(
      n_hog_s = n(),
      pct_hog_s = n_hog_s / (tot_mzn * sample_manz) # Total hogares por grupo sobre el total de obs
    ) %>% 
    ungroup() %>% 
    left_join(data_gse, by = "GSE_ISMT_pers") %>% 
    mutate(
      pct_error =  abs(pct_hog - pct_hog_s)
    )
    
  sample_estrat %>% summarise(error = sum(pct_error)) %>% as.numeric()
}

# Replicar 100 veces
result_estr <- replicate(100, m_estratif(data))
mean(result_estr)
# Temuco : 0.1853136 - 0.1881098
# Santiago: 0.1979878 - 0.1941693


# Muestreo espacializado por zona censal ----------------------------------
# No es estratificado, al menos en Temuco, porque se usan las 86 zonas censales

# Seleccionar una muestra similar a las distribución por zona homogenea
n_abc1 <- 80
n_c2 <- 57
n_c3 <- 109
n_d <- 115
n_e <- 23

m_zc <-  function(data)   {

    # Temuco tiene 86 zonas Censales, usar todas
  # data_zc <- data %>% 
  #     select(geocode) %>%
  #     unique()
  
  data_zc <- data %>% 
    filter(!is.na(GSE_ISMT_zc) & !is.na(GSE_ISMT_pers)) %>% 
    select(geocode, GSE_ISMT_zc, ISMTptj_zc) %>%
    unique() %>% 
    # Construir GSE E con el 6% más bajo
    mutate(
      rank = percent_rank(ISMTptj_zc),
      GSE_proxy = case_when(
        rank < 0.06 ~ "E",
        rank >= 0.06 & GSE_ISMT_zc == "D" ~ "D",
        # rank >= 0.06 & rank < 0.36 ~ "D",
        # rank >= 0.36 & rank < 0.64 ~ "C3",
        # rank >= 0.64 & rank < 0.79 ~ "C2",
        # rank >= 0.79 ~ "ABC1",
        TRUE ~ NA_character_)
    )
  
  data_abc1 <- data_zc %>% filter(GSE_ISMT_zc == "ABC1") %>% sample_n(n_abc1)
  data_c2 <- data_zc %>% filter(GSE_ISMT_zc == "C2") %>% sample_n(n_c2)
  data_c3 <- data_zc %>% filter(GSE_ISMT_zc == "C3") %>% sample_n(n_c3)
  data_d <- data_zc %>% filter(GSE_proxy == "D") %>% sample_n(n_d)
  data_e <- data_zc %>% filter(GSE_proxy == "E") %>% sample_n(n_e)
  
  data_zc <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(geocode)
  
  n_zc <- data_zc %>% summarise(n()) %>% as.numeric
  
  sample_zc <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_zc, by = "geocode") %>% 
    group_by(geocode) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    group_by(GSE_ISMT_pers) %>% 
    summarise(
      n_hog_s = n(),
      pct_hog_s = n_hog_s/n_zc
    ) %>% 
    ungroup() %>% 
    left_join(data_gse, by = "GSE_ISMT_pers") %>% 
    mutate(
      pct_error =  abs(pct_hog - pct_hog_s)
    )
  
  sample_zc %>% summarise(error = sum(pct_error)) %>% as.numeric()
}

# Replicar 100 veces
result_zc <- replicate(100, m_zc(data))
mean(result_zc)
# Temuco ---- 0.07930511 - 86 zonas censales, 4 personas por zona 
# Santiago -- 0.05152551 - 1539 zonas censales 1 persona por zona censal
# Santiago - 0.08313031 - 0.08908066

# Muestreo espacializado zonas homogeneas ---------------------------------

# Seleccionar una muestra similar a las distribución por zona homogenea
n_abc1 <- 80
n_c2 <- 57
n_c3 <- 109
n_d <- 115
n_e <- 23

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
        # rank >= 0.06 & rank < 0.36 ~ "D",
        # rank >= 0.36 & rank < 0.64 ~ "C3",
        # rank >= 0.64 & rank < 0.79 ~ "C2",
        # rank >= 0.79 ~ "ABC1",
        TRUE ~ NA_character_)
    ) 

  data_abc1 <- data_zh %>% filter(GSE_ISMT_zh == "ABC1") %>% sample_n(n_abc1)
  data_c2 <- data_zh %>% filter(GSE_ISMT_zh == "C2") %>% sample_n(n_c2)
  data_c3 <- data_zh %>% filter(GSE_ISMT_zh == "C3") %>% sample_n(n_c3)
  data_d <- data_zh %>% filter(GSE_proxy == "D") %>% sample_n(n_d)
  data_e <- data_zh %>% filter(GSE_proxy == "E") %>% sample_n(n_e)
  
  data_zh <- rbind(data_abc1, data_c2, data_c3, data_d, data_e) %>% select(cluster)
  
  n_zh <- data_zh %>% summarise(n()) %>% as.numeric
  
  sample_zh <- data %>% 
    filter(!is.na(GSE_ISMT_pers)) %>%
    inner_join(data_zh, by = "cluster") %>% 
    group_by(cluster) %>%
    sample_n(1) %>%
    ungroup() %>%
    group_by(GSE_ISMT_pers) %>% 
    summarise(
      n_hog_s = n(),
      pct_hog_s = n_hog_s/n_zh
    ) %>% 
    ungroup() %>% 
    left_join(data_gse, by = "GSE_ISMT_pers") %>% 
    mutate(
      pct_error =  abs(pct_hog - pct_hog_s)
    )
  
  sample_zh %>% summarise(error = sum(pct_error)) %>% as.numeric()
  
}

# Replicar 100 veces
result_zh <- replicate(100, m_zh(data))
mean(result_zh)

# Temuco 
# 0.06726904 con 4 personas por zona 
# 0.08768771 - 0.09112519 con 2 persoanas - total 344 muestra

# Santiago
# 0.0938711 seleccion aleatoria por GSE zona homogenea con distribución parecida a la de la población
# 0.130898 Solo con proxy GSE 