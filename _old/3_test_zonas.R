# Nombre Programa: 3_montecarlo
# Ubicacion: GitHub/Tesis_RT
# Autor: Monica Flores
# Fecha Creacion: 08/02/2018
# Proyecto: Tesis Doctorado Ricardo Truffello
# Objetivo: Preparar datos prueba script max_p y montecarlo
# Output: 
# Notas:

# install.packages("MonteCarlo")

library(tidyverse)
library(MonteCarlo)

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# Leer datos con GSE por manzana
#gse_mont <- readRDS("Output/Data_Temuco_Montecarlo.Rds") %>% filter(parentesco==1) #filtrar sólo jefes de hogar
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_mediana.Rds") %>% filter(parentesco==1) 
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_ismt.Rds") %>% filter(parentesco==1) 
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_ismt_mediana.Rds") %>% filter(parentesco==1)
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_cluster.Rds") %>% filter(parentesco==1)
# gse_mont <- readRDS("Output/Data_Temuco_cluster_mediana_ismt_8.Rds") %>% filter(parentesco==1)
gse_mont <- readRDS("Output/Data_Nunoa_cluster_ismt_1.Rds") %>% filter(parentesco==1)
head(gse_mont)

# Calcular probabilidad de coincidir con  GSE Manzana y GSE Zona Censal---------

# Definir función detectar si GSE persona y zona geográfica son iguales
test <- function(pers, zona){
  decision <- pers == zona  # get test decision
  return(list("decision"=decision)) # return result:Output of TRUEs and FALSEs
}

## Prueba Zona Censal
test0 <- test(gse_mont$GSE_ISMT_pers, gse_mont$GSE_ISMT_zc) %>%
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0))

test0 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 

# Temuco
# 0.3560929 probabilidad zona (promedio)
# 0.3336777 probabilidad zona (mediana)
# 0.3798036 probabilidad zona (mediana)  - fitro mzn islas
# 0.3867884 probabilidad zona ISMT (mediana)
# 0.385037 probabilidad zona ISMT (mediana) - fitro mzn islas
# Nunoa
# 0.4794668 probabilidad zona ISMT 
# 0.4086112 probabilidad zona EDUC

## Prueba Zona Homogenea
test1 <- test(gse_mont$GSE_ISMT_pers, gse_mont$GSE_ISMT_zh) %>%
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0))

test1 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 

# 0.2585055 probabilidad zona homogenea 3 (mediana EDUC) - fitro mzn islas - seed 100/floor 1600
# 0.271439 probabilidad zona homogenea 4 (mediana EDUC) - fitro mzn islas - seed 50 / floor 1000
# 0.3221418 probabilidad zona homogenea 5 (mediana ISMT) - seed 100 / floor 1000
# 0.3175821 probabilidad zona homogenea 6 (mediana ISMT) - seed 100 / floor 1500
# 0.3148007 probabilidad zona homogenea 7 (random ISMT) - seed 100 / floor 1500
# 0.3226485 probabilidad zona homogenea 7 (random ISMT) - seed 100 / floor 1000

# Ñuñoa
# 0.4862735 probabilidad zona homogenea 1 (mediana ISMT) - seed 100 / floor 1000 - 117 zonas
# 0.4751366 probabilidad zona homogenea 2 (mediana ISMT) - seed 100 / floor 1500 - 81 zonas

# Medir varianza cluster
gse_mont %>% 
  mutate(
    ISMT = as.numeric(scale(ptje_ISMT))
  ) %>% 
  group_by(cluster) %>% 
  summarise(
    sd_clust = sd(ISMT, na.rm = TRUE)
  ) %>% ungroup() %>% 
  summarise(
    max_sd = max(sd_clust),
    min_sd = min(sd_clust),
    mean_sd = mean(sd_clust),
    med_sd = median(sd_clust)
  )
#- max_sd min_sd mean_sd med_sd
#  1.11   0.597   0.932  0.945
# Nunoa - 1
#   1.47  0.573   0.911  0.868

# medir varianza zona censal  
gse_mont %>% 
  mutate(
    ISMT = as.numeric(scale(ptje_ISMT))
  ) %>% 
  group_by(geocode) %>% 
  summarise(
    sd_clust = sd(ISMT, na.rm = TRUE)
  ) %>% ungroup() %>% 
  summarise(
    max_sd = max(sd_clust),
    min_sd = min(sd_clust),
    mean_sd = mean(sd_clust),
    med_sd = median(sd_clust)
  )
# max_sd min_sd mean_sd med_sd
#  1.09  0.571   0.835  0.862
# Nunoa
#  1.29  0.690   0.899  0.861


## Prueba Zona/ manzana
test2 <- test(gse_mont$GSE_ISMT_mzn, gse_mont$GSE_zh) %>%
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0))

test2 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
# 0.4122869 probabilidad coincidir con GSE manzana / zona homogenea 4 
# 0.6640931 probabilidad coincidir con GSE manzana / zona censal  
# Nunoa
# 0.8883214 probabilidad coincidir con GSE manzana / zona censal educ  
# 0.7658846 probabilidad coincidir con GSE manzana / zona censal ISMT
# 0.7569836 probabilidad coincidir con GSE manzana / zona homogenea ISMT

## Prueba manzanas
test3 <- test(gse_mont$GSE_ISMT_pers, gse_mont$GSE_ISMT_mzn) %>% 
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0)) 

test3 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
# 0.3841399 probabilidad mzn (promedio)
# 0.4184666 probablidad mzn (mediana)
# 0.4749272 probablidad mzn ISMT (mediana)
# 0.4866726 probablidad mzn ISMT (mediana) - fitro mzn islas
# Nunoa
# 0.5118445 probabilidad mzn ISMT
# 0.4237231 probabilidad mzn/pers



# Prueba montecarlo zonas homogeneas --------------------------------------

pruebas <- c(1:100)

# for (n in pruebas) {

result <- map_dbl(pruebas, function(n) {
  # Leer datos de vecindad max_p
  vecindad <- st_read(glue("{dir_loc}/Vecindad/GSE_maxp_prueba{n}.shp")) 
  
  ## Prueba correspondencia GSE: zona homognea / hogar
  test3 <- test(vecindad$GSE_pers, vecindad$GSE_maxp) %>%
    as.data.frame() %>% 
    mutate(dec_num = if_else(decision == TRUE, 1, 0))
  
  test3 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
  
})


# promedio resultado
mean(result)

# plotear resultado
ggplot(result) + geom_density(aes(x=prob_zona))

