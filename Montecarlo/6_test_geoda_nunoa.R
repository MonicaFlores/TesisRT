# Comparación varianza output geoda

library(tidyverse)
library(glue)

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# # Definir floor
# floor <- 1500 
# # Leer datos
# data <- readRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))
data <- readRDS("Output/Geoda_Stgo_clusters.Rds")
head(data)

test_7k <- data %>% group_by(clust_7k, GSE_ISMT_zh_7k) %>% 
  summarise(
    var_7k = var(ISMT_pers, na.rm = TRUE)
  )

var_prom_7k <- mean(test_7k$var_7k, na.rm = TRUE)
var_sum_7k <- sum(test_7k$var_7k, na.rm = TRUE)

test_10k <- data %>% group_by(clust_10k, GSE_ISMT_zh_10k) %>% 
  summarise(
    var_10k = var(ISMT_pers, na.rm = TRUE)
  )

var_prom_10k <- mean(test_10k$var_10k, na.rm = TRUE)
var_sum_10k <- sum(test_10k$var_10k, na.rm = TRUE)

test_15k <- data %>% group_by(clust_15k, GSE_ISMT_zh_15k) %>% 
  summarise(
    var_15k = var(ISMT_pers, na.rm = TRUE)
  )

var_prom_15k <- mean(test_15k$var_15k, na.rm = TRUE)
var_sum_15k <- sum(test_15k$var_15k, na.rm = TRUE)


data %>% group_by(GSE_ISMT_zc) %>% summarise(n()) # No hay E
data %>% group_by(GSE_ISMT_zh_7k) %>% summarise(n()) # Solo hay ABC1, C2, C3
data %>% group_by(GSE_ISMT_zh_10k) %>% summarise(n()) # Solo hay ABC1, C2, C3
data %>% group_by(GSE_ISMT_zh_15k) %>% summarise(n()) # Solo hay ABC1, C3
