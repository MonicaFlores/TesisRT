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
setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
# setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# Leer datos con GSE por manzana
#gse_mont <- readRDS("Output/Data_Temuco_Montecarlo.Rds") %>% filter(parentesco==1) #filtrar sólo jefes de hogar
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_mediana.Rds") %>% filter(parentesco==1) 
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_ismt.Rds") %>% filter(parentesco==1) 
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_ismt_mediana.Rds") %>% filter(parentesco==1)
# gse_mont <- readRDS("Output/Data_Temuco_Montecarlo_cluster.Rds") %>% filter(parentesco==1)
gse_mont <- readRDS("Output/Data_Temuco_cluster_mediana_educ_4.Rds") %>% filter(parentesco==1)
head(gse_mont)

# Calcular probabilidad de coincidir con  GSE Manzana y GSE Zona Censal---------

# Definir función detectar si GSE persona y zona geográfica son iguales
test <- function(pers, zona){
  decision <- pers == zona  # get test decision
  return(list("decision"=decision)) # return result:Output of TRUEs and FALSEs
}

## Prueba Zona 
test1 <- test(gse_mont$GSE_pers, gse_mont$GSE_zh) %>%
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0))

test1 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
# 0.3560929 probabilidad zona (promedio)
# 0.3336777 probabilidad zona (mediana)
# 0.3798036 probabilidad zona (mediana)  - fitro mzn islas
# 0.3867884 probabilidad zona ISMT (mediana)
# 0.385037 probabilidad zona ISMT (mediana) - fitro mzn islas

# 0.2585055 probabilidad zona homogenea 3 (mediana EDUC) - fitro mzn islas - seed 100/floor 1600
# 0.271439 probabilidad zona homogenea 4 (mediana EDUC) - fitro mzn islas - seed 50/floor 1000

## Prueba Zona/ manzana
test2 <- test(gse_mont$GSE_mzn, gse_mont$GSE_zona) %>%
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0))

test2 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
# 0.4122869 probabilidad coincidir con GSE manzana / zona homogenea 4 
# 0.6640931 probabilidad coincidir con GSE manzana / zona censal  

# Prueba con muestra de 1000 - Zona Censal
test_zc_sample <- test1 %>% sample_n(1000) %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
# ~0.344 probabilidad zona (mediana) - sample_n(1000)


## Prueba manzanas
test3 <- test(gse_mont$GSE_ISMT_pers, gse_mont$GSE_ISMT_mzn) %>% 
  as.data.frame() %>% 
  mutate(dec_num = if_else(decision == TRUE, 1, 0)) 

test3 %>% summarise(prob_zona = mean(dec_num, na.rm=TRUE)) 
# 0.3841399 probabilidad mzn (promedio)
# 0.4184666 probablidad mzn (mediana)
# 0.4749272 probablidad mzn ISMT (mediana)
# 0.4866726 probablidad mzn ISMT (mediana) - fitro mzn islas

# Prueba con muestra de 1000 - manzana
test2_result <- test2 %>% sample_n(1000) %>% summarise(prob_mzn = mean(dec_num, na.rm=TRUE))  
# ~0.437 probablidad mzn (mediana) - sample_n(1000)



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



# Ejemplos uso paquete MonteCarlo -----------------------------------------



# Ejemplo Montecarlo con base R 
# http://condor.depaul.edu/pgomez1/WNPL/R_code_files/rworkshop13.pdf

# Paquete MonteCarlo
# https://cran.r-project.org/web/packages/MonteCarlo/vignettes/MonteCarlo-Vignette.html

##### Necesitamos probar si efectivamente estamos minimizando la varianza

######################### Ejemplo ################################

# Define function that generates data and applies the method of interest
ttest<-function(n,loc,scale){

  # generate sample:
  sample<-rnorm(n, loc, scale)

  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)

  # get test decision:
  decision<-abs(stat)>1.96

  # return result:
  return(list("decision"=decision))
}

# # Funcion resumida
# ttest<-function(n,loc,scale){
#   
#   sample<-rnorm(n, loc, scale)
#   stat<-sqrt(n)*mean(sample)/sd(sample)
#   return(list("stat"=stat))
# }

# define parameter grid:

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

# collect parameter grids in list:
param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)

# Funcion MonteCarlo
MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)

df<-MakeFrame(MC_result)
head(df)


##  plot estimated densities for the distribution of the t-test with different sample sizes n.

# library(dplyr)
# library(ggplot2)
# tbl <- tbl_df(df)
# ggplot(filter(tbl, loc==0, scale==1)) + geom_density(aes(x=stat, col=factor(n)))


##### Otro ejemplo 
##### compare the mean() and the median() as estimators for the expected value of a Gaussian random variable.

# # Definir la función
# mean_vs_median<-function(n,scale){
#   
#   # generate sample
#   sample<-rnorm(n, 0, scale)
#   
#   # calculate estimators
#   mean_sample<-mean(sample)
#   median_sample<-median(sample)
#   
#   # return results
#   return(list("mean"=mean_sample, "median"=median_sample))
# }
# 
# n_grid<-c(50, 250, 500)
# scale_grid<-c(1, 2, 4)
# 
# param_list=list("n"=n_grid, "scale"=scale_grid)
# 
# # run simulation:
# erg_mean_median<-MonteCarlo(func=mean_vs_median, nrep=1000, param_list=param_list)
# 
# # Tabla
# MakeTable(output=erg_mean_median, rows="n", cols="scale", digits=2, include_meta=FALSE)
# 

# Prueba test montecarlo
GSEpersona <- gse_mont$GSE_pers
GSEzona <- gse_mont$GSE_zona

# collect parameter grids in list:
param_list <- list("pers"=GSEpersona, "zona"=GSEzona)

# Funcion MonteCarlo
MC_result<-MonteCarlo(func=test, nrep=100, param_list=param_list, max_grid = 1000) # not working
# Error in MonteCarlo(func = test, nrep = 100, param_list = param_list,  : 
#                       Grid size is very large. If you still want to run the simulation change max_grid.