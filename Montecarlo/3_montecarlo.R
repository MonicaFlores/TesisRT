# Nombre Programa: 3_montecarlo
# Ubicacion: GitHub/Tesis_RT
# Autor: Monica Flores
# Fecha Creacion: 08/02/2018
# Proyecto: Tesis Doctorado Ricardo Truffello
# Objetivo: Preparar datos prueba script max_p y montecarlo
# Output: 
# Notas:

#install.packages("MonteCarlo")

library(sf)
library(tidyverse)
library(MonteCarlo)

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# Leer datos con GSE por manzana
mzn_gse <- readRDS("Output/Data_Temuco_Montecarlo.Rds")

# Leer datos de vecindad max_p
#### vecindad <- st_read()

# Unir datos manzana gse y vecindad 
mzn_mont <- mzn_gse %>%  left_join(vecindad, by="manzent")


# Montecarlo --------------------------------------------------------------

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


# Funcion resumida
ttest<-function(n,loc,scale){
  
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  return(list("stat"=stat))
}

# Funcion MonteCarlo
MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)

df<-MakeFrame(MC_result)
head(df)


#  plot estimated densities for the distribution of the t-test with different sample sizes n.

library(dplyr)
library(ggplot2)
tbl <- tbl_df(df)
ggplot(filter(tbl, loc==0, scale==1)) + geom_density(aes(x=stat, col=factor(n)))


##### Otro ejemplo 
##### compare the mean() and the median() as estimators for the expected value of a Gaussian random variable.

# Definir la función
mean_vs_median<-function(n,scale){
  
  # generate sample
  sample<-rnorm(n, 0, scale)
  
  # calculate estimators
  mean_sample<-mean(sample)
  median_sample<-median(sample)
  
  # return results
  return(list("mean"=mean_sample, "median"=median_sample))
}

n_grid<-c(50, 250, 500)
scale_grid<-c(1, 2, 4)

param_list=list("n"=n_grid, "scale"=scale_grid)

# run simulation:
erg_mean_median<-MonteCarlo(func=mean_vs_median, nrep=1000, param_list=param_list)

# Tabla
MakeTable(output=erg_mean_median, rows="n", cols="scale", digits=2, include_meta=FALSE)

