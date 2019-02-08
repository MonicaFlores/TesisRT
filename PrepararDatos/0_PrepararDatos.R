# Nombre Programa: 0_PrepararDatos
# Ubicacion: GitHub/Tesis_RT
# Autor: Monica Flores
# Fecha Creacion: 07/02/2018
# Proyecto: Tesis Doctorado Ricardo Truffello
# Objetivo: Preparar datos prueba script max_p y montecarlo
# Output: 
# Notas:

library(sf)
library(tidyverse)

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# leer base censal R09
censo2012_R09 <- readRDS("Censo2012_R09.Rds") %>% filter(year == 2012 & region == 9)
head(censo2012_R09)

# Leer shape manzanas y arreglar manzent
shp_mz <- st_read("Input/mzn_AUC_temuco.shp") %>% 
  mutate(
    MANZENT = (CUT*1000000000) + (DISTRITO*10000000) + (1*1000000) + (ZONA*1000) + MANZANA, # Rehacer Manzent
    geocode = (CUT*1000000) + (DISTRITO*10000) + (1*1000) + (ZONA)  # Crear codigo zona
  )

# Leer shape zonas y arreglar geocode
shp_zc <- st_read("Input/zona_AUC_temuco2012.shp") %>% 
  mutate(
    geocode = (CUT*1000000) + (COD_DISTRI*10000) + (1*1000) + (COD_ZONA) # Crear codigo zona
  )

# Guardar shape zona 
shp_zc %>% st_write("Output/Shape/zona_temuco_clean.shp", quiet = TRUE, delete_layer = TRUE)

# Data para max_p ---------------------------------------------------------



# Preparar datos manzana para script max_p
R09_mzn_data <- censo2012_R09 %>% 
  mutate(persona=1) %>% 
  group_by(manzent) %>%
  summarise(
    POB = sum(persona, na.rm = TRUE),
    EDUC = mean(if_else(parentesco ==1, escolaridad, NA_integer_), na.rm = TRUE) #Promedio de años escolaridad jefe de hogar por manzana
  )
  
# Guardar shape para vecindad max_p
shp_mz_vec <- shp_mz %>% left_join(R09_mzn_data, by = c("MANZENT" = "manzent"))

# Prueba: Plotear EDUC
ggplot() + geom_sf(data=shp_mz_vec, aes(fill=-EDUC))

# Guardar shape
shp_mz_vec %>% st_write("Output/Shape/mzn_temuco_clean.shp", quiet = TRUE, delete_layer = TRUE)
# test <- st_read("Output/Shape/mzn_temuco_clean.shp")


# Data para montecarlo-----------------------------------------



#Filtrar por manzanas dentro de temuco
mzn_temuco <- shp_mz %>% st_set_geometry(NULL) %>% transmute(manzent = MANZENT)
c2012_temuco <- censo2012_R09 %>% inner_join(mzn_temuco, by = "manzent")

# Calcular centiles escolaridad basado en el promedio por manzana
quant_temuco <- quantile(R09_mzn_data$EDUC, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
rownames(quant_temuco)

# Determinar tope centil GSE x ciudad 
E <- quant_temuco["6%",]
D <- quant_temuco["36%",]
C3 <- quant_temuco["64%",]
C2 <- quant_temuco["79%",]
ABC1 <- quant_temuco["100%",]

# seleccionar base final
mzn_mont <- c2012_temuco %>% 
  filter(parentesco==1) %>% #filtrar sólo jefes de hogar
  mutate(
    GSE = case_when(
      escolaridad <= E ~ "E",
      escolaridad > E & escolaridad <= D ~ "D",
      escolaridad > D & escolaridad <= C3 ~ "C3",
      escolaridad > C3 & escolaridad  <= C2 ~ "C2",
      escolaridad > C2 & escolaridad  <= ABC1 ~ "ABC1"
    ),
    centil = percent_rank(escolaridad)
  ) %>% 
  select(region, geocode, manzent, nviv, nhogar, personan, parentesco, escolaridad, centil, GSE)

#Guardar
mzn_mont %>% saveRDS("Output/Data_Temuco_Montecarlo.Rds")
