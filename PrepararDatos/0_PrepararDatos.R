library(sf)
library(tidyverse)

# Directorio
setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")

# leer base censal R09
censo2012_R09 <- readRDS("TotalCenso1992_2017_Persona_Clean_R13.Rds") %>% filter(year == 2012 & region == 9)
head(censo2012_R09)

# Preparar datos para script max_p
R09_mzn_data <- censo2012_R09 %>% 
  mutate(persona=1) %>% 
  group_by(manzent) %>%
  summarise(
    POB = sum(persona),
    EDUC = if_else(parentesco ==1, mean(escolaridad)) #Promedio de años escolaridad jefe de hogar
  )
  
# Leer shapes
shp_mz <- st_read("Input/mzn_AUC_temuco.shp")
# shp_zc <- st_read("Input/zona_AUC_temuco2012.shp")

# Guardar shape para vecindad max_p
shp_mz_vec <- shp_mz %>% left_join(R09_mzn_data)
shp_mz_vec %>% st_write("Output/mzn_temuco_clean.shp", quiet = TRUE, delete_layer = TRUE)


# Data para montecarlo-----------------------------------------

#Filtrar por manzanas dentro de temuco
mzn_temuco <- shp_mz %>% st_set_geometry(NULL) %>% select(manzent)
c2012_temuco <- censo2012_R09 %>% inner_join(mzn_temuco)

# Calcular centiles escolaridad
quant_temuco <- quantile(c2012_temuco$escolaridad, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
rownames(quant_R04)

# Determinar tope centil GSE x ciudad 
E <- quant_temuco["6%",]
D <- quant_temuco["36%",]
C3 <- quant_temuco["64%",]
C2 <- quant_temuco["79%",]
ABC1 <- quant_temuco["100%",]

# seleccionar base final
mzn_mont <- c2012_temuco %>% 
  mutate(persona=1) %>% 
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
  select(geocode, manzent, persona, nviv, nhog, nper, parentesco, escolaridad, centil, GSE)

#Guardar
mzn_mont %>% saveRDS("Output/Data_Temuco_Montecarlo.Rds")
