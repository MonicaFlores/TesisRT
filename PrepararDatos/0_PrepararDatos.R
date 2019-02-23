# Nombre Programa: 0_PrepararDatos
# Ubicacion: GitHub/Tesis_RT
# Autor: Monica Flores
# Fecha Creacion: 07/02/2018
# Proyecto: Tesis Doctorado Ricardo Truffello
# Objetivo: Preparar datos prueba script max_p y montecarlo
# Output: 
# Notas:
options(scipen = 999)

library(sf)
library(tidyverse)

# Directorio
setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
# setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

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
# shp_zc <- st_read("Input/zona_AUC_temuco2012.shp") %>%
#   mutate(
#     geocode = (CUT*1000000) + (COD_DISTRI*10000) + (1*1000) + (COD_ZONA) # Crear codigo zona
#   )
# 
# # Guardar shape zona 
# shp_zc %>% st_write("Input/zona_temuco_clean.shp", quiet = TRUE, delete_layer = TRUE)

# Data para max_p ---------------------------------------------------------


# Preparar datos manzana para script max_p
R09_mzn_data <- censo2012_R09 %>% 
  mutate(persona=1) %>% 
  group_by(manzent) %>%
  summarise(
    POB = sum(persona, na.rm = TRUE),
    EDUC = median(if_else(parentesco ==1, escolaridad, NA_integer_), na.rm = TRUE) #Mediana de años escolaridad jefe de hogar por manzana
  ) %>% 
  ungroup() %>% 
  filter(!is.na(manzent))
  
# Shape con info censal
shp_mz_vec <- shp_mz %>% 
  left_join(R09_mzn_data, by = c("MANZENT" = "manzent")) %>% 
  mutate(MANZENT= as.character(MANZENT))
# # Prueba: Plotear EDUC
# ggplot() + geom_sf(data=shp_mz_vec, aes(fill=-EDUC))

# Shape para max_p
shp_maxp <- shp_mz_vec %>% 
  na.omit(POB) %>% 
  mutate(
    IDMZ = as.character(MANZENT),
    id = row_number(),
    EDUC = scale(EDUC)
    ) %>% 
  select(IDMZ, id, POB, EDUC) 

# # Guardar shape
# shp_maxp %>% st_write("Shapes/mzn_temuco.shp", quiet = TRUE, delete_layer = TRUE)
# 
# test <- st_read("Shapes/mzn_temuco.shp")


# Data para montecarlo-----------------------------------------

## Calcular GSE a partir de educacion del jefe de hogar

#Filtrar por manzanas dentro de temuco
mzn_temuco <- shp_mz %>% st_set_geometry(NULL) %>% transmute(manzent = as.character(MANZENT))

c2012_temuco <- censo2012_R09 %>% 
  mutate(manzent = as.character(manzent)) %>%  
  inner_join(mzn_temuco, by = "manzent")

# Calcular centiles escolaridad basado en la mediana por manzana
quant_temuco <- quantile(shp_mz_vec$EDUC, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
rownames(quant_temuco)

# Determinar tope centil GSE x ciudad 
E <- quant_temuco["6%",]
D <- quant_temuco["36%",]
C3 <- quant_temuco["64%",]
C2 <- quant_temuco["79%",]
ABC1 <- quant_temuco["100%",]

# Sacar GSE mediana manzana
prom_mzn <- shp_mz_vec %>% rename(manzent=MANZENT, educ_mzn=EDUC) %>% 
  st_set_geometry(NULL) %>% #Dejar geometría nula ya que viene de un shape
  group_by(manzent) %>% 
  mutate(
    GSE_mzn = case_when(
      educ_mzn <= E ~ "E",
      educ_mzn > E & educ_mzn <= D ~ "D",
      educ_mzn > D & educ_mzn <= C3 ~ "C3",
      educ_mzn > C3 & educ_mzn  <= C2 ~ "C2",
      educ_mzn > C2 ~ "ABC1"
    )) %>% 
  select(manzent, educ_mzn, GSE_mzn) %>% 
  ungroup()

# Sacar GSE promedio Zona 
prom_zc <- c2012_temuco %>% 
  mutate(persona = 1) %>% # Dummy persona para calc. población
  group_by(geocode) %>% 
  summarise(
    pob_zona = sum(persona, na.rm = TRUE), # Población zona - promedio 2997 personas
    educ_zona = median(if_else(parentesco == 1, escolaridad, NA_integer_), na.rm = TRUE) #Promedio de años escolaridad jefe de hogar
  ) %>% 
  ungroup() %>% 
  mutate(
    GSE_zona = case_when(
      educ_zona <= E ~ "E",
      educ_zona > E & educ_zona <= D ~ "D",
      educ_zona > D & educ_zona <= C3 ~ "C3",
      educ_zona > C3 & educ_zona  <= C2 ~ "C2",
      educ_zona > C2 ~ "ABC1"
    ))


# Explorar datos población ZC ------------------------------------------------

#Calcular minimo, media, mediana, max poblacion zona censal
prom_zc %>% 
  summarise(
    n_zonas = n(),
    min= min(pob_zona),
    max = max(pob_zona),
    prom = mean(pob_zona),
    median = median(pob_zona)
  )
# n_zonas   min   max  prom median
#      88   187  6183 2997.  2790.

# Percentiles población 
quant_pob <- quantile(prom_zc$pob_zona, probs = seq(0, 1, 0.1), na.rm=TRUE) %>% as.data.frame() 
# Percentil 10 = 1599.4

# Calculo GSE max_p -------------------------------------------------------

############# Correr script python vecindarios max_p #################

# Leer output script max_p
z_homogeneas <- st_read("Shapes/output_4/output_4.shp") %>% 
  st_set_geometry(NULL) %>% 
  mutate(manzent = as.character(IDMZ)) %>% 
  select(manzent, cluster)

# Comprobar n clusters = solution.p en script max_p
test <- z_homogeneas %>% group_by(cluster) %>% summarise(n_obs = n())

# Unir id zonas homogeneas a base censal
c2012_temuco_clust <- c2012_temuco %>% 
  left_join(z_homogeneas, by="manzent")

#Calcular GSE zonas homogeneas
prom_zh <- c2012_temuco_clust %>%   
  mutate(persona = 1) %>% 
  group_by(cluster) %>% 
  summarise(
    pob_zh = sum(persona, na.rm = TRUE), # Población zona - promedio 2997 personas
    educ_zh = median(if_else(parentesco == 1, escolaridad, NA_integer_), na.rm = TRUE) #Promedio de años escolaridad jefe de hogar
  ) %>% 
  ungroup() %>% 
  mutate(
    GSE_zh = case_when(
      educ_zh <= E ~ "E",
      educ_zh > E & educ_zh <= D ~ "D",
      educ_zh > D & educ_zh <= C3 ~ "C3",
      educ_zh > C3 & educ_zh  <= C2 ~ "C2",
      educ_zh > C2 ~ "ABC1"
    ))

# pruebas <- c(1:100)
# 
# for (n in pruebas) {
# 
#   # Leer base de datos
#   vecindad <- st_read(glue("{dir_loc}/Vecindad/GSE_maxp_prueba{n}.shp"))


# Leer archivo GSE ISMT -------------------------------------------------------

ismt <- readRDS("ISMT2012_R09_mediana.Rds") %>% 
  mutate(manzent = as.character(manzent)) %>% 
  select(manzent, ptje_ISMT, GSE_ISMT_pers, ISMTptj, GSE_ISMT_mzn, ISMTptj_zc, GSE_ISMT_zc)

# unir resultados a base personas ----------------------------------------
  
  
# seleccionar base final
mzn_mont <- c2012_temuco_clust %>% 
    filter(parentesco==1 & !is.na(cluster)) %>% #filtrar sólo jefes de hogar y NAs zonas homogeneas 
    mutate(
      GSE_pers = case_when(
        escolaridad <= E ~ "E",
        escolaridad > E & escolaridad <= D ~ "D",
        escolaridad > D & escolaridad <= C3 ~ "C3",
        escolaridad > C3 & escolaridad  <= C2 ~ "C2",
        escolaridad > C2 ~ "ABC1"
      ),
      centil_educ_pers = percent_rank(escolaridad)
    ) %>% 
    left_join(prom_mzn, by = "manzent") %>% # Unir manzana
    left_join(prom_zc, by = "geocode") %>% # Unir zona censal
    left_join(ismt, by = "manzent") %>% # Unir GSE ISMT manzana y zona censal
    left_join(prom_zh, by = "cluster") %>% # Unir zonas homogeneas
    select(region, geocode, manzent, nviv, nhogar, personan, parentesco, escolaridad, centil_educ_pers,
           educ_mzn, educ_zona, GSE_pers, GSE_mzn, GSE_zona, 
           ptje_ISMT, GSE_ISMT_pers, ISMTptj, GSE_ISMT_mzn, ISMTptj_zc, GSE_ISMT_zc,
           cluster, GSE_zh)
  
  #Guardar
  # mzn_mont %>% saveRDS("Output/Data_Temuco_Montecarlo_mediana.Rds")
  mzn_mont %>% saveRDS("Output/Data_Temuco_cluster_mediana_educ_4.Rds")

# }
