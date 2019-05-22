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
library(glue)

# Directorio
# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

# leer base censal 
# censo2012_R09 <- readRDS("Censo2012_R09.Rds") %>% filter(year == 2012 & region == 9)
censo2012_R13 <- readRDS("Censo2012_R13.Rds") %>% filter(year == 2012 & region == 13)
head(censo2012_R13)

# Leer shape manzanas y arreglar manzent
shp_mz <- st_read("Input/mzn_AUC_stgo.shp") %>% 
  mutate(
    MANZENT = (CUT*1000000000) + (DISTRITO*10000000) + (1*1000000) + (ZONA*1000) + MANZANA, # Rehacer Manzent
    geocode = (CUT*1000000) + (DISTRITO*10000) + (1*1000) + (ZONA)  # Crear codigo zona
  )

# #Leer shape zonas y arreglar geocode
# shp_zc <- st_read("Input/zona_stgo2012.shp") %>%
#   mutate(
#     geocode = (CUT*1000000) + (COD_DISTRI*10000) + (1*1000) + (COD_ZONA) # Crear codigo zona
#   )
# 
# # Guardar shape zona
# shp_zc %>% st_write("Input/zona_stgo2012_clean.shp", quiet = TRUE, delete_layer = TRUE)

# Leer archivo GSE ISMT -------------------------------------------------------

# ismt <- readRDS("ISMT2012_R09_mediana.Rds") %>%
ismt <- readRDS("ISMT2012_R13.Rds") %>%
  mutate(manzent = as.character(manzent)) %>% 
  select(manzent, folio, nviv, nhogar, ptje_ISMT, GSE_ISMT_pers, 
         ISMT_mzn, GSE_ISMT_mzn, ISMTptj_zc, GSE_ISMT_zc)

# Mediana ISMT por manzana
ismt_mzn <- ismt %>%
  group_by(manzent) %>% 
  summarise(
    ISMT = median(ptje_ISMT, na.rm = TRUE)
  ) %>% 
  ungroup()

# seleccionar ptje ismt por jefe de hogar
ismt_join <- ismt %>% select(manzent, folio, nviv, nhogar, ptje_ISMT)

# Leer quantiles ISMT y redeficnir GSE
# quant_ISMT <- readRDS("ISMT_quant_R09.Rds")
quant_ISMT <- readRDS("ISMT_quant_R13.Rds")

E_ismt <- quant_ISMT["6%",]
D_ismt <- quant_ISMT["36%",]
C3_ismt <- quant_ISMT["64%",]
C2_ismt <- quant_ISMT["79%",]
ABC1_ismt <- quant_ISMT["100%",]

# Guardar ISMT mzn 2012

# ismt_mzn_ <- ismt %>% distinct(manzent, ISMT_mzn, GSE_ISMT_mzn)
# 
# mzn_ismt2012 <- shp_mz  %>% transmute(manzent = as.character(MANZENT)) %>% left_join(ismt_mzn_, by = "manzent") %>% filter(!is.na(ISMT_mzn))
# 
# mzn_ismt2012 %>% st_write("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output/Shapefiles/shape_ISMT2012_R13/ISMT2012_R13.shp", delete_layer = TRUE)


# Data para max_p ---------------------------------------------------------

# Preparar datos manzana para script max_p 
# R09_mzn_data <- censo2012_R09 %>% 
R13_mzn_data <- censo2012_R13 %>% 
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
  # left_join(R09_mzn_data, by = c("MANZENT" = "manzent")) %>% 
  left_join(R13_mzn_data, by = c("MANZENT" = "manzent")) %>% 
  mutate(MANZENT= as.character(MANZENT))

###########################################################################

# Iterar sobre cada comuna ------------------------------------------------

# comunas <- shp_mz_vec %>% st_set_geometry(NULL) %>% select(CUT) %>% unique()
# comunas <- comunas %>% unlist() %>%  set_names(NULL)
# 41 comunas, de las cuales queda nula 1 - San Jose de Maipo 13203

# comunas <- c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114,
#             13115, 13116, 13117, 13118, 13119, 13120, 13121, 13123, 13124, 13125, 13126, 13127, 13128, 13130,
#             13131, 13132, 13201, 13203, 13301, 13302, 13401, 13403, 13601, 13604, 13605, 13122, 13129)
# 
# # comunas <- c(13120, 13101) # Test Nunoa y Santiago
# 
# for (comuna in comunas) {
# 
# # Shape para max_p
# shp_maxp <- shp_mz_vec %>%
#   left_join(ismt_mzn, by = c("MANZENT"="manzent")) %>%
#   na.omit(POB) %>%
#   filter(CUT == comuna) %>%
#   mutate(
#     IDMZ = as.character(MANZENT),
#     id = row_number(),
#     EDUC = as.numeric(scale(EDUC)),
#     ISMT = as.numeric(scale(ISMT))
#     ) %>%
#   select(IDMZ, id, POB, ISMT) #EDUC)
# 
# # Guardar shape
# shp_maxp %>% st_write(glue("Shapes/mzn_stgo_ismt_{comuna}.shp"), quiet = TRUE, delete_layer = TRUE)
# 
# }


# Data para montecarlo-----------------------------------------

## Calcular GSE a partir del ISMT

#Filtrar por manzanas dentro de AUC
mzn_ciudad <- shp_mz  %>% transmute(manzent = as.character(MANZENT))

# c2012_ <- censo2012_R09 %>% 
c2012_ <- censo2012_R13 %>% 
  mutate(manzent = as.character(manzent)) %>%  
  inner_join(mzn_ciudad, by = "manzent")
  
# Sacar GSE promedio Distrito -- 356 distritos censales
prom_dc <- c2012_ %>% 
  left_join(ismt_join, by = c("manzent", "folio", "nviv", "nhogar")) %>% 
  #filter(comuna == 13120) %>%  # Filtrar comuna = Nunoa
  mutate(persona = 1) %>% # Dummy persona para calc. población
  group_by(comuna, distrito) %>% 
  summarise(
    pob_dc = sum(persona, na.rm = TRUE), # Población zona - promedio 2997 personas
    ISMT_dc = median(if_else(parentesco == 1, ptje_ISMT, NA_real_), na.rm = TRUE) #Promedio ISMT jefe de hogar
  ) %>% 
  ungroup() %>% 
  mutate(
    GSE_dc = case_when(
      ISMT_dc <= E_ismt ~ "E",
      ISMT_dc > E_ismt & ISMT_dc <= D_ismt ~ "D",
      ISMT_dc > D_ismt & ISMT_dc <= C3_ismt ~ "C3",
      ISMT_dc > C3_ismt & ISMT_dc  <= C2_ismt ~ "C2",
      ISMT_dc > C2_ismt ~ "ABC1"
    ))



# Leer barrios ------------------------------------------------------------

barrios <- st_read("Input/ID_barrios") %>% st_set_geometry(NULL) %>% 
  transmute(
    manzent = as.character(CODINE011),
    id_barrio = as.character(ID_Barrios)
  )

# Unir ID barrios a base censal
c2012_ <- c2012_ %>% 
  left_join(barrios, by = "manzent") 

prom_barrio <- c2012_ %>% 
  filter(!is.na(id_barrio)) %>% 
  left_join(ismt_join, by = c("manzent", "folio", "nviv", "nhogar")) %>% 
  mutate(persona = 1) %>% 
  group_by(id_barrio) %>% 
  summarise(
    pob_barrio = sum(persona, na.rm = TRUE), # Población zona - promedio 2997 personas
    ISMT_barrio = median(if_else(parentesco == 1, ptje_ISMT, NA_real_), na.rm = TRUE) #Promedio ISMT jefe de hogar
  ) %>%
  ungroup() %>% 
  mutate(
    GSE_barrio  = case_when(
      ISMT_barrio <= E_ismt ~ "E",
      ISMT_barrio > E_ismt & ISMT_barrio <= D_ismt ~ "D",
      ISMT_barrio > D_ismt & ISMT_barrio <= C3_ismt ~ "C3",
      ISMT_barrio > C3_ismt & ISMT_barrio  <= C2_ismt ~ "C2",
      ISMT_barrio > C2_ismt ~ "ABC1"
    )
  )
  
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

# Temuco
# n_zonas   min   max  prom median
#      88   187  6183 2997.  2790.

# Gran Santiago
# n_zonas   min   max  prom median
#   1539    16  10290 3635.   3542

# `mean(pob_barrio)` -- 1088 barrios
# <dbl>
#   1              4781.

# `mean(pob_dc)` -- 356 distritos
# <dbl>
#   1         15715.

# nunoa 
# n_zonas   min   max  prom median
#      55  1751  4712 3398.   3438


# Percentiles población 
quant_pob <- quantile(prom_zc$pob_zona, probs = seq(0, 1, 0.1), na.rm=TRUE) %>% as.data.frame() 
# Percentil 10 = 1599.4 Temuco
# 10%   1974.6 Santiago

# Calculo GSE max_p -------------------------------------------------------


############# Correr script python vecindarios max_p #################

comunas <- c(13101, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114,
             13115, 13116, 13117, 13119, 13120, 13121,  13124, 13125, 13126, 13127, 13128, #13130, 13122,
             13131, 13132, 13601, 13605, 13129,
             # 13203, queda fuera en delaunay
             # 13102, 13201, 13403, 13122, quedan fuera en f1500
             # 13604 queda fuera en floor 2500
             # 13102,  13201,13604
             13118, 13123, 13301, 13302, 13401  # quedan fuera en geoda
             )

floor <- "2500" # Definir floor

# Leer output script max_p - unir todas las comunas de stgo
z_homogeneas <- data.frame()

for(comuna in comunas){

  # ReadInMerge <- st_read(glue("Output/Geoda/{floor}/geoda_{comuna}.shp")) %>%
  ReadInMerge <- st_read(glue("Shapes/output_f{floor}_{comuna}")) %>%
    st_set_geometry(NULL) %>%
    transmute(
      manzent = as.character(IDMZ),
      cluster = str_c(comuna, cluster)
    )

  z_homogeneas <- bind_rows(z_homogeneas, ReadInMerge)
}

# Comprobar n clusters = solution.p en script max_p
z_homogeneas %>% group_by(cluster) %>% summarise(n_obs = n()) %>% print() # 7.044 zonas, output maxp 7.058 
# floor 1500 - 2140 zonas homogeneas
# floor 2000 - 1612 zonas homogeneas
# Floor 10000 (31 comunas) - 305 zh

# Unir id zonas homogeneas a base censal
c2012_clust <- c2012_ %>% 
  inner_join(z_homogeneas, by="manzent") %>% 
  left_join(ismt_join, by = c("manzent", "folio", "nviv", "nhogar"))

#Calcular GSE zonas homogeneas
prom_zh <- c2012_clust %>%   
    mutate(persona = 1) %>% 
    group_by(cluster) %>% 
    summarise(
      pob_zh = sum(persona, na.rm = TRUE), # Población zona 
      med_ISMT_zh = median(if_else(parentesco == 1, ptje_ISMT, NA_real_), na.rm = TRUE) #Promedio de ISMT jefe de hogar
    ) %>% 
    ungroup() %>% 
    mutate(
      GSE_ISMT_zh = case_when(
        med_ISMT_zh <= E_ismt ~ "E",
        med_ISMT_zh > E_ismt & med_ISMT_zh <= D_ismt ~ "D",
        med_ISMT_zh > D_ismt & med_ISMT_zh <= C3_ismt ~ "C3",
        med_ISMT_zh > C3_ismt & med_ISMT_zh  <= C2_ismt ~ "C2",
        med_ISMT_zh > C2_ismt ~ "ABC1"
    ))


# unir resultados a base personas ----------------------------------------
  
  
# seleccionar base final
mzn_mont <- c2012_clust %>% 
    filter(parentesco==1 & !is.na(cluster)) %>% #filtrar sólo jefes de hogar y NAs zonas homogeneas 
    mutate(
      ISMT_pers = ptje_ISMT
    ) %>%
    left_join(prom_barrio, by = "id_barrio") %>% # Unir barrios
    left_join(prom_dc, by = c("comuna", "distrito")) %>% # Unir distritos
    left_join(ismt, by = c("manzent", "folio", "nviv", "nhogar", "ptje_ISMT")) %>% # Unir GSE ISMT manzana y zona censal
    left_join(prom_zh, by = "cluster") %>% # Unir zonas homogeneas
    select(region, geocode, manzent, nviv, nhogar, personan, parentesco, escolaridad, #centil_educ_pers, GSE_pers,
           # educ_mzn, educ_zona,  GSE_mzn, GSE_zona, 
           ISMT_pers, GSE_ISMT_pers, ISMT_mzn, GSE_ISMT_mzn, ISMTptj_zc, GSE_ISMT_zc, 
           med_ISMT_zh, cluster, GSE_ISMT_zh,
           comuna, distrito, ISMT_dc, GSE_dc,
           id_barrio, ISMT_barrio, GSE_barrio
           )
  

  #Guardar
  # mzn_mont %>% saveRDS("Output/Data_Temuco_Montecarlo_mediana.Rds")
  # mzn_mont %>% saveRDS("Output/Data_Temuco_cluster_mediana_ismt_8.Rds")
  # mzn_mont %>% saveRDS("Output/Data_Nunoa_cluster_ismt_1.Rds")
  # mzn_mont %>% saveRDS(glue("Output/Maxp_Stgo_cluster_f{floor}.Rds"))
  mzn_mont %>% saveRDS("Output/Geoda_Stgo_cluster_10000.Rds")
  


