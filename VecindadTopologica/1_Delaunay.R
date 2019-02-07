##### Voronoi #####
install.packages(c("tripack", "spdep", "RANN", "foreach", "rgdal"))

library(tripack)
library(spdep)
library(RANN)
library(foreach)
library(rgdal)
library(sf)

options(scipen=999)

# setwd("C:/Users/cit1/Documents/Max_P")
# setwd("//SVWIN022/00.cit/05.INVESTIGACION/2018_DELITO_BAC_SPD/Max_P_ej")
setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")

####### Triangulation [Creacion de Vecinos]  #####

#Explorar datos conce
mzn_conce <- st_read("Input/mzn_concepcion02")
auc_conce <- st_read("Input/R08_Concepcion")
gran_conce <- st_read("Input/mzn_gran_concepcion02") # Merge de las 10 comunas del gran concepcion
#Sobreponer manzanas 2002 a AUC Conce Zonas Censales 2017
ggplot() + geom_sf(data=auc_conce) + geom_sf(data = mzn_conce, aes(fill="red", alpha=0.2))
ggplot() + geom_sf(data=auc_conce) + geom_sf(data = gran_conce, aes(fill="red", alpha=0.2))
#Solo es zona centro de Conce - agregar manzanas San Pedro, Talcahuano

# Leer datos - shape manzanas, ciudad -------------------------------------

# Leer shape manzanas ciudad
# shape <- readOGR("Shapes",paste0("mzs_",city),stringsAsFactors=F)
shape <- readOGR("Input/mzn_gran_concepcion02", stringsAsFactors=F)
rownames(shape@data) = shape$id = 0:(nrow(shape@data)-1) # Generar variable índice
shape$IDMZ = as.character(shape$IDMZ) # Guardar ID Manzana como caracter

# Guardar shape manzana (overwrite)
writeOGR(shape, "Shapes","mzn_gran_concepcion02",driver="ESRI Shapefile", overwrite_layer=T)


# Preparar datos - centroides ---------------------------------------------

# Extraer los centroides de las manzanas
ciudad <- SpatialPointsDataFrame(shape, shape@data, proj4string = CRS(proj4string(shape)))
ciudad$IDMZ=NULL
ids <- ciudad@data # Lista de IDs por manzana
coords <- coordinates(ciudad) # Lista de coordenadas
IDs <- as.numeric(row.names(ciudad)) # Lista de IDs - de nuevo?


# Generar traingulación ---------------------------------------------------

vecs1 <- tri2nb(coords, row.names = IDs) # Delauney Triangulation - generar vecinos
plot(vecs1, coords) # Plotear triangulación

# Fijar conjunto de vecinos ------------------------------------------

metros <- 900 # Definir distancia máxima entre vecinos en metros 

# Función obtiene vecinos que esten a menos de 900 metros ó un mínimo de 3 vecinos 
thresh <- foreach(v = 1:length(vecs1)) %do%  {
  
  # Extraer coordenadas
  source <- coords[v,] # Coordenadas de cada elemento
  neighs <- coords[vecs1[[v]],] # Coordenadas de cada vecino
  x <- neighs[1,] # Coordenadas de cada vecino
  
  # Aplicar función cálculo de distancia a cada vecino de cada punto
  e <- apply(neighs,1, function(x) { 
    d <- sqrt((x[1] - source[1])^2 + (x[2] - source[2])^2) # Distancia en metros
    d
  })
  
  # Ordenar y encontrar vecinos mínimos y máximos
  orden <- e[order(e)] # Ordenar por distancia
  minimos <- orden[1:2] # Obtener los 2 vecinos más cercanos
  maximos <- orden[1:min(3,length(e))] # Obtener los vecinos más lejanos
  
 # Vecino en menos de 900 metros ó dentro del conjunto de mínimos y máximos
  f <- unlist(lapply(1:length(e), function(x) { # asigna False
    (e[x] < metros | # distancia menor a máximo en metros ó
      names(e[x]) %in% names(minimos)) & # Vecinos en el conjunto de mínimo Y
      names(e[x]) %in% names(maximos) # Vecinos en el cojunto de máximos
  }))
  
}

# Aplicar función thresh a listado de vecinos
# obs=1
vecs <- vecs1 # Definir objeto vecs
for (obs in 1:length(vecs1)) {
  vecs[[obs]] <- vecs[[obs]] [thresh[[obs]]]
}

plot(vecs,coords) # Plotear triangulación final

# Traspasar vecindad a líneas
lw <- nb2listw(vecs) # Spatial Weights For Neighbours Lists
lineas <- listw2lines(lw, coords, proj4string = CRS(proj4string(ciudad))) # Use Arc-Type Shapefiles For Import And Export Of Weights
plot.nb(vecs, coords) # Plotear


# Guardar shapes ----------------------------------------------------------

writeOGR(lineas,"Shapes","vecinos",driver="ESRI Shapefile",overwrite_layer=T)
write.nb.gal(vecs, "Output/weights.gal", oldstyle=TRUE, shpfile=NULL, ind=NULL)
