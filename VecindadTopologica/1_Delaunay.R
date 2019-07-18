
#install.packages(c("tripack", "spdep", "RANN", "foreach", "rgdal"))

library(tripack)
library(spdep)
library(RANN)
library(foreach)
library(rgdal)
library(sf)
library(glue)

options(scipen=999)

# setwd("/Users/MoniFlores/Desktop/Tesis RT/Data")
setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

####### Triangulation [Creacion de Vecinos]  #####

comunas <- c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114,
             13115, 13116, 13117, 13118, 13119, 13120, 13121, 13123, 13124, 13125, 13126, 13127, 13128, 13130,
             13131, 13132, 13201, #13203, San José de Maipo falla - solo tiene 3 manzanas con POB válida dentro del AUC
             13301, 13302, 13401, 13403, 13601, 13604, 13605, 13122, 13129)

for (comuna in comunas) {

  # Leer datos - shape manzanas, ciudad -------------------------------------
  
  shape <- readOGR(glue("Shapes/mzn_stgo_ismt_{comuna}.shp"), stringsAsFactors=F)
  
  # Preparar datos - centroides ---------------------------------------------
  
  # Extraer los centroides de las manzanas
  ciudad <- SpatialPointsDataFrame(shape, shape@data, proj4string = CRS(proj4string(shape)))
  ciudad$IDMZ=NULL
  ids <- ciudad@data 
  coords <- coordinates(ciudad) # Lista de coordenadas
  IDs <- as.numeric(row.names(ciudad)) # Lista de IDs por manzana
  
  
  # Generar vecindad ---------------------------------------------------
  
  # Vecindad original
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
  vecs <- vecs1 # Definir objeto vecs
  for (obs in 1:length(vecs1)) {
    vecs[[obs]] <- vecs[[obs]] [thresh[[obs]]]
  }
  
  plot(vecs,coords) # Plotear triangulación final
  
  # Traspasar vecindad a líneas
  lw <- nb2listw(vecs) # Spatial Weights For Neighbours Lists
  lineas <- listw2lines(lw, coords, proj4string = CRS(proj4string(ciudad))) # Use Arc-Type Shapefiles For Import And Export Of Weights
  # plot.nb(vecs, coords) # Plotear
  
  
  # Guardar shapes ----------------------------------------------------------
  
  writeOGR(lineas,"Shapes", glue("vecinos_stgo_ismt_{comuna}"),driver="ESRI Shapefile",overwrite_layer=T)
  write.nb.gal(vecs, glue("Output/weights_stgo_ismt_{comuna}.gal"), oldstyle=TRUE, shpfile=NULL, ind=NULL)

}
