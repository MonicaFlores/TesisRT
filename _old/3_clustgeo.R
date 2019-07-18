library(tidyverse)
library(spdep)
library(Imap)
library(sf)
library(glue)

setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data")

shape <- readOGR("Shapes/mzn_stgo_ismt_nunoa.shp", stringsAsFactors=F) # prueba Nunoa

# Preparar datos - centroides ---------------------------------------------

# Extraer los centroides de las manzanas
ciudad <- SpatialPointsDataFrame(shape, shape@data, proj4string = CRS(proj4string(shape)))
ciudad$IDMZ=NULL
ids <- ciudad@data 

coords <- coordinates(ciudad) %>%  # Lista de coordenadas
  as_data_frame() %>% 
  transmute(
    index = as.numeric(row.names(ciudad)) + 1,
    lat = coords.x2,
    lon = coords.x1
  )



dat <- shape@data %>% select(ISMT)
head(dat)


# Funcion matriz distancias -----------------------------------------------

ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}



# Matriz ------------------------------------------------------------------

# Matriz de distancias 

matrix <- round(GeoDistanceInMetresMatrix(coords))

# Clusterizacion ----------------------------------------------------------

n <- nrow(dat)
D0 <- dist(dat) # Definir distancia a partir de ISMT

Delta <- D^2/(2*n)
tree <- hclust(Delta,method="ward.D")
sum(tree$height)


# Con peso poblacion
wt <- shape@data$POB # non uniform weights

tree <- hclustgeo(D0,wt=wt)
sum(tree$height)

# Plot dendogram
plot(tree,hang=-1,label=FALSE, xlab="",sub="",
     main="Ward dendrogram with D0 only",cex.main=0.8,cex=0.8,cex.axis=0.8,cex.lab=0.8)
#plot(tree,hang=-1,xlab="",sub="",main="Ward dendrogram with D0 only",
#      cex.main=0.8,cex=0.8,labels=city_label,cex.axis=0.8,cex.lab=0.8)

# Definición número de zonas homogeneas -----------------------------------


# Definir n clusters
zh <- 40

# Clusterizacion con restriccion geografica

D1 <- as.dist(matrix) # Distancia geografica

# rect.hclust(tree,k=zh,border=c(zh:1))
# legend("topright", legend= paste("cluster",1:zh), fill=1:zh, cex=0.8,bty="n",border="white")
# 
# 
# # plotear mapa
# P5 <- cutree(tree,zh)
# sp::plot(shape,border="grey",col=P5,main="5 clusters partition obtained with D0 only",cex.main=0.8)
# legend("topleft", legend=paste("cluster",1:zh), fill=1:zh, cex=0.8,bty="n",border="white")


range.alpha <- seq(0,1,0.1)
K <- zh # Número de grupos

cr <- choicealpha(D0,D1,range.alpha,K,graph=TRUE) # Grafico sugiere cruce en alpha = 0.3, probar con alpha 0.5

tree <- hclustgeo(D0,D1, alpha=0.5)
P5bis <- cutree(tree, zh)

sp::plot(shape ,border="grey",col=P5bis, main="40 clusters partition obtained \n with alpha=0.4 and geographical distances",cex.main=0.8)
legend("topleft", legend=paste("cluster",1:zh), fill=1:zh, bty="n",border="white")
