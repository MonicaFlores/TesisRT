##### Voronoi #####
library(tripack)
library(spdep)
library(RANN)
library(foreach)
library(rgdal)
options(scipen=999)
####### Triangulation [Creacion de Vecinos]  #####
reg=11
city=1110101
# setwd("C:/Users/cit1/Documents/Max_P")
setwd("//SVWIN022/00.cit/05.INVESTIGACION/2018_DELITO_BAC_SPD/Max_P_ej")
shape=readOGR("Shapes",paste0("mzs_",city),stringsAsFactors=F)
rownames(shape@data)=shape$id=0:(nrow(shape@data)-1)
shape$IDMZ=as.character(shape$IDMZ)
writeOGR(shape,"Shapes",paste0("mzs_",city),driver="ESRI Shapefile",overwrite_layer=T)

ciudad = SpatialPointsDataFrame(shape, shape@data, proj4string = CRS(proj4string(shape)))
ids=ciudad@data
ciudad$IDMZ=NULL
coords = coordinates(ciudad)
IDs = as.numeric(row.names(ciudad))
vecs1=tri2nb(coords,row.names = IDs) # Delauney Triangulation}
plot(vecs1,coords)

metros=900

thresh=foreach(v = 1:length(vecs1)) %do%  {
  source=coords[v,]
  neighs=coords[vecs1[[v]],]
  x=neighs[1,]
  e=apply(neighs,1, function(x) {
    d=sqrt((x[1] - source[1])^2 + (x[2] - source[2])^2)
    d
  })
  orden=e[order(e)]
  minimos=orden[1:2]
  maximos=orden[1:min(3,length(e))]
  f=unlist(lapply(1:length(e), function(x) { # asigna False
    (e[x]<metros|names(e[x])%in%names(minimos))&names(e[x])%in%names(maximos)
  }))
}

# obs=1
vecs=vecs1
for (obs in 1:length(vecs1)) {
  vecs[[obs]]=vecs[[obs]][thresh[[obs]]]
}

plot(vecs,coords)

lw=nb2listw(vecs)
lineas=listw2lines(lw,coords,proj4string = CRS(proj4string(ciudad)))
plot.nb(vecs,coords)

writeOGR(lineas,"Shapes","vecinos",driver="ESRI Shapefile",overwrite_layer=T)
write.nb.gal(vecs, "Datos/weights.gal", oldstyle=TRUE, shpfile=NULL, ind=NULL)
