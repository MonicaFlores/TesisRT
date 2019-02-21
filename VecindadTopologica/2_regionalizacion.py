# -*- coding: utf-8 -*-
"""
Created on Thu Dec  6 15:13:49 2018

@author: matias.garreton
"""

#get_ipython().run_line_magic('cd', '//svwin022/00.cit/05.INVESTIGACION/2018_DELITO_BAC_SPD/Max_P_ej')
#get_ipython().run_line_magic('cd', '/Users/MoniFlores/Desktop/Tesis RT/Data')
get_ipython().run_line_magic('cd', 'C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data')


import numpy as np
import pandas as pd
import geopandas as gpd
import pysal

np.random.seed(100)

#shp = gpd.read_file("/Users/MoniFlores/Desktop/Tesis RT/Data/Shapes/mzn_temuco.shp")
#shp = gpd.read_file("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data/Shapes/mzn_temuco.shp")
shp = gpd.read_file("Shapes/mzn_temuco_filter.shp")
w = pysal.open("Output/weights_filter.gal").read()
#z = shp.drop(['geometry', 'id','POB','IDMZ'], axis=1).values 
#p = shp.drop(['geometry', 'id','EDUC','IDMZ'], axis=1).values
# Dar vuelta las variables... p población... z homegeneity variable
p = shp.drop(['geometry', 'id','POB','IDMZ'], axis=1).values 
z = shp.drop(['geometry', 'id','EDUC','IDMZ'], axis=1).values

floor = 75
solution = pysal.region.Maxp(w, z, floor, floor_variable=p, initial=100) 

solution.p
min([len(region) for region in solution.regions])
max([len(region) for region in solution.regions])
#solution.regions[0]

lbls = pd.Series(solution.area2region)
lbdat = pd.DataFrame(data={'cluster':lbls.values,'id':lbls.index.astype(int)})
lbdat.id=lbdat.id-1

shpreg=shp.merge(lbdat,on='id')

shpreg.to_file('Shapes/output')

#-------------------------------

# Comprobar si es mejor que una regionalización random
solution.cinference(nperm=99, maxiter=100)
solution.cpvalue