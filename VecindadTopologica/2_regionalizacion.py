# -*- coding: utf-8 -*-
"""
Created on Thu Dec  6 15:13:49 2018

@author: matias.garreton
"""

#get_ipython().run_line_magic('cd', '//svwin022/00.cit/05.INVESTIGACION/2018_DELITO_BAC_SPD/Max_P_ej')
get_ipython().run_line_magic('cd', '/Users/MoniFlores/Desktop/Tesis RT/Data')


import numpy as np
import pysal
import pandas as pd
import geopandas as gpd

np.random.seed(100)

shp = gpd.read_file("/Users/MoniFlores/Desktop/Tesis RT/Data/Shapes/mzn_temuco.shp")
w = pysal.open("Output/weights.gal").read()
z = shp.drop(['geometry', 'id','POB','IDMZ'], axis=1).values 
p = shp.drop(['geometry', 'id','EDUC','IDMZ'], axis=1).values


floor = 500
solution = pysal.region.Maxp(w, z, floor, floor_variable=p, initial=100) 

solution.p
min([len(region) for region in solution.regions])
max([len(region) for region in solution.regions])
#solution.regions[0]

lbls = pd.Series(solution.area2region)
lbdat=pd.DataFrame(data={'cluster':lbls.values,'id':lbls.index.astype(int)})
lbdat.id=lbdat.id-1

shpreg=shp.merge(lbdat,on='id')

shpreg.to_file('Shapes/output')
