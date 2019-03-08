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
#shp = gpd.read_file("Shapes/mzn_stgo_ismt_filter.shp")

comunas = [13101, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114,
        13115, 13116, 13117, 13118, 13119, 13120, 13121, 13123, 13124, 13125, 13126, 13127, 13128, 13130,
        13131, 13132, 13301, 13302, 13401, 13601, 13605, 13129
        # 13604, 13102, 13201, 13403, 13122
        ] 

# 13203 san jose de maipo - queda fuera en Delaunay
# Fallan con 2000: 13102, 13201, 13403, 13122, (Cerrillos, Puente Alto, Calera de Tango, Peñalolen)
# Fallan con 2500: 13604, 13102, 13201, 13403, 13122

comunas = [13120]

for comuna in comunas:

    shp = gpd.read_file("Shapes/mzn_stgo_ismt_" + str(comuna) + ".shp")
    #w = pysal.open("Output/weights_stgo_ismt_" + str(comuna) + ".gal").read()
    w = pysal.open("Output/weights_stgo_ismt_nunoa_test5.gal").read()
    z = shp.drop(['geometry', 'id','POB','IDMZ'], axis=1).values # eliminar todo menos EDUC (ISMT)
    p = shp.drop(['geometry', 'id','ISMT','IDMZ'], axis=1).values # eliminar todo menos POB
    #p = shp.drop(['geometry', 'id','EDUC','IDMZ'], axis=1).values # eliminar todo menos POB
    
    floor = 2000
    solution = pysal.region.Maxp(w, z, floor, floor_variable=p, initial=100) 
    
    print(solution.p)
    min([len(region) for region in solution.regions])
    max([len(region) for region in solution.regions])
    #solution.regions[0]
    
    lbls = pd.Series(solution.area2region)
    lbdat = pd.DataFrame(data={'cluster':lbls.values,'id':lbls.index.astype(int)})
    lbdat.id=lbdat.id-1
    
    
    shpreg=shp.merge(lbdat,on='id') 
    #shpreg=pd.merge(shp, lbdat,on='id', how='left') # Alternativa
    
    
    # shpreg.to_file("Shapes/output_f2500_" + str(comuna))
    shpreg.to_file("Shapes/test5_output_f250_2_" + str(comuna))



#-------------------------------

# Comprobar si es mejor que una regionalización random
# solution.cinference(nperm=99, maxiter=100)
# solution.cpvalue