# dongmeic

import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()

arcpy.env.workspace = "H:/13.fall_2018/fire_suppression"
outfolder = "H:/13.fall_2018/fire_suppression/presence/"

