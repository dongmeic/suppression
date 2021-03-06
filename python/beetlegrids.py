#-------------------------------------------------------------------------------
# Name:        beetlegrids.py
# Purpose:     beetle presence from 1997 to 2017 on a 10 km grid in the American West
# Updates:     Added year 2017 data and used mpb10km as the area of interest
# Author:      dongmeic
#
# Created:     11/01/2018
# Copyright:   (c) dongmeic 2018
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()

arcpy.env.workspace = "H:/13.fall_2018/fire_suppression/mpb_us"
outfolder = "H:/13.fall_2018/fire_suppression/presence/"


mask = "H:/13.fall_2018/fire_suppression/mpb10km/mpb10km_us_gridpts.shp"
arcpy.MakeFeatureLayer_management(mask,"mask")
dist = "10000 Meters"
    
for year in range(1997,2018):
    usfc = "us_mpb_" + str(year)+".shp"
    arcpy.MakeFeatureLayer_management(usfc,"usfc_{0}".format(year))
    fieldList = arcpy.ListFields("mask")
    field_names = [f.name for f in fieldList]
    newfield = "prs_"+ str(year)
    if newfield in field_names:
        pass
    else:
        arcpy.AddField_management("mask", newfield, "SHORT", "", "", 10)
    arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "usfc_{0}".format(year), dist, "NEW_SELECTION")
    with arcpy.da.UpdateCursor("mask", ['prs_{0}'.format(year)]) as cursor:
        for row in cursor:
            row[0] = 1
            cursor.updateRow(row)
    del row
    del cursor
    arcpy.SelectLayerByAttribute_management("mask", "SWITCH_SELECTION")
    with arcpy.da.UpdateCursor("mask", ['prs_{0}'.format(year)]) as cursor:
        for row in cursor:
            row[0] = 0
            cursor.updateRow(row)
    del row
    del cursor
    arcpy.SelectLayerByAttribute_management("mask", "CLEAR_SELECTION")
    print(arcpy.GetMessages(0))
    print('{0} year update finished'.format(year))

arcpy.CopyFeatures_management("mask", outfolder+"mpb10km_presence.shp")
layer = "mask"
del layer

stop = timeit.default_timer()
print stop - start
