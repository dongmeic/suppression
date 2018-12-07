# dongmeic

import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()

arcpy.env.workspace = "H:/13.fall_2018/fire_suppression"
outfolder = "H:/13.fall_2018/fire_suppression/presence/"

mpb10km_pts = "mpb10km/mpb10km_us_gridpts.shp"
#arcpy.MakeFeatureLayer_management(mpb10km_pts, "mpb10km")
if 0:
    CONUS_WUI = "CONUS_WUI_cp12_d.gdb/CONUS_WUI_cp12"
    arcpy.MakeFeatureLayer_management(CONUS_WUI, "WUI")
    arcpy.SelectLayerByAttribute_management ("WUI", "NEW_SELECTION", '"WUIFLAG90" > 0 OR "WUIFLAG00" > 0 OR "WUIFLAG10" > 0')
    WUI = outfolder+"WUI.shp"
    arcpy.CopyFeatures_management("WUI", WUI) # did not work

# Near analysis to get distance to WUI from grid points
try:
    # set local variables
    in_features = mpb10km_pts
    near_features = WUI
    
    # find features only within search radius
    search_radius = "10000 Meters"
    
    # find location nearest features
    location = "NO_LOCATION"
    
    # avoid getting angle of neares features
    angle = "NO_ANGLE"
    
    # execute the function
    arcpy.Near_analysis(in_features, near_features, search_radius, location, angle)
    
    # get geoprocessing messages
    print(arcpy.GetMessages())

except arcpy.ExecuteError:
    print(arcpy.GetMessages(2))
    
except Exception as err:
    print(err.args[0])

