'''------------------------------------------------------------------
 Module Name: Netstream
 Source Name: netstream.py
 Version:     netstream 1.0.0
 Author:      Daniel Miller
 Description: A collection of functions for running the netstream 
              suite of programs in ArcGIS using python scripts.
---------------------------------------------------------------------'''
# Created February 16, 2015
# Last edited September 13, 2017
# Copyright (c) Daniel Miller 2015
#-----------------------------------------------------------------------------------
import os
import sys
import arcpy
import subprocess

#=========================================================================
def getpath(infilename):
    """Find the path from a filename
         Required argument: input file name, including full path
         Returns string with the path"""
    iloc1 = infilename.rfind("\\") + 1
    return infilename[:iloc1]  # Path to reference DEM and other files
#===========================================================================
def getid(infilename):
    """Get the data ID from the name of a reference file"""
    iloc1 = infilename.rfind("_") + 1
    iloc2 = infilename.rfind(".")
    return infilename[iloc1:iloc2]
#==========================================================================
def getname(infilename,ref_id):
    """Get the netstream file name from the full-path name"""
    iloc1 = infilename.rfind("\\") + 1
    iloc2 = infilename.rfind(ref_id)
    if iloc1 > -1:
        return infilename[iloc1:iloc2]
    else:
        return infilename[:]    
#=============================================================    
def program_return_status(scratch,program_name):
    """Check return status of program"""
    checkfilename = scratch + program_name + ".txt"
    try:
        checkfile = open(checkfilename, 'r')
        returncode = checkfile.readline()
        if returncode == "":
            return -1
        else:
          return int(returncode)
    except:
        return -1
    checkfile.close()
#=============================================================================
def isflt(inraster):
    """Return True if infile exists as a flt raster, False otherwise"""
    try:
        raster_prop = arcpy.Describe(inraster)
        if raster_prop.format == "FLT":
            return True
        else:
            if arcpy.Exists(inraster + ".flt"):
                return True
            else:
                return False
    except:
        return False
#=====================================================================================
def getflt(inraster):
    """Check type of inraster, convert to .flt if needed"""
    raster_prop = arcpy.Describe(inraster)
    if raster_prop.format == "FLT":
        return inraster
    else:   
        outflt = inraster + ".flt"
        if arcpy.Exists(outflt) == True:
            return outflt
        else:
            try:
                outflt = inraster + ".flt"
                arcpy.RasterToFloat_conversion(inraster, outflt)
                return outflt
            except:
                return ""
#===============================================================================
def delete_flt(infile):
    """Delete specified float raster file"""  
    if os.path.isfile(infile + ".flt"):
        try:
            os.remove(infile + ".flt")
        except:
            print("Unable to delete file: " + infile + ".flt")
    if os.path.isfile(infile + ".hdr"):
        try:
            os.remove(infile + ".hdr")
        except:
            print("Unable to delete file: " + infile + ".flt")
    if os.path.isfile(infile + ".prj"):
        try:
            os.remove(infile + ".prj")
        except:
            print("Unable to delete file: " + infile + ".prj")
    if os.path.isfile(infile + ".flt.xml"):
        try:
            os.remove(infile + ".flt.xml")
        except:
            print("Unable to delete file: " + infile + ".flt.xml")
    if os.path.isfile(infile + ".log"):
        try:
            os.remove(infile + ".log")
        except:
            print("Unable to delete file: " + infile + ".log")
#================================================================================  
def is_shp(infile):
    """Return True if infile exists as a shapefile, False otherwise"""
    desc = arcpy.Describe(infile)
    if desc.datatype == "ShapeFile":
        return True
    else:
        if arcpy.Exists(infile + ".shp") == True:
            return True
        else:
            return False
#================================================================================
def getshapefile(infile):
    """Check type of input feature class, convert to shapefile if needed"""
    desc = arcpy.Describe(infile)
    if desc.datatype == "ShapeFile":
        return infile
    else:
        outshapefile = infile + ".shp"
        if arcpy.Exists(outshapefile) == True:
            return outshapefile
        else:
            try:
                outshapefile = infile + ".shp"
                arcpy.FeatureClassToShapefile_conversion(infile, outshapefile)
                return outshapefile
            except arcpy.ExecuteError:
                print (arcpy.GetMessages())
                return ""
#==============================================================================
def getparameterext(ref_path):
    """Check for presence of netrace parameter file in ref_path, return the extention"""
    allfiles = os.listdir(ref_path)
    for filename in allfiles:
        if filename.find("Netrace_parameters") >= 0:
            iloc1 = filename.rfind(".") + 1
            return filename[iloc1:]
    for filename in allfiles:
        if filename.find("netrace_parameters") >= 0:
            iloc1 = filename.rfind(".") + 1
            return filename[iloc1:]    
#====================================================================================
def kill(kill_list):
    """Delete all files contained in kill_list"""
    for file in kill_list:
        success = False
        if arcpy.Exists(file) == True:
            try:
                arcpy.Delete_management(file)
                success = True
            except arcpy.ExecuteError:
                print (arcpy.GetMessages())
        if not success:
            if os.path.isfile(file) == True:
                try:
                    os.remove(file)
                except:
                    print ("Unable to delet file" + file)
#====================================================================================
def clipnet(refDEM, outpath, basins, basin_id, rasterlist, scratch, netmap_path):
    """Build a clipnet input file and run clipnet.exe"""

    import time
    
    refpath = getpath(refDEM)
    if len(scratch) == 0:
        scratch = refpath
        
    if len(netmap_path) == 0:
        netmap_path = "c:\\work\\netmap_files"
    
    if refDEM[-3:] == "flt":
        dem = refDEM
    else:
        dem = getflt(refDEM) 
        if len(dem) == 0:
            return -1 # error code
    
    # Create the input file for clipNet
    outfilename = scratch + "clipnetinput.txt"
    outfile = open(outfilename,'w')
    outfile.write("# Input file for ClipNet\n")
    outfile.write("#   Created by ArcGIS using clipNet.py\n")
    outfile.write("#   on " + time.asctime() + "\n")
    outfile.write("REFERENCE DEM: " + dem + "\n")
    outfile.write("SCRATCH PATH:" + scratch + "\n")
    outfile.write("OUTPUT PATH: " + outpath + "\n")
    outfile.write("BASIN POLYGON SHAPEFILE: FILE = " + basins + ", ID FIELD = " + basin_id + "\n")
    if len(rasterlist) > 0:
        this_string = "RASTERS TO CLIP: "
        for rastername in rasterlist:
            iloc1 = rastername.rfind("\\") + 1
            iloc2 = rastername.rfind("_")   
            new_string = this_string + rastername[iloc1:iloc2] + ", "
            this_string = new_string
        iloc1 = len(new_string) - 2
        this_string = new_string[:iloc1] + "\n"
        outfile.write(this_string)
    outfile.close()

    command = netmap_path +  "clipNet.exe " + outfilename 
    os.system(command)
    
    return program_return_status(scratch,'clipnet')
#===================================================================================
def netrace(outpath, ref_path, this_id, param_ext, precipfile, scratch, netmap_path, param_path):
    """Build a netrace input file and run netrace.exe"""
   
    import time
    
    if len(scratch) == 0:
        scratch = ref_path
        
    if len(netmap_path) == 0:
        netmap_path = "c:\\work\\netmap_files"
        
    inputfilename = scratch + "netraceinput.txt"
    inputfile = open(inputfilename, "w")
    inputfile.write("# Input file for netrace\n")
    inputfile.write("#   created by ArcGIS using bigDWMbuilder.py\n")
    inputfile.write("#   on " + time.asctime() + "\n")
    demfile = ref_path + "elev_" + this_id
    outreach = outpath + "reach_" + this_id
    inputfile.write("DEM FILE: " + demfile + "\n")
    inputfile.write("OUTPUT REACH SHAPEFILE: " + outreach + "\n")
    inputfile.write("REACH LENGTH: FIXED LENGTH = 100., FILTER = 30. \n")
    inputfile.write("PARAMETER FILE PATH: " + param_path + "\n")
    inputfile.write("PARAMETER FILE EXTENSION: " + param_ext + "\n")
    inputfile.write("SCRATCH PATH:" + scratch + "\n")
    inputfile.write("ATTRIBUTE LIST: \n")
    inputfile.write("REACH ID: FIELD NAME = ID, OUTNODE = NO \n")
    inputfile.write("MEAN ANNUAL PRECIPITATION: FILE = " + precipfile + ", OUTPUT FIELD = PRECIP_M, UNITS = mm\n")
    inputfile.write("MEAN ANNUAL FLOW: OUTPUT FIELD = MEANANNCMS\n")
    inputfile.write("WIDTH:\n")
    inputfile.write("END LIST:\n")
    inputfile.close()

    command = netmap_path + "netrace.exe " + inputfilename
    os.system(command)
    
    return program_return_status(scratch, 'netrace')

#===================================================================================
def netrace2(ref_ID, reference_DEM, outpath, basins, basin_id, rasterlist, scratch, netmap_path, ref_path, param_ext, precipfile, param_path):#(outpath, ref_path, this_id, param_ext, precipfile, scratch, netmap_path, param_path):
    """Build a netrace input file and run netrace2.exe - includes clipnet. Replaces netrace.exe and clipnet"""

    import time

    #refpath = getpath(refDEM)
    #if len(scratch) == 0:
        #scratch = refpath

    if len(netmap_path) == 0:
        netmap_path = "c:\\work\\netmap_files"


    if len(netmap_path) == 0:
        netmap_path = "c:\\work\\netmap_files"

    inputfilename = scratch + "netrace2input.txt"
    inputfile = open(inputfilename, "w")
    inputfile.write("# Input file for netrace2\n")
    inputfile.write("#   created by ArcGIS using bigDWMbuilder.py\n")
    inputfile.write("#   on " + time.asctime() + "\n")
    outreach = outpath + "reach_" + ref_ID
    inputfile.write("DEM FILE: " + reference_DEM + "\n")
    inputfile.write("OUTPUT REACH SHAPEFILE: " + outreach + "\n")
    inputfile.write("REACH LENGTH: FIXED LENGTH = 100., FILTER = 30. \n")
    inputfile.write("PARAMETER FILE PATH: " + param_path + "\n")
    inputfile.write("PARAMETER FILE EXTENSION: " + param_ext + "\n")
    inputfile.write("SCRATCH PATH:" + scratch + "\n")

    inputfile.write("#-------------------------------------------------------" + "\n")
    inputfile.write("# Clipnet portion" + "\n")
    inputfile.write("#-------------------------------------------------------" + "\n")
    inputfile.write("OUTPUT PATH: " + scratch + "\n")
    if len(rasterlist) > 0:
        this_string = "RASTERS TO CLIP: "
        for rastername in rasterlist:
            iloc1 = rastername.rfind("\\") + 1
            iloc2 = rastername.rfind("_")   
            new_string = this_string + rastername[iloc1:iloc2] + ", "
            this_string = new_string
        iloc1 = len(new_string) - 2
        this_string = new_string[:iloc1] + "\n"
        inputfile.write(this_string)    
    
    inputfile.write("ATTRIBUTE LIST: \n")
    inputfile.write("REACH ID: FIELD NAME = ID, OUTNODE = NO \n")
    inputfile.write("CONTRIBUTING AREA: \n")
    inputfile.write("MEAN ANNUAL PRECIPITATION: FILE = " + precipfile + ", OUTPUT FIELD = PRECIP_M, UNITS = mm\n")
    inputfile.write("MEAN ANNUAL FLOW: OUTPUT FIELD = MEANANNCMS\n")
    inputfile.write("WIDTH:\n")
    inputfile.write("BASIN: FILE= " + basins + ", ID FIELD = " + basin_id + ", OUTPUT FIELD = DATASET, NODATA OUTSIDE = YES, ELEVATION CUTOFF = 5.0\n") 
    inputfile.write("END LIST:\n")
    inputfile.close()

    command = netmap_path + "netrace2.exe " + inputfilename
    os.system(command)

    return program_return_status(scratch, 'netrace')
#=============================================================================================
def freqRatio(DEM,scratch,LSpoly,LSfield,LSraster,LSvalue,rasterList,susceptibilityRaster,
        probabilityRaster,numProbBins,freqCSV,probCSV,randomCSV,maskRaster,bySum,executablePath):
    """Build an input file and execute Frequency Ratio"""
    
    import time
    
    if len(scratch) == 0:
        scratch = getPath(DEM)
    if not scratch.endswith("\\"):
        holdscratch = scratch
        scratch = holdscratch + "\\"
        
    inputfilename = str(scratch) + "\\input_freqRatio.txt"
    inputfile = open(inputfilename, 'w')
    inputfile.write("# Input file for Frequency Ratio\n")
    inputfile.write("#    Created by ArcGIS python script freqRatio\n")
    inputfile.write("#      on " + time.asctime() + "\n")
    inputfile.write("DEM: " + DEM + "\n")
    inputfile.write("SCRATCH DIRECTORY: " + scratch + "\n")
    
    if len(LSpoly) > 0:
        inputfile.write("POLYGON FILE: " + LSpoly + ", VALUE = " + LSvalue + ", ID FIELD = " + LSfield + "\n")
    else:
        if len(LSvalue) > 0:
            inputfile.write("LANDSLIDE RASTER: " + LSraster + ", VALUE = " + LSvalue + "\n")
        else:
            inputfile.write("LANDSLIDE RASTER: " + LSraster + "\n")
    
    for row in rasterList:
        raster = row[0]
        nbins = row[1]
        window = row[2]
        polyOrder = row[3]
        recurring = row[4]
        inputfile.write("INPUT RASTER: " + raster + ", NBINS = " + nbins + ", WINDOW = " + 
                        window + ", POLY ORDER = " + polyOrder + ", CIRCLE = " + recurring + "\n")
        
    if len(maskRaster) > 0:
        inputfile.write("MASK RASTER: " + maskRaster + "\n")
    
    if len(susceptibilityRaster) > 0:
        if bySum.find('True') > -1:
            inputfile.write("OUTPUT SUSCEPTIBILITY RASTER: " + susceptibilityRaster + ", SUM\n")
        else:
            inputfile.write("OUTPUT SUSCEPTIBILITY RASTER: " + susceptibilityRaster + ", PRODUCT\n")
                        
    if len(probabilityRaster) > 0:
        inputfile.write("OUTPUT PROBABILITY RASTER: " + probabilityRaster + 
                        ", NBINS = " + numProbBins + "\n")
    
    if len(freqCSV) > 0:
        inputfile.write("OUTPUT CSV: " + freqCSV + "\n")
        
    if len(probCSV) > 0:
        inputfile.write("OUTPUT PROB CSV: " + probCSV + "\n")
        
    if len(randomCSV) > 0:
        inputfile.write("OUTPUT RAND CSV: " + randomCSV + "\n")
        
    inputfile.write("EXECUTABLE PATH: " + executablePath)
        
    inputfile.close()
    
    if executablePath.endswith("\\"):
        command = executablePath + "freqRatio " + scratch + "input_freqRatio.txt"
    else:
        command = executablePath + "\\" + "freqRatio " + scratch + "input_freqRatio.txt"

    os.system(command)
    
    return program_return_status(scratch, "FrequencyRatio")
    
#----------------------------------------------------------------------------------------------
def makeGrids(DEM,scratch,lengthScale,executablePath,rasters):
    """Build an input file and execute MakeGrids"""
    
    import time
    
    if len(scratch) == 0:
        scratch = getPath(DEM)
    if not scratch.endswith("\\"):
        holdscratch = scratch
        scratch = holdscratch + "\\"
        
    inputfilename = str(scratch) + "\\input_makeGrids.txt"
    inputfile = open(inputfilename, 'w')
    inputfile.write("# Input file for MakeGrids\n")
    inputfile.write("#    Created by ArcGIS python tool Surface Metrics\n")
    inputfile.write("#      on " + time.asctime() + "\n")
    if DEM.index(".flt") > 1:
        inputfile.write("DEM: " + DEM[:-4] + "\n")
    else:
        inputfile.write("DEM: " + DEM + "\n")
    inputfile.write("SCRATCH DIRECTORY: " + scratch + "\n")
    inputfile.write("LENGTH SCALE: " + lengthScale + "\n")
    
    if "Grad" in rasters:
        inputfile.write("GRID: GRADIENT, OUTPUT FILE = " +  rasters.get("Grad") + "\n")
        
    if "Plan" in rasters:
        inputfile.write("GRID: PLAN CURVATURE, OUTPUT FILE = " + rasters.get("Plan") + "\n")
        
    if "Prof" in rasters:
        inputfile.write("GRID: PROFILE CURVATURE, OUTPUT FILE = " + rasters.get("Prof") + "\n")
    
    if executablePath.endswith("\\"):
        command = executablePath + "makegrids " + scratch + "input_makeGrids.txt"
    else:
        command = executablePath + "\\" + "makegrids " + scratch + "input_makeGrids.txt"
    
    inputfile.write(command)
    inputfile.close()  
    
    try:
        subprocess.call([command])
    except OSError:
        print('error running ' + command)
    
    if "Dev" in rasters:
        inputfilename = str(scratch) + "\\input_localRelief.txt"
        inputfile = open(inputfilename, 'w')
        inputfile.write("# Input file for LocalRelief\n")
        inputfile.write("#    Created by ArcGIS python tool Surface Metrics\n")
        inputfile.write("#      on " + time.asctime() + "\n")
        if DEM.index(".flt") > 1:
            inputfile.write("DEM: " + DEM[:-4] + "\n")
        else:
            inputfile.write("DEM: " + DEM + "\n")
        inputfile.write("SCRATCH DIRECTORY: " + scratch + "\n")
        radius = float(lengthScale)/2.
        inputfile.write("RADIUS: " + str(radius) + "\n")
        devParam = rasters["Dev"]
        if "Resample" in devParam:
            inputfile.write("DOWN SAMPLE: " + devParam["Resample"] + "\n")
        if "Interval" in devParam:
            inputfile.write("SAMPLE INTERVAL: " + devParam["Interval"] + "\n")
  
        if executablePath.endswith("\\"):
           command = executablePath + "localRelief " + scratch + "input_localRelief.txt"
        else:
            command = executablePath + "\\" + "localRelief " + scratch + "input_localRelief.txt"
         
        inputfile.write(command)
        inputfile.close()      
        subprocess.run(command)        
    
    return program_return_status(scratch, "MakeGrids")
#------------------------------------------------------------------------------------------
def readInputFile(inputfilename):
    """Translate a standard netstream ascii input file to a dictionary with keywords as keys and arguments as items."""
    
    inputfile = open(inputfilename, "r")
    input = inputfile.readlines()
    inputfile.close()
    record = {}
    
    for line in input:
        rec = line.strip() # strip out all leading and trailing spaces
        iloc2 = rec.find("#") # check to see if first character is a comment indicator
        if iloc2 == 0: # comment
            continue
        iloc1 = rec.find(":") + 1 # a colon indicates end of keyword
        if iloc1 > 0: # found a keyword
            if iloc2 < 0:
                iloc2 = len(rec) + 1
            arg = rec[iloc1:iloc2]
            key = rec[:iloc1-1]
            n = 1
            while key in record:
                n = n + 1
                key = key + str(n)
            args = arg.split(",")
            argList = []
            for arg in args:
                thisArg = arg.replace("\'","")
                argList.append(thisArg)
            record[key] = argList
            
    return record