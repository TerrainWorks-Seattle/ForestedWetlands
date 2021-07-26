'''----------------------------------------------------------------------------------
 ToolBox Name: DEM Utilities
 Source Name: DEMutilities.pyt
 Version:     1.0.0
 Author:      Daniel Miller, 2017
 Tools:       Surface Metrics, Topographic Wetness Index
              
 Description: A set of tools for working with DEMs
----------------------------------------------------------------------------------'''
import arcpy
import importlib
import os
import subprocess
import time
import sys
from arcpy import sa

sys.path.insert(1, os.path.join(sys.path[0], '..'))
import netstream

importlib.reload(netstream)

class Toolbox(object):
    def __init__(self):
        """DEMutilities provides tools for DEM analyses."""
        self.label = "DEM Utilities"
        self.alias = "DEMutil"

        # List of tool classes associated with this toolbox
        self.tools = [SurfaceMetrics, TopographicWetnessIndex]

class SurfaceMetrics(object):
    def __init__(self):
        """"------------------------------------------------------------------------------------------------
        Tool Name: Surface Metrics
        Version: 1.0.0, python 3.6.6, ArcGIS Pro
        Author: Dan Miller, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description: 
        -------------------------------------------------------------------------------------------------"""
        self.label = "Surface Metrics"
        self.description = "Calculate elevation derivatives"
        self.canRunInBackground = False
#------------------------------------------------------------------------------------------------------------
    def getParameterInfo(self):
        """Define parameter definitions""" 
        
        param0 = arcpy.Parameter(
            displayName="Input DEM",
            name="DEM",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input") 
        
        param1 = arcpy.Parameter(
            displayName = "Length Scale (m)",
            name = "Length Scale",
            datatype = "String",
            parameterType = "Required",
            direction = "Input")
        
        param2 = arcpy.Parameter(
            displayName = "Create Gradient Raster",
            name = "Gradient",
            datatype = "GPBoolean",
            parameterType = "Optional",
            direction = "Input")
       
        param3 = arcpy.Parameter(
            displayName = "Create Plan Curvature Raster",
            name = "PlanCurv",
            datatype = "GPBoolean",
            parameterType = "Optional",
            direction = "Input")
        
        param4 = arcpy.Parameter(
            displayName = "Create Profile Curvature Raster",
            name = "ProfCurv",
            datatype = "GPBoolean",
            parameterType = "Optional",
            direction = "Input")    
        
        param5 = arcpy.Parameter(
            displayName = "Create Local Relief (DEV) Raster",
            name = "ElevDev",
            datatype = "GPBoolean",
            parameterType = "Optional",
            direction = "Input")
        
        param6 = arcpy.Parameter(
            displayName = "Resampling interval (DEM cells)",
            name = "downSample",
            datatype = "String",
            parameterType = "Optional",
            direction = "Input",
            enabled = False)
        
        param7 = arcpy.Parameter(
            displayName = "Sampling interval (resampled DEM cells)",
            name = "sampleInterval",
            datatype = "String",
            parameterType = "Optional",
            direction = "Input",
            enabled = False)
        
        param8 = arcpy.Parameter(
            name = 'Scratch',
            displayName = 'Scratch Folder',
            datatype = 'DEFolder',
            parameterType = 'Optional',
            direction = 'Input')
        
        param9 = arcpy.Parameter(
            name = 'executablePath',
            displayName = 'Path to executable files',
            datatype = 'DEFolder',
            parameterType = 'Optional',
            direction = 'Input',
            enabled = False)
        
        params = [param0, param1, param2, param3, param4, param5, param6, param7, param8, param9]
    #   0 = DEM
    #   1 = LengthScale
    #   2 = Gradient
    #   3 = Plan Curvature
    #   4 = Profile Curvature
    #   5 = Local elevation deviation
    #   6 = Down sampling
    #   7 = Sampling interval
    #   8 = Scratch directory
    #   9 = executable Path
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        if parameters[5].value:
            parameters[6].enabled = True
            parameters[7].enabled = True
        else:
            parameters[6].enabled = False
            parameters[7].enabled = False

        if parameters[0].value or parameters[8].value:
            if parameters[8].value:
                scratchDir = parameters[8].valueAsText
            elif parameters[0].value:
                scratchDir = arcpy.Describe(parameters[0].value).path

            # Check that MakeGrids.exe is in the scratch directory
            foundMakeGrids = True
            if parameters[2].value or parameters[3].value or parameters[4].value:
                foundMakeGrids = findFile("MakeGrids.exe", scratchDir)
            # If local elevation deviation is requested, check for LocalRelief.exe
            foundLocalRelief = True
            if parameters[5].value:
                foundLocalRelief = findFile("LocalRelief.exe", scratchDir)
            
            if not foundMakeGrids or not foundLocalRelief:
                parameters[9].enabled = True
            else:
                parameters[9].enabled = False
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        # Generate error message if exe path doesn't contain proper files or is expected and missing,
        # otherwise clear any errors
        errormsg = ""
        if parameters[9].enabled:
            exedir = parameters[9].valueAsText
            if exedir:
                if parameters[2].value or parameters[3].value or parameters[4].value:
                    # Check if makegrids is in scratch directory
                    foundMakeGrids = findFile("MakeGrids.exe", exedir)
                    if not foundMakeGrids:
                        errormsg += "Unable to find MakeGrids.exe in " + exedir + "\n"
                
                if parameters[5].value:
                    # Check if localrelief is in scratch directory
                    foundLocalRelief = findFile("LocalRelief.exe", exedir)
                    if not foundLocalRelief:
                        errormsg += "Unable to find LocalRelief.exe in " + exedir + "\n"
            else:
                errormsg += "Unable to locate both MakeGrids.exe and LocalRelief.exe in scratch location.\n"
            if len(errormsg) > 0:
                    errormsg += "Enter a valid path to the files.\n"
                    parameters[9].setErrorMessage(errormsg)
            else:
                parameters[9].clearMessage()
        else:
            parameters[9].clearMessage()
        return
                
    def execute(self, parameters, messages):
        """The source code of the tool."""
        DEM = parameters[0].valueAsText
        descDEM = arcpy.Describe(parameters[0].value)
        printDEMInfo(descDEM, messages)

        length = parameters[1].valueAsText
        makeGrad = parameters[2].value
        makePlan = parameters[3].value
        makeProf = parameters[4].value
        makeDev = parameters[5].value
        resample = parameters[6].value
        interval = parameters[7].value
        
        if parameters[8].value:
            scratchPath = parameters[8].valueAsText.strip(" ")
        else:
            scratchPath = descDEM.path
        if not scratchPath.endswith("\\"):
            scratchPath = scratchPath + "\\"
        messages.addMessage("Scratch folder: " + scratchPath)
        
        if descDEM.extension != "flt":
            DEM = convertRasterToFlt(descDEM, scratchPath)
        
        if parameters[9].enabled and parameters[9].value:
            executablePath = parameters[9].ValueAsText
        else:
            executablePath = os.getcwd

        adjustedLength = convertLength(length, descDEM, messages)

        requestedRasters = makeRasterList(
            descDEM, 
            length, 
            grad=makeGrad, 
            plan=makePlan, 
            prof=makeProf, 
            dev=makeDev,
            messages=messages
        )

        # Create input file and run makegrids
        command = executablePath + ("" if executablePath.endswith("\\") else "\\") + "makegrids"
        inputfilename = writeInputFileMG(DEM, adjustedLength, requestedRasters, scratchPath, command, self.label)
        try:
            subprocess.call([command,inputfilename])
        except OSError:
            messages.addErrorMessage('MakeGrids failed')
        

        if "Dev" in requestedRasters:
            inputfilename = str(scratchPath) + "input_localRelief.txt"
            inputfile = open(inputfilename, 'w')
            inputfile.write("# Input file for LocalRelief\n")
            inputfile.write("#    Created by ArcGIS python tool +" + self.label + "\n")
            inputfile.write("#      on " + time.asctime() + "\n")
            if DEM.index(".flt") > 1:
                inputfile.write("DEM: " + DEM[:-4] + "\n")
            else:
                inputfile.write("DEM: " + DEM + "\n")
            inputfile.write("SCRATCH DIRECTORY: " + scratchPath + "\n")
            radius = float(adjustedLength)/2.
            inputfile.write("RADIUS: " + str(radius) + "\n")
            if resample:
                inputfile.write("DOWN SAMPLE: " + resample + "\n")
            if interval:
                inputfile.write("SAMPLE INTERVAL: " + resample + "\n")
            inputfile.write("OUTPUT DEV RASTER: " + requestedRasters["Dev"] + "\n")
        
            inputfile.close()     
            
            command = executablePath + ("" if executablePath.endswith("\\") else "\\") + "localRelief"
 
            try:
                subprocess.run([command, inputfilename])
            except OSError:
                messages.addErrorMessage('LocalRelief failed')

        # Reformat .flt rasters as .tif
        for key in requestedRasters:
            rasterPath = requestedRasters[key]
            r = arcpy.Raster(rasterPath + '.flt')
            r.save(rasterPath + '.tif')
            os.remove(rasterPath + '.flt')
            
        return

class TopographicWetnessIndex(object):
    def __init__(self):
        """"------------------------------------------------------------------------------------------------
        Tool Name: Topographic Wetness Index
        Version: 1.0.0, python 3.6.6, ArcGIS Pro
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description: 
        -------------------------------------------------------------------------------------------------"""
        self.label = "Topographic Wetness Index"
        self.description = "Calculate topographic wetness index"
        self.canRunInBackground = False
#------------------------------------------------------------------------------------------------------------
    def getParameterInfo(self):
        """Define parameter definitions""" 
        
        param0 = arcpy.Parameter(
            displayName="Input DEM",
            name="DEM",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input"
        ) 
        
        param1 = arcpy.Parameter(
            displayName = "Length Scale (m)",
            name = "Length Scale",
            datatype = "String",
            parameterType = "Required",
            direction = "Input"
        )
        
        param2 = arcpy.Parameter(
            displayName = "Existing gradient raster",
            name = "grad",
            datatype = "DERasterDataset",
            parameterType = "Optional",
            direction = "Input"
        )

        param3 = arcpy.Parameter(
            displayName = "Existing plan curvature raster",
            name = "plan",
            datatype = "DERasterDataset",
            parameterType = "Optional",
            direction = "Input"
        )

        param4 = arcpy.Parameter(
            displayName = "Existing B contour raster",
            name = "bcon",
            datatype = "DERasterDataset",
            parameterType = "Optional",
            direction = "Input"
        )

        param5 = arcpy.Parameter(
            name = 'Scratch',
            displayName = 'Scratch Folder',
            datatype = 'DEFolder',
            parameterType = 'Optional',
            direction = 'Input'
        )
        
        param6 = arcpy.Parameter(
            name = 'executablePath',
            displayName = 'Path to executable files',
            datatype = 'DEFolder',
            parameterType = 'Optional',
            direction = 'Input',
            enabled = False
        )
        
        params = [param0, param1, param2, param3, param4, param5, param6]
    #   0 = DEM
    #   1 = LengthScale
    #   2 = Gradient raster
    #   3 = Plan Curvature raster
    #   4 = B Contour raster
    #   5 = Scratch directory
    #   6 = executable Path
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        if parameters[0].value or parameters[5].value:
            if parameters[5].value:
                scratchDir = parameters[5].valueAsText
            elif parameters[0].value:
                scratchDir = arcpy.Describe(parameters[0].value).path

            # Check if makegrids is in scratch directory
            foundMakeGrids = findFile("MakeGrids.exe", scratchDir)
            
            if not foundMakeGrids:
                parameters[6].enabled = True
            else:
                parameters[6].enabled = False        
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        # Generate error message if exe path doesn't contain proper files or is expected and missing,
        # otherwise clear any errors
        errormsg = ""
        if parameters[6].enabled:
            exedir = parameters[6].valueAsText
            if exedir:
                # Check if makegrids is in scratch directory
                foundMakeGrids = findFile("MakeGrids.exe", exedir)
                if not foundMakeGrids:
                    errormsg += "Unable to find MakeGrids.exe in " + exedir + "\n"

                foundBuildGrids = findFile("BuildGrids.exe", exedir)
                if not foundBuildGrids:
                    errormsg += "Unable to find BuildGrids.exe in " + exedir + "\n"
            else:
                errormsg += "Unable to find both MakeGrids.exe and BuildGrids.exe in scratch location\n"
            if len(errormsg) > 0:
                    errormsg += "Enter a valid path to the files.\n"
                    parameters[6].setErrorMessage(errormsg)
            else:
                parameters[6].clearMessage()
        else:
            parameters[6].clearMessage()
        return           
                
    def execute(self, parameters, messages):
        """The source code of the tool."""
        DEM = parameters[0].valueAsText
        descDEM = arcpy.Describe(parameters[0].value)
        printDEMInfo(descDEM, messages)
        name = descDEM.basename
        DEMID = name[name.rfind('_')+1:len(name)]
        DEMunits = "m" if descDEM.spatialReference.metersPerUnit == 1.0 else "f"

        length = parameters[1].valueAsText
        grad = parameters[2].valueAsText
        plan = parameters[3].valueAsText
        bcon = parameters[4].valueAsText

        if parameters[5].value:
            scratchPath = parameters[5].valueAsText.strip(" ")
        else:
            scratchPath = descDEM.path
        if not scratchPath.endswith("\\"):
            scratchPath = scratchPath + "\\"
        messages.addMessage("Scratch folder: " + scratchPath)
        if descDEM.extension != "flt":
            DEM = convertRasterToFlt(descDEM, scratchPath)
        
        if parameters[6].enabled and parameters[6].value:
            executablePath = parameters[6].ValueAsText
        else:
            executablePath = os.getcwd

        adjustedLength = convertLength(length, descDEM, messages)

        rasters = makeRasterList(
            descDEM, 
            length, 
            grad=(grad is None),
            plan=(plan is None),
            bcon=(bcon is None),
            messages=messages
        )

        if len(rasters) > 0:
            # Create input file and run makegrids
            command = executablePath + ("" if executablePath.endswith("\\") else "\\") + "makegrids"
            inputfilename = writeInputFileMG(DEM, adjustedLength, rasters, scratchPath, command, self.label)
            try:
                subprocess.call([command,inputfilename])
            except OSError:
                messages.addErrorMessage('MakeGrids failed')
        
        inputfilename = str(scratchPath) + "input_buildGrids.txt"
        inputfile = open(inputfilename, 'w')
        inputfile.write("# Input file for BuildGrids\n")
        inputfile.write("#    Created by ArcGIS python tool +" + self.label + "\n")
        inputfile.write("#      on " + time.asctime() + "\n")
        if DEM.index(".flt") > 1:
            inputfile.write("DEM: " + DEM[:-4] + "\n")
        else:
            inputfile.write("DEM: " + DEM + "\n")
        inputfile.write("DEMID: " + DEMID + "\n")
        inputfile.write("DEM UNITS: " + DEMunits + "\n")
        inputfile.write("LENGTH SCALE: " + length + "\n")
        inputfile.write("SCRATCH: " + scratchPath + "\n")
        inputfile.write("AREA SLOPE THRESHOLD LOW GRADIENT: 30.\n")
        inputfile.write("AREA SLOPE THRESHOLD HIGH GRADIENT: 60.\n")
        inputfile.write("PLAN CURVATURE THRESHOLD LOW GRADIENT: 100000.15\n")
        inputfile.write("PLAN CURVATURE THRESHOLD HIGH GRADIENT: 100000.3\n")
        gradpath = grad if grad else rasters.get("Grad") + ".flt"
        inputfile.write("GRADIENT FILE: " + gradpath + "\n")
        planpath = plan if plan else rasters.get("Plan") + ".flt"
        inputfile.write("PLAN CURVATURE FILE: " + planpath + "\n")
        bconpath = bcon if bcon else rasters.get("Bcon") + ".flt"
        inputfile.write("BCON FILE: " + bconpath + "\n")

        inputfile.close()
        
        command = executablePath + ("" if executablePath.endswith("\\") else "\\") + "buildgrids"
        try:
            subprocess.run([command, inputfilename])
        except OSError:
            messages.addErrorMessage('BuildGrids failed')
            raise

        accpath = scratchPath + "accum_" + DEMID + ".flt"
        TWI = sa.Ln(sa.Divide(accpath, gradpath))
        out_path = descDEM.path + "\\twi_" + length + ".tif"
        TWI.save(out_path)
        return

#############################################################################
####### Helper functions ####################################################
#############################################################################

# Print DEM info messages
# In: DEM description
# Out: DEM path
def printDEMInfo(desc, messages):
    workingDir = os.getcwd()
    filepath = desc.path + "\\" + desc.name
    messages.addMessage("Working directory: " + workingDir)
    messages.addMessage("DEM: " + filepath)
    messages.addMessage("..datatype: " + desc.dataType)
    if len(desc.extension) > 0:
        messages.addMessage("..extension: " + desc.extension)
    return filepath

# Convert raster to .flt
# In: DEM description, scratch path
# Out: Converted DEM
def convertRasterToFlt(desc, scratchPath):
    inraster = desc.path + "\\" + desc.name
    outraster = scratchPath + desc.basename + ".flt"
    if not netstream.isflt(outraster):
        arcpy.RasterToFloat_conversion(inraster, outraster)
    return outraster

# Convert input length to raster's units
# In: length scale, DEM description, {messages}
# Out: Converted length scale
def convertLength(length, desc, messages=None):
    sr = desc.spatialReference
    cellSize = desc.children[0].meanCellHeight
    unitScale = sr.metersPerUnit
    unitName = sr.linearUnitName
    adjustedLength = float(length)/(unitScale)
    if messages:
        messages.addMessage("Length scale (m): " + length)
        messages.addMessage("Conversion rate (m/DEM unit): " + str(unitScale))
        messages.addMessage("Adjusted length scale (" + unitName + "): " + str(adjustedLength))
    return adjustedLength

# Create list of requested raster filepaths
# In: DEM description, length scale, {make path to: grad, plan, prof, bcon, dev rasters}, {resample}, {interval}
# Out: List of paths to rasters and input for dev raster
def makeRasterList(desc, length, grad=False, plan=False, prof=False, bcon=False, dev=False, messages=None):
    rasters = {}
    
    if grad:
        gradRaster = desc.path + '\\grad_' + length
        rasters["Grad"] = gradRaster
        if messages:
            messages.addMessage("Creating gradient raster")
            messages.addMessage("  " + gradRaster)
        
    if plan:
        planRaster = desc.path + '\\plan_' + length
        rasters["Plan"] = planRaster
        if messages:
            messages.addMessage("Creating plan curvature raster")
            messages.addMessage("  " + planRaster)
    
    if prof:
        profRaster = desc.path + '\\prof_' + length
        rasters["Prof"] = profRaster
        if messages:
            messages.addMessage("Creating profile curvature raster")
            messages.addMessage("  " + profRaster)
    
    if bcon:
        bconRaster = desc.path + '\\bcon_' + length
        rasters["Bcon"] = bconRaster
        if messages:
            messages.addMessage("Creating BContour raster")
            messages.addMessage("  " + bconRaster)

    if dev:
        devRaster = desc.path + '\\dev_' + length
        rasters["Dev"] = devRaster
        if messages:
            messages.addMessage("Creating local deviation raster")
            messages.addMessage("  " + devRaster)
    return rasters

# Create the MakeGrids input file
# In: DEM, length scale, raster path list, scratch directory, full path of the MakeGrids executable, {toolname}
# Out: the input file name used when calling MakeGrids
def writeInputFileMG(DEM, length, rasters, scratchPath, executable, toolName=None):
    inputfilename = scratchPath + "input_makeGrids.txt"
    inputfile = open(inputfilename, 'w')
    inputfile.write("# Input file for MakeGrids\n")
    if toolName:
        inputfile.write("#    Created by ArcGIS python tool " + toolName + "\n")
    inputfile.write("#    on " + time.asctime() + "\n")
    if DEM.index(".flt") > 1:
        inputfile.write("DEM: " + DEM[:-4] + "\n")
    else:
        inputfile.write("DEM: " + DEM + "\n")
    inputfile.write("SCRATCH DIRECTORY: " + scratchPath + "\n")
    inputfile.write("LENGTH SCALE: " + str(length) + "\n")

    if "Grad" in rasters:
        inputfile.write("GRID: GRADIENT, OUTPUT FILE = " + rasters.get("Grad") + "\n")
            
    if "Plan" in rasters:
        inputfile.write("GRID: PLAN CURVATURE, OUTPUT FILE = " + rasters.get("Plan") + "\n")
            
    if "Prof" in rasters:
        inputfile.write("GRID: PROFILE CURVATURE, OUTPUT FILE = " + rasters.get("Prof") + "\n")
    
    if "Bcon" in rasters:
        inputfile.write("GRID: BCONTOUR, OUTPUT FILE = " + rasters.get("Bcon") + "\n")

    inputfile.write(executable + "\n")
    inputfile.write(inputfilename)
    inputfile.close()
    return inputfilename

# Checks that the file is in the specified directory
# In: name of the file, path to the directory
# Out: True if the file was found, False otherwise
def findFile(filename, directory):
    foundFile = False
    try:
        with os.scandir(directory) as entries:
            for entry in entries:
                if entry.is_file():

                    if entry.name == filename:
                        foundFile = True
                        break
    except FileNotFoundError:
        pass
    finally:
        return foundFile

