'''----------------------------------------------------------------------------------
 ToolBox Name: DEM Utilities
 Source Name: DEMutilities.pyt
 Version:     1.0.0
 Author:      Daniel Miller, 2017
 Tools:       Surface Metrics
              
 Description: A set of tools for working with DEMs
----------------------------------------------------------------------------------'''
import arcpy
import importlib
import os
import subprocess
import time
import sys

sys.path.insert(1, os.path.join(sys.path[0], '..'))
import netstream

importlib.reload(netstream)

class Toolbox(object):
    def __init__(self):
        """DEMutilities provides tools for DEM analyses."""
        self.label = "DEM Utilities"
        self.alias = "DEMutil"

        # List of tool classes associated with this toolbox
        self.tools = [SurfaceMetrics]

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
            enabled = True)
        
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

    #def isLicensed(self):
        #"""Set whether tool is licensed to execute."""
        #return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        
        if parameters[8].value:
            scratchDir = parameters[8].valueAsText
        else:
            scratchDir = os.getcwd()
        
    # If local elevation deviation is requested, get resampling parameters        
        if parameters[5].value:
            parameters[6].enabled = True
            parameters[7].enabled = True
        #  Also check to see that the executable file is where we think it is
        #    if not parameters[9].enabled:
        #        foundLocalRelief = False
        #        with os.scandir(scratchDir) as entries:
        #            for entry in entries:
        #                if entry.is_file():
        #                    if (entry.name.find("localrelief.exe") >= 0):
        #                        foundLocalRelief = True
        #        if not foundLocalRelief:
        #            parameters[9].enabled = True
            
        #if (parameters[2].value or parameters[3].value or parameters[4].value):
            # make sure we can find the makegrids.exe file
        #    if not parameters[9].enabled:
        #        foundMakeGrids = False
        #        with os.scandir(scratchDir) as entries:
        #            for entry in entries:
        #                if entry.is_file():
        #                    if (entry.name.find("makegrids.exe") >= 0):
        #                        foundMakeGrids = True
        #        if not foundMakeGrids:
        #            parameters[9].enabled = True
                                
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
    
      #  if parameters[9].enabled:
      #      if (parameters[2].value or parameters[3].value or parameters[4].value):
      #          foundMakeGrids = False
      #          with os.scandir(parameters[9].ValueAsText) as entries:
      #              for entry in entries:
      #                  if (entry.name.find("MakeGrids.exe") >= 0):
      #                      foundMakeGrids = True
      #          if not foundMakeGrids:
      #              parameters[9].setErrorMessage("Cannot find executable files in this directory")
      #      if parameters[5].value:
      #          foundLocalRelief = False
      #          with os.scandir(parameters[9].ValueAsText) as entries:
      #              for entry in entries:
      #                  if (entry.name.find("LocalRelief.exe") >= 0):
      #                      foundLocalRelief = True
      #          if not foundLocalRelief:
      #              parameters[9].setErrorMessage("Cannot find executable files in this directory")
        return            
                
    def execute(self, parameters, messages):
        """The source code of the tool."""
        
        workingDir = os.getcwd()
        messages.addMessage("Working directory: " + workingDir)
        
        descDEM = arcpy.Describe(parameters[0].value)
        messages.addMessage("DEM: " + descDEM.path + "\\" + descDEM.name)
        messages.addMessage("..datatype: " + descDEM.dataType)
        if (len(descDEM.extension) > 0):
            messages.addMessage("..extension: " + descDEM.extension)
        DEM = descDEM.path + "\\" + descDEM.name
        
        if parameters[8].value:
            scratchPath = parameters[8].valueAsText.strip(" ")
        else:
            scratchPath = descDEM.path
        
        if not scratchPath.endswith("\\"):
            scratchPath = scratchPath + "\\"
            
        messages.addMessage("Scratch folder: " + scratchPath)
        
        killRasterList = []        

        if descDEM.extension != "flt":
            inraster = descDEM.path + "\\" + descDEM.name
            outraster = scratchPath + descDEM.basename + ".flt"
            if not netstream.isflt(outraster):
                arcpy.RasterToFloat_conversion(inraster, outraster)
                killRasterList.append(outraster)
            DEM = outraster
            
        if parameters[1].value:
            length = parameters[1].valueAsText
            
        messages.addMessage("Length scale (m): " + length)
            
        rasters = {}
        
        if parameters[2].value:
            gradRaster = descDEM.path + '\\grad_' + length
            rasters["Grad"] = gradRaster
            messages.addMessage("Creating gradient raster")
            messages.addMessage("  " + gradRaster)
            
        if parameters[3].value:
            planRaster = descDEM.path + '\\plan_' + length
            rasters["Plan"] = planRaster
            messages.addMessage("Creating plan curvature raster")
            messages.addMessage("  " + planRaster)
        
        if parameters[4].value:
            profRaster = descDEM.path + '\\prof_' + length
            rasters["Prof"] = profRaster
            messages.addMessage("Creating profile curvature raster")
            messages.addMessage("  " + profRaster)
            
        if parameters[5].value:
            devRaster = descDEM.path + '\\dev_' + length
            devParams = {}
            devParams["Raster"] = devRaster
            if parameters[6].value:
                devParams["Resample"] = parameters[6].ValueAsText
            if parameters[7].value:
                devParams["Interval"] = parameters[7].ValueAsText
            rasters["Dev"] = devParams
            
        if parameters[9].value:
            executablePath = parameters[9].ValueAsText
        else:
            executablePath = workingDir
                
        inputfilename = str(scratchPath) + "\\input_makeGrids.txt"
        inputfile = open(inputfilename, 'w')
        inputfile.write("# Input file for MakeGrids\n")
        inputfile.write("#    Created by ArcGIS python tool Surface Metrics\n")
        inputfile.write("#      on " + time.asctime() + "\n")
        if DEM.index(".flt") > 1:
            inputfile.write("DEM: " + DEM[:-4] + "\n")
        else:
            inputfile.write("DEM: " + DEM + "\n")
        inputfile.write("SCRATCH DIRECTORY: " + scratchPath + "\n")
        inputfile.write("LENGTH SCALE: " + length + "\n")
        
        if "Grad" in rasters:
            inputfile.write("GRID: GRADIENT, OUTPUT FILE = " +  rasters.get("Grad") + "\n")
                
        if "Plan" in rasters:
            inputfile.write("GRID: PLAN CURVATURE, OUTPUT FILE = " + rasters.get("Plan") + "\n")
                
        if "Prof" in rasters:
            inputfile.write("GRID: PROFILE CURVATURE, OUTPUT FILE = " + rasters.get("Prof") + "\n")
            
        if executablePath.endswith("\\"):
            command = executablePath + "makegrids "
        else:
            command = executablePath + "\\" + "makegrids "
        
        inputFileName = scratchPath + "input_makeGrids.txt"
        inputfile.write(command + "\n")
        inputfile.write(inputFileName)
        
        inputfile.close()      
        
        try:
            subprocess.call([command,inputFileName])
        except OSError:
            messages.addErrorMessage('MakeGrids failed')
                
        if "Dev" in rasters:
            inputfilename = str(scratchPath) + "\\input_localRelief.txt"
            inputfile = open(inputfilename, 'w')
            inputfile.write("# Input file for LocalRelief\n")
            inputfile.write("#    Created by ArcGIS python tool Surface Metrics\n")
            inputfile.write("#      on " + time.asctime() + "\n")
            if DEM.index(".flt") > 1:
                inputfile.write("DEM: " + DEM[:-4] + "\n")
            else:
                inputfile.write("DEM: " + DEM + "\n")
            inputfile.write("SCRATCH DIRECTORY: " + scratchPath + "\n")
            radius = float(length)/2.
            inputfile.write("RADIUS: " + str(radius) + "\n")
            devParam = rasters["Dev"]
            if "Resample" in devParam:
                inputfile.write("DOWN SAMPLE: " + devParam["Resample"] + "\n")
            if "Interval" in devParam:
                inputfile.write("SAMPLE INTERVAL: " + devParam["Interval"] + "\n")
            inputfile.write("OUTPUT DEV RASTER: " + devParam["Raster"] + "\n")
        
            inputfile.close()     
            
            if executablePath.endswith("\\"):
                command = executablePath + "localRelief "
            else:
                command = executablePath + "\\" + "localRelief "
            
            inputFileName = scratchPath + "input_localRelief.txt"     
 
            try:
                subprocess.run([command, inputFileName])
            except OSError:
                messages.addErrorMessage('LocalRelief failed')
        
        return 
