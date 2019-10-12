'''----------------------------------------------------------------------------------
 ToolBox Name: DEM Utilities
 Source Name: DEMutilities.pyt
 Version:     1.0.0
 Author:      Daniel Miller, 2017
 Tools:       Surface Metrics
              Surface Smoothing
              
 Description: A set of tools for working with DEMs
----------------------------------------------------------------------------------'''
import arcpy
import os
import subprocess
import time
import sys
import scipy.ndimage.filters as filters

sys.path.insert(1, os.path.join(sys.path[0], '..'))
import netstream

reload(netstream)

class Toolbox(object):
    def __init__(self):
        """DEMSmoothing provides tools for filtering DEM noise."""
        self.label = "DEM Smoothing"
        self.alias = "DEMsmooth"

        # List of tool classes associated with this toolbox
        self.tools = [PercentileFilter, GaussianFilter, PMFilter]

class PercentileFilter(object):
    def __init__(self):
        """"------------------------------------------------------------------------------------------------
        Tool Name: Percentile Filter
        Version: 1.0.0, python 2.7, ArcGIS 10
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description:
        -------------------------------------------------------------------------------------------------"""
        self.label = "Percentile Filter"
        self.description = "Smooth DEM based on pth percentile neighbor"
        self.canRunInBackground = False
#-----------------------------------------------------------------------------------------------------------
    def getParameterInfo(self):
        """Define parameter definitions"""
        param0 = arcpy.Parameter(
            displayName="Input DEM",
            name="DEM",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input")

        # Percentile filter parameters
        param1 = arcpy.Parameter(
            displayName="Percentile",
            name="p",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")
        param1.value = 50

        param2 = arcpy.Parameter(
            displayName="Footprint size",
            name="size",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")
        param2.value = 3

        param3 = arcpy.Parameter(
            displayName="Output file name",
            name="outname",
            datatype="String",
            parameterType="Required",
            direction="Input")

        params = [param0, param1, param2, param3]
        #   0 = DEM
        #   1 = Percentile value
        #   2 = Footprint size
        #   3 = Output filename
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        # Provide output file path as modified input file name
        if parameters[0].value and parameters[1].value and not parameters[3].value:
            DEMdesc = arcpy.Describe(parameters[0].value)
            parameters[3].value = DEMdesc.path +"\\" + DEMdesc.basename + "_PercSm" + str(parameters[1].value)
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        ### Print DEM info
        DEMdesc = arcpy.Describe(parameters[0])
        in_file = DEMdesc.path + "\\" + DEMdesc.name + DEMdesc.extension
        workingDir = os.getcwd()
        messages.addMessage("Working Directory: " + workingDir)
        messages.addMessage("DEM: " + in_file)
        messages.addMessage("..datatype: " + DEMdesc.dataType)
        if (len(DEMdesc.extension) > 0):
            ext = DEMdesc.extension
            messages.addMessage("..extension: " + ext)
        else:
            ext = ".tif"
        raster = arcpy.Raster(in_file)
        lowerleft = arcpy.Point(raster.extent.XMin, raster.extent.YMin)
        arcpy.env.overwriteOutput = True
        arcpy.env.outputCoordinateSystem = in_file

        in_array = arcpy.RasterToNumPyArray(raster, nodata_to_value=-99999) #TODO: Check if None values will have an effect on filter
        out_array = filters.percentile_filter(in_array, percentile=parameters[1].value,size=parameters[2].value)
        out_raster = arcpy.NumPyArrayToRaster(out_array, lower_left_corner=lowerleft, x_cell_size=3, value_to_nodata=-99999)
        out_path = parameters[3].value + ext
        out_raster.save(out_path)
        messages.addMessage("Saved raster to " + out_path)
        del raster
        del out_raster
        return

class GaussianFilter(object):
    def __init__(self):
        """"------------------------------------------------------------------------------------------------
        Tool Name: Gaussian Filter
        Version: 1.0.0, python 2.7, ArcGIS 10
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description:
        -------------------------------------------------------------------------------------------------"""
        self.label = "Gaussian Filter"
        self.description = "Smooth DEM based on Gaussian kernel"
        self.canRunInBackground = False
#-----------------------------------------------------------------------------------------------------------
    def getParameterInfo(self):
        """Define parameter definitions"""
        param0 = arcpy.Parameter(
            displayName="Input DEM",
            name="DEM",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input")

        param1 = arcpy.Parameter(
            displayName="Standard Deviation",
            name="sdev",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input")

        param2 = arcpy.Parameter(
            displayName="Order",
            name="order",
            datatype="GPLong",
            parameterType="Optional",
            direction="Input")
        param2.filter.type = "ValueList"
        param2.filter.list = [1, 2, 3]
        param2.value = 1
        
        param3 = arcpy.Parameter(
            displayName="Output file name",
            name="outname",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input")
        
        params = [param0, param1, param2, param3]
        #   TODO: Organize parameters, combine where possible
        #   0 = DEM
        #   1 = Standard Deviation
        #   2 = Order
        #   3 = Output filename
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        # Provide output file path as modified input file name
        if parameters[0].value and parameters[1].value and not parameters[3].value:
            DEMdesc = arcpy.Describe(parameters[0])
            parameters[3] = DEMdesc.path +"\\" + DEMdesc.basename + "_GaussSm" + parameters[1]
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        workingDir = os.getcwd()
        messages.addMessage("Working Directory: " + workingDir)

        raster = arcpy.Raster(parameters[0])
        in_array = arcpy.RasterToNumPyArray(raster) #TODO: Check if None values will have an effect on filter
        out_array = filters.gaussian_filter(in_array, sigma=parameters[1],order=parameters[2])
        out_raster = arcpy.NumPyArrayToRaster()
        out_raster.save(parameters[3])
        return

class PMFilter(object):
    def __init__(self):
        """"------------------------------------------------------------------------------------------------
        Tool Name: Perona-Malik Filter
        Version: 1.0.0, python 2.7, ArcGIS 10
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description:
        -------------------------------------------------------------------------------------------------"""
        self.label = "PMFilter"
        self.description = "Smooth DEM using Perona-Malik anisotropic diffusion"
        self.canRunInBackground = False
#-----------------------------------------------------------------------------------------------------------
    def getParameterInfo(self):
        """Define parameter definitions"""

        param0 = arcpy.Parameter(
            displayName="Input DEM",
            name="DEM",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input")

        param1 = arcpy.Parameter(
            displayName="Iterations",
            name="niter",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")
        param1.value = 10

        param2 = arcpy.Parameter(
            displayName="K",
            name="k",
            datatype="GPDouble",
            parameterType="Optional",
            direction="Input")

        param3 = arcpy.Parameter(
            displayName="Gamma",
            name="gamma",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input")
        param3.value = 1
        
        params = None
        #   TODO: Organize parameters, combine where possible
        #   0 = DEM
        #   1 = Percentile: footprint, percentile
        #   2 = Gaussian: sigma, order(?)
        #   3 = Anisotropic Diffusion (Perona-Malik): footprint, iterations, kappa, gamma, cfun
        #   4 = Maximum Homogeneity Neighborhood: footprint, 
        #   5 = 
        #   6 = 
        #   7 = 
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
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        return