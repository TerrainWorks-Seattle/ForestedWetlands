'''----------------------------------------------------------------------------------
 ToolBox Name: DEM Smoothing
 Source Name: DEMSmoothing.pyt
 Version:     1.0.0
 Author:      Daniel Lorigan, 2019
 Tools:       Percentile Filter, Gaussian Filter, Anisotropic Filter
 Description: A set of tools for smoothing DEMs
----------------------------------------------------------------------------------'''
import arcpy
import importlib
import os
import subprocess
import time
import sys
import scipy.ndimage.filters as filters
import numpy as np
from numpy import matlib

sys.path.insert(1, os.path.join(sys.path[0], '..'))
import netstream

importlib.reload(netstream)

class Toolbox(object):
    def __init__(self):
        """DEMSmoothing provides tools for filtering DEM noise."""
        self.label = "DEM Smoothing"
        self.alias = "DEMSmoothing"

        # List of tool classes associated with this toolbox
        self.tools = [PercentileFilter, GaussianFilter, AnisotropicFilter]

# Process the input DEM
def rasterFromDEM(inputfile, messages):
        DEMdesc = arcpy.Describe(inputfile)
        in_file = DEMdesc.path + "\\" + DEMdesc.name
        workingDir = os.getcwd()
        messages.addMessage("Working Directory: " + workingDir)
        messages.addMessage("DEM: " + in_file)
        messages.addMessage("..datatype: " + DEMdesc.dataType)
        if (len(DEMdesc.extension) > 0):
            messages.addMessage("..extension: " + DEMdesc.extension)
        raster = arcpy.Raster(in_file)
        arcpy.env.overwriteOutput = True
        arcpy.env.outputCoordinateSystem = in_file
        return raster

def rasterToNP(raster):
    in_array = arcpy.RasterToNumPyArray(raster, nodata_to_value=-99999)
    in_array = in_array.astype(float)
    in_array[in_array==-99999] = np.nan
    return in_array

def saveNumpyArrayAsRaster(array, in_raster, out_path, messages=None):
    array[array==np.nan] = -99999
    try:
        out_raster = arcpy.NumPyArrayToRaster(array,\
            lower_left_corner=arcpy.Point(in_raster.extent.XMin, in_raster.extent.YMin),\
            x_cell_size=in_raster.meanCellWidth,\
            y_cell_size=in_raster.meanCellHeight,\
            value_to_nodata=-99999)
        saveRaster(out_raster, out_path, messages)
        del out_raster
    except RuntimeError:
        bs = 8192           # ~.54 GB per block (8192^2 * 64)
        blockfilelist = []
        blockNum = 0
        for y in range(0, in_raster.height, bs):
            for x in range(0, in_raster.width, bs):
                blockLLx = in_raster.extent.XMin + x * in_raster.meanCellWidth
                blockLLy = max([in_raster.extent.YMax - (y+bs) * in_raster.meanCellHeight, \
                                in_raster.extent.YMin])

                blockRaster = arcpy.NumPyArrayToRaster(array[y:y+bs, x:x+bs],\
                            lower_left_corner=arcpy.Point(blockLLx, blockLLy),\
                            x_cell_size=in_raster.meanCellWidth,\
                            y_cell_size=in_raster.meanCellHeight,\
                            value_to_nodata=-99999)
                temp = ('_%i.' % blockNum).join(out_path.rsplit('.', 1))
                if arcpy.Exists(temp):
                    arcpy.Delete_management(temp)
                blockRaster.save(temp)

                blockfilelist.append(temp)
                blockNum += 1
        arcpy.Mosaic_management(';'.join(blockfilelist[1:]), blockfilelist[0])
        if arcpy.Exists(out_path):
            arcpy.Delete_management(out_path)
        arcpy.Rename_management(blockfilelist[0], out_path)

        for fileitem in blockfilelist:
            if arcpy.Exists(fileitem):
                arcpy.Delete_management(fileitem)
        del blockRaster

def saveRaster(raster, path, messages=None):
    raster.save(path)
    messages.addMessage("Saved raster to " + path)

# Convert input length to raster's units
# In: length scale, DEM description, {messages}
# Out: Converted length scale
def lengthToCells(length, desc, messages=None):
    sr = desc.spatialReference
    cellSize = desc.children[0].meanCellHeight
    unitScale = sr.metersPerUnit
    numCells = float(length)/(float(cellSize)*unitScale)
    return numCells

def generateCircleFootprint(radius):
    diameter = int(radius*2) + 1
    footprint = np.zeroes(shape=(diameter, diameter))
    Y, X = np.ogrid[:diameter, :diameter]
    dist_from_center = np.sqrt((X - radius)**2 + (Y - radius)**2)
    mask = dist_from_center <= radius
    footprint[mask] = 1
    return footprint

class PercentileFilter(object):
    def __init__(self):
        """"--------------------------------------------------------------------------
        Tool Name: Percentile Filter
        Version: 1.0.0, python 3.7, ArcGIS Pro
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description:
        ---------------------------------------------------------------------------"""
        self.label = "Percentile Filter"
        self.description = "Smooth DEM using Pth percentile neighbor values"
        self.canRunInBackground = False
#-------------------------------------------------------------------------------------
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

        param2 = arcpy.Parameter(
            displayName="Filter Radius (m)",
            name="radius",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")

        param3 = arcpy.Parameter(
            displayName="Output file name",
            name="outname",
            datatype="String",
            parameterType="Required",
            direction="Input")

        params = [param0, param1, param2, param3]
        #   0 = DEM
        #   1 = Percentile value
        #   2 = Footprint radius
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
        if not parameters[3].altered and parameters[0].value and parameters[1].value \
            and parameters[2].value:
            DEMdesc = arcpy.Describe(parameters[0].value)
            parameters[3].value =  DEMdesc.path +"\\" + DEMdesc.basename + "_perc"\
                + "_p" + parameters[1].valueAsText\
                + "_r" + parameters[2].valueAsText + ".tif"
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        if parameters[3].value and arcpy.Exists(parameters[3].value):
            parameters[3].setWarningMessage(
                "Output +" + parameters[3].value + " exists. It will be overwritten."
            )
        if parameters[1].value != None:
            if parameters[1].value < 0 or parameters[1].value > 100:
                parameters[1].setErrorMessage(
                    "Percentile value must be an integer within [0, 100]."
                )
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        raster = rasterFromDEM(parameters[0], messages)
        lowerleft = arcpy.Point(raster.extent.XMin, raster.extent.YMin)
        descDEM = arcpy.Describe(raster)
        cellsize = descDEM.children[0].meanCellHeight
        radius = lengthToCells(parameters[2].value, descDEM, messages)
        footprint = generateCircleFootprint(radius)
        in_array = rasterToNP(raster)
        out_array = filters.percentile_filter(in_array, percentile=parameters[1].value,\
            footprint=footprint)
        saveNumpyArrayAsRaster(out_array, raster, parameters[3].valueAsText, messages)
        del raster
        return

class GaussianFilter(object):
    def __init__(self):
        """"--------------------------------------------------------------------------
        Tool Name: Gaussian Filter
        Version: 1.0.0, python 3.7, ArcGIS Pro
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description:
        ---------------------------------------------------------------------------"""
        self.label = "Gaussian Filter"
        self.description = "Smooth DEM using a Gaussian kernel"
        self.canRunInBackground = False
#-------------------------------------------------------------------------------------
    def getParameterInfo(self):
        """Define parameter definitions"""
        param0 = arcpy.Parameter(
            displayName="Input DEM",
            name="DEM",
            datatype="DERasterDataset",
            parameterType="Required",
            direction="Input")

        param1 = arcpy.Parameter(
            displayName="Filter Radius (m)",
            name="radius",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")

        param2 = arcpy.Parameter(
            displayName="Order",
            name="order",
            datatype="GPLong",
            parameterType="Optional",
            direction="Input")
        param2.filter.type = "ValueList"
        param2.filter.list = [0, 1, 2, 3, 4]
        param2.value = 0
        
        param3 = arcpy.Parameter(
            displayName="Output file name",
            name="outname",
            datatype="String",
            parameterType="Required",
            direction="Input")
        
        params = [param0, param1, param2, param3]
        #   0 = DEM
        #   1 = Filter Radius
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
        if not parameters[3].altered and parameters[0].value and parameters[1].value:
            DEMdesc = arcpy.Describe(parameters[0].value)
            parameters[3].value = DEMdesc.path +"\\" + DEMdesc.basename + "_gauss" \
                + "_" + parameters[1].valueAsText + ".tif"
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        if parameters[3].value and arcpy.Exists(parameters[3].value):
            parameters[3].setWarningMessage(
                "Output +" + parameters[3].value + " exists. It will be overwritten.")
        
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        raster = rasterFromDEM(parameters[0], messages)
        lowerleft = arcpy.Point(raster.extent.XMin, raster.extent.YMin)
        descDEM = arcpy.Describe(raster)
        cellsize = descDEM.children[0].meanCellHeight
        radius = lengthToCells(parameters[2].value, descDEM, messages)
        sigma = radius/3.0
        in_array = rasterToNP(raster)
        out_array = filters.gaussian_filter(in_array, sigma=parameters[1].value, \
            order=parameters[2].value, truncate=3.0)
        saveNumpyArrayAsRaster(out_array, raster, parameters[3].valueAsText, messages)
        del raster
        return

class AnisotropicFilter(object):
    def __init__(self):
        """"--------------------------------------------------------------------------
        Tool Name: Anisotropic Filter
        Version: 1.0.0, python 3.7, ArcGIS Pro
        Author: Daniel Lorigan, 2019
        Required arguments:
            
        Optional Arguments:
            
        Description:
        ---------------------------------------------------------------------------"""
        self.label = "Anisotropic Filter"
        self.description = "Smooth DEM using Perona-Malik anisotropic diffusion"
        self.canRunInBackground = False
#-------------------------------------------------------------------------------------
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

        param2 = arcpy.Parameter(
            displayName="Estimate threshold K values",
            name="estimate_k",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input")
        param2.value = True

        param3 = arcpy.Parameter(
            displayName="K",
            name="k",
            datatype="GPDouble",
            parameterType="Optional",
            direction="Input",
            enabled=False)

        param4 = arcpy.Parameter(
            displayName="K2",
            name="k2",
            datatype="GPDouble",
            parameterType="Optional",
            direction="Input",
            enabled=False)

        param5 = arcpy.Parameter(
            displayName="Sigma",
            name="sigma",
            datatype="GPDouble",
            parameterType="Optional",
            direction="Input")
        
        param6 = arcpy.Parameter(
           displayName="Re-estimate K for each iteration",
           name="estimate_each",
           datatype="GPBoolean",
           parameterType="Optional",
           direction="Input")
        param6.value = False

        param7 = arcpy.Parameter(
            displayName="Output file name",
            name="outname",
            datatype="String",
            parameterType="Required",
            direction="Input")

        params = [param0, param1, param2, param3, param4, param5, param6, param7]
        #   0 = DEM
        #   1 = Iterations
        #   2 = Estimate K
        #   3 = K
        #   4 = K2
        #   5 = Sigma
        #   6 = Estimate each
        #   7 = Output filename
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        # Provide output file path as modified input file name
        if not parameters[7].altered and parameters[0].value and parameters[1].value:
            DEMdesc = arcpy.Describe(parameters[0].value)
            parameters[7].value = DEMdesc.path + "\\" + DEMdesc.basename + "_aniso" \
                + "_i" + parameters[1].valueAsText + ".tif"
        if parameters[2].value:
            parameters[3].enabled = False
            parameters[4].enabled = False
            parameters[5].enabled = True
            parameters[6].enabled = True
        else:
            parameters[3].enabled = True
            parameters[4].enabled = True
            parameters[5].enabled = False
            parameters[6].enabled = False
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        if parameters[5].value and arcpy.Exists(parameters[5].value):
            parameters[5].setWarningMessage(
                "Output +" + parameters[5].value + " exists. It will be overwritten.")
        if not parameters[2].value and not parameters[3].value:
            parameters[3].setErrorMessage(
                "A value for K must be provided when estimation is turned off.\
                     Please enter a value.")
        if parameters[2].value and parameters[6].value:
            parameters[6].setWarningMessage(
                "Enabling this parameter can significantly impact performance."
            )
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""

        raster = rasterFromDEM(parameters[0], messages)
        lowerleft = arcpy.Point(raster.extent.XMin, raster.extent.YMin)
        cellsize = arcpy.Describe(raster).children[0].meanCellHeight
        
        in_array = rasterToNP(raster)
        if parameters[2].value:
            out_array = anisodiff(
                    in_array,
                    iters=parameters[1].value,
                    s=parameters[5].value,
                    estimate_each=parameters[6].value,
                    cellsize=cellsize,
                    messages=messages
                )
        else:
            out_array = anisodiff(
                    in_array,
                    iters=parameters[1].value,
                    k=parameters[3].value,
                    k2=parameters[4].value,
                    cellsize=cellsize,
                    messages=messages
                )

        arcpy.SetProgressorLabel("Generating raster")
        arcpy.SetProgressorPosition(99)
        saveNumpyArrayAsRaster(out_array, raster, parameters[7].valueAsText, messages)
        del raster
        return

###########################################################################
####### Helper functions for Perona-Malik Anisotropic Diffusion ###########
###########################################################################

# Calculates anisotropic diffusion of input array using Perona-Malik method
# in_array = input array, iters = number of iterations, k = edge threshold, gamma, s = optional sigma
def anisodiff(in_array, iters, k=None, k2=None, s=None, estimate_each=False, cellsize=1.0, messages=None):
    arcpy.SetProgressor("step")
    if k == None:
        if s == None:
            # Calculate an s from the input array
            s = minimumSD(in_array, 64)
            messages.addMessage("Using sigma = "+str(s))
        kS, kE, s = calcThreshold(in_array, s=s, messages=messages)
    else:
        kS = k
        kE = k
        if k2 != None:
            kE = k2
    messages.addMessage("Using k="+str(kS)+", k2="+str(kE))
    arcpy.SetProgressorPosition(0)
    arcpy.SetProgressorLabel("Applying anisotropic diffusion")
    # initialize output array
    out_array = in_array.copy()
    # initialize variables
    deltaS = np.zeros_like(out_array)
    deltaE = deltaS.copy()
    NS = deltaS.copy()
    EW = deltaS.copy()
    cS = np.ones_like(out_array)
    cE = cS.copy()
    for i in range(iters):
        arcpy.SetProgressorPosition(i*100//iters)
        # calculate the gradient in the south and east directions
        deltaS[:-1,: ] = np.diff(out_array,axis=0)
        deltaE[: ,:-1] = np.diff(out_array,axis=1)
        # Update matrices
        S = deltaS/(cellsize*(1.+(deltaS/kS)**2))
        E = deltaE/(cellsize*(1.+(deltaE/kE)**2))
        # subtract a copy that has been shifted Northwest by one to account for northern and western slope
        NS[:] = S
        EW[:] = E
        NS[1:,:] -= S[:-1,:]
        EW[:,1:] -= E[:,:-1]
        # update the image
        out_array += NS+EW
        if k == None and estimate_each and i < iters-1:
            kS, kE, s = calcThreshold(out_array, s=s, messages=messages)
            messages.addMessage("Using k="+str(kS)+", k2="+str(kE))
    return out_array

# Calculates and returns optimal k-values in South and East directions for the input raster.
# arr = input array
# s = sigma to use for raster smoothing: 0 is unsmoothed, -1 to generates a sigma based on min sigma of array
def calcThreshold(arr, s, messages=None):
    # Smooth raster to calculate slopes from
    arcpy.SetProgressorLabel("Applying preliminary gaussian filter")
    arr = filters.gaussian_filter(arr, sigma=s, order=0, truncate=3.0)

    arcpy.SetProgressorLabel("Estimating threshold")
    # Find optimal N/S threshold
    diff = np.diff(arr, axis=0)
    hist, bin_edges = np.histogram(diff[~np.isnan(diff)], bins=50, \
        range=(np.nanmin(diff), np.nanmax(diff)))
    threshold = findKnee(hist)
    kS = bin_edges[threshold]

    # Find optimal E/W threshold
    diff = np.diff(arr, axis=1)
    hist, bin_edges = np.histogram(diff[~np.isnan(diff)], bins=50, \
        range=(np.nanmin(diff), np.nanmax(diff)))
    threshold = findKnee(hist)
    kE = bin_edges[threshold]

    return kS, kE, s

# Finds the knee/elbow of a curve by finding the furthest point from a line spanning the curve.
def findKnee(curve):
    npoints = len(curve)
    coords = np.array([range(npoints), curve]).T
    first = coords[0]
    vector = coords[-1] - first
    vectorNorm = vector / np.sqrt(np.sum(vector**2))
    fromFirst = coords - first
    scalarProduct = np.sum(fromFirst * matlib.repmat(vectorNorm, npoints, 1), axis=1)
    fromFirstParallel = np.outer(scalarProduct, vectorNorm)
    vectorToLine = fromFirst - fromFirstParallel
    # Calculate each point's distance from the vector
    distToLine = np.sqrt(np.sum(vectorToLine ** 2, axis=1))
    knee_idx = np.argmax(distToLine)
    return knee_idx

# Estimates the minimum standard deviation of values in an array based on blocks of size*size dimension.
# a = input array, size = block size (recommended to be at least 25)
def minimumSD(a, size):
    arcpy.SetProgressorLabel("Calculating sigma")
    min_sd = 9999
    rows = a.shape[0]
    cols = a.shape[1]
    n = size**2
    # Slide size x size window along size-wide strips of DEM to estimate appropriate sigma
    for r in range(rows//size):
        r = r*size
        arcpy.SetProgressorPosition(r*100//(rows))
        # Initialize the base window
        window = a[r:r+size, 0:size-1]

        # Handle NaNs
        if np.isnan(window).any():
            tot = 0
        else:
            tot = np.sum(window)

        for c in range(cols-size+1):
            window = a[r:r+size, c:c+size]  # Add the next column
            if not np.isnan(window).any():
                if tot == 0:
                    tot = np.sum(window)
                else:
                    new_col = window[:,-1:]
                    tot += np.sum(new_col)
                
                # Calculate the SD of the window
                m = tot/n
                dev = lambda x: abs(x-m)
                d = dev(window)
                total_d = np.sum(d)
                sd = total_d/n

                if sd < min_sd:
                    min_sd = sd

                # Remove the window's first column
                old_col = window[:,:1]
                tot -= np.sum(old_col)
    return min_sd

###########################################################################
