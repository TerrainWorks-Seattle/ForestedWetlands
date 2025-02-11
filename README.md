---
editor_options: 
  markdown: 
    wrap: 72
---

# Forested Wetlands

Here you'll find resources for building and applying statistical models
for identifying potential wetland locations using ArcGIS. These
resources include tools to: - use a Digital Elevation Model (DEM) to
calculate topographic attributes associated with wetland formation, -
use a data set of point locations with known wetland presence or absence
to build a random forest model to predict the probability of finding a
wetland as a function of topographic and other attributes, - test the
model by applying it to new data sets, - use the model to predict
wetland presence in other locations.

These tools and the methods they use were developed by Dan Miller at
[TerrainWorks](http://www.terrainworks.com) and Meghan Halabisky at the
University of Washington [Remote Sensing and Geospatial Analysis
Lab](https://sites.uw.edu/rsgal/) as part of a wetland mapping project
funded by the Cooperative Monitoring, Evaluation, and Research (CMER)
Committee of the Washington Forest Practices Board and overseen by the
Wetland Scientific Advisory Group (WetSAG). The methods are described in
[Halabisky et al, 2023](https://doi.org/10.5194/hess-27-3687-2023).

To use these tools with ArcGIS Pro, click on the download button to save
the contents of this repository to your hard drive.

The repository includes the following files and folders: - WIP tool
manual_V2.pdf, which contains instructions for installing and running
the tools in ArcGIS. - Wetland_Intrinsic_Potential_Tool_Phase1.pdf,
which contains the report for the first phase of this project, completed
in 2017. - Wetland_Mapping_Tool_Phase2.pdf, which contains the report
describing the methods developed for the tools provided here, with
examples of application of the tools for building and applying a
random-forest model for predicting wetland presence at four areas in
western Washington. - DEMutilities, a folder containing files for the
DEMutilities ArcGIS toolbox. These files include executable programs for
calculating elevation derivatives and python scripts to use those
programs with ArcGIS to create raster files. - WetlandTools, a folder
containing files for the Wetland Tools ArcGIS tool box. These files
include R scripts for building and applying a random forest model for
predicting wetland presence.
