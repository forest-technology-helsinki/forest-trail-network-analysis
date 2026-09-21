Example data

This directory contains anonymized example spatial data for demonstrating the forest trail network analysis workflow.

The example dataset contains data from two study sites (Sites 2 and 4) selected from the larger dataset used in the study. The complete study dataset is not distributed in this repository.

Directory structure
example/site02/ — example data for Site 2
example/site04/ — example data for Site 4
Spatial data

The example data are provided as ESRI Shapefiles. All component files belonging to each shapefile are included.

The main spatial layers are:

Area — boundary of the study site.
Centreline — processed centreline representation of the forest machine trail network used for network-level metrics.
Patches — spatial patches resulting from fragmentation of the site by the trail network and used to calculate fragmentation metrics.
Trails — reference trail data associated with the study site.

For example, Site 2 contains Area2, Centreline2, Patches2, and Trails2.

Coordinate reference system

The example spatial data use:

ETRS89 / UTM zone 35N (EPSG:25835)

Coordinates and distances are therefore expressed in metres.

Use with the R scripts

The site-level analysis script in R/01_site_metrics.R automatically detects compatible example sites in data/example/ and calculates site-level fragmentation and trail-network metrics.

The cell-level analysis in R/02_cell_metrics.R additionally requires tessellation-derived datasets (Tessellation, T_Centreline, and Cell_Patches). These additional layers are not part of the current example dataset.

The example data are intended to demonstrate the structure and processing workflow. They do not constitute the complete dataset used in the study.
