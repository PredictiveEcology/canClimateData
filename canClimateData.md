---
title: "canClimateData"
author: "Alex Chubaty"
date: "22 February 2024"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Prepare Canadian historic and projected climate data for use with LandR.CS and fireSense.

Currently supports study areas in AB, BC, SK, MB, ON, QC, NT, and YT, for some CMIP6 climate scenarios (see below).

# Parameters

Provide a summary of user-visible parameters.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> bufferDist </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 20000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Distance (m) to buffer `studyArea` and `rasterToMatch`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateGCM </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Global Circulation Model to use for climate projections: currently 'CanESM5' or 'CNRM-ESM2-1'. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateSSP </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 370 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> SSP emissions scenario for `climateGCM`: one of 245, 370, or 585. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> historicalFireYears </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1991, 19.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> range of years captured by the historical climate data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedFireYears </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2011, 20.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> range of years captured by the projected climate data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quickCheck </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> `prepClimateData` uses `prepInputs` internally; set this to `TRUE` to avoid the slow process of digesting potentially MANY files. This will use `file.size` only, if `TRUE`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> User-defined label for the current stuyd area. If `NA`, a hash of `studyArea` will be used. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> usePrepInputs </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> There are currently two ways to run this module: using `reproducible::prepInputs` and a custom approach using googledrive directly. The direct googledrive approach was the original approach; `usePrepInputs = TRUE` is a rewrite from that original code. On Dec 11, 2023, the two ways would be similar, but they may diverge over time. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant. </td>
  </tr>
</tbody>
</table>

# Events

This module consists of a single `init` event that performs all the data preparation.

# Data dependencies

## Input data

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> template raster corresponding to `studyArea`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchReporting </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> template raster corresponding to `studyAreaReporting`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> study area used for simulation (buffered to mitigate edge effects) </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaReporting </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> study area used for reporting/post-processing </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

### Historic and projected climate data

### Prerequisites

1. DEM from GEOTOPO15 product
2. Climate data using [ClimateNA](https://climatena.ca) with projected climate scenario(s).

### Available data sets

<img src="../../../../../../R-dev/climateData/extdata/tile_map.png" width="852" />


```r
## historic data
available("historic")[["years"]]
available("historic_normals")[["periods"]]

## future climate scenarios
available("future")[["gcms"]]
available("future")[["ssps"]]
```

```
##   [1] 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910 1911 1912 1913 1914 1915
##  [16] 1916 1917 1918 1919 1920 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930
##  [31] 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945
##  [46] 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960
##  [61] 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975
##  [76] 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990
##  [91] 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005
## [106] 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
## [121] 2021 2022
## [1] "1901_1930" "1911_1940" "1921_1950" "1931_1960" "1941_1970" "1951_1980"
## [7] "1971_2000" "1981_2010" "1991_2020"
## [1] "CanESM5"     "CNRM-ESM2-1"
## [1] "245" "370" "585"
```

### Additional data sets

Other study areas and/or climate scenarios can be added by making suitable `.zip` archives available via Google Drive and updating the `climateDataURLs.csv` file accordingly.

**NOTE:** the following naming conventions should be used for the `.zip` archives:

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Description </th>
   <th style="text-align:left;"> Archive.Filename </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Historic normals: 1951-1980Y and 1981_2010Y </td>
   <td style="text-align:left;"> studyAreaNameLong_normals.zip </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Historic monthly: 1991-2020 </td>
   <td style="text-align:left;"> studyAreaNameLong.zip </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Projected annual: 2011-2100 </td>
   <td style="text-align:left;"> climateGCM_climateSSP_studyAreaNameLong_annual,zip </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Projected monthly: 2011-2100 </td>
   <td style="text-align:left;"> climateGCM_climateSSP_studyAreaNameLong,zip </td>
  </tr>
</tbody>
</table>

## Output data

Description of the module outputs.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ATAstack </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> annual projected mean annual temperature anomalies </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CMIstack </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> annual projected mean climate moisture deficit </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CMInormal </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Climate Moisture Index Normals from 1950-2010 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> historicalClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of a single raster stack - historical MDC calculated from ClimateNA data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of a single raster stack - projected MDC calculated from ClimateNA data </td>
  </tr>
</tbody>
</table>

# Links to other modules

Originally developed to provide inputs to `gmcsDataPrep` (for use with climate-sensitive LandR Biomass projections; see `Biomass_core`) and the `fireSense` suite of modules.
