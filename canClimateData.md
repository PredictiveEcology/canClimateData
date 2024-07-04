---
title: "canClimateData Manual"
subtitle: "v.1.0.3"
date: "Last updated: 2024-07-04"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  bibliography: citations/references_canClimateData.bib
citation-style: citations/ecology-letters.csl
link-citations: true
always_allow_html: true
---

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:canClimateData) *canClimateData*



[![made-with-Markdown](figures/markdownBadge.png)](http://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

# canClimateData module

#### Authors:

Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut], Alex M Chubaty <achubaty@for-cast.ca> [aut, cre], Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut], Tati Micheletti <tati.micheletti@gmail.com> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Prepare Canadian historic and projected climate data for use with `LandR.CS` and `fireSense.`

Historical and projected climate data were generated using the [`climateData`](https://github.com/PredictiveEcology/climateData) package, which provides wrappers for calling [ClimateNA](https://climatena.ca) to generate the climate data on a local machine.
These datasets were then archived and uploaded to Google Drive, and an SQLite database (shipped with the package) is used to record which datasets are available, and how to retrieve them.

Currently supports study areas across Canada, for some CMIP6 climate scenarios (see below).

**Available data sets:**

<img src="../../../../../../../../mnt/shared_cache/renv/cache/v5/R-4.3/x86_64-pc-linux-gnu/climateData/2.2.1/be4e1197d00d03ddb206d4f7c44b471d/climateData/extdata/tile_map.png" width="852" />


``` r
## historic data
available("historical")[["years"]]
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
```

``` r
available("historical_normals")[["periods"]]
```

```
## [1] "1901_1930" "1911_1940" "1921_1950" "1931_1960" "1941_1970" "1951_1980"
## [7] "1971_2000" "1981_2010" "1991_2020"
```

``` r
## future climate scenarios
available("future")[["gcms"]]
```

```
## [1] "CanESM5"     "CNRM-ESM2-1"
```

``` r
available("future")[["ssps"]]
```

```
## [1] "245" "370" "585"
```

Additional data sets will be added in the future.

### Module inputs and parameters

#### Input objects

Table \@ref(tab:moduleInputs-canClimateData) shows the full list of module inputs.

See `?climateData::prepClimateLayers` for examples constructing a custom `climateVariables` object to use to add arbitrary climate variables to the output lists of climate rasters.

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-canClimateData)(\#tab:moduleInputs-canClimateData)List of (ref:canClimateData) input objects and their description.</caption>
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
   <td style="text-align:left;"> climateVariables </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> a list, named by climate variable using 'projected_' or 'historical_' prefixes, with each list element containing a list of three arguments: vars - the raw variables used to derive the target variable, fun - the quoted function used to derive the target variable, where 'quote(calcAsIs)' denotes target variables that ARE the raw variable, and dots - additional arguments passed to 'fun'. See the .inputObjects for examples of how to build this object and ?climateData::prepClimateLayers for how it is used . The GCM, SSP, and selected years, whether projected or historical, are set by module parameters and must be identical for all variables </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> template raster corresponding to `studyArea`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> study area used for simulation (buffered to mitigate edge effects) </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

#### Parameters

Table \@ref(tab:moduleParams-canClimateData) shows the full list of module parameters.


<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-canClimateData)(\#tab:moduleParams-canClimateData)List of (ref:canClimateData) parameters and their description.</caption>
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
   <td style="text-align:left;"> Distance (m) to buffer `studyArea` and `rasterToMatch` to mitigate edge effects for simulation. </td>
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
   <td style="text-align:left;"> historicalClimatePeriod </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> 1951_198.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> period to use for historical climate normals if `climateVariables` not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> historicalClimateYears </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1991, 19.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> range of years captured by the historical climate data if `climateVariables` not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> outputDir </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Directory path for prepared climate data rasters. User should specify a shared directory (e.g., when running multiple simulation replicates) to avoid multiple copies of the climate data. If not specified by the user, a subdirectory of the simulation output directory will be used (i.e., `file.path(outputPath(sim), 'climate')`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimatePeriod </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> period to use for projected climate normals if `climateVariables` not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateYears </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2011, 20.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> range of years captured by the projected climate data if `climateVariables` not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedType </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> forecast </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> one of 'forecast' or 'hindcast' to prepare either projected future, or sampled historical data for hindcast studies, respectively. </td>
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
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> User-defined label for the current study area. If `NA`, a hash of `studyArea` will be used. </td>
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

### Events

This module consists of a single `init` event that performs all data preparation.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-canClimateData)).

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-canClimateData)(\#tab:moduleOutputs-canClimateData)List of (ref:canClimateData) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
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

### Links to other modules

Originally developed to provide inputs to [`gmcsDataPrep`](https://github.com/ianmseddy/gmcsDataPrep) (for use with climate-sensitive LandR Biomass projections; see [`Biomass_core`](https://github.com/PredictiveEcology/Biomass_core)) and the [`fireSense`](https://github.com/PredictiveEcology/fireSense) suite of modules.

### Getting help

<https://github.com/PredictiveEcology/canClimateData/issues>
