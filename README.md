# Urban Water Conservation Policies in the United States

This repository contains scripts for reproducing the analysis in J.M. Gilligan _et al._, 
"Urban Water Conservation Policies in the United States."

## Pre-requisites

To run the replications, you will need to install R (version 3.4.0 or above),
RStudio, LaTeX (preferably the TeXLive or MikTeX distributions).

You can obtain R for Windows, Linux, MacOS, and other operating systems from 
<https://www.r-project.org> or <https://cran.rstudio.com/> (get R version 3.4.0 or later). 
You can obtain RStudio for Windows, Linux, or MacOS from
<https://www.rstudio.com>. You can obtain a LaTeX distribution from 
<https://ctan.org/starter>.  If you are running Windows, you will also need to install RTools, which you can 
obtain at <https://cran.rstudio.com/bin/windows/Rtools/> (get RTools version 3.5 or later).

and the following R packages:

* `devtools`
* `extrafont`
* `fontcm`
* `geosphere`
* `ggthemes`
* `gridExtra`
* `hms`
* `janitor`
* `knitr`
* `loo`
* `maps`
* `ncdf4`
* `pacman`
* `parallel`
* `raster`
* `reshape2`
* `rstan`
* `tidyverse`
* `RColorBrewer`
* `viridis`
* `xtable`
* `zoo`

You can install these packages by running the following command from R or from the R console in RStudio:
```
install.packages(c("devtools", "extrafont", "fontcm", "geosphere", "ggthemes", "gridExtra", "hms", "janitor", "knitr", 
                   "loo", "maps", "ncdf4", "pacman", "parallel", "raster", "reshape2", "rstan", "tidyverse", 
                   "RColorBrewer", "viridis", "xtable", "zoo"))
```

You will also need to install the following packages from github (e.g., using the `devtools::install_github` function):

* `dkahle/gmap`
* `jonathan-g/jgmcmc@jgmcmc`
* `jonathan-g/jgally@jgally`
* `slowkow/ggrepel`

You can install these packages using the following commands from R or the R Console in RStudio:
```
devtools::install_github("dkahle/gmap")
devtools::install_github("jonathan-g/jgmcmc@jgmcmc")
devtools::install_github("jonathan-g/jgally@jgally")
devtools::install_github("slowkow/ggrepel")
```

## Performing Analyses and Generating Manuscript:

Once you have everything installed, open RStudio and 
set the global options from the "Tools/Global Options" menu, go to "Sweave", and choose "Weave Rnw files using" to 
"knitr" and "Typeset LaTeX into PDF using" to "pdfLaTeX".

Next, open the file `urban_water_conservation_policies.Rnw` in RStudio.
You can set the relevant processing options in this file (see below) and then click on the "Compile PDF" button in 
the editing pane or use the "File/Compile PDF" menu option. You can also compile from the R Console by typing 
`knit("urban_water_conservation_policies.Rnw", envir=new.env())`

Similarly, you can generate the Supporting Information by opening `urban_water_conservation_policies_si.Rnw`
in RStudio and compiling to PDF using the "Compile PDF" button, the "File/Compile PDF" menu option, or
by typing 
`knit("urban_water_conservation_policies_si.Rnw", envir=new.env())`
at the R Console.

## Processing Options

You can control which parts of the analysis you reproduce by changing the variables at the beginning of the file
`urban_water_conservation_policies.Rnw` (legal values are `TRUE` or `FALSE`:

* `do_cache`: Turn on knitr's data-caching mechanism. This variable should be set to `FALSE`. 
  If you set it to `TRUE`, this may speed up subsequent runs if you are editing and re-running code in the scripts or 
  `urban_water_conservation_policies.Rnw`, but my experience with this is that caching introduces a lot of potential
  for errors and doesn't speed things up noticeably if you are fitting the large `test_models` and `pooled_test_models`
  data sets.
* `download_data_sets`: Download fresh copies of the data sets (from the supporting information) from
  <https://doi.org/10.6084/m9.figshare.5714944>. These data sets contain the VWCI scores and all the covariates 
  necessary to perform the regression analyses described in the paper.
* `recalc_pvi`: Recalculate the PVI scores for cities and states. This requires you to obtain the raw 
  state and county-level election returns for the 2008 and 2012 presidential elections
  (available from CQ Press, as cited in the paper). I downloaded the returns in groups, by state:
  * `2008_al_fl.csv` and `2012_al_fl.csv` for Alabama through Florida
  * `2008_ga_ky.csv` and `2012_ga_ky.csv` for Georgia through Kentucky
  * `2008_la_ne.csv` and `2012_la_ne.csv` for Louisiana through Nebraska
  * `2008_nv_nm.csv` and `2012_nv_nm.csv` for Nevada through New Mexico
  * `2008_ny_sd.csv` and `2012_ny_sd.csv` for New York through South Dakota
  * `2008_tn_wy.csv` and `2012_tn_wy.csv` for Tennessee through Wyoming
  These files should reside in the `data/proprietary/` directory.
* `recalc_explanatory`: Build a data frame with the following explanatory variables
  * population
  * population growth rate
  * surface water use as a fraction of public water supply
  * Democratic share of the two-party vote in the 2008 and 2012 presidential elections.
  * National Democratic vote share.
  * Partisan Voting Index (PVI)
  * Population density
  * Population density growth rate
  * MSA area
* `recalc_vwci`: Assign FIPS codes to MSA-level VWCI scores (for geolocation).
* `recalc_city_coords`: Recalculate the geographic location of the MSAs.
* `recalc_climate`: Recalculate the average temperature, precipitation, and K&ouml;ppen aridity index for cities 
  and states.
* `recalc_msa_variables`: Assemble a data-frame with MSA-level variables and covariates.
* `recalc_standardized_data`: Transform raw MSA- and state-level variables into standardized forms, as described in the
  paper.
* `resample_models`: Run the regression analysis for VWCI, Requirements, and Rebates using the baseline regression model
  described in the paper (hierarchical beta-binomial, with varying intercept).
* `resample_test_models`: Run the alternate regresion models described in the "Robustness Checks" sections of the paper \
  and supporting information.
* `resample_pooled_test_models`: Run the baseline and alternate regression models using the alternate normalization of
  MSA-level covariates (normalized relative to all MSAs, as opposed to representing the difference between the MSA and
  the state).
* `force_resampling`: Force re-running all regression models that are selected by `resample_models`, 
  `resample_test_models`, and `resample_pooled_test_models`. This is only necessary if `do_cache` is set to `TRUE`, 
  which I do not recommend; if `do_cache` is set to `FALSE`, this has no effect and all selected models are 
  automatically re-sampled.
