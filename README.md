
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epichecks

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/R4IDSR/epichecks.svg?branch=master)](https://travis-ci.org/R4IDSR/epichecks)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/R4IDSR/epichecks?branch=master&svg=true)](https://ci.appveyor.com/project/R4IDSR/epichecks)
[![Codecov test
coverage](https://codecov.io/gh/R4IDSR/epichecks/branch/master/graph/badge.svg)](https://codecov.io/gh/R4IDSR/epichecks?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epichecks)](https://CRAN.R-project.org/package=epichecks)
<!-- badges: end -->

The goal of {epichecks} is to provide functions for simplifying data
quality checks and threshold analyses for IDSR data. The package further
contains helper functions that automate the production of feedback
documents for countries.

## Installation

Currently the package is not on CRAN. Once it is - you can install the
released version of epichecks from [CRAN](https://CRAN.R-project.org)
with:

``` r
# install.packages("epichecks")
```

In order to install the package you will first need to install an extra
bit of software called
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).  
You can download the installer from:
<https://cran.r-project.org/bin/windows/Rtools/>  
Please install the version highlighted in green.

Once this is installed and you have restarted your computer, the
development version of the package can be installed from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("R4IDSR/epichecks")
```

If you do not have a LaTeX editor installed on your computer, then
please run the following code to install TinyTex. This is needed in
order to be able to create Word and PDF documents using *R markdown*.

``` r
install.packages('tinytex')
tinytex::install_tinytex()
# to uninstall TinyTeX, run tinytex::uninstall_tinytex() 
```

## Folder set up

In order for the funtions to work you need to have folders set up
correctly.

Create one main folder, in example called **WHO AFRO**.  
Within this folder create an R project (e.g. **WHO\_AFRO.Rproj**) -
*remember to open R through this project every time - to have the
correct root directory*.  
Within the **WHO AFRO** folder have a **Data** folder.  
The **Data** folder contains the *PHE dataset* and a **Processed**
folder which in turn has a folder for each calendar week.  
Place *IDSR data* for each country as *CSV* files in to the appropriate
calendar week folder.  
Within the **WHO AFRO** folder create an **Outputs** folder, and within
that a **Verification** folder. The package will create a folder for
each calendar week and place, for each country, an excel with flags and
a pdf letter with flags.

As an example:

<img src="man/figures/folder_layout.png"/>

## Weekly Country Feedback

This section explains how to make weekly feedback for countries based on
pre-processed IDSR data.

Open your R project (e.g. **WHO\_AFRO.Rproj**) and type the below code.

This will produce outputs for week 35 of 2018 as an example. See
?week\_report for details of parameters that can be adjusted. *n.b. this
can take a couple of minutes to run*

``` r
library(epichecks)
week_report(current_week = "2018-W35")
```

This function creates an excel (**Country\_2018\_W35.xlsx**) with two
tabs, the first flags missing disease counts data and the second flags
when disease counts data exceeds pre-defined thresholds.  
It also creates a pdf letter (**Country\_2018\_W35.pdf**) which
summarises the flags in the excel.  
These two files are created for each country.

In addition it creates two files for internal WHO use.  
The first is a summary of countries reporting, with counts of diseases,
missings, and threshold flags (**SummaryReport\_2018\_W35.xlsx**).  
The second is an *R* dataset with all the countries for that week
combined. (**Merged.Rds**)

Three dictionaries are included in the {epichecks} package, and used
internally for processing.  
These include country names, disease names and threshold alert
definitions.

<img src="man/figures/weekly_flow.png"/>

## WHO AFRO Monthly Bulletin

This section explains how to use an *R markdown* template to create
monthly WHO AFRO bulletins which includes information from **IDSR data**
and **PHE data**.

To understand the basics of opening and using templates see this short
[walkthrough](https://r4epis.netlify.com/outbreaks/#getting-started)
from the *R4Epis* project.

Once {epichecks} is installed you should be able to see the **WHO AFRO
monthly bulletin** template as below. Save your RMD file in the root
directory (i.e. the same place where your \*\*WHO\_AFRO.Rproj is saved).

<img src="man/figures/template.png"/>

You then need to **update the calendar week** sections of the code, see
below. This can then be changed each month to produce the appropriate
report.

<img src="man/figures/code_updates.png"/>

You can then knit to create a word document in the same folder where the
*Rmd* file is saved.

Please note that the ‘epichecks’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
