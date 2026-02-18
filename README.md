# CEOdata <img src='man/figures/logo.png' align="right" height="139" />

Easy and convenient access to the datasets / microdata of the ["Centre d'Estudis d'Opinió"](https://ceo.gencat.cat/ca/inici/), the Catalan institution for polling and public opinion. The package uses the data stored in the open data platform of the Generalitat de Catalunya and returns it in a tidy format (tibble).

The main function, `CEOdata()` can return either:
1. An accumulated microdata series (identified by `series`), or
2. A single study dataset (identified by `reo`)

## Installation

To install from GitHub (development version), run:
```
install.packages("remotes")
remotes::install_github("ceopinio/CEOdata")
```

## Basic usage

### 1. Load the default accumulated series

By default, `CEOdata()` returns the accumulated microdata series "BOP_presencial":
```
library(CEOdata)
d <- CEOdata()
```
This is equivalent to:
```
CEOdata(series = "BOP_presencial")
```
### 2. List available accumulated series

You can inspect the catalogue of accumulated microdata series:
```
m <- CEOaccumulated_meta()
unique(m$codi_serie)
```
Each row corresponds to a series identified by codi_serie.

### 3. Load a specific accumulated series
```
d_tel <- CEOdata(series = "BOP_telefonica")
```
The data are downloaded from the Generalitat open data platform and returned as a tibble.

### 4. List available studies (REOs)

You can inspect the catalogue of individual studies using:
```
meta <- CEOmeta()
```

### 5. Load a specific study by REO code
```
d1145 <- CEOdata(reo = "1145")
```
This downloads the microdata corresponding to the study with REO code "1145".

### 6. Raw SPSS labels vs R factors

By default, SPSS labelled variables are converted into standard R factors.

To keep the original haven-labelled format:
```
d_raw <- CEOdata(series = "BOP_presencial", raw = TRUE)
```
The same applies when loading a specific REO:
```
d_reo_raw <- CEOdata(reo = "1145", raw = TRUE)
```
## Vignettes
To access the package vignettes, please use:
```
vignette('using_CEOdata')
vignette('working_with_survey_data_using_the_CEOdata_package')
vignette('cheatsheet')
```

Report bugs, request improvements, ask questions or provide ideas at the [issues page](https://github.com/ceopinio/CEOdata/issues/).
