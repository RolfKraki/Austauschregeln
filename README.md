# Austauschregeln

Shiny application for displaying potential replacement provenances by taxon and
target origin region (UG).

## Project structure

- `app.R`: Shiny application with interactive UG map and compact results table.
- `R/create_csv_from_exchange_matrices.R`: validates the analytical result
  object and generates the app data.
- `data/input/`: local input object; the `.RData` file is not committed.
- `data/exchange_rules_app.csv`: stable app-ready dataset.
- `data/output/`: local dated diagnostic exports; not committed.
- `data/shp/`: polygon data for the 22 Ursprungsgebiete.

## Required R packages

Install these once if necessary:

```r
install.packages(c(
  "shiny",
  "dplyr",
  "purrr",
  "here",
  "sf",
  "leaflet",
  "DT"
))
```

## Prepare the data

Copy `meanPairwisePCA4neighbors_v4d_tables.RData` into `data/input/`, then
run from the project root:

```r
source("R/create_csv_from_exchange_matrices.R")
```

This writes the stable Shiny input to `data/exchange_rules_app.csv`.

## Run the app

```r
shiny::runApp()
```

The map uses definitive green and orange-red colors for evaluated neighboring
UGs. Pale colors indicate provisional decisions where the donor or target
sample size is below five.
