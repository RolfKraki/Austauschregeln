# Austauschregeln

Shiny application for displaying potential replacement provenances by taxon and
target origin region (UG).

## Project structure

- `app.R`: Shiny application.
- `R/create_csv_from_exchange_matrices.R`: validates the analytical result
  object and generates the app data.
- `data/input/`: local input object; the `.RData` file is not committed.
- `data/exchange_rules_app.csv`: stable app-ready dataset.
- `data/output/`: local dated diagnostic exports; not committed.

## Prepare the data

Copy `meanPairwisePCA4neighbors_v4d_tables.RData` into `data/input/`, then
run from the project root:

```r
source("R/create_csv_from_exchange_matrices.R")
```

## Run the app

```r
shiny::runApp()
```
