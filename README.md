# Austauschregeln

Shiny application for displaying potential replacement provenances by taxon and
target origin region (UG).

## Project structure

- `app.R`: Shiny application with interactive UG map and compact results table.
- `scripts/create_csv_from_exchange_matrices.R`: validates the analytical result
  object and generates the app data.
- `data/input/`: local input object; the `.RData` file is not committed.
- `data/exchange_rules_app.csv`: stable app-ready dataset.
- `data/output/`: local dated diagnostic exports; not committed.
- `data/shp/`: polygon data for the 22 Ursprungsgebiete.
- `scripts/import_admixture_images.R`: one-time import and whitespace trimming of the
  taxon-specific Admixture PNGs.
- `www/admixture/`: processed PNGs served by Shiny.

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
  "DT",
  "magick"
))
```

## Prepare the data

Copy `meanPairwisePCA4neighbors_v4d_tables.RData` into `data/input/`, then
run from the project root:

```r
source("scripts/create_csv_from_exchange_matrices.R")
```

This writes the stable Shiny input to `data/exchange_rules_app.csv`.

## Run the app

The one-time scripts are deliberately stored outside `R/`, because Shiny
automatically sources `.R` files in that directory when the app starts.

```r
shiny::runApp()
```

The map uses definitive green and orange-red colors for evaluated neighboring
UGs. Pale colors indicate provisional decisions where the donor or target
sample size is below five.

## Import the Admixture images once

The application expects images named `<TAXON>_K<K>.png` in
`www/admixture/`. From the project root, run:

```r
source("scripts/import_admixture_images.R")
```

The script reads the original PNGs below the configured local `Data4d`
folder, removes uniform outer whitespace, reduces oversized images, and leaves
the originals untouched. Taxa with `K = 1` do not need an image; the app shows
the UG outline instead.
