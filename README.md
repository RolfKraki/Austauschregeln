# Austauschregeln

Shiny application for displaying potential replacement provenances by taxon and
target origin region (UG).

## Project structure

- `app.R`: Shiny application with interactive UG map and compact results table.
- `scripts/create_csv_from_exchange_matrices.R`: validates the analytical result
  object and generates the app data.
- `scripts/export_exchange_rules_matrix.R`: exports Excel matrices of allowed
  neighbouring donor UGs for all target UGs and taxa.
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
The export records the input `.RData` file's modification date and the
`source_version` specified near the top of the script. Increase that version
when the analysis results change, then commit the regenerated CSV so the app
shows the new `Datenstand` and version.

## Export an Excel summary matrix

With `meanPairwisePCA4neighbors_v4d_tables.RData` in `data/input/`, run:

```r
install.packages("openxlsx") # once; here is already required above
source("scripts/export_exchange_rules_matrix.R")
```

This creates `data/output/exchange_rules_summary.xlsx` and replaces that local
workbook on later runs. It reads the RData directly and leaves the app CSV
unchanged.

- `Zulaessig`: target UGs 1–22 in the first column, followed by one column per
  taxon, using the app's species names. Cells list confirmed and provisionally allowed
  neighbouring donor UGs together, in numerical order and separated by commas
  (for example `2, 5*, 8`). An asterisk marks each provisionally allowed UG.
- `Vorlaeufig`: the same matrix for provisionally allowed donors, shown separately
  because at least one sample size is below five.
- `Taxa`: taxon codes and species names.
- `Hinweise`: decision criteria, cell meanings and source/export dates.

Both matrices follow the app's sample-size and decision rules. Donor and target
must each have at least two sampled individuals. Confirmed decisions use
`state_obs == "allowed"` with `too_small == FALSE`; provisional decisions use
`allowed_obs == TRUE` with `too_small == TRUE`. Only neighbouring donors are
listed, and the target itself is excluded. Direction is preserved: a donor
listed for one target is not automatically allowed in the reverse direction.

`-` means no confirmed or provisionally allowed donor in `Zulaessig`, or no
provisionally allowed donor in `Vorlaeufig`. Text `NA` means no
neighbouring pair is evaluable for that taxon and target, in either class.
An omitted donor can also be unevaluated; these cells are summaries of allowed
transfers, not complete lists of rejections. All taxa in the RData are retained,
including aggregates and cytotypes; unknown display names fall back to taxon
codes. The script checks all 484 unique donor-target combinations per taxon and
reads both saved matrices back to verify their contents.

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
