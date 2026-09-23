# Local input data

Place the locally generated input object here with the exact filename:

`meanPairwisePCA4neighbors_v4d_tables.RData`

The file is intentionally excluded from Git because it is a generated binary
analysis object. The export script reads it from this directory and produces:

- `data/exchange_rules_app.csv` for the Shiny app;
- a dated complete table under `data/output/` for local checking.

The app's `Datenstand` is the modification date of this local `.RData` file,
recorded when the CSV is generated. Copying or downloading the file may change
its modification date; check it before exporting. The manually maintained
`source_version` near the top of the export script starts at `1.0.0` and
should be increased when the analysis results change. Both values are written
to the CSV and shown in the app and its exports.

Run the export from the repository root with:

```r
source("scripts/create_csv_from_exchange_matrices.R")
```
