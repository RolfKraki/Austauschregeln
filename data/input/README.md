# Local input data

Place the locally generated input object here with the exact filename:

`meanPairwisePCA4neighbors_v4d_tables.RData`

The file is intentionally excluded from Git because it is a generated binary
analysis object. The export script reads it from this directory and produces:

- `data/exchange_rules_app.csv` for the Shiny app;
- a dated complete table under `data/output/` for local checking.

Run the export from the repository root with:

```r
source("R/create_csv_from_exchange_matrices.R")
```
