#############################
# Safe input for Shiny app
#############################

library(dplyr)
library(purrr)
library(here)

# ------------------------------------------------------------------
# 1. Project-relative file paths
# ------------------------------------------------------------------

project_dir <- here::here()
input_dir <- file.path(project_dir, "data", "input")
output_dir <- file.path(project_dir, "data", "output")

source_file <- file.path(
  input_dir,
  "meanPairwisePCA4neighbors_v4d_tables.RData"
)

date_tag <- format(Sys.Date(), "%Y-%m-%d")

long_file <- file.path(
  output_dir,
  paste0("exchange_rules_long_", date_tag, ".csv")
)

# Stable filename read by app.R and committed with the app.
app_file <- file.path(
  project_dir,
  "data",
  "exchange_rules_app.csv"
)

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(source_file)) {
  stop(
    "Input file not found. Copy the RData file to:\n",
    source_file
  )
}

# ------------------------------------------------------------------
# 2. Load the RData file safely
# ------------------------------------------------------------------

load_environment <- new.env(parent = emptyenv())
loaded_objects <- load(file = source_file,envir = load_environment)

if (length(loaded_objects) != 1L) {
  stop(
    "The RData file must contain exactly one object. Found: ",
    paste(loaded_objects, collapse = ", ")
  )
}

out.list <- load_environment[[loaded_objects]]

if (!is.list(out.list) || is.null(names(out.list))) {
  stop("The loaded object is not a named list.")
}

# ------------------------------------------------------------------
# 3. Function for safe UG conversion
# ------------------------------------------------------------------

normalise_ug <- function(x, variable, taxon) {
  
  x_character <- trimws(as.character(x))
  
  # Also works if values happen to be formatted as "UG 01"
  x_digits <- gsub("[^0-9]", "", x_character)
  
  x_integer <- suppressWarnings(
    as.integer(x_digits)
  )
  
  invalid <- (
    is.na(x_integer) |
      x_integer < 1L |
      x_integer > 22L
  )
  
  if (any(invalid)) {
    bad_values <- unique(x_character[invalid])
    
    stop(
      "Invalid ", variable, " identifiers for taxon ",
      taxon, ": ",
      paste(bad_values, collapse = ", ")
    )
  }
  
  x_integer
}

# ------------------------------------------------------------------
# 4. Combine taxa and normalise identifiers
# ------------------------------------------------------------------

exchange_rules_long <- imap_dfr(
  out.list,
  function(x, taxon) {
    
    if (is.null(x$full_mat)) {
      stop("Taxon ", taxon, " has no full_mat element.")
    }
    
    if (is.null(x$threshold$used)) {
      stop("Taxon ", taxon, " has no threshold$used value.")
    }
    
    required_columns <- c(
      "donor",
      "target",
      "W_j",
      "T_obs",
      "allowed_obs",
      "is_neighbor",
      "n_donor",
      "n_target",
      "too_small",
      "state_obs"
    )
    
    missing_columns <- setdiff(
      required_columns,
      names(x$full_mat)
    )
    
    if (length(missing_columns) > 0L) {
      stop(
        "Taxon ", taxon,
        " is missing columns: ",
        paste(missing_columns, collapse = ", ")
      )
    }
    
    threshold_admix <- x$threshold$threshold_admix
    threshold_zone  <- x$threshold$threshold_zone
    threshold_used  <- x$threshold$used
    
    if (length(threshold_used) != 1L) {
      stop(
        "threshold$used is not a single value for taxon ",
        taxon
      )
    }
    
    x$full_mat |>
      mutate(
        taxon = taxon,
        
        # Numeric IDs are authoritative
        donor_id = normalise_ug(
          donor,
          variable = "donor",
          taxon = taxon
        ),
        
        target_id = normalise_ug(
          target,
          variable = "target",
          taxon = taxon
        ),
        
        # Standardised logical fields
        is_neighbor = as.logical(is_neighbor),
        too_small = as.logical(too_small),
        allowed_obs = as.logical(allowed_obs),
        
        # Standardised decision field
        state_obs = trimws(as.character(state_obs)),
        
        # Taxon-specific thresholds
        threshold_admix = as.numeric(threshold_admix),
        threshold_zone  = as.numeric(threshold_zone),
        threshold_used  = as.numeric(threshold_used),
        
        # Display labels only
        donor_ug = sprintf("UG %02d", donor_id),
        target_ug = sprintf("UG %02d", target_id)
      ) |>
      relocate(
        taxon,
        donor_id,
        target_id,
        donor_ug,
        target_ug,
        threshold_admix,
        threshold_zone,
        threshold_used
      )
  }
)

# ------------------------------------------------------------------
# 5. Validate the combined data
# ------------------------------------------------------------------

# Every taxon should have one row per donor-target combination
duplicate_combinations <- exchange_rules_long |>
  count(taxon, donor_id, target_id) |>
  filter(n != 1L)

if (nrow(duplicate_combinations) > 0L) {
  print(duplicate_combinations)
  
  stop(
    "Duplicate or missing donor-target combinations were detected."
  )
}

# Expected 22 × 22 combinations per taxon
unexpected_taxon_sizes <- exchange_rules_long |>
  count(taxon, name = "rows") |>
  filter(rows != 22L * 22L)

if (nrow(unexpected_taxon_sizes) > 0L) {
  print(unexpected_taxon_sizes)
  
  stop(
    "At least one taxon does not contain exactly 484 combinations."
  )
}

# Check permissible state_obs values
unexpected_states <- setdiff(
  unique(exchange_rules_long$state_obs),
  c("allowed", "not_allowed", "not_considered")
)

unexpected_states <- unexpected_states[
  !is.na(unexpected_states)
]

if (length(unexpected_states) > 0L) {
  stop(
    "Unexpected state_obs values: ",
    paste(unexpected_states, collapse = ", ")
  )
}

# Diagnose the earlier UG 01–09 problem
neighbor_check <- exchange_rules_long |>
  group_by(target_id) |>
  summarise(
    total_rows = n(),
    neighboring_rows = sum(
      is_neighbor %in% TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(target_id)

print(neighbor_check)

targets_without_neighbors <- neighbor_check |>
  filter(neighboring_rows == 0L)

if (nrow(targets_without_neighbors) > 0L) {
  stop(
    "No neighbouring combinations were found for target UG(s): ",
    paste(
      sprintf("%02d", targets_without_neighbors$target_id),
      collapse = ", "
    ),
    ". The is_neighbor variable must be corrected upstream."
  )
}

# Check whether evaluated decisions agree with T_obs < threshold
decision_check <- exchange_rules_long |>
  filter(
    is_neighbor %in% TRUE,
    !(too_small %in% TRUE),
    !is.na(T_obs),
    !is.na(threshold_used),
    state_obs %in% c("allowed", "not_allowed")
  ) |>
  mutate(
    expected_allowed = T_obs < threshold_used,
    stored_allowed = state_obs == "allowed",
    consistent = expected_allowed == stored_allowed
  )

decision_conflicts <- decision_check |>
  filter(!consistent)

if (nrow(decision_conflicts) > 0L) {
  warning(
    nrow(decision_conflicts),
    " evaluated rows disagree with T_obs < threshold_used. ",
    "state_obs will nevertheless remain authoritative."
  )
}

# ------------------------------------------------------------------
# 6. Create the app status
# ------------------------------------------------------------------

app_rules <- exchange_rules_long |>
  mutate(
    status = case_when(
      !(is_neighbor %in% TRUE) ~
        "not_neighbor",
      
      state_obs == "allowed" ~
        "allowed",
      
      state_obs == "not_allowed" ~
        "not_allowed",
      
      state_obs == "not_considered" &
        too_small %in% TRUE ~
        "insufficient_sample",
      
      TRUE ~
        "not_evaluated"
    ),
    
    status_label = recode(
      status,
      allowed =
        "Zulässige Ersatzherkunft",
      not_allowed =
        "Nicht zulässige Ersatzherkunft",
      insufficient_sample =
        "Nicht bewertet: Stichprobe zu klein",
      not_evaluated =
        "Nicht bewertet",
      not_neighbor =
        "Kein benachbartes Ursprungsgebiet"
    )
  ) |>
  select(
    taxon,
    donor_id,
    target_id,
    donor_ug,
    target_ug,
    status,
    status_label,
    state_obs,
    allowed_obs,
    is_neighbor,
    too_small,
    n_donor,
    n_target,
    W_j,
    T_obs,
    threshold_used
  ) |>
  arrange(
    taxon,
    target_id,
    donor_id
  )

# ------------------------------------------------------------------
# 7. Final app-data validation
# ------------------------------------------------------------------

print(
  app_rules |>
    count(status)
)

print(
  app_rules |>
    filter(is_neighbor %in% TRUE) |>
    count(target_id, status) |>
    arrange(target_id, status)
)

# ------------------------------------------------------------------
# 8. Write both CSV files
# ------------------------------------------------------------------

# Base R is used because readr/vroom generated a version warning
write.csv(
  exchange_rules_long,
  file = long_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8",
  quote = TRUE
)

write.csv(
  app_rules,
  file = app_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8",
  quote = TRUE
)

# ------------------------------------------------------------------
# 9. Read the app file back and verify it
# ------------------------------------------------------------------

app_test <- read.csv(
  app_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

stopifnot(
  nrow(app_test) == nrow(app_rules),
  identical(
    sort(unique(app_test$target_id)),
    1:22
  ),
  identical(
    sort(unique(app_test$donor_id)),
    1:22
  )
)

# ------------------------------------------------------------------
# 10. Report output locations
# ------------------------------------------------------------------

message("Master CSV: ", long_file)
message("App CSV: ", app_file)
message(
  "Data preparation complete. Start the app separately with shiny::runApp()."
)
