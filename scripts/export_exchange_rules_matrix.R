# Excel matrices of allowed neighbouring donor UGs for each target UG.
# Run from the project root:
#   install.packages(c("here", "openxlsx")) # once
#   source("scripts/export_exchange_rules_matrix.R")
# Input: data/input/meanPairwisePCA4neighbors_v4d_tables.RData
# Output: data/output/exchange_rules_summary.xlsx (replaced on each run).
# Does not regenerate or modify the app CSV.

export_exchange_rules_matrix <- function(input_file, output_file,
                                         taxon_labels = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop('Install the Excel package first: install.packages("openxlsx")')
  }
  if (!file.exists(input_file)) stop("Input file not found: ", input_file)

  env <- new.env(parent = emptyenv())
  objects <- load(input_file, envir = env)
  if (length(objects) != 1L) {
    stop("Expected exactly one named taxon list in the RData file.")
  }
  taxa_data <- env[[objects]]
  if (!is.list(taxa_data) || !length(taxa_data) ||
      is.null(names(taxa_data)) || anyNA(names(taxa_data)) ||
      any(!nzchar(trimws(names(taxa_data)))) || anyDuplicated(names(taxa_data))) {
    stop("The RData object must be a non-empty list with unique taxon names.")
  }

  as_ug <- function(x, field, taxon) {
    value <- trimws(as.character(x))
    valid <- !is.na(value) & grepl("^(UG\\s*)?0*[1-9][0-9]*$", value)
    id <- suppressWarnings(as.integer(sub("^UG\\s*", "", value)))
    if (any(!valid | is.na(id) | !(id %in% 1:22))) {
      stop("Invalid ", field, " for ", taxon, "; expected UGs 1-22.")
    }
    id
  }
  as_flag <- function(x, field, taxon) {
    value <- toupper(trimws(as.character(x)))
    if (any(!is.na(value) & !(value %in% c("TRUE", "FALSE", "1", "0")))) {
      stop("Invalid logical field ", field, " for ", taxon)
    }
    result <- rep(NA, length(value))
    result[value %in% c("TRUE", "1")] <- TRUE
    result[value %in% c("FALSE", "0")] <- FALSE
    result
  }
  as_count <- function(x, field, taxon) {
    value <- suppressWarnings(as.numeric(as.character(x)))
    missing <- is.na(x) | trimws(as.character(x)) == ""
    invalid <- !missing & (is.na(value) | !is.finite(value) |
                            value < 0 | value != floor(value))
    if (any(invalid)) stop("Invalid sample count ", field, " for ", taxon)
    value[missing] <- NA_real_
    value
  }

  taxa <- sort(names(taxa_data))
  labels <- taxa
  if (!is.null(taxon_labels)) {
    matched <- unname(taxon_labels[taxa])
    labels[!is.na(matched)] <- matched[!is.na(matched)]
    if (anyNA(matched)) warning("Using taxon codes for missing display names: ",
                               paste(taxa[is.na(matched)], collapse = ", "))
  }
  if (anyDuplicated(labels)) stop("Taxon display names must be unique.")
  ordering <- order(labels, taxa)
  taxa <- taxa[ordering]
  labels <- labels[ordering]
  confirmed <- provisional <- data.frame(`Ziel-UG` = 1:22, check.names = FALSE)

  for (i in seq_along(taxa)) {
    taxon <- taxa[i]
    d <- taxa_data[[taxon]]$full_mat
    required <- c("donor", "target", "is_neighbor", "state_obs",
                  "allowed_obs", "too_small", "n_donor", "n_target")
    if (!is.data.frame(d) || !all(required %in% names(d))) {
      stop("Missing full_mat data/columns for ", taxon, ": ",
           paste(setdiff(required, names(d)), collapse = ", "))
    }
    donor <- as_ug(d$donor, "donor", taxon)
    target <- as_ug(d$target, "target", taxon)
    if (nrow(d) != 484L || anyDuplicated(paste(donor, target))) {
      stop("Expected all 484 unique donor-target pairs for ", taxon)
    }
    neighbor <- as_flag(d$is_neighbor, "is_neighbor", taxon)
    small <- as_flag(d$too_small, "too_small", taxon)
    allowed <- as_flag(d$allowed_obs, "allowed_obs", taxon)
    n_donor <- as_count(d$n_donor, "n_donor", taxon)
    n_target <- as_count(d$n_target, "n_target", taxon)
    state <- trimws(as.character(d$state_obs))
    if (any(!is.na(state) & !(state %in% c("allowed", "not_allowed", "not_considered")))) {
      stop("Unexpected state_obs for ", taxon)
    }
    if (anyNA(neighbor) || anyNA(small)) {
      stop("Missing is_neighbor or too_small flags for ", taxon)
    }
    # Preserve direction: donor is listed in the row for its TARGET.
    # Match app.R: N < 2 is never evaluable; too_small takes precedence
    # over state_obs, and provisional decisions use allowed_obs.
    eligible <- neighbor & donor != target &
      !is.na(n_donor) & !is.na(n_target) & n_donor >= 2 & n_target >= 2
    evaluated <- eligible & !small & state %in% c("allowed", "not_allowed")
    provisional_evaluated <- eligible & small & !is.na(allowed)
    keep_confirmed <- evaluated & state %in% "allowed"
    keep_provisional <- provisional_evaluated & allowed %in% TRUE

    summarise_zones <- function(keep) {
      vapply(1:22, function(ug) {
        # NA means no evaluable neighbouring pair of either class.
        if (!any(target == ug & (evaluated | provisional_evaluated))) return("NA")
        zones <- sort(unique(donor[target == ug & keep]))
        if (!length(zones)) return("-")
        paste(zones, collapse = ", ")
      }, character(1))
    }
    confirmed[[labels[i]]] <- summarise_zones(keep_confirmed)
    provisional[[labels[i]]] <- summarise_zones(keep_provisional)
  }

  notes <- data.frame(
    Eintrag = c("Zulaessig", "Vorlaeufig", "Richtung", "Zellwerte", "NA",
                "Unvollstaendige Bewertung", "Quelle", "Quelldatum", "Exportdatum"),
    Beschreibung = c(
      "Bestätigt zulässige Nachbar-UGs: state_obs = allowed, too_small = FALSE, N in beiden UGs >= 2.",
      "Eher zulässige Nachbar-UGs: too_small = TRUE, allowed_obs = TRUE, N in beiden UGs >= 2. Separat von bestätigten Zulassungen.",
      "Zeile = Ziel-UG; Zellinhalt = benachbarte Herkunfts-/Spender-UGs. Das Ziel-UG selbst wird nicht aufgeführt.",
      "UG-Nummern aufsteigend, durch Komma getrennt. - = keine zulässige Herkunft in der jeweiligen Bewertungsklasse.",
      "NA = für dieses Taxon und Ziel-UG ist keine benachbarte Herkunft bewertbar (weder bestätigt noch vorläufig).",
      "Listen enthalten nur bewertbare Paare; eine nicht aufgeführte UG kann auch unbewertet sein. NA und - sind Textwerte.",
      basename(input_file),
      format(file.info(input_file)$mtime, "%Y-%m-%d"),
      format(Sys.Date(), "%Y-%m-%d")
    ), check.names = FALSE
  )
  taxon_key <- data.frame(Taxon = taxa, Art = labels, check.names = FALSE)
  wb <- openxlsx::createWorkbook()
  sheets <- list(Zulaessig = confirmed, Vorlaeufig = provisional,
                 Taxa = taxon_key, Hinweise = notes)
  header <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#285E52",
                                  textDecoration = "bold", wrapText = TRUE,
                                  valign = "center")
  for (sheet in names(sheets)) {
    tab <- sheets[[sheet]]
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tab, headerStyle = header, withFilter = TRUE)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE)
    openxlsx::setColWidths(wb, sheet, 1, if (sheet == "Hinweise") 30 else 16)
    openxlsx::setColWidths(wb, sheet, 2:ncol(tab),
                          if (sheet == "Hinweise") 100 else 27)
    openxlsx::setRowHeights(wb, sheet, 1, 65)
    openxlsx::addStyle(wb, sheet,
                      openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                      rows = 2:(nrow(tab) + 1L), cols = seq_len(ncol(tab)),
                      gridExpand = TRUE)
    if (sheet == "Hinweise") {
      openxlsx::setRowHeights(wb, sheet, 2:(nrow(tab) + 1L), 48)
    }
  }
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  # Verify the saved matrix, including comma-separated text and all 22 UGs.
  for (sheet in c("Zulaessig", "Vorlaeufig")) {
    actual <- openxlsx::read.xlsx(output_file, sheet = sheet, check.names = FALSE,
                                 na.strings = character())
    expected <- sheets[[sheet]]
    if (!identical(names(actual), names(expected)) ||
        !isTRUE(all.equal(actual, expected, check.attributes = FALSE))) {
      stop("Excel read-back verification failed for sheet ", sheet)
    }
  }
  message("Excel summary: ", normalizePath(output_file, winslash = "/"))
  invisible(list(confirmed = confirmed, provisional = provisional))
}

# Display names match app.R; unknown taxa retain their original codes.

taxon_labels <- c(
  "ACH.AGG" = "Achillea millefolium agg.",
  "ACH.MIL" = "Achillea millefolium s.str.",
  "ACH.PRA" = "Achillea collina/pratensis",
  "AGR.CAP" = "Agrostis capillaris",
  "AGR.EUP" = "Agrimonia eupatoria",
  "ANT.ODO" = "Anthoxanthum odoratum",
  "ARR.ELA" = "Arrhenatherum elatius",
  "BIS.OFF" = "Bistorta officinalis",
  "BRO.ERE" = "Bromus erectus",
  "CAM.R2x" = "Campanula rotundifolia 2x",
  "CAM.R4x" = "Campanula rotundifolia 4x",
  "CAM.ROT" = "Campanula rotundifolia s.l.",
  "CEN.JAC" = "Centaurea jacea",
  "COR.CAN" = "Corynephorus canescens",
  "CYN.CRI" = "Cynosurus cristatus",
  "EUP.C4x" = "Euphorbia cyparissias 4x",
  "EUP.CYP" = "Euphorbia cyparissias s.l.",
  "FES.NIG" = "Festuca nigrescens",
  "FES.RRU" = "Festuca rubra s.str.",
  "FES.RUB" = "Festuca rubra s.l.",
  "FIL.ULM" = "Filipendula ulmaria",
  "GAL.ALB" = "Galium album",
  "HYP.RAD" = "Hypochaeris radicata",
  "KNA.A4x" = "Knautia arvensis 4x",
  "LAT.PRA" = "Lathyrus pratensis",
  "LEU.AGG" = "Leucanthemum vulgare s.l.",
  "LEU.IRC" = "Leucanthemum ircutianum",
  "LEU.VUL" = "Leucanthemum vulgare s.str.",
  "LOT.COR" = "Lotus corniculatus",
  "LYC.FLO" = "Lychnis flos-cuculi",
  "PIM.S2x" = "Pimpinella saxifraga 2x",
  "PIM.S4x" = "Pimpinella saxifraga 4x",
  "PIM.SAX" = "Pimpinella saxifraga s.l.",
  "PRU.VUL" = "Prunella vulgaris",
  "RAN.ACR" = "Ranunculus acris",
  "SAL.PRA" = "Salvia pratensis",
  "SIL.VUL" = "Silene vulgaris",
  "THY.PUL" = "Thymus pulegioides",
  "TRA.AGG" = "Tragopogon pratensis s.l.",
  "TRA.ORI" = "Tragopogon orientalis",
  "TRA.PRA" = "Tragopogon pratensis/minor"
)

if (!requireNamespace("here", quietly = TRUE)) {
  stop('Install the path package first: install.packages("here")')
}
export_exchange_rules_matrix(
  input_file = here::here("data", "input",
                          "meanPairwisePCA4neighbors_v4d_tables.RData"),
  output_file = here::here("data", "output", "exchange_rules_summary.xlsx"),
  taxon_labels = taxon_labels
)
