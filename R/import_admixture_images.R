# One-time import of the taxon-specific Admixture maps ------------------------
#
# Run this file from the Austauschregeln_App project root.
# The source PNGs stay unchanged. Processed copies are written to
# www/admixture/, which is the directory served by Shiny.

required_package <- "magick"

if (!requireNamespace(required_package, quietly = TRUE)) {
  stop(
    "Package 'magick' is required. Install it once with: ",
    "install.packages('magick')"
  )
}

source_root <- normalizePath(
  "C:/Users/michal/Nextcloud4/Cloud/RegioDiv/Data4d",
  winslash = "/",
  mustWork = TRUE
)

destination_dir <- file.path("www", "admixture")
dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)

kopt_min <- c(
  "ACH.AGG" = 2, "ACH.MIL" = 1, "ACH.PRA" = 2,
  "AGR.EUP" = 4, "AGR.CAP" = 2, "ANT.ODO" = 3,
  "ARR.ELA" = 2, "BIS.OFF" = 3, "BRO.ERE" = 2,
  "CAM.ROT" = 2, "CAM.R2x" = 4, "CAM.R4x" = 2,
  "CEN.JAC" = 4, "COR.CAN" = 4, "CYN.CRI" = 2,
  "EUP.CYP" = 2, "EUP.C4x" = 4,
  "FES.RUB" = 2, "FES.NIG" = 2, "FES.RRU" = 1,
  "FIL.ULM" = 3, "GAL.ALB" = 4, "HYP.RAD" = 2,
  "KNA.A4x" = 5, "LAT.PRA" = 3,
  "LEU.AGG" = 2, "LEU.IRC" = 3, "LEU.VUL" = 1,
  "LOT.COR" = 2, "LYC.FLO" = 4,
  "PIM.SAX" = 2, "PIM.S2x" = 2, "PIM.S4x" = 2,
  "PRU.VUL" = 4, "RAN.ACR" = 3, "SAL.PRA" = 4,
  "SIL.VUL" = 2, "THY.PUL" = 4,
  "TRA.AGG" = 2, "TRA.PRA" = 4, "TRA.ORI" = 2
)

# K = 1 taxa deliberately have no Admixture image. The app draws the UG
# outline for them.
image_taxa <- names(kopt_min)[!is.na(kopt_min) & kopt_min > 1]

import_log <- lapply(image_taxa, function(taxon) {
  k <- unname(kopt_min[[taxon]])
  stem <- tolower(taxon)

  source_file <- file.path(
    source_root,
    stem,
    "Admixture",
    paste0(stem, "_Admixture.interpolQ.K", k, ".png")
  )

  destination_file <- file.path(
    destination_dir,
    paste0(taxon, "_K", k, ".png")
  )

  if (!file.exists(source_file)) {
    return(data.frame(
      taxon = taxon,
      k = k,
      status = "missing",
      source = source_file,
      destination = destination_file
    ))
  }

  image <- magick::image_read(source_file)

  # The source plots use a common layout: the taxon title and K label occupy
  # the upper 15%, while the actual map begins below that band.
  image_info <- magick::image_info(image)
  crop_top <- floor(image_info$height * 0.15)

  image <- magick::image_crop(
    image,
    geometry = magick::geometry_area(
      width = image_info$width,
      height = image_info$height - crop_top,
      x_off = 0,
      y_off = crop_top
    ),
    repage = TRUE
  )

  # Remove remaining uniform outer whitespace, preserve aspect ratio, and
  # keep only the resolution required by the small app preview.
  image <- magick::image_trim(image, fuzz = 8)
  image <- magick::image_resize(image, "300x440>")

  # A limited palette is sufficient for these maps and considerably reduces
  # the GitHub repository size.
  image <- magick::image_quantize(
    image,
    max = 128,
    colorspace = "rgb",
    dither = TRUE
  )

  magick::image_write(
    image,
    path = destination_file,
    format = "png",
    depth = 8,
    defines = c(
      "png:compression-level" = "9",
      "png:compression-filter" = "5"
    )
  )

  data.frame(
    taxon = taxon,
    k = k,
    status = "copied",
    source = source_file,
    destination = destination_file
  )
})

import_log <- do.call(rbind, import_log)
print(import_log[, c("taxon", "k", "status")], row.names = FALSE)

missing_files <- import_log$source[import_log$status == "missing"]

if (length(missing_files) > 0L) {
  warning(
    length(missing_files),
    " expected PNG file(s) were not found. See 'import_log' for paths."
  )
} else {
  message(
    "All ", nrow(import_log),
    " Admixture images were written to: ",
    normalizePath(destination_dir, winslash = "/", mustWork = TRUE)
  )
}
