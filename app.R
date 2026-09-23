library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(DT)

# Data -----------------------------------------------------------------------

app_data_file <- file.path(
  "data",
  "exchange_rules_app.csv"
)

rules <- read.csv(
  app_data_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_columns <- c(
  "taxon", "donor_id", "target_id", "state_obs", "allowed_obs",
  "is_neighbor", "too_small", "n_donor", "n_target"
)

missing_columns <- setdiff(required_columns, names(rules))
if (length(missing_columns) > 0L) {
  stop(
    "The app data are missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if ("data_version" %in% names(rules)) {
  available_versions <- unique(
    trimws(as.character(rules$data_version))
  )
  available_versions <- available_versions[
    !is.na(available_versions) & nzchar(available_versions)
  ]

  data_version <- if (length(available_versions) > 0L) {
    available_versions[1]
  } else {
    format(
      file.info(app_data_file)$mtime,
      "%Y-%m-%d %H:%M:%S"
    )
  }
} else {
  data_version <- format(
    file.info(app_data_file)$mtime,
    "%Y-%m-%d %H:%M:%S"
  )
}

data_version_file <- gsub(
  "[^0-9-]",
  "",
  substr(data_version, 1, 10)
)

as_app_logical <- function(x) {
  case_when(
    x %in% TRUE ~ TRUE,
    toupper(trimws(as.character(x))) == "TRUE" ~ TRUE,
    toupper(trimws(as.character(x))) == "FALSE" ~ FALSE,
    TRUE ~ NA
  )
}

rules <- rules |>
  mutate(
    taxon = trimws(as.character(taxon)),
    donor_id = as.integer(as.character(donor_id)),
    target_id = as.integer(as.character(target_id)),
    state_obs = trimws(as.character(state_obs)),
    allowed_obs = as_app_logical(allowed_obs),
    is_neighbor = as_app_logical(is_neighbor),
    too_small = as_app_logical(too_small)
  )

if (anyNA(rules$donor_id) || anyNA(rules$target_id)) {
  stop("Invalid donor_id or target_id in app data.")
}

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
  "EUP.CYP" = "Euphorbia cyparissias",
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
  "PIM.SAX" = "Pimpinella saxifraga",
  "PRU.VUL" = "Prunella vulgaris",
  "RAN.ACR" = "Ranunculus acris",
  "SAL.PRA" = "Salvia pratensis",
  "SIL.VUL" = "Silene vulgaris",
  "THY.PUL" = "Thymus pulegioides",
  "TRA.AGG" = "Tragopogon pratensis s.l.",
  "TRA.ORI" = "Tragopogon orientalis",
  "TRA.PRA" = "Tragopogon pratensis/minor"
)

taxa <- sort(unique(rules$taxon))

missing_taxon_labels <- setdiff(taxa, names(taxon_labels))
if (length(missing_taxon_labels) > 0L) {
  stop(
    "Missing display names for taxa: ",
    paste(missing_taxon_labels, collapse = ", ")
  )
}

taxon_choices <- setNames(
  names(taxon_labels),
  unname(taxon_labels)
)

species_picture_files <- c(
  "ANT.ODO" = "ant.odo_02_800px.jpg",
  "ARR.ELA" = "Arr.ela_800.jpg",
  "CAM.ROT" = "cam.rot_square_800px_v2.JPG",
  "CAM.R2x" = "cam.rot_square_800px_v2.JPG",
  "CAM.R4x" = "cam.rot_square_800px_v2.JPG",
  "CYN.CRI" = "cyn_cri_02_black_800.JPG",
  "FES.RUB" = "fes.rub_02_black_800px.JPG",
  "FES.RRU" = "fes.rub_02_black_800px.JPG",
  "FES.NIG" = "fes.rub_02_black_800px.JPG",
  "GAL.ALB" = "gal.alb_black_800px.JPG",
  "HYP.RAD" = "hyp.rad_03_black_800px.jpg",
  "KNA.A4x" = "kna.arv_01_black_800.JPG",
  "LEU.AGG" = "leu.agg_03_black_800px.JPG",
  "LEU.IRC" = "leu.agg_03_black_800px.JPG",
  "LEU.VUL" = "leu.agg_03_black_800px.JPG",
  "LOT.COR" = "lot.cor_800px.JPG",
  "RAN.ACR" = "ran.acr_04_black_800px.JPG",
  "SAL.PRA" = "sal.pra_04_black_800px.JPG",
  "TRA.AGG" = "tra.agg_01_black_800px.jpg",
  "TRA.PRA" = "tra.agg_01_black_800px.jpg",
  "TRA.ORI" = "tra.agg_01_black_800px.jpg"
)

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

missing_k <- setdiff(taxa, names(kopt_min))
if (length(missing_k) > 0L) {
  stop(
    "Missing kopt_min values for taxa: ",
    paste(missing_k, collapse = ", ")
  )
}

# Map polygons ---------------------------------------------------------------

shape_file <- file.path(
  "data", "shp", "Regiosaatgut2_Dissolve3_trans.shp"
)

if (!file.exists(shape_file)) {
  stop("UG shapefile not found: ", shape_file)
}

ug_shape <- st_read(
  shape_file,
  quiet = TRUE,
  options = "ENCODING=UTF-8"
)

if (!"Herkunft" %in% names(ug_shape)) {
  stop(
    "The shapefile needs a 'Herkunft' column. Found: ",
    paste(names(ug_shape), collapse = ", ")
  )
}

ug_shape <- ug_shape |>
  mutate(ug_id = as.integer(as.character(Herkunft))) |>
  st_make_valid() |>
  st_transform(4326) |>
  st_crop(
    xmin = 5.5,
    ymin = 47.0,
    xmax = 15.5,
    ymax = 55.2
  )

if (anyNA(ug_shape$ug_id) || !all(1:22 %in% ug_shape$ug_id)) {
  stop("The shapefile does not contain valid identifiers for UGs 01-22.")
}

ug_label_points <- ug_shape |>
  st_transform(3035) |>
  st_point_on_surface() |>
  st_transform(4326)

label_coordinates <- st_coordinates(ug_label_points)
ug_label_points$label_lng <- label_coordinates[, 1]
ug_label_points$label_lat <- label_coordinates[, 2]

map_bbox <- st_bbox(ug_shape)
map_lon_padding <- unname((map_bbox["xmax"] - map_bbox["xmin"]) * 0.05)
map_lat_padding <- unname((map_bbox["ymax"] - map_bbox["ymin"]) * 0.05)

map_colors <- c(
  other = "#F7F7F7",
  neighbor = "#BDBDBD",
  allowed = "#2E8B57",
  not_allowed = "#D95F02",
  allowed_small = "#A8D5BA",
  not_allowed_small = "#F4B183",
  target = "#2C7FB8"
)

# UI -------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(htmltools::HTML(
      "
      body { overflow-y: auto; font-size: 15px; }
      .container-fluid { padding: 7px 12px; }
      h2 { margin: 3px 0 1px 0; font-size: 26px; }
      h4 { margin: 4px 0 6px 0; font-size: 17px; }
      .app-subtitle { color: #5f6368; margin-bottom: 16px; font-size: 14px; }
      .selectize-control { z-index: 2000 !important; }
      .selectize-dropdown { z-index: 20000 !important; }
      .selectize-input,
      .selectize-dropdown,
      .form-control {
        font-size: 14px !important;
      }

      .control-row {
        background: #f5f6f7; border-radius: 7px;
        padding: 5px 9px 0 9px; margin-bottom: 6px;
      }
      .control-row .form-group { margin-bottom: 5px; }
      .control-row label { font-size: 14px; margin-bottom: 2px; }

      .side-panel-stack {
        width: 100%;
        height: calc(100vh - 165px);
        min-height: 320px; max-height: 680px;
        display: flex; flex-direction: column; gap: 8px;
      }
      .side-picture-column {
        padding-left: 0px;
      }
      #admixture_panel {
        flex: 2 1 0; min-height: 0;
      }
      .species-thumbnail {
        flex: 1 1 0; min-height: 0; width: 100%;
        position: relative;
        border: 2px solid #68737d; border-radius: 7px;
        background: #fafafa;
        display: flex; align-items: center; justify-content: center;
        color: #8a9298; font-size: 11px;
        overflow: hidden;
      }
      #species_picture {
        position: relative; width: 100%; height: 100%;
      }
      .species-thumbnail img {
        width: 100%; height: 100%; object-fit: cover;
        display: block;
      }
      .species-name-overlay {
        position: absolute; right: 5px; bottom: 5px;
        max-width: calc(100% - 10px);
        padding: 2px 5px; border-radius: 3px;
        background: rgba(255,255,255,0.82);
        color: #30363b; font-size: 10px; line-height: 1.2;
        text-align: right;
      }
      .species-picture-missing {
        width: 100%; height: 100%;
        display: flex; align-items: center; justify-content: center;
        color: #8a9298; font-size: 11px;
      }
      .admixture-panel {
        height: 100%; min-height: 0;
        border: 2px solid #68737d; border-radius: 7px;
        padding: 6px; background: #f7f8f9;
        display: flex; flex-direction: column;
      }
      .admixture-title {
        margin: 0 0 5px 0; color: #454b50;
        font-size: 14px; font-weight: 600;
      }
      .admixture-map {
        flex: 1 1 auto; min-height: 0; width: 100%;
        background: white; border: 1px solid #d9dde1;
        border-radius: 5px; overflow: hidden;
        display: flex; align-items: center; justify-content: center;
      }
      .admixture-map img {
        width: 100%; height: 100%; object-fit: contain;
      }
      .admixture-map .shiny-plot-output {
        width: 100% !important; height: 100% !important;
      }
      .admixture-legend {
        margin-top: 6px; padding: 6px 7px;
        border: 1px solid #d9dde1; border-radius: 5px;
        background: white; color: #454b50;
        font-size: 11px; line-height: 1.4;
      }
      .admixture-missing {
        padding: 8px; color: #777; font-size: 11px;
        text-align: center;
      }

      .map-panel {
        width: 100%; max-width: none; margin: 0;
        height: calc(100vh - 165px);
        min-height: 320px; max-height: 680px;
        border: 2px solid #68737d; border-radius: 7px;
        overflow: hidden; background: #eef2f3;
      }
      .leaflet-container {
        height: 100% !important;
        background: #dceaf3 !important;
      }

      .table-panel {
        border: 1px solid #d9dde1; border-radius: 7px;
        padding: 7px 9px; background: white;
      }
      .table-panel table {
        width: 100% !important;
        font-size: 14px; margin-bottom: 0;
      }
      .table-panel table th,
      .table-panel table td {
        white-space: nowrap !important;
      }
      .table-panel table th:nth-child(2),
      .table-panel table td:nth-child(2) {
        min-width: 205px;
      }
      .table-panel .table > thead > tr > th,
      .table-panel .table > tbody > tr > td {
        padding: 4px 5px;
      }

      .dt-buttons .dt-button,
      .dt-buttons .btn {
        font-size: 11px !important;
        line-height: 1.2 !important;
        padding: 2px 6px !important;
        margin: 0 3px 5px 0 !important;
      }

      .info.legend {
        font-size: 11px; line-height: 14px;
        padding: 4px 6px; max-width: 180px;
      }
      .info.legend i {
        width: 11px; height: 11px; margin-right: 4px;
      }

      .leaflet-top.leaflet-left {
        width: 100%;
      }
      .map-title-wrapper {
        width: calc(100% - 20px);
        box-sizing: border-box;
      }
      .map-title-box {
        width: 100%; max-width: none;
        box-sizing: border-box;
        background: rgba(255,255,255,0.92);
        border: 1px solid #c8cdd1;
        border-radius: 5px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.18);
        color: #30363b;
        font-size: 11px;
        font-weight: 600;
        line-height: 1.3;
        padding: 5px 7px;
      }

      .map-instructions {
        max-width: 180px;
        background: rgba(255,255,255,0.92);
        border: 1px solid #c8cdd1;
        border-radius: 5px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.18);
        color: #3c4043;
        font-size: 11px;
        line-height: 1.35;
        padding: 5px 7px;
      }

      .methods-box {
        position: static; width: 100%; margin-top: 8px;
        padding: 9px 11px; box-sizing: border-box;
        border: 2px solid #68737d; border-radius: 7px;
        background: #f7f8f9; color: #454b50;
        font-size: 11px; line-height: 1.45;
      }
      .methods-box-title {
        margin-bottom: 5px; color: #30363b;
        font-size: 13px; font-weight: 600;
      }
      .methods-box p { margin: 0 0 6px 0; }
      .methods-box p:last-child { margin-bottom: 0; }

      .ug-number-label {
        background: rgba(255,255,255,0.38);
        border: 0; box-shadow: none;
        color: #777f85; font-weight: 600; font-size: 10px;
        padding: 0 2px;
      }
      .ug-number-label-white {
        background: rgba(40,40,40,0.18);
        color: #ffffff;
        text-shadow: 0 1px 2px rgba(0,0,0,0.8);
      }
      "
    ))
  ),

  h2("Ersatzherkünfte für Regiosaatgut"),
  div(
    class = "app-subtitle",
    textOutput("app_subtitle", inline = TRUE)
  ),

  fluidRow(
    column(
      4,
      div(
        class = "map-panel",
        leafletOutput("ug_map", height = "100%")
      )
    ),
    
    column(
      2,
      class = "side-picture-column",
      div(
        class = "side-panel-stack",
        uiOutput("admixture_panel"),
        div(
          class = "species-thumbnail",
          uiOutput("species_picture")
        )
      )
    ),
    
    
    column(
      6,
      fluidRow(
        class = "control-row",
        column(
          8,
          selectInput(
            "taxon", "Art oder Taxon (Bitte selektieren)",
            choices = taxon_choices,
            selected = sample(taxa, size = 1L),
            width = "100%"
          )
        ),
        column(
          4,
          selectInput(
            "target", "Ziel-UG",
            choices = NULL,
            width = "100%"
          )
        )
      ),
      div(
        class = "table-panel",
        h4("Benachbarte Herkunftsgebiete"),
        DTOutput("results")
      ),
      div(
        class = "methods-box",
        div(
          class = "methods-box-title",
          "Methodik und Zitation"
        ),
        tags$p(
          "Die Bewertung von Nachbar-Ursprungsgebieten (UG) als mögliche ",
          "Ersatzherkünfte für ein Ziel-UG basiert auf genetischen Distanzen ",
          "zwischen Individuen, die anhand von ddRAD-SNP-Daten ermittelt ",
          "wurden (Durka et al. 2025). Für das Ziel-Ursprungsgebiet (UG) ",
          "wird die mittlere genetische Distanz zu einer potenziellen ",
          "Ersatzherkunft mit der mittleren Distanz innerhalb des Ziel-UG ",
          "verglichen. Die Ersatzherkunft wird als genetisch unbedenklich ",
          "eingestuft, wenn die aus dem Vergleich resultierende Differenz ",
          "einen artspezifischen Referenzwert nicht überschreitet. Dieser ",
          "Referenzwert wird aus der räumlich-genetischen Struktur der ",
          "jeweiligen Art abgeleitet. Wird keine plausible räumlich ",
          "strukturierte Differenzierung festgestellt (K = 1), dienen die ",
          "mittleren Unterschiede zwischen allen UGs als Referenzwert. ",
          "Blasse Farben kennzeichnen vorläufige Bewertungen, wenn in ",
          "mindestens einem der verglichenen UGs weniger als fünf ",
          "Individuen untersucht wurden."
        ),
        tags$p(
          tags$strong("Bei Verwendung bitte zitieren: "),
          "Durka et al. (2025), die NuL-Publikation zu den genetisch ",
          "begründeten Austauschregeln sowie den BfN-Bericht zum ",
          "RegioDiv-Projekt (vollständige bibliografische Angaben werden ",
          "ergänzt)."
        )
      )
    )
  )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$taxon, {
    targets <- rules |>
      filter(taxon == input$taxon) |>
      distinct(target_id) |>
      arrange(target_id) |>
      pull(target_id)

    previous_target <- isolate(
      suppressWarnings(as.integer(input$target))
    )

    selected_target <- if (
      !is.na(previous_target) &&
      previous_target %in% targets
    ) {
      previous_target
    } else {
      targets[1]
    }

    updateSelectInput(
      session,
      "target",
      choices = setNames(targets, sprintf("UG %02d", targets)),
      selected = selected_target
    )
  }, ignoreInit = FALSE)

  observeEvent(input$ug_map_shape_rightclick, {
    clicked_ug <- suppressWarnings(
      as.integer(input$ug_map_shape_rightclick$id)
    )

    available_targets <- rules |>
      filter(taxon == input$taxon) |>
      distinct(target_id) |>
      pull(target_id)

    if (!is.na(clicked_ug) && clicked_ug %in% available_targets) {
      updateSelectInput(
        session,
        "target",
        selected = clicked_ug
      )
    }
  })

  output$species_picture <- renderUI({
    req(input$taxon)

    file_name <- unname(species_picture_files[input$taxon])

    if (
      length(file_name) == 1L &&
      !is.na(file_name) &&
      file.exists(file.path("www", "species_pics", file_name))
    ) {
      tagList(
        tags$img(
          src = paste0("species_pics/", file_name),
          alt = taxon_labels[[input$taxon]]
        ),
        div(
          class = "species-name-overlay",
          tags$em(taxon_labels[[input$taxon]])
        )
      )
    } else {
      div(
        class = "species-picture-missing",
        "Kein Artenbild verfügbar"
      )
    }
  })

  output$admixture_panel <- renderUI({
    req(input$taxon)
    k <- unname(kopt_min[[input$taxon]])

    if (is.na(k)) {
      return(
        div(
          class = "admixture-panel",
          div(class = "admixture-title", "Genetische Struktur"),
          div(
            class = "admixture-map",
            div(class = "admixture-missing", "Kein K bestimmt")
          ),
          div(
            class = "admixture-legend",
            "Für dieses Taxon wurde kein Referenzwert aus einer ",
            "Clusterlösung abgeleitet."
          )
        )
      )
    }

    if (k == 1) {
      map_content <- plotOutput(
        "cluster_shape",
        width = "100%",
        height = "100%"
      )
      explanation <- tagList(
        tags$strong("Referenzwert: K = 1. "),
        "Der Referenzwert für die Austauschentscheidung basiert hier auf ",
        "den mittleren Unterschieden zwischen den UGs, da keine distinkte ",
        "räumlich-genetische Struktur gefunden wurde."
      )
    } else {
      file_name <- paste0(input$taxon, "_K", k, ".png")
      local_file <- file.path("www", "admixture", file_name)

      map_content <- if (file.exists(local_file)) {
        tags$img(
          src = file.path("admixture", file_name),
          alt = paste0(
            "Interpolierte Clusterzugehörigkeit für ",
            taxon_labels[[input$taxon]],
            ", K = ", k
          )
        )
      } else {
        div(
          class = "admixture-missing",
          paste0("Admixture-Karte für K = ", k, " fehlt")
        )
      }

      explanation <- tagList(
        tags$strong(paste0("Referenzwert: K = ", k, ". ")),
        "Der Referenzwert für die Austauschentscheidung wurde aus dieser ",
        "Clusterlösung abgeleitet. Die Karte zeigt die zugehörige ",
        "interpolierte genetische Clusterzugehörigkeit."
      )
    }

    div(
      class = "admixture-panel",
      div(
        class = "admixture-title",
        if (k == 1) {
          "UG-Struktur (K = 1)"
        } else {
          paste0("Genetische Cluster (K = ", k, ")")
        }
      ),
      div(class = "admixture-map", map_content),
      div(class = "admixture-legend", explanation)
    )
  })

  output$cluster_shape <- renderPlot({
    old_par <- par(
      mar = c(0, 0, 0, 0),
      oma = c(0, 0, 0, 0),
      xaxs = "i",
      yaxs = "i"
    )
    on.exit(par(old_par), add = TRUE)

    plot(
      st_geometry(ug_shape),
      col = "#D8DDE1",
      border = "#68737d",
      lwd = 0.7,
      axes = FALSE,
      reset = FALSE
    )
  },
  bg = "transparent",
  res = 110,
  execOnResize = TRUE
  )

  selected_rules <- reactive({
    req(input$taxon, input$target)

    rules |>
      filter(
        taxon == input$taxon,
        target_id == as.integer(input$target)
      )
  })

  neighboring_results <- reactive({
    selected_rules() |>
      filter(is_neighbor %in% TRUE) |>
      mutate(
        display_decision = case_when(
          too_small %in% TRUE & allowed_obs %in% TRUE ~
            "Vorläufig zulässig (N < 5)",
          too_small %in% TRUE & allowed_obs %in% FALSE ~
            "Vorläufig nicht zulässig (N < 5)",
          state_obs == "allowed" ~ "Zulässig",
          state_obs == "not_allowed" ~ "Nicht zulässig",
          TRUE ~ "Nicht bewertet"
        )
      )
  })

  map_data <- reactive({
    req(input$target)
    target_id <- as.integer(input$target)

    decisions <- neighboring_results() |>
      select(
        donor_id, display_decision, allowed_obs, too_small,
        state_obs, n_donor, n_target
      )

    ug_shape |>
      left_join(decisions, by = c("ug_id" = "donor_id")) |>
      mutate(
        map_class = case_when(
          ug_id == target_id ~ "target",
          too_small %in% TRUE & allowed_obs %in% TRUE ~ "allowed_small",
          too_small %in% TRUE & allowed_obs %in% FALSE ~ "not_allowed_small",
          state_obs == "allowed" ~ "allowed",
          state_obs == "not_allowed" ~ "not_allowed",
          !is.na(display_decision) ~ "neighbor",
          TRUE ~ "other"
        ),
        decision_label = case_when(
          ug_id == target_id ~ "Gewähltes Zielgebiet",
          !is.na(display_decision) ~ display_decision,
          TRUE ~ "Kein benachbartes Herkunftsgebiet"
        ),
        fill_color = unname(map_colors[map_class]),
        border_color = if_else(ug_id == target_id, "#084C7F", "#666666"),
        border_weight = if_else(ug_id == target_id, 3, 1),
        popup_text = paste0(
          "<strong>UG ", sprintf("%02d", ug_id), "</strong><br>",
          decision_label,
          ifelse(!is.na(n_donor), paste0("<br>N Herkunft: ", n_donor), ""),
          ifelse(!is.na(n_target), paste0("<br>N Zielgebiet: ", n_target), "")
        ),
        hover_text = paste0(
          "UG ", sprintf("%02d", ug_id), ": ", decision_label
        )
      )
  })

  output$app_subtitle <- renderText({
    paste0(
      "Bewertung benachbarter Ursprungsgebiete nach Art oder Taxon",
      " | Datenstand: ",
      data_version
    )
  })

  output$ug_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        dragging = FALSE,
        scrollWheelZoom = FALSE,
        doubleClickZoom = FALSE,
        boxZoom = FALSE,
        keyboard = FALSE,
        touchZoom = FALSE,
        attributionControl = FALSE,
        zoomSnap = 0.1,
        zoomDelta = 0.1
      )
    ) |>
      addTiles(
        urlTemplate = paste0(
          "https://server.arcgisonline.com/ArcGIS/rest/services/",
          "World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}"
        ),
        group = "basemap",
        options = tileOptions(
          minZoom = 5,
          maxZoom = 8,
          noWrap = TRUE
        ),
        attribution = "Esri, USGS, NOAA"
      ) |>
      fitBounds(
        lng1 = unname(map_bbox["xmin"]) - map_lon_padding,
        lat1 = unname(map_bbox["ymin"]) - map_lat_padding,
        lng2 = unname(map_bbox["xmax"]) + map_lon_padding,
        lat2 = unname(map_bbox["ymax"]) + map_lat_padding
      ) |>
      htmlwidgets::onRender(
        "
        function(el, x) {
          var map = this;

          function layerIdFor(layer) {
            var registry = map.layerManager._byLayerId || {};

            if (registry.shape) {
              var nestedKeys = Object.keys(registry.shape);
              for (var i = 0; i < nestedKeys.length; i++) {
                if (registry.shape[nestedKeys[i]] === layer) {
                  return nestedKeys[i];
                }
              }
            }

            var keys = Object.keys(registry);
            for (var j = 0; j < keys.length; j++) {
              if (registry[keys[j]] === layer) {
                var parts = keys[j].split('\\n');
                if (parts.length === 1 || parts[0] === 'shape') {
                  return parts[parts.length - 1];
                }
              }
            }

            return null;
          }

          function bindRightClick(layer) {
            window.setTimeout(function() {
              var id = layerIdFor(layer);
              if (id === null || !layer || typeof layer.on !== 'function') {
                return;
              }

              layer.on('contextmenu', function(e) {
                if (e.originalEvent) {
                  L.DomEvent.preventDefault(e.originalEvent);
                }

                Shiny.setInputValue(
                  el.id + '_shape_rightclick',
                  {id: id, nonce: Math.random()},
                  {priority: 'event'}
                );
              });
            }, 0);
          }

          map.on('layeradd', function(e) {
            bindRightClick(e.layer);
          });

          map.eachLayer(function(layer) {
            bindRightClick(layer);
          });
        }
        "
      )
  })

  observe({
    polygons <- map_data()

    label_data <- ug_label_points |>
      left_join(
        polygons |>
          st_drop_geometry() |>
          select(ug_id, map_class),
        by = "ug_id"
      )

    leafletProxy(
      "ug_map",
      data = polygons,
      session = session
    ) |>
      clearGroup("decision") |>
      clearControls() |>
      addPolygons(
        group = "decision",
        layerId = ~as.character(ug_id),
        fillColor = ~fill_color,
        fillOpacity = 1,
        color = ~border_color,
        weight = ~border_weight,
        opacity = 1,
        smoothFactor = 0.4,
        label = ~lapply(hover_text, htmltools::HTML),
        popup = ~lapply(popup_text, htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#222222",
          fillOpacity = 1,
          bringToFront = TRUE
        )
      ) |>
      addLabelOnlyMarkers(
        data = label_data |> filter(map_class == "other"),
        group = "decision",
        lng = ~label_lng,
        lat = ~label_lat,
        label = ~sprintf("%02d", ug_id),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          className = "ug-number-label"
        ),
        options = markerOptions(interactive = FALSE)
      ) |>
      addLabelOnlyMarkers(
        data = label_data |> filter(map_class != "other"),
        group = "decision",
        lng = ~label_lng,
        lat = ~label_lat,
        label = ~sprintf("%02d", ug_id),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          className = "ug-number-label ug-number-label-white"
        ),
        options = markerOptions(interactive = FALSE)
      ) |>
      addLegend(
        position = "bottomright",
        colors = unname(map_colors[c(
          "neighbor", "allowed", "not_allowed",
          "allowed_small", "not_allowed_small", "target"
        )]),
        labels = c(
          "Benachbart, nicht bewertet",
          "Zulässig",
          "Nicht zulässig",
          "Vorläufig zulässig (N < 5)",
          "Vorläufig nicht zulässig (N < 5)",
          "Zielgebiet"
        ),
        opacity = 0.9,
        title = "Bewertung"
      ) |>
      addControl(
        html = paste0(
          "<div class='map-instructions'>",
          "Rechtsklick: Ziel-UG wählen",
          "</div>"
        ),
        position = "bottomleft",
        className = "map-instructions-wrapper"
      ) |>
      addControl(
        html = paste0(
          "<div class='map-title-box'>",
          "Ursprungsgebiete für regionales gebietseigenes Saat- und ",
          "Pflanzgut krautiger Arten nach ErMiV ",
          "(Anlage zu § 2 Nummer 6 und 7)",
          "</div>"
        ),
        position = "topleft",
        className = "map-title-wrapper"
      )

  })

  output$results <- renderDT({
    result <- neighboring_results() |>
      arrange(
        factor(
          display_decision,
          levels = c(
            "Zulässig",
            "Vorläufig zulässig (N < 5)",
            "Nicht zulässig",
            "Vorläufig nicht zulässig (N < 5)",
            "Nicht bewertet"
          )
        ),
        donor_id
      ) |>
      transmute(
        Herkunftsgebiet = sprintf("UG %02d", donor_id),
        Bewertung = display_decision,
        N_donor = n_donor,
        N_target = n_target,
        Datenstand = data_version
      )

    names(result) <- c(
      "Herkunftsgebiet",
      "Bewertung",
      "N donor",
      "N target",
      "Datenstand"
    )

    export_filename <- paste0(
      "Austauschregeln_",
      input$taxon,
      "_UG",
      sprintf("%02d", as.integer(input$target)),
      "_",
      data_version_file
    )

    validate(
      need(
        nrow(result) > 0,
        "Für diese Auswahl wurden keine benachbarten Herkunftsgebiete gefunden."
      )
    )

    datatable(
      result,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bt",
        buttons = list(
          list(
            extend = "copy",
            text = "Kopieren",
            exportOptions = list(columns = 0:4)
          ),
          list(
            extend = "csv",
            text = "CSV",
            filename = export_filename,
            exportOptions = list(columns = 0:4)
          ),
          list(
            extend = "excel",
            text = "Excel",
            filename = export_filename,
            exportOptions = list(columns = 0:4)
          )
        ),
        columnDefs = list(
          list(targets = 0, width = "105px"),
          list(targets = 1, width = "235px"),
          list(targets = c(2, 3), width = "75px"),
          list(targets = 4, visible = FALSE)
        ),
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE
      ),
      class = "compact stripe hover",
      width = "100%"
    )
  })

}

shinyApp(ui, server)
