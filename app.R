library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(DT)

# Data -----------------------------------------------------------------------

rules <- read.csv(
  file.path("data", "exchange_rules_app.csv"),
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
      body { overflow-y: auto; }
      .container-fluid { padding: 7px 12px; }
      h2 { margin: 3px 0 1px 0; font-size: 23px; }
      h4 { margin: 4px 0 6px 0; font-size: 15px; }
      .app-subtitle { color: #5f6368; margin-bottom: 6px; }

      .control-row {
        background: #f5f6f7; border-radius: 7px;
        padding: 5px 9px 0 9px; margin-bottom: 6px;
      }
      .control-row .form-group { margin-bottom: 5px; }
      .control-row label { font-size: 12px; margin-bottom: 2px; }

      .taxon-with-thumb {
        display: flex; align-items: flex-end; gap: 8px;
      }
      .taxon-select { flex: 1 1 auto; min-width: 0; }
      .species-thumbnail {
        flex: 0 0 62px; width: 62px; height: 62px;
        border: 1px dashed #aeb4b9; border-radius: 6px;
        background: #fafafa;
        display: flex; align-items: center; justify-content: center;
        color: #9aa0a6; font-size: 9px; margin-bottom: 5px;
      }

      .map-panel {
        width: 100%; max-width: 555px; margin: 0 auto;
        border: 1px solid #d9dde1; border-radius: 7px;
        overflow: hidden; background: #eef2f3;
      }
      .leaflet-container { background: #eef2f3 !important; }

      .table-panel {
        border: 1px solid #d9dde1; border-radius: 7px;
        padding: 7px 9px; background: white;
      }
      .table-panel table { font-size: 12px; margin-bottom: 0; }
      .table-panel .table > thead > tr > th,
      .table-panel .table > tbody > tr > td {
        padding: 4px 5px;
      }

      .dt-buttons .dt-button,
      .dt-buttons .btn {
        font-size: 10px !important;
        line-height: 1.2 !important;
        padding: 2px 6px !important;
        margin: 0 3px 5px 0 !important;
      }

      .info.legend {
        font-size: 9px; line-height: 12px;
        padding: 4px 6px; max-width: 180px;
      }
      .info.legend i {
        width: 11px; height: 11px; margin-right: 4px;
      }

      .map-instructions {
        max-width: 180px;
        background: rgba(255,255,255,0.92);
        border: 1px solid #c8cdd1;
        border-radius: 5px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.18);
        color: #3c4043;
        font-size: 10px;
        line-height: 1.3;
        padding: 5px 7px;
      }

      .ug-number-label {
        background: rgba(255,255,255,0.70);
        border: 0; box-shadow: none;
        color: #303030; font-weight: 700; font-size: 10px;
        padding: 0 2px;
      }
      "
    ))
  ),

  h2("Ersatzherkünfte für Regiosaatgut"),
  div(
    class = "app-subtitle",
    "Bewertung benachbarter Ursprungsgebiete nach Art oder Taxon"
  ),

  fluidRow(
    class = "control-row",
    column(
      5,
      div(
        class = "taxon-with-thumb",
        div(
          class = "taxon-select",
          selectInput(
            "taxon", "Art oder Taxon",
            choices = taxon_choices,
            selected = taxa[1],
            width = "100%"
          )
        ),
        div(
          class = "species-thumbnail",
          "Artenbild"
        )
      )
    ),
    column(
      2,
      selectInput(
        "target", "Ziel-UG",
        choices = NULL, width = "100%"
      )
    )
  ),

  fluidRow(
    column(
      4,
      h4(textOutput("selection_text")),
      div(
        class = "map-panel",
        leafletOutput("ug_map", height = "620px")
      )
    ),
    column(
      8,
      div(
        class = "table-panel",
        h4("Benachbarte Herkunftsgebiete"),
        DTOutput("results")
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

    updateSelectInput(
      session,
      "target",
      choices = setNames(targets, sprintf("UG %02d", targets)),
      selected = targets[1]
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

  observeEvent(input$ug_map_shape_click, {
    clicked_ug <- suppressWarnings(
      as.integer(input$ug_map_shape_click$id)
    )

    if (!is.na(clicked_ug)) {
      updateSelectInput(session, "target", selected = clicked_ug)
    }
  })

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

  output$selection_text <- renderText({
    req(input$taxon, input$target)
    paste0(
      unname(taxon_labels[[input$taxon]]),
      " – Zielgebiet UG ",
      sprintf("%02d", as.integer(input$target))
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
        attributionControl = TRUE
      )
    ) |>
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        options = tileOptions(noWrap = TRUE)
      ) |>
      setView(
        lng = 10.5,
        lat = 51.1,
        zoom = 6
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
        }
        "
      )
  })

  observe({
    polygons <- map_data()

    leafletProxy(
      "ug_map",
      data = polygons,
      session = session
    ) |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addPolygons(
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
        data = ug_label_points,
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
          "<strong>Kartenbedienung</strong><br>",
          "Mit der Maus: Details anzeigen<br>",
          "Rechtsklick: UG als Ziel wählen",
          "</div>"
        ),
        position = "topleft",
        className = "map-instructions-wrapper"
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
        N_target = n_target
      )

    names(result) <- c(
      "Herkunftsgebiet",
      "Bewertung",
      "N donor",
      "N target"
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
        buttons = c("copy", "csv", "excel"),
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE
      ),
      class = "compact stripe hover"
    )
  })

}

shinyApp(ui, server)
