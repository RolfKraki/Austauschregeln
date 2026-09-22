library(shiny)
library(dplyr)
library(sf)
library(leaflet)

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

taxa <- sort(unique(rules$taxon))

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
  st_transform(4326)

if (anyNA(ug_shape$ug_id) || !all(1:22 %in% ug_shape$ug_id)) {
  stop("The shapefile does not contain valid identifiers for UGs 01-22.")
}

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
    tags$style(HTML(
      "
      .app-subtitle { color: #5f6368; margin-bottom: 18px; }
      .control-panel {
        background: #f5f6f7; border-radius: 8px;
        padding: 16px; margin-bottom: 16px;
      }
      .map-panel {
        border: 1px solid #d9dde1; border-radius: 8px;
        overflow: hidden; background: white;
      }
      .results-table { margin-top: 14px; max-width: 760px; }
      .results-table table { font-size: 13px; }
      "
    ))
  ),

  h2("Ersatzherkünfte für Regiosaatgut"),
  div(
    class = "app-subtitle",
    "Bewertung benachbarter Ursprungsgebiete nach Art oder Taxon"
  ),

  fluidRow(
    column(
      3,
      div(
        class = "control-panel",
        selectInput(
          "taxon", "Art oder Taxon",
          choices = taxa, selected = taxa[1], width = "100%"
        ),
        selectInput(
          "target", "Ziel-Ursprungsgebiet",
          choices = NULL, width = "100%"
        ),
        tags$hr(),
        tags$p(
          tags$strong("Hinweis:"),
          " Blasse Farben kennzeichnen vorläufige Bewertungen bei N < 5."
        )
      )
    ),

    column(
      9,
      h4(textOutput("selection_text")),
      div(
        class = "map-panel",
        leafletOutput("ug_map", height = "610px")
      ),
      div(
        class = "results-table",
        h4("Benachbarte Herkunftsgebiete"),
        tableOutput("results")
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
      input$taxon,
      " – Zielgebiet UG ",
      sprintf("%02d", as.integer(input$target))
    )
  })

  output$ug_map <- renderLeaflet({
    polygons <- map_data()

    leaflet(polygons) |>
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) |>
      fitBounds(
        unname(map_bbox["xmin"]), unname(map_bbox["ymin"]),
        unname(map_bbox["xmax"]), unname(map_bbox["ymax"])
      ) |>
      addPolygons(
        fillColor = ~fill_color,
        fillOpacity = 0.82,
        color = ~border_color,
        weight = ~border_weight,
        opacity = 1,
        smoothFactor = 0.4,
        label = ~lapply(hover_text, htmltools::HTML),
        popup = ~lapply(popup_text, htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#222222",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
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
      )
  })

  output$results <- renderTable({
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
        N_Herkunft = n_donor
      )

    names(result) <- c(
      "Herkunftsgebiet",
      "Bewertung",
      "N Herkunft"
    )

    validate(
      need(
        nrow(result) > 0,
        "Für diese Auswahl wurden keine benachbarten Herkunftsgebiete gefunden."
      )
    )

    result
  },
  striped = TRUE,
  bordered = FALSE,
  hover = TRUE,
  spacing = "xs",
  width = "auto"
  )
}

shinyApp(ui, server)
