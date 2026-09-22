library(shiny)
library(dplyr)

# -------------------------------------------------------------------
# 1. Read and prepare data
# -------------------------------------------------------------------

rules <- read.csv(
  file = file.path("data", "exchange_rules_app.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_columns <- c(
  "taxon",
  "donor_id",
  "target_id",
  "status",
  "status_label",
  "is_neighbor",
  "n_donor",
  "n_target"
)

missing_columns <- setdiff(required_columns, names(rules))

if (length(missing_columns) > 0L) {
  stop(
    "The app data are missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Use numeric identifiers internally.
# Leading zeros are added only when labels are displayed.
rules <- rules |>
  mutate(
    taxon = trimws(as.character(taxon)),
    
    donor_id = as.integer(
      trimws(as.character(donor_id))
    ),
    
    target_id = as.integer(
      trimws(as.character(target_id))
    ),
    
    status = trimws(as.character(status)),
    status_label = trimws(as.character(status_label)),
    
    # Ensures correct interpretation even if imported as text
    is_neighbor = case_when(
      is_neighbor %in% TRUE ~ TRUE,
      toupper(as.character(is_neighbor)) == "TRUE" ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Stop with a useful error if UG identifiers could not be read
if (anyNA(rules$donor_id)) {
  stop(
    "Some donor UG identifiers could not be converted to integers."
  )
}

if (anyNA(rules$target_id)) {
  stop(
    "Some target UG identifiers could not be converted to integers."
  )
}

taxa <- sort(unique(rules$taxon))

# -------------------------------------------------------------------
# 2. User interface
# -------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Ersatzherkünfte für Regiosaatgut"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "taxon",
        label = "Art oder Taxon",
        choices = taxa,
        selected = taxa[1]
      ),
      
      selectInput(
        inputId = "target",
        label = "Ziel-Ursprungsgebiet",
        choices = NULL
      )
    ),
    
    mainPanel(
      
      h3("Mögliche Ersatzherkünfte"),
      
      textOutput("selection_text"),
      
      br(),
      
      tableOutput("results")
    )
  )
)

# -------------------------------------------------------------------
# 3. Server logic
# -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Update available target UGs when the taxon changes
  observeEvent(
    input$taxon,
    {
      available_targets <- rules |>
        filter(taxon == input$taxon) |>
        distinct(target_id) |>
        filter(!is.na(target_id)) |>
        arrange(target_id) |>
        pull(target_id)
      
      updateSelectInput(
        session = session,
        inputId = "target",
        
        # Names are displayed; values are used internally
        choices = setNames(
          available_targets,
          sprintf("UG %02d", available_targets)
        ),
        
        selected = available_targets[1]
      )
    },
    ignoreInit = FALSE
  )
  
  # Filter the data for the selected taxon and target UG
  selected_results <- reactive({
    
    req(input$taxon)
    req(input$target)
    
    selected_target <- as.integer(input$target)
    
    rules |>
      filter(
        taxon == input$taxon,
        target_id == selected_target,
        is_neighbor
      ) |>
      arrange(
        factor(
          status,
          levels = c(
            "allowed",
            "not_allowed",
            "insufficient_sample",
            "not_evaluated"
          )
        ),
        donor_id
      ) |>
      transmute(
        `Herkunftsgebiet` = sprintf(
          "UG %02d",
          donor_id
        ),
        
        Bewertung = status_label,
        
        `N Herkunft` = n_donor,
        
        `N Zielgebiet` = n_target
      )
  })
  
  # Display the current selection
  output$selection_text <- renderText({
    
    req(input$taxon)
    req(input$target)
    
    paste0(
      "Taxon: ",
      input$taxon,
      " | Zielgebiet: UG ",
      sprintf("%02d", as.integer(input$target))
    )
  })
  
  # Display the results
  output$results <- renderTable({
    
    result <- selected_results()
    
    validate(
      need(
        nrow(result) > 0,
        paste0(
          "Für ",
          input$taxon,
          " und UG ",
          sprintf("%02d", as.integer(input$target)),
          " wurden keine benachbarten Herkunftsgebiete gefunden."
        )
      )
    )
    
    result
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "m"
  )
}

# -------------------------------------------------------------------
# 4. Start app
# -------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)