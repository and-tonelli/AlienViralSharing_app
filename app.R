library(shiny)
library(leaflet)
library(tidyverse)
if (FALSE) {
  library(munsell)
}
library(DT)
library(stringr)
library(bslib)
library(shiny)
library(ggplot2)
if (FALSE) {
  library(munsell)
}


load("data/DataForShiny2026.RData")


newui <- bslib::page_sidebar(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2a3a82",
    success = "#7B8A8B",
    base_font = "Verdana"
  ),
  
  title = "Global Alien-driven Viral Sharing",
  
  tags$head(
    tags$style(HTML("
      /* navy */
      table.dataTable tbody tr.selected td,
      table.dataTable tbody tr.selected {
        background-color: #2a3a82 !important;
        color: white !important; /* white */
        box-shadow: inset 0 0 0 9999px #2a3a82  !important;
      }

      /* Fix the Hover Color*/
      table.dataTable.hover tbody tr:hover td,
      table.dataTable.display tbody tr:hover td {
        background-color: #596aa8 !important;
        cursor: pointer !important;
      }

      /* Native Root Override for DT */
      :root {
        --dt-row-selected: 42, 61, 130 !important; /* RGB for #2a3a82 */
        --dt-row-selected-text: 0, 0, 0 !important;
      }

      /* Card Headers (Slate Green) */
      .card-header {
        background-color: #7B8A8B !important;
        color: white !important;
      }
      
      /* Sidebar Selection Color (Blue Navy) */
      .selectize-dropdown .active {
        background-color: #2a3a82 !important;
        color: white !important;
      }
      
      .bslib-sidebar-layout {
        --_sidebar-width: 500px; /* Change this to your preferred size */
      }
    "))
  ),
  
  sidebar = bslib::sidebar(
    collapsible = FALSE,
    width = "40%",
    open = "always",
    selectInput("reg_select", "Region (Landmass)", choices = choices_regions),
    selectInput("sp_select", "Species", choices = choices_species),
    hr(),
    uiOutput("species_details_ui"),
    hr(),
    helpText("Click the centroid of an established alien range (red marker) to see sharing statistics.")
  ),
  layout_columns(
    row_heights = c(3, 2)), 
  bslib::card(
    bslib::card_header("Established alien ranges"),
    leafletOutput("map")
  ),
  
  bslib::card(
    bslib::card_header("Drivers of viral sharing"),
    plotOutput("shap_plot")
  )
)


server <- function(input, output, session) {
  
  # Reactive values to track state
  selected_species <- reactiveVal(NULL)
  selected_point_id <- reactiveVal(NULL)
  
  # Filter data based on sidebar selection
  filtered_data <- reactive({
    df <- link_data
    if (input$reg_select != "All") {
      df <- df %>% filter(Landmass == input$reg_select)
    }
    if (input$sp_select != "All") {
      df <- df %>% filter(Alien == input$sp_select)
    }
    df
  })
  
  # Render base map
  output$map <- renderLeaflet({
    data_to_show <- filtered_data()
    
    leaflet(data_to_show) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~LonA, lat = ~LatA,
        color = "#ab3232",
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.7,
        group = "Alien Range",
        layerId = ~row_id, # Ensure link_data has a row_id column
        popup = ~paste("<b>Species:</b> <i>", Alien, "</i><br>",
                       "<b>Pathway:</b>", Pathway, "<br>",
                       "<b>Number of new sharing events:</b>", n, "<br>")
      ) %>%
      addCircleMarkers(
        lng = ~LonN, lat = ~LatN,
        color = "#389667",
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.6,
        group = "Native Range",
        popup = ~paste(
          "<b>Centroid of native range</b><br>",
          "<b>Species:</b> <i>", Alien, "</i><br>"
          # ,
          # "<b>Status:</b> Native"
        )
      )
  })
  
  
  # Handle Marker Clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    
    # 1.
    current_df <- filtered_data()
    species_name <- current_df %>%
      filter(row_id == click$id) %>%
      pull(Alien)
    
    # 2
    selected_species(species_name)
    selected_point_id(click$id)
    
    # 3. 
    # updateSelectInput(session, "sp_select", selected = species_name)
  })
  
  
  # UI sharing tab 
  output$species_details_ui <- renderUI({
    
    req(selected_species(), 
        selected_point_id()
    )
    
    tagList(
      h4("Predicted viral sharing for: ", em(selected_species()), style = "font-size: 16px; margin-bottom: 5px;"),
      p(strong("New viral sharing events in bold"), style = "font-size: 14px; margin-bottom: 2px;"),
      p("Select native species:", style = "font-size: 14px;"),
      DTOutput("sharing_table")
    )
  })
  
  observeEvent(input$sp_select, {
    if(input$sp_select != "All") {
      selected_species(input$sp_select)
      selected_point_id(NULL) # Reset ID
    } else {
      selected_species(NULL)
      selected_point_id(NULL)
      leafletProxy("map") %>% clearGroup("Native Raster")
    }
  })
  
  # Sharing encounters to display
  output$sharing_table <- renderDT({
    req(selected_species(), selected_point_id())
    
    # Get coordinates
    target_info <- link_data %>% filter(row_id == selected_point_id())
    
    df <- link_explo %>%
      filter(
        Alien == selected_species(),
        abs(LonA - target_info$LonA) < 0.0001,
        abs(LatA - target_info$LatA) < 0.0001
      ) %>%
      select(Species1, mean_prob, vir_bin) %>%   # keep vir_bin
      arrange(desc(mean_prob)) %>%
      mutate(mean_prob = round(mean_prob, 2),
             showing = ifelse(vir_bin == 0 & mean_prob >= 0.5, 1, 0)) %>% 
      dplyr::select(-vir_bin)
    
    datatable(
      df,
      selection = 'single',
      options = list(
        pageLength = 10,
        dom = 'ftp',
        columnDefs = list(
          list(visible = FALSE, targets = 3)  # hide 'showing' (3rd column)
        )
      ),
      colnames = c("Native Species", "Sharing Prob", "showing")
    ) %>%
      formatStyle(
        "showing",
        target = "row",
        fontWeight = styleEqual(1, "bold"),
        fontSize = '13px'
      )
  })
  
  # SHAP plots
  output$shap_plot <- renderPlot({
    req(selected_species(), selected_point_id())
    s <- input$sharing_table_rows_selected
    req(s)
    
    target_info <- link_data %>% filter(row_id == selected_point_id())
    
    # filter tab
    data_table_filtered <- link_explo %>%
      filter(
        Alien == selected_species(),
        abs(LonA - target_info$LonA) < 0.0001,
        abs(LatA - target_info$LatA) < 0.0001
      ) %>%
      arrange(desc(mean_prob))
    
    partner_name <- data_table_filtered$Species1[s]
    
    plot_data <- data_and_S %>%
      filter(Species2 == selected_species(), Species1 == partner_name)
    
    req(nrow(plot_data) > 0)
    
    var_labels <- c(
      "trait_sim_gow" = "Trait similarity",
      "foraging_sim" = "Foraging similarity",
      "phylo_sim" = "Phylogenetic similarity",
      "Overlapping_Cells_log" = "Geographical overlap",
      "log_sum_cit" = "Log(sum of citations)"
    )
    # Filtered tab
    plot_data <- plot_data %>%
      mutate(Var_Pretty = ifelse(Var %in% names(var_labels), var_labels[Var], Var))
    
    # Normal ggpot
    ggplot(plot_data, aes(x = reorder(Var_Pretty, Shap), y = Shap, fill = Shap > 0)) +
      geom_bar(stat = "identity", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#ab3232", "FALSE" = "#3632ab"), guide = "none") +
      theme_minimal(base_size = 14) +
      labs(
        title = "Drivers of sharing:",
        subtitle = paste0("<i>", selected_species(), "</i> and <i>", partner_name, "</i>"),
        x = NULL,
        y = "SHAP Value contribution"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = ggtext::element_markdown(size = 14, color = "grey30"), #html render
        
        axis.text.y = element_text(size = 13, face = "italic"),
        axis.text.x = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 20, 10, 5)
      )
  })
}


shinyApp(newui, server)
