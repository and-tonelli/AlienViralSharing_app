library(shiny)
library(leaflet)
library(tidyverse)
if (FALSE) {
  library(munsell)
}
library(DT)
library(stringr)
library(shiny)
library(ggplot2)
if (FALSE) {
  library(munsell)
}
library(shinyWidgets)

load("data/DataForShinyVirAliNet.RData")

newui <- bslib::page_sidebar(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2a3a82",
    success = "#7B8A8B",
    base_font = "Verdana"
  ),
  
  title = "VirAliNet: Alien-driven Viral Sharing",
  
  tags$head(
    tags$style(HTML("
      /* navy */
      table.dataTable tbody tr.selected td,
      table.dataTable tbody tr.selected {
        background-color: #2a3a82 !important;
        color: white !important;
        box-shadow: inset 0 0 0 9999px #2a3a82  !important;
      }

      /* Hover Color */
      table.dataTable.hover tbody tr:hover td,
      table.dataTable.display tbody tr:hover td {
        background-color: #596aa8 !important;
        cursor: pointer !important;
      }

      /* Native Root Override for DT */
      :root {
        --dt-row-selected: 42, 61, 130 !important;
        --dt-row-selected-text: 255, 255, 255 !important;
      }

      /* Card Headers */
      .card-header {
        background-color: #7B8A8B !important;
        color: white !important;
      }
      
      /* Sidebar Selection Color */
      .selectize-dropdown .active {
        background-color: #2a3a82 !important;
        color: white !important;
      }
      
      .bslib-sidebar-layout {
        --_sidebar-width: 500px;
      }
    "))
  ),
  
  sidebar = bslib::sidebar(
    collapsible = FALSE,
    width = "40%",
    open = "always",
    
    selectInput("continent_select", "Continent (Macroregion)", choices = c("All")),
    selectInput("reg_select", "Region (Landmass)", choices = c("All")),
    selectInput("sp_select", "Species", choices = c("All")),
    
    prettySwitch(
      inputId = "use_alt",
      label = "Use even research effort",
      value = FALSE,
      status = "success",
      fill = TRUE
    ),
    
    tags$small(
      "Assumes even research effort (median) across species",
      style = "color: #6c757d;"
    ),
    
    hr(),
    uiOutput("species_details_ui")
  ),
  
  bslib::card(
    bslib::card_header("Established alien ranges"),
    leafletOutput("map", height = "600px")
  ),
  
  bslib::card(
    bslib::card_header("Drivers of viral sharing"),
    plotOutput("shap_plot")
  )
)

server <- function(input, output, session) {
  
  # Reactive Data Accessors
  current_link_data <- reactive({
    if (isTRUE(input$use_alt)) link_data2 else link_data
  })
  
  current_link_explo <- reactive({
    if (isTRUE(input$use_alt)) link_explo2 else link_explo
  })
  
  current_data_and_S <- reactive({
    if (isTRUE(input$use_alt)) data_and_S2 else data_and_S
  })
  
  # Reactive values to track state
  selected_species <- reactiveVal(NULL)
  selected_point_id <- reactiveVal(NULL)
  
  # --- DYNAMIC CASCADING MENUS ---
  
  # 1. Initialize Continent choices
  observe({
    req(exists("nested_tab"))
    conts <- sort(unique(nested_tab$Continent))
    updateSelectInput(session, "continent_select", choices = c("All", conts))
  })
  
  # 2. Update Region based on Continent
  observeEvent(input$continent_select, {
    req(exists("nested_tab"))
    df <- nested_tab
    if (input$continent_select != "All") {
      df <- df %>% filter(Continent == input$continent_select)
    }
    regs <- sort(unique(df$Landmass))
    updateSelectInput(session, "reg_select", choices = c("All", regs), selected = "All")
  }, ignoreInit = TRUE)
  
  # 3. Update Species based on filters (removed use_alt to avoid resetting selection)
  observeEvent(c(input$continent_select, input$reg_select), {
    df <- current_link_data()
    
    if (input$continent_select != "All") {
      df <- df %>% filter(Continent == input$continent_select)
    }
    
    if (input$reg_select != "All") {
      df <- df %>% filter(Landmass == input$reg_select)
    }
    
    sps <- sort(unique(df$Alien))
    curr_sp <- isolate(input$sp_select)
    sel <- if (!is.null(curr_sp) && curr_sp %in% sps) curr_sp else "All"
    
    updateSelectInput(session, "sp_select",
                      choices = c("All", sps),
                      selected = sel)
  }, ignoreInit = TRUE)
  
  # --- FILTER DATA FOR MAP ---
  filtered_data <- reactive({
    df <- current_link_data()
    if (input$continent_select != "All") {
      df <- df %>% filter(Continent == input$continent_select)
    }
    if (input$reg_select != "All") {
      df <- df %>% filter(Landmass == input$reg_select)
    }
    if (input$sp_select != "All") {
      df <- df %>% filter(Alien == input$sp_select)
    }
    df
  })
  
  # --- MAP RENDERING ---
  
  # Initial Map Load (One-time)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addControl(
        html = "<div id='map-help-container'>
                  <div id='help-collapsed' style='display: none; cursor: pointer; background: white; padding: 6px 10px; border-radius: 4px; box-shadow: 0 1px 4px rgba(0,0,0,0.2); font-weight: bold; font-size: 13px; color: #2a3a82;' onclick=\"document.getElementById('help-expanded').style.display='block'; this.style.display='none';\" title='Show help'>
                    &#8505;
                  </div>
                  <div id='help-expanded' style='position: relative; font-size: 12px; color: #333; background: white; padding: 6px 20px 6px 10px; border-radius: 4px; box-shadow: 0 1px 4px rgba(0,0,0,0.2); line-height: 1.4;'>
                    <span style='position: absolute; top: 2px; right: 5px; cursor: pointer; font-size: 16px; font-weight: bold; color: #888; line-height: 1;' onclick=\"document.getElementById('help-collapsed').style.display='block'; this.parentElement.style.display='none';\" title='Close'>&times;</span>
                    Click the centroid of an established alien range (<strong style='color:#ab3232;'>red</strong> marker) to see sharing statistics.<br>
                    Click the centroid of a native range (<strong style='color:#389667;'>green</strong> marker) to view details.
                  </div>
                </div>",
        position = "bottomleft")
  })
  
  # Incremental Update (Using LeafletProxy to avoid reset on switch/filter)
  observe({
    data_to_show <- filtered_data()
    
    leafletProxy("map", data = data_to_show) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~LonA, lat = ~LatA,
        color = "#ab3232",
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.7,
        group = "Alien Range",
        layerId = ~row_id,
        label = ~lapply(paste("<b>Species:</b> <i>", Alien, "</i><br>",
                              "<b>Pathway:</b>", Pathway, "<br>",
                              "<b>Number of new sharing events:</b>", n, "<br>"), htmltools::HTML),
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
        label = ~lapply(paste("<b>Centroid of native range</b><br>",
                              "<b>Species:</b> <i>", Alien, "</i><br>"), htmltools::HTML),
        popup = ~paste("<b>Centroid of native range</b><br>",
                       "<b>Species:</b> <i>", Alien, "</i><br>")
      )
  })
  
  # --- INTERACTION LOGIC ---
  
  # Handle Marker Clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    
    current_df <- current_link_data()
    species_name <- current_df %>%
      filter(row_id == click$id) %>%
      pull(Alien)
    
    # If the user clicks a point, explicitly store it
    selected_point_id(click$id)
    
    if(length(species_name) > 0) {
      selected_species(species_name[1])
      # Update sidebar but prevent the observer from clearing point ID immediately
      updateSelectInput(session, "sp_select", selected = species_name[1])
    }
  })
  
  # Handle Species Dropdown Change
  observeEvent(input$sp_select, {
    req(input$sp_select)
    if(input$sp_select != "All") {
      # If current point ID does not belong to the newly selected species, 
      # we set it to NULL so the table shows unique potential partners for that species.
      df_for_species <- current_link_data() %>% filter(Alien == input$sp_select)
      curr_pt <- isolate(selected_point_id())
      
      if (!is.null(curr_pt)) {
        if (!(curr_pt %in% df_for_species$row_id)) {
          selected_point_id(NULL)
        }
      }
      
      selected_species(input$sp_select)
    } else {
      selected_species(NULL)
      selected_point_id(NULL)
    }
  })
  
  # --- UI COMPONENTS ---
  output$species_details_ui <- renderUI({
    req(selected_species())
    
    tagList(
      h4("Predicted viral sharing for: ", em(selected_species()), style = "font-size: 16px; margin-bottom: 5px;"),
      p(strong("New viral sharing events in bold"), style = "font-size: 14px; margin-bottom: 2px;"),
      p("Select native species:", style = "font-size: 14px;"),
      DTOutput("sharing_table")
    )
  })
  
  # Aggregated Sharing Table logic
  output$sharing_table <- renderDT({
    req(selected_species())
    
    pt_id <- selected_point_id()
    
    # Base filter for species using current data source (effort switch dependent)
    df_base <- current_link_explo() %>%
      filter(Alien == selected_species())
    
    # Conditional logic for showing results
    if (!is.null(pt_id)) {
      # 1. SPECIFIC POINT: filter by coordinates
      target_info <- current_link_data() %>% filter(row_id == pt_id)
      if (nrow(target_info) > 0) {
        df_display_raw <- df_base %>%
          filter(
            abs(LonA - target_info$LonA) < 0.0001,
            abs(LatA - target_info$LatA) < 0.0001
          )
      } else {
        df_display_raw <- df_base
      }
    } else {
      # 2. NO POINT CLICKED: Show unique partners for the species
      # Group by native species and take the maximum probability/status
      df_display_raw <- df_base %>%
        group_by(Species1) %>%
        summarise(
          mean_prob = max(mean_prob, na.rm = TRUE),
          vir_bin = max(vir_bin, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    # Prepare display data
    df_display <- df_display_raw %>%
      select(Species1, mean_prob, vir_bin) %>%
      arrange(desc(mean_prob)) %>%
      mutate(mean_prob = round(mean_prob, 2),
             showing = ifelse(vir_bin == 0 & mean_prob >= 0.5, 1, 0)) %>%
      dplyr::select(-vir_bin)
    
    datatable(
      df_display,
      selection = 'single',
      options = list(
        pageLength = 10,
        dom = 'ftp',
        columnDefs = list(list(visible = FALSE, targets = 3))
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
    req(selected_species())
    s <- input$sharing_table_rows_selected
    req(s)
    
    # Get the display dataframe exactly as it is in the table to find the partner name
    pt_id <- selected_point_id()
    df_base <- current_link_explo() %>%
      filter(Alien == selected_species())
    
    if (!is.null(pt_id)) {
      target_info <- current_link_data() %>% filter(row_id == pt_id)
      if (nrow(target_info) > 0) {
        df_current_view <- df_base %>%
          filter(
            abs(LonA - target_info$LonA) < 0.0001,
            abs(LatA - target_info$LatA) < 0.0001
          )
      } else {
        df_current_view <- df_base
      }
    } else {
      df_current_view <- df_base %>%
        group_by(Species1) %>%
        summarise(
          mean_prob = max(mean_prob, na.rm = TRUE),
          vir_bin = max(vir_bin, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    data_table_sorted <- df_current_view %>% arrange(desc(mean_prob))
    partner_name <- data_table_sorted$Species1[s]
    
    plot_data <- current_data_and_S() %>%
      filter(Species2 == selected_species(), Species1 == partner_name)
    
    req(nrow(plot_data) > 0)
    
    var_labels <- c(
      "trait_sim_gow" = "Trait similarity",
      "foraging_sim" = "Foraging similarity",
      "phylo_sim" = "Phylogenetic similarity",
      "Overlapping_Cells_log" = "Geographical overlap",
      "log_sum_cit" = "Log(sum of citations)"
    )
    
    plot_data <- plot_data %>%
      mutate(Var_Pretty = ifelse(Var %in% names(var_labels), var_labels[Var], Var))
    
    ggplot(plot_data, aes(x = reorder(Var_Pretty, Shap), y = Shap, fill = Shap > 0)) +
      geom_bar(stat = "identity", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#ab3232", "FALSE" = "#3632ab"), guide = "none") +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("<b>Alien: <b><i>", selected_species(), "</i> &nbsp;&nbsp; <b>Native:<b> <i>", partner_name, "</i>"),
        x = NULL,
        y = "Contribution to viral sharing"
      ) +
      theme(
        plot.title = ggtext::element_markdown(size = 16),
        axis.text.y = element_text(size = 12, face = "italic"),
        panel.grid.minor = element_blank()
      )
  })
}

shinyApp(newui, server)
