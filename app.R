library(shiny)
library(leaflet)
library(tidyverse)
if (FALSE) {
  library(munsell)
}
library(DT)
library(stringr)
library(bslib)
library(ggplot2)
library(ggtext)  # Required for rendering markdown in ggplot subtitle
library(shinyWidgets)

# Load data
load("data/DataForShiny2026v2.1.RData")

# CompletePoints %>% 
#   merge.data.frame(., SharingEncounters[c(1, 3:7)], by.x = c(1, 2), by.y = c(1, 2)) %>% 
#   filter(pred_bin == 1, vir_bin == 0) %>% 
#   group_by(Alien, LonA, LatA, LonN, LatN, Landmass) %>% 
#   summarise(n = n()) -> link_data
# 
# nested_tab <- tibble(Landmass = c("All", sort(choices_regions[-1])),
#                      Continent = c("All", "Africa",
#                                    "Asia", "Europe",
#                                    "Europe", "Asia",
#                                    "Oceania", "Asia",
#                                    "Europe", "Asia",
#                                    "Europe", "Asia",
#                                    "Europe", "Europe",
#                                    "Europe", "North America",
#                                    "Europe", "Europe",
#                                    "Europe", "Asia",
#                                    "Europe", "Europe",
#                                    "Europe", "Europe",
#                                    "Asia", "Asia",
#                                    "Europe", "North America",
#                                    "Asia", "Europe",
#                                    "Europe", "North America",
#                                    "Europe", "Europe",
#                                    "North America", "North America",
#                                    "Oceania", "Asia",
#                                    "North America", "Europe",
#                                    "Europe", "Europe",
#                                    "Asia", "Europe",
#                                    "Europe", "Europe",
#                                    "South America", "Oceania",
#                                    "Oceania", "Europe",
#                                    "Asia", "Europe",
#                                    "Asia", "Asia"))

# nested_tab$Landmass[nested_tab$Landmass == "Europe"] <- "Europe (mainland)"
# nested_tab$Landmass[nested_tab$Landmass == "Africa"] <- "Africa (mainland)"
# nested_tab$Landmass[nested_tab$Landmass == "Asia"] <- "Asia (mainland)"
# nested_tab$Landmass[nested_tab$Landmass == "South America"] <- "South America (mainland)"
# nested_tab$Landmass[nested_tab$Landmass == "North America"] <- "North America (mainland)"

# link_data$Landmass[link_data$Landmass == "Europe"] <- "Europe (mainland)"
# link_data$Landmass[link_data$Landmass == "Africa"] <- "Africa (mainland)"
# link_data$Landmass[link_data$Landmass == "Asia"] <- "Asia (mainland)"
# link_data$Landmass[link_data$Landmass == "South America"] <- "South America (mainland)"
# link_data$Landmass[link_data$Landmass == "North America"] <- "North America (mainland)"
#  
# link_data2$Landmass[link_data2$Landmass == "Europe"] <- "Europe (mainland)"
# link_data2$Landmass[link_data2$Landmass == "Africa"] <- "Africa (mainland)"
# link_data2$Landmass[link_data2$Landmass == "Asia"] <- "Asia (mainland)"
# link_data2$Landmass[link_data2$Landmass == "South America"] <- "South America (mainland)"
# link_data2$Landmass[link_data2$Landmass == "North America"] <- "North America (mainland)"


# link_data <- merge.data.frame(link_data, nested_tab, by = "Landmass")
# link_data2 <- merge.data.frame(link_data2, nested_tab, by = "Landmass")



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
      /* navy selection in DT */
      table.dataTable tbody tr.selected td,
      table.dataTable tbody tr.selected {
        background-color: #2a3a82 !important;
        color: white !important;
        box-shadow: inset 0 0 0 9999px #2a3a82 !important;
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

      /* Card Headers (Slate Green) */
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

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive data sources
  current_link_data <- reactive({
    if (isTRUE(input$use_alt)) link_data2 else link_data
  })
  
  current_link_explo <- reactive({
    if (isTRUE(input$use_alt)) link_explo2 else link_explo
  })
  
  current_data_and_S <- reactive({
    if (isTRUE(input$use_alt)) data_and_S2 else data_and_S
  })
  
  # Reactive values to track selected state
  selected_species <- reactiveVal(NULL)
  # This tracks the specific point clicked on the map. 
  # If NULL, we show results for the whole region.
  selected_point_id <- reactiveVal(NULL)
  
  # 1. Initialize Continent choices
  observe({
    req(exists("nested_tab"))
    conts <- sort(unique(nested_tab$Continent))
    updateSelectInput(session, "continent_select", choices = c("All", conts))
  })
  
  # 2. Update Region choices
  observeEvent(input$continent_select, {
    req(exists("nested_tab"))
    df_nested <- nested_tab
    if (input$continent_select != "All") {
      df_nested <- df_nested %>% filter(Continent == input$continent_select)
    }
    regs <- sort(unique(df_nested$Landmass))
    updateSelectInput(session, "reg_select", choices = c("All", regs), selected = "All")
  }, ignoreInit = TRUE)
  
  # 3. Update Species choices
  observeEvent(c(input$continent_select, input$reg_select, input$use_alt), {
    df_sp <- current_link_data()
    if (input$continent_select != "All") df_sp <- df_sp %>% filter(Continent == input$continent_select)
    if (input$reg_select != "All") df_sp <- df_sp %>% filter(Landmass == input$reg_select)
    
    sps <- sort(unique(df_sp$Alien))
    curr_sp <- isolate(input$sp_select)
    sel <- if (!is.null(curr_sp) && curr_sp %in% sps) curr_sp else "All"
    
    updateSelectInput(session, "sp_select", choices = c("All", sps), selected = sel)
  }, ignoreInit = TRUE)
  
  # 4. Filter data for the Map
  filtered_data <- reactive({
    df <- current_link_data()
    if (input$continent_select != "All") df <- df %>% filter(Continent == input$continent_select)
    if (input$reg_select != "All") df <- df %>% filter(Landmass == input$reg_select)
    if (input$sp_select != "All") df <- df %>% filter(Alien == input$sp_select)
    df
  })
  
  # 5. Map Rendering
  output$map <- renderLeaflet({
    data_to_show <- filtered_data()
    
    leaflet(data_to_show) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~LonA, lat = ~LatA,
        color = "#ab3232", radius = 8, stroke = FALSE, fillOpacity = 0.7,
        group = "Alien Range", layerId = ~row_id,
        popup = ~paste("<b>Species:</b> <i>", Alien, "</i><br>", "<b>Pathway:</b>", Pathway, "<br>")
      ) %>%
      addCircleMarkers(
        lng = ~LonN, lat = ~LatN,
        color = "#389667", radius = 8, stroke = FALSE, fillOpacity = 0.6,
        group = "Native Range"
      )
  })
  
  # 6. Interaction Logic
  # When a marker is clicked, set the specific point ID
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    
    current_df <- filtered_data()
    target <- current_df %>% filter(row_id == click$id)
    
    if(nrow(target) > 0) {
      selected_point_id(click$id)
      # Also update the sidebar to match the species clicked
      if(input$sp_select != target$Alien[1]) {
        updateSelectInput(session, "sp_select", selected = target$Alien[1])
      }
    }
  })
  
  # When species changes in sidebar, reset point_id so we show all results for that species
  observeEvent(input$sp_select, {
    if(input$sp_select != "All") {
      selected_species(input$sp_select)
      # Reset point ID to NULL so table shows the 'aggregated' view for the new species
      # unless it was triggered by a map click (handled by checking if the click matches the current selection)
      # To keep it simple: manual sidebar change = reset to full view.
      selected_point_id(NULL) 
    } else {
      selected_species(NULL)
      selected_point_id(NULL)
    }
  })
  
  # 7. UI Output
  output$species_details_ui <- renderUI({
    req(selected_species())
    tagList(
      h4("Predicted viral sharing for: ", em(selected_species())),
      p(if(is.null(selected_point_id())) "Showing results across selected region." else "Showing results for selected map location."),
      DTOutput("sharing_table")
    )
  })
  
  output$sharing_table <- renderDT({
    req(selected_species())
    
    # Base exploitation data for the species
    df_explo <- current_link_explo() %>% filter(Alien == selected_species())
    
    # Step A: Define the spatial scope
    if (!is.null(selected_point_id())) {
      # Scope is limited to the specific clicked marker
      target_info <- current_link_data() %>% filter(row_id == selected_point_id())
      req(nrow(target_info) > 0)
      
      df <- df_explo %>%
        filter(
          abs(LonA - target_info$LonA) < 0.0001,
          abs(LatA - target_info$LatA) < 0.0001
        ) %>%
        select(Species1, mean_prob, vir_bin)
    } else {
      # Scope is the entire selected Continent/Region
      spatial_filter <- current_link_data() %>% filter(Alien == selected_species())
      if (input$continent_select != "All") spatial_filter <- spatial_filter %>% filter(Continent == input$continent_select)
      if (input$reg_select != "All") spatial_filter <- spatial_filter %>% filter(Landmass == input$reg_select)
      
      df <- df_explo %>%
        inner_join(spatial_filter %>% select(LonA, LatA), by = c("LonA", "LatA")) %>%
        group_by(Species1) %>%
        summarise(mean_prob = max(mean_prob, na.rm = TRUE), vir_bin = max(vir_bin, na.rm = TRUE))
    }
    
    # Final formatting
    df <- df %>%
      arrange(desc(mean_prob)) %>%
      mutate(mean_prob = round(mean_prob, 2),
             showing = ifelse(vir_bin == 0 & mean_prob >= 0.5, 1, 0)) %>% 
      dplyr::select(Species1, mean_prob, showing)
    
    datatable(df, selection = 'single', options = list(pageLength = 10, dom = 'ftp', columnDefs = list(list(visible = FALSE, targets = 3)))) %>%
      formatStyle("showing", target = "row", fontWeight = styleEqual(1, "bold"))
  })
  
  # 8. SHAP Plot
  output$shap_plot <- renderPlot({
    req(selected_species())
    s <- input$sharing_table_rows_selected
    req(s)
    
    # We need the partner name from the same logic used in the table
    df_explo <- current_link_explo() %>% filter(Alien == selected_species())
    if (!is.null(selected_point_id())) {
      target_info <- current_link_data() %>% filter(row_id == selected_point_id())
      data_table_filtered <- df_explo %>%
        filter(abs(LonA - target_info$LonA) < 0.0001, abs(LatA - target_info$LatA) < 0.0001) %>%
        arrange(desc(mean_prob))
    } else {
      spatial_filter <- current_link_data() %>% filter(Alien == selected_species())
      if (input$continent_select != "All") spatial_filter <- spatial_filter %>% filter(Continent == input$continent_select)
      if (input$reg_select != "All") spatial_filter <- spatial_filter %>% filter(Landmass == input$reg_select)
      
      data_table_filtered <- df_explo %>%
        inner_join(spatial_filter %>% select(LonA, LatA), by = c("LonA", "LatA")) %>%
        group_by(Species1) %>%
        summarise(mean_prob = max(mean_prob, na.rm = TRUE)) %>%
        arrange(desc(mean_prob))
    }
    
    partner_name <- data_table_filtered$Species1[s]
    plot_data <- current_data_and_S() %>% filter(Species2 == selected_species(), Species1 == partner_name)
    req(nrow(plot_data) > 0)
    
    var_labels <- c("trait_sim_gow" = "Trait similarity", "foraging_sim" = "Foraging similarity", 
                    "phylo_sim" = "Phylogenetic similarity", "Overlapping_Cells_log" = "Geographical overlap", 
                    "log_sum_cit" = "Log(sum of citations)")
    
    plot_data <- plot_data %>% mutate(Var_Pretty = ifelse(Var %in% names(var_labels), var_labels[Var], Var))
    
    ggplot(plot_data, aes(x = reorder(Var_Pretty, Shap), y = Shap, fill = Shap > 0)) +
      geom_bar(stat = "identity", width = 0.7) + coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#ab3232", "FALSE" = "#3632ab"), guide = "none") +
      theme_minimal(base_size = 14) +
      labs(title = paste0("<b>Alien: <b><i>", selected_species(), "</i> &nbsp; <b>Native:<b> <i>", partner_name, "</i>"), x = NULL, y = "SHAP value") +
      theme(plot.title = ggtext::element_markdown(size = 16))
  })
}

shinyApp(newui, server)

shinylive::export(appdir = "C:/Users/ndrto/Documents/GitHub/AlienViralSharing_app", destdir = "docs")
