# Load packages ----
library(shiny)
library(shinythemes)
library(maps)
library(mapproj)
library(ggplot2)
library(sf)

# Load data ----
RQ_df <- readRDS("GeoToxHazard-App/nata_tox21_sp.rds")
assay_list <-read.csv("GeoToxHazard-App/assay_list.csv")
assay_list <- as.list(assay_list)
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
heatmap_df <- read.csv("GeoToxHazard-App/heatmap_df.csv")
chem_count_df <- readRDS("GeoToxHazard-App/chem_count_sp.rds")
localG_df <- readRDS("GeoToxHazard-App/localG_sp.rds")

# User interface ----
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("NATA TOX21"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create culumative hazard quotient maps for complex mixtures acting on molecular initiating events."),
      
      selectInput(inputId = "assay", 
                  label = "Choose a variable to display",
                  choices = assay_list,
                  selected = "LTEA_HepaRG_CYP1A1_up"),
      
    ),
    
    #mainPanel(plotOutput("chem_count_map"),
    mainPanel(plotOutput("RQ_map", width = 800, height = 400),
    mainPanel(plotOutput("Gi_plot", width = 800, height = 400),
    mainPanel(plotOutput("heatmap_plot", width = 800, height = 350)
    )
    )
    )
  )
)


# Server logic ----
server <- function(input, output) {

  # Subset data
  variable <- reactive({
    input$assay
  })
  
  
  output$chem_count_map <- renderPlot({
    
    chem_count_map <- ggplot(data = chem_count_df, aes(fill = get(input$assay))) +
      geom_sf(lwd = 0)+
      theme_bw()+
      labs(fill="Chemical Count")+
      geom_sf(data = states, fill = NA, size=0.15)+
      #scale_fill_viridis_c(direction=-1)+
      scale_fill_distiller(name="# Chemicals", palette = "YlGnBu", direction = 1) +
      theme(text = element_text(size = 14)) 
    print(chem_count_map)
  })
  
  output$RQ_map <- renderPlot({
    
    RQ_map <- ggplot(data = RQ_df, aes(fill = get(input$assay))) +
      geom_sf(lwd = 0)+
      scale_fill_distiller(name="RQ Sum", palette = "YlGnBu", direction = 1, trans = "sqrt") +
      theme_bw()+
      labs(fill="RQ Sum")+
      geom_sf(data = states, fill = NA, size=0.15)+
      theme(text = element_text(size = 14)) 
    print(RQ_map)
  })
  
  output$heatmap_plot<- renderPlot({
    
    heatmap_plot <- ggplot(data = subset(heatmap_df, assay_name == input$assay), aes (chemical_name, assay_name,  fill= ACC)) + 
      geom_tile()+
      theme_bw()+
      #scale_fill_viridis_c(direction=-1)+
      scale_fill_distiller(palette = "YlGnBu", direction = -1) +
      labs(y="Assay", x="Chemical", fill = "ACC (μM)")+
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      theme(text = element_text(size = 16)) 

    print(heatmap_plot)
  
  })
  
  ##  weight_county_tpo_sf$Gi_pval = as.factor() 
  output$Gi_plot<- renderPlot({
    Gi_plot <-  ggplot() +
      geom_sf(data = localG_df, aes(fill = get(input$assay)), lwd = 0)+  
      scale_fill_gradient2(high = "#C41E3A", mid = "white", low = "#000080", midpoint = 0, 
                           guide = guide_colourbar(title = "Gi* score"))+
      geom_sf(data = subset(localG_df, get(input$assay) > 3.886 | get(input$assay) < -3.886), fill = NA, size=0.25)+
      geom_sf(data = states, fill = NA, size=0.15)+
      theme_bw()+
      theme(text = element_text(size = 14)) 
    print(Gi_plot) 

  })
  
  
}

shinyApp(ui, server)