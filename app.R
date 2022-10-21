# Load packages ----
library(shiny)
library(shinythemes)
library(maps)
library(mapproj)
library(ggplot2)
library(sf)
library(readr)
library(rsconnect)
library(forcats)
library(scales)

# Load data ----

RQ_df <-  readr::read_rds("GeoToxHazard-App/nata_tox21_sp.rds")
assay_list <- read.csv("GeoToxHazard-App/assay_list.csv",stringsAsFactors=FALSE, sep=",", header=TRUE)
assay_list <- as.list(assay_list)
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
heatmap_df <- read.csv("GeoToxHazard-App/heatmap_df.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)
chem_count_df <- readr::read_rds("GeoToxHazard-App/chem_count_sp.rds")
localG_df <- readr::read_rds("GeoToxHazard-App/localG_sp.rds")

# User interface ----
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Risk of Molecular Target Activation From Exposure to Chemical Mixtures"),
  
  sidebarLayout(
    sidebarPanel(
      helpText ("Calculate the cumulative hazard quotient maps for chemical mixtures acting on molecular targets", 
               br(),
               "  ",
               br(),
               "Exposure Data: National Air Toxics Assessment (NATA)",
               br(),
               "Hazard Data: TOX21 High Throughput Screening Assays"),
      
      selectInput(inputId = "assay", 
                  label = "Select an assay:",
                  choices = assay_list,
                  selected = "LTEA_HepaRG_CYP1A1_up"),
      
    ),
    
    mainPanel(plotOutput("chem_count_map", width = 800, height = 400),
    mainPanel(plotOutput("heatmap_plot", width = 1000, height = 350),
    mainPanel(plotOutput("RQ_map", width = 800, height = 400),
    mainPanel(plotOutput("Gi_plot", width = 800, height = 400)
    )
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
    chem_count_map <- ggplot(data = subset(chem_count_df, assay_name == input$assay)) +
      geom_sf(aes(fill = chem_count), lwd = 0)+  
      scale_fill_distiller(name="Chemical Count", palette = "YlGnBu", direction = 1, breaks= pretty_breaks()) +
      geom_sf(data = states, fill = NA, size=0.15)+
      theme_bw()+
      theme(text = element_text(size = 14)) 
    print(chem_count_map)
  })
  
  output$RQ_map <- renderPlot({
    
    RQ_map <- ggplot(data = RQ_df, aes(fill = get(input$assay))) +
      geom_sf(lwd = 0)+
      scale_fill_distiller(name="Summed Hazard Quotients", palette = "YlGnBu", direction = 1, trans = "sqrt") +
      theme_bw()+
      geom_sf(data = states, fill = NA, size=0.15)+
      theme(text = element_text(size = 14)) 
    print(RQ_map)
  })
  
  output$heatmap_plot<- renderPlot({
    
    heatmap_plot <- 
      ggplot(data = subset(heatmap_df, assay_name == input$assay), aes (fct_reorder(chemical_name, ACC), assay_name,  fill= ACC)) + 
      geom_tile()+
      theme_bw()+
      #scale_fill_viridis_c(direction=-1)+
      scale_fill_distiller(palette = "YlGnBu", direction = -1) +
      labs(y="Assay", x="Chemical", fill = "Activity Concentration Cutoff (μM)")+
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      theme(text = element_text(size = 16)) 

    print(heatmap_plot)
  
  })
  
  ##  weight_county_tpo_sf$Gi_pval = as.factor() 
  output$Gi_plot<- renderPlot({
    Gi_plot <-  ggplot() +
      geom_sf(data = localG_df, aes(fill = get(input$assay)), lwd = 0)+  
      scale_fill_gradient2(high = "#C41E3A", mid = "white", low = "#000080", midpoint = 0, 
                           guide = guide_colourbar(title = "Gi* Score"))+
      geom_sf(data = subset(localG_df, get(input$assay) > 3.886 | get(input$assay) < -3.886), fill = NA, size=0.25)+
      geom_sf(data = states, fill = NA, size=0.15)+
      theme_bw()+
      theme(text = element_text(size = 14)) 
    print(Gi_plot) 

  })
  
  
}

shinyApp(ui, server)