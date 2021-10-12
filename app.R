# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(ggplot2)
library(sf)

# Load data ----
counties <- readRDS("GeoToxHazard-App/nata_tox21_sp.rds")
assay_list <-read.csv("GeoToxHazard-App/assay_list.csv")
assay_list <- as.list(assay_list)
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
heatmap_df <- read.csv("GeoToxHazard-App/heatmap_df.csv")

# User interface ----
ui <- fluidPage(
  titlePanel("NATA TOX21"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create culumative hazard quotient maps for complex mixtures acting on molecular initiating events."),
      
      selectInput(inputId = "assay", 
                  label = "Choose a variable to display",
                  choices = assay_list
                  ),
      
    ),
    
    mainPanel(plotOutput("RQ_map"),
    mainPanel(plotOutput("heatmap_plot"))
    #mainPanel(plotOutput("Gi_plot"))
    )
  )
)


# Server logic ----
server <- function(input, output) {

  # Subset data
  variable <- reactive({
    input$assay
  })
  
  
  output$RQ_map <- renderPlot({
    counties <- readRDS("GeoToxHazard-App/nata_tox21_sp.rds")
    RQ_map <- ggplot(data = counties, aes(fill = get(input$assay))) +
      geom_sf(lwd = 0)+
      scale_fill_distiller(name="RQ Sum", palette = "YlGnBu", direction = 1, trans = "sqrt") +
      theme_bw()+
      labs(fill="RQ Sum")+
      geom_sf(data = states, fill = NA, size=0.15)+
      theme(text = element_text(size = 16)) 
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
      theme(text = element_text(size = 20)) 

    print(heatmap_plot)
  
  })
  
  output$Gi_plot<- renderPlot({
    Gi_plot <-  ggplot() +
      geom_sf(data = weight_county_tpo_sf, aes(fill=local_g), lwd = 0)+
      geom_sf(data = subset(weight_county_tpo_sf, Gi_pval == "TRUE"), aes(group=Gi_pval), fill = NA, size=0.25)+
      geom_sf(data = states, fill = NA, size=0.15)+
      theme_bw()+
      scale_fill_gradient2(high = "#C41E3A", mid = "white", low = "#000080", midpoint = 0,
                           guide = guide_colourbar(title = "Gi* score"))+
      theme(text = element_text(size = 16)) 
    print(Gi_plot) 

  })
  
  
}

shinyApp(ui, server)