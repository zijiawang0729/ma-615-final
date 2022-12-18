library(shiny)
library(googleway)
library(dplyr)
library(rsconnect)

rsconnect::setAccountInfo(name='ghslf0-zijia-wang', token='13FCFA76DF9827D75397F8AC77842855', secret='AHnm6jPuRI+TS5gR8bspdQwRaaXrCUReRHIaZhSM')
setwd("~/Desktop/MSSP/MA615/ma-615-final/615 shinny")
stops <- read.csv("stops.txt") 
HR <- read.csv("HR.csv") 
LR <- read.csv("LR.csv") 

## find id that only use for rapid transit.
a <- unique(HR$from_stop_id)
b <- unique(HR$to_stop_id)
c <- unique(c(a,b))

LR <- filter(LR, route_id != "Mattapan")
d <- unique(LR$from_stop_id)
e <- unique(LR$to_stop_id)
f <- unique(c(d,e))
g <- data.frame(unique(c(c,f))) %>% rename("stop_code" = "unique.c.c..f..")

stops <- stops %>% inner_join(g, by="stop_code") 


api_key <- "AIzaSyCacg2DRBvEFi9CHTJQUJK_ZCuN8Cwnlug"
map_key <- "https://maps.googleapis.com/maps/api/directions/json?origin=Disneyland&destination=Universal+Studios+Hollywood&key=AIzaSyCacg2DRBvEFi9CHTJQUJK_ZCuN8Cwnlug"



ui <- navbarPage("615 Shiny", position = c("static-top"),tabPanel("MAP",
                                                                  google_mapOutput(outputId = "map"),
                                                                  selectInput(inputId = "origin", label = "Select origin:", multiple = FALSE, choices = sort(stops$stop_name)),
                                                                  selectInput(inputId = "destination", label = "Select destination:", multiple = FALSE, choices = sort(stops$stop_name)),
                                                                  selectInput(inputId = "mode", label = "Select mode:", multiple = FALSE, choices = sort(c("driving", "walking", "bicycling", "transit"))),
                                                                  actionButton(inputId = "getRoute", label = "Get Route")
))


server <- function(input, output, session) {
  
  
  
  output$map <- renderGoogle_map({
    google_map(key = map_key,
               search_box = TRUE, 
               scale_control = TRUE, 
               height = 1000) %>%
      add_traffic()
  })
  
  observeEvent(input$getRoute,{
    
    print("getting route")
    
    o <- paste("input$orgin", ", Boston, MA, the United States")
    d <- paste("input$destination", ", Boston, MA, the United States")
    m <- input$mode
    
    res <- google_directions(key = api_key,
                             origin = o,
                             destination = d,
                             mode = m)
    
    df_route <- data.frame(route = res$routes$overview_polyline$points)
    
    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address)
    )
    
    df_way$order <- as.character(1:nrow(df_way))
    
    google_map_update(map_id = "map") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way,
                  info_window = "end_address",
                  label = "order")
  })
}


shinyApp(ui, server)
