library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(markdown)
library(bslib)
library(DT)
library(shinyWidgets)
library(plotly)
library(shinyBS)

sf::sf_use_s2(FALSE)

boundary_lines <- readRDS('data/nps_boundary_lines.RDS') 
states <- readRDS('data/states.RDS')


# Catchment boundaries
inside <- readRDS('data/catch.RDS') 

# ATTAINS
lines <- readRDS('data/lines.RDS') 
areas <- readRDS('data/areas.RDS')
points <- readRDS('data/points.RDS')
# Download Table
attains_data <- readRDS('data/attains_table.RDS')

#Tier 2.5 and Tier 3 Waters
orw <- readRDS('data/orw_.RDS')

# For park mapper
nps_points <- readRDS('data/nps_points.RDS')

# NHD
nhd_lines <- readRDS('data/map_flowlines.RDS')
nhd_waterbodies <- readRDS('data/map_waterbodies.RDS')
nhd_areas <- readRDS('data/map_areas.RDS')

#ATTAINS Watershed
ws_lines <-readRDS('data/ws_lines.RDS')
ws_areas <-readRDS('data/ws_areas.RDS')
ws_points <- readRDS('data/ws_points.RDS')
ws <- readRDS('data/ws.RDS')

ui <- navbarPage("National Park Service Water Quality",
                 
                 tabPanel("Explore Parkwide Impairments",
                          
                          fluidPage(
                            fluidRow(
                              column(3, 
                                     h6("This is a draft version of a data viewer to accompany NPCA's evaluation of state water quality assessments and the National Park Service System.
              The underlying data used to develop this map comes from the EPA's most recent ATTAINS geospatial database. The pie chart plots the fraction of park units impaired by the selected water quality
              parameter(s). When selecting impairments, only parks with ALL selected impairments will be displayed."),
              fluidRow(pickerInput(
                inputId = "impairment", 
                label = "Select Impairments:",
                choices = #sort(unique(inside$Grouped_Impairments)),
                   sort(c("HABITAT ALTERATIONS","ALGAL GROWTH","ORGANIC ENRICHMENT/OXYGEN DEPLETION","PATHOGENS","MERCURY",
                                  "PH/ACIDITY/CAUSTIC CONDITIONS","PESTICIDES","TOXIC ORGANICS","AMMONIA","METALS OTHER THAN HG",
                                  "SALINITY/TOTAL DISSOLVED SOLIDS/CHLORIDES/SULFATES","DIOXINS","TURBIDITY","TOXIC INORGANICS","RADIATION",
                                  "HYDROLOGIC ALTERATION","NUISANCE EXOTIC SPECIES","NUTRIENTS","SEDIMENT","TEMPERATURE","OTHER CAUSE",
                                  "CAUSE UNKNOWN - IMPAIRED BIOTA","CAUSE UNKNOWN","POLYCHLORINATED BIPHENYLS - PCBS","OIL AND GREASE",
                                  "TRASH","TASTE, COLOR, AND ODOR","FISH CONSUMPTION ADVISORY","NOXIOUS AQUATIC PLANTS","TOTAL TOXICS",
                                  "CAUSE UNKNOWN - FISH KILLS","CHLORINE")),
                selected = c("PATHOGENS"),
                options = list('actions-box' = TRUE),
                multiple = TRUE)),
              
              fluidRow(plotOutput("plot1"))),
              column(9,
                     h6(""),
                     h6(""),
                     br(),
                     leafletOutput("map1"), height = "100%")))),
              
              
              tabPanel("Explore by Park",
                       
                       fluidPage(
                         fluidRow(
                           column(3, 
                                  h6("This is a draft version of a data viewer to accompany NPCA's evaluation of state water quality assessments and the National Park Service System.
              The underlying data used to develop this map comes from the EPA's most recent ATTAINS geospatial database. The pie chart plots water quality status by catchment 
              area for a general assessment of how much of a particular park is assessed by their state water quality management agency."),
              fluidRow(pickerInput(
                inputId = "park", 
                label = "Select National Park:",
                choices = sort(boundary_lines$Park),
                selected = "Gauley River National Recreation Area",
                options = list('actions-box' = TRUE),
                multiple = FALSE)),
              fluidRow(plotOutput("plot2"),
                       #br(),
                       downloadButton("downloadData", "Download Selected Data")
              )),
              
              column(9,
                     h6(""),
                     h6(""),
                     br(),
                     leafletOutput("map2"), height = "100%")),
              br(),
              fluidRow(class = "table",
                       # Table
                       dataTableOutput("table"))))
              
              
              
              
)

server <- function(input, output, session) {
  
  
  filtered_impairment <- reactive({
    
    # Filter data based on selected impairments
    if(!isTruthy(input$impairment)){nps_points<-filter(nps_points, Impairments=="Bogus")
    
    }else{
      nps_points <- filter(nps_points, 
                           #grepl(paste0(input$impairment, collapse= "|"), 
                           grepl(paste0("(?=.*",paste((input$impairment),collapse=")(?=.*"),")"),
                                 Impairments, perl=T))
    }
    
    nps_points
    
  })
  
  filtered_states <- reactive({
    
    states <- filter(states, STUSPS %in% filtered_impairment()$State)
    
    states
    
  })
  
  
  output$plot1 <- renderPlot({
    validate (need(nrow(filtered_impairment()) > 0, message = "No impairment(s) selected."))
    pie <- nps_points %>%
      st_drop_geometry() %>%
      mutate(baddies = ifelse(Park %in% filtered_impairment()$Park, "Impaired", "Not Impaired")) %>%
      group_by(baddies) %>%
      distinct(Park, .keep_all = TRUE) %>%
      dplyr::summarize(count=n())
    pie <- pie %>%
      mutate(prop = count/351) %>%  
      mutate(ypos = cumsum(prop) - 0.5*prop) %>%
      mutate(legend = paste0(baddies, " (", scales::percent(prop, accuracy=0.01), ")"))
    
    ggplot(data=pie, aes(x="", y=count, fill=legend)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=c("#813B00","#C2CAD7")) +
      guides(fill=guide_legend(title="")) +
      theme_void() + # remove background, grid, numeric label
      theme(text = element_text(size = 20)) 
  })
  
  #  generarte the map object 
  output$map1 <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4)) %>%
      setView(lng = -105.07592352501446, 
              lat = 40.59085658003177, 
              zoom = 6) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") #%>%
    # addLayersControl(
    #   # baseGroups = c("OpenStreetMap", "Light"),
    #   overlayGroups = c(
    #     "Catchments", "ATTAINS", "Tier 2.5 and Tier 3 Waters"
    #   ),
    #   position = "topleft",
    #   options = layersControlOptions(collapsed = TRUE)
    # )
  })
  
  #  generarte the map object 
  output$map1 <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      setView(lng = -105.07592352501446, 
              lat = 40.59085658003177, 
              zoom = 2) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron")
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map1", suspendWhenHidden=FALSE)
  
  #set location to park
  observe({
    c0 <- reactive({
      filtered_impairment() %>%
        st_bbox() %>%
        unname()
    })
    
    leafletProxy('map1') %>% 
      clearMarkers() %>%
      clearShapes() %>%
      fitBounds(lng1 =  c0()[1], lat1 = c0()[2], lng2 =  c0()[3], lat2 = c0()[4]) %>%
      # based on NPCA feedback - want to make sure states where impairments exist are represented-ish. Think APPA; if impaired, 
      # want a way of showing where it exists
      addPolylines(
        data = filtered_states(),
        fillColor = "",
        fillOpacity = 1,
        color = "#059FA4",
        weight = 2) %>%
      addCircles(
        data = nps_points,
        fill = "#C2CAD7",
        color = "#C2CAD7",
        fillOpacity = 0.5,
        radius = 1) %>%
      #group = "ATTAINS",
      # popup = paste0("Park: ", filtered_impairment()$Park,
      #                "<br>",
      #                "Impairments: ", filtered_impairment()$Impairments))
      addCircles(
        data = filtered_impairment(),
        fill = "#813B00",
        color = "#813B00",
        fillOpacity = 1,
        radius = 6, 
        weight = 16,
        #group = "ATTAINS",
        popup = paste0("Park: ", filtered_impairment()$Park,
                       "<br>",
                       "Impairments: ", filtered_impairment()$Impairments)) 
  })
  
  filtered_data <- reactive({
    
    # Filter data based on selected Park Unit.
    if(!isTruthy(input$park)){attains_data<-filter(attains_data,Park=="Bogus")
    }else{
      attains_data <- filter(attains_data, Park == input$park)
    }
    
    attains_data
    
  })
  
  orwer <- reactive({
    
    orw <- filter(orw, Park == input$park)
    
    orw
    
  })
  
  liner <- reactive({
    
    lines <- filter(lines, Park == input$park)
    
    lines
    
  })
  
  pointer <- reactive({
    
    points <- filter(points, Park == input$park)
    
    points
    
  })
  
  areaer <- reactive({
    
    areas <- filter(areas, Park == input$park)
    
    areas
    
  })
  
  nhd_liner <- reactive({
    
    lines <- filter(nhd_lines, UNIT_NAME == input$park)
    
    lines
    
  })
  
  nhd_waterbodyer <- reactive({
    
    lines <- filter(nhd_waterbodies, UNIT_NAME == input$park)
    
    lines
    
  })
  
  nhd_areaer <- reactive({
    
    lines <- filter(nhd_areas, UNIT_NAME == input$park)
    
    lines
    
  })
  
  ws_liner <- reactive({

    ws_lines <- filter(ws_lines, Park == input$park)

    ws_lines

  })

  ws_pointer <- reactive({

    ws_points <- filter(ws_points, Park == input$park)

    ws_points

  })

  ws_areaer <- reactive({

    ws_areas <- filter(ws_areas, Park == input$park)

    ws_areas

  })
  
  watersheder <- reactive({
    
    ws <- filter(ws, Park == input$park)
    
    ws
    
  })
  
  insider <- reactive({
    
    inside <- filter(inside, Park == input$park)
    
    inside
    
  })
  
  boundary_liner <- reactive({
    
    boundary_lines <- filter(boundary_lines, Park == input$park)
    
    boundary_lines
    
  })
  
  # code for selecting/highlighting... broken/half-baked
  # observeEvent(input$table_rows_selected, {
  #   selectedRow <- input$table_rows_selected
  #   output$selectedRow <- renderPring(selectedRow)
  #   
  #   selected <- reactive({input$updatedData_rows_selected})
  #   
  #   if (!is.null(selectedRow)){
  #     try(selectedFeature1 <- filter(liner(), assessmentunitidentifier == selectedRow$Assessment_Code))
  #     try(selectedFeature2 <- filter(areaer(), assessmentunitidentifier == selectedRow$Assessment_Code))
  #     try(selectedFeature3 <- filter(pointer(), assessmentunitidentifier == selectedRow$Assessment_Code))
  #   }
  #   
  #   
  #   
  # })
  
  
  output$plot2 <- renderPlot({
    validate (need(nrow(insider()) > 0, message = "No catchment data currently available at this park unit."))
    pie <- insider() %>%
      st_drop_geometry() %>%
      distinct(nhdplusid, .keep_all = TRUE) %>%
      dplyr::group_by(Assessment_Category, col) %>%
      dplyr::summarize(count=sum(as.numeric(catchment_area))) %>%
      ungroup()
    pie <- pie %>%
      mutate(Assessment_Category = factor(x = Assessment_Category, levels = Assessment_Category)) %>% 
      mutate(prop = count/sum(pie$count)) %>%  
      mutate(ypos = cumsum(prop)- 0.5*prop) %>%
      mutate(legend = paste0(Assessment_Category, " (", scales::percent(prop), ")"))
    
    ggplot(data=pie, aes(x="", y=count, fill=legend)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=pie$col) +
      guides(fill=guide_legend(title="Status by Catchment Area")) +
      theme_void() + # remove background, grid, numeric label
      theme(text = element_text(size = 20)) 
  })
  
  #  generarte the map object 
  output$map2 <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4)) %>%
      setView(lng = -105.07592352501446, 
              lat = 40.59085658003177, 
              zoom = 6) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      addLayersControl(
        # baseGroups = c("OpenStreetMap", "Light"),
        overlayGroups = c(
          "Catchments", "ATTAINS", "Tier 2.5 and Tier 3 Waters", "Water Features", "Upstream ATTAINS"
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Tier 2.5 and Tier 3 Waters", "Upstream ATTAINS")
      )
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map2", suspendWhenHidden=FALSE)
  
  #set location to park
  observe({
    c1 <- reactive({
      boundary_liner() %>%
        #filter(Park  == input$park) %>%
        st_set_agr("constant") %>% # attributes constant over geometries (suppresses warning message)
        st_bbox() %>%
        unname()
      #sf::st_centroid() %>%
      #st_coordinates()
    })
    #  
    leafletProxy('map2') %>% 
      clearMarkers() %>%
      clearShapes() %>%
      clearImages() %>%
      fitBounds(lng1 =  c1()[1], lat1 = c1()[2], lng2 =  c1()[3], lat2 = c1()[4]) %>%
      
      addPolylines(
        data = boundary_liner(),
        fillColor = "",
        fillOpacity = 1,
        color = "black",
        weight = 2) %>%
      
      addPolylines(
        data = watersheder(),
        group = "Upstream ATTAINS",
        fillColor = "",
        fillOpacity = 1,
        color = "black",
        weight = 1) %>%
      
      addPolylines(
        data = nhd_liner(),
        fillColor = "black",
        group = "Water Features",
        fillOpacity = 0,
        color = "black",
        weight = 0.3)  %>%
      
      addPolygons(
        data = nhd_waterbodyer(),
        group = "Water Features",
        fillColor = "",
        fillOpacity = 0.55,
        color = "black",
        weight = 0.5) %>%
      
      addPolygons(
        data = nhd_areaer(),
        group = "Water Features",
        fillColor = "",
        fillOpacity = 0.55,
        color = "black",
        weight = 0.5) %>%
      
      addPolygons(
        data = insider(),
        group = "Catchments",
        fillColor = insider()$col,
        fillOpacity = 0.55,
        color = "black",
        weight = 1) %>%
      
      addPolygons(
        data = areaer(),
        fillColor = areaer()$col,
        group = "ATTAINS",
        fillOpacity = 0.55,
        color = "black",
        weight = 2,
        popup = paste0("Status: ", areaer()$Assessment_Category,
                       "<br>",
                       "State ID: ", areaer()$assessmentunitidentifier,
                       "<br>",
                       "Impairments: ", areaer()$Impairments,
                       "<br>",
                       "URL: ", areaer()$Link)) %>%
      addPolylines(
        data = orwer(),
        fillColor = "#00714a",
        group = "Tier 2.5 and Tier 3 Waters",
        fillOpacity = 0,
        color = "#00714a",
        weight = 6,
        popup = paste0("Designation: ", orwer()$designation_name,
                       "<br>",
                       "Extent: ", orwer()$entire_extent_of_orw))  %>%
      
      addPolylines(
        data = liner(),
        fillColor = liner()$dark_col,
        group = "ATTAINS",
        fillOpacity = 1,
        color = liner()$dark_col,
        weight = 4.5,
        popup = paste0("Status: ", liner()$Assessment_Category,
                       "<br>",
                       "State ID: ", liner()$assessmentunitidentifier,
                       "<br>",
                       "Impairments: ", liner()$Impairments,
                       "<br>",
                       "URL: ", liner()$Link),
        highlightOptions = highlightOptions(
          color = "yellow",
          opacity = 1,
          weight = 3,
          bringToFront = TRUE
        )) %>%
      addCircles(
        data = pointer(),
        fill = pointer()$col,
        color = "black",
        fillOpacity = 0.5,
        group = "ATTAINS",
        popup = paste0("Status: ", pointer()$Assessment_Category,
                       "<br>",
                       "State ID: ", pointer()$assessmentunitidentifier,
                       "<br>",
                       "Impairments: ", pointer()$Impairments,
                       "<br>",
                       "URL: ", pointer()$Link)) %>%
      addPolygons(
        data = ws_areaer(),
        fillColor = ws_areaer()$col,
        group = "Upstream ATTAINS",
        fillOpacity = 0.55,
        color = "black",
        weight = 2,
        popup = paste0("Status: ", ws_areaer()$Assessment_Category,
                       "<br>",
                       "State ID: ", ws_areaer()$assessmentunitidentifier,
                       "<br>",
                       "Impairments: ", ws_areaer()$Impairments,
                       "<br>",
                       "URL: ", ws_areaer()$Link)) %>%
    addPolylines(
      data = ws_liner(),
      fillColor = ws_liner()$dark_col,
      group = "Upstream ATTAINS",
      fillOpacity = 1,
      color = ws_liner()$dark_col,
      weight = 4.5,
      popup = paste0("Status: ", ws_liner()$Assessment_Category,
                     "<br>",
                     "State ID: ", ws_liner()$assessmentunitidentifier,
                     "<br>",
                     "Impairments: ", ws_liner()$Impairments,
                     "<br>",
                     "URL: ", ws_liner()$Link)) %>%
      addCircles(
        data = ws_pointer(),
        fill = ws_pointer()$col,
        color = "black",
        fillOpacity = 0.5,
        group = "Upstream ATTAINS",
        popup = paste0("Status: ", ws_pointer()$Assessment_Category,
                       "<br>",
                       "State ID: ", ws_pointer()$assessmentunitidentifier,
                       "<br>",
                       "Impairments: ", ws_pointer()$Impairments,
                       "<br>",
                       "URL: ", ws_pointer()$Link)) #%>%
    #   addPolygons(
    #     data = selectedFeature2,
    #     fillColor = areaer()$col,
    #     fillOpacity = 0.55,
    #     color = '#04FFF7',
    #     weight = 2) %>%
    #   addPolylines(data = selectedFeature1,
    #                fillColor = '#04FFF7',
    #                color = '#04FFF7',
    #                weight = 4.5) %>%
    # addCircles(data = selectedFeature3, 
    #            fill = "#04FFF7",
    #            color = '#04FFF7',
    #            fillOpacity = 0.5)
    
  })
  
  output$table <- DT::renderDataTable({
    validate (need(nrow(filtered_data()) > 0, message="No ATTAINS data in this park unit."))
    tableee <- filtered_data()
    tableee$URL <- paste0('<a  target=_blank href=', tableee$URL, '>', tableee$URL,'</a>' )
    DT::datatable(tableee , selection = 'single', escape = FALSE, options = list(autoWidth=TRUE, scrollX=TRUE, scrollY = "400px", scrollCollapse = TRUE))},
    options = list(autoWidth=TRUE, scrollX=TRUE, scrollY = "400px", scrollCollapse = TRUE))
  
  
  
  
  # Create a .csv of selected data for download.
  output$downloadData <- downloadHandler(
    filename = "ATTAINS.csv",
    content = function(file) {
      write_csv(filtered_data(),file)})
  
  
}

## Run the app. 
shinyApp(ui = ui, server = server)