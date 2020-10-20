library(tidyverse)
library(readxl)
library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(lubridate)
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(geojsonio)) install.packages("geojsonio")
library(htmltools)
library(DT)
library(lubridate)
library(mapview)
library(foreign)
library(webshot)
webshot::install_phantomjs()

source("LoadAlerts.R")



# LOAD DATA
# ---------------------

# STEP 1 - LOAD ALERTS INTO A DATAFRAME

# 1.1 Raw Data

options(DT.options = list(language=list("https://cdn.datatables.net/plug-ins/1.10.21/i18n/French.json"),
                          buttons = list()))

current_date <- Sys.Date()

pcodes_adm3 <- read_csv("INPUT/DATA/PCodes/MLI_PCodesAdmin3.csv")

pcodes_adm2 <- pcodes_adm3%>%
  select(Admin1_Nam,Pcode_Ad_1,Admin2_Nam,Pcode_Ad_2)%>%
  distinct()

pcodes_adm1<- pcodes_adm3%>%
  select(Admin1_Nam,Pcode_Ad_1)%>%
  distinct()

# 1.2 Base Map


mli_adm2_shp <- readOGR("INPUT/Shapes/mli_adm2_withMenaka.shp")
mli_adm1_shp <- readOGR("INPUT/Shapes/mli_adm1_withMenaka.shp")

bins_adm2 <- c(0,1000,5000,10000,20000, Inf)
bins_adm1 <- c(0,10000,30000,50000,100000, Inf) 

REACH_colors <- colorRamp(c("#FCE3E4","#FAD3D4", "#F8BABB", "#F6A9AA","#F49495","#EE5859"))

# basemap <- leaflet(mli_adm2_shp)%>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
#   addPolygons(weight = 1, stroke = F)
  
### SHINY DASHBOARD

ui <- bootstrapPage(
  navbarPage(collapsible = TRUE,
             theme = shinytheme("flatly"),
             title ="RRM - Mali", id="nav",
                tabPanel("Alertes", id = "cercles",
                                  div(class = "outer",
                                      tags$head(includeCSS("style.css")),
                                      leafletOutput(paste0("mymap"), width="100%", height="60%"),
                                      absolutePanel(id = "controls", class = "panel panel-default",
                                                    top = 75, left = 70, width = 300, fixed=TRUE,
                                                    draggable = FALSE, height = "auto",
                                                    radioButtons("adm_level", h5("Niveau administratif d'accueil"),
                                                      choices = c("Cercles" = "cercles", "Régions" = "regions")),
                                                    dateRangeInput("plot_date",
                                                                label = h5("Dates"),
                                                                start = as.Date("2018-01-01","%Y-%m-%d"),
                                                                end = as.Date(current_date,"%Y-%m-%d"),
                                                                format = "dd.mm.yyyy",
                                                                language = "fr",
                                                                weekstart =1
                                                                ),
                                                    fileInput("datafile", "Charger données", accept = c("application/vnd.ms-excel", 
                                                                                                        "application/vnd.ms-excel.sheet.macroenabled.12",
                                                                                                        "text/csv",
                                                                                                        ".xlsx",
                                                                                                        ".xlsm")),
                                                    downloadButton("dl", "Télécharger carte")
                                                    ),
                                      dataTableOutput("alertsTable")
                                  )
                
                 )
             )
  )

  
server <- function(input, output, session){
  
  Sys.setlocale("LC_ALL", "fr_FR")
  
  Alerts <- reactive({
    
    req(input$datafile)
    
    inFile <- input$datafile
    
    dataPath <- inFile$datapath
  
    loadAlerts(dataPath = dataPath, pcodes_adm2 = pcodes_adm2)
    
  })
  
  basemap <- reactive({
    leaflet(mli_adm2_shp)%>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
    addPolygons(weight = 1, stroke = F, color = "grey")
  })
  

  options(DT.options = list(language=list(url="https://cdn.datatables.net/plug-ins/1.10.21/i18n/French.json")))
  
  reactive_db_adm2 <- reactive({
    req(Alerts)
     db <- Alerts() %>%
       filter(Date_evenement >= input$plot_date[1] & Date_evenement <= input$plot_date[2])%>%
       group_by(Pcode_Ad_2)%>%
       summarise(sum_HH =  sum(Effectifs_Estimatif_des_menages, na.rm = T), nb_alerts = n())%>%
       ungroup()
     
     names(db) <- c("Pcode_Ad_2", "sum_HH", "nb_alerts")
     db

  })
  reactive_polygons_adm2 <- reactive({
      mli_adm2_shp[mli_adm2_shp$admin2Pcod %in% reactive_db_adm2()$Pcode_Ad_2, ]%>%
      sp::merge(as.data.frame(reactive_db_adm2()), by.x ="admin2Pcod", by.y = "Pcode_Ad_2")
    
  })
  
  output$mymap <- renderLeaflet({
    basemap()
  })
  
  reactive_db_adm1 <- reactive({
    req(Alerts)
    db <- Alerts() %>% 
      filter(Date_evenement >= input$plot_date[1] & Date_evenement <= input$plot_date[2])%>%
      group_by(Pcode_Ad_1)%>%
      summarise(sum_HH =  sum(Effectifs_Estimatif_des_menages, na.rm = T), nb_alerts = n())%>%
      ungroup()

    names(db) <- c("Pcode_Ad_1", "sum_HH", "nb_alerts")
    db
    
  })
  reactive_polygons_adm1 <- reactive({

    mli_adm1_shp[mli_adm1_shp$admin1Pcod %in% reactive_db_adm1()$Pcode_Ad_1, ]%>%
      sp::merge(as.data.frame(reactive_db_adm1()), by.x ="admin1Pcod", by.y = "Pcode_Ad_1")

  })
  

  observe({
    
    rrm_pal_adm2 <- colorBin(REACH_colors, domain = Alerts()$Effectifs_Estimatif_des_menages, bins = bins_adm2)
    
    rrm_pal_adm1 <- colorBin(REACH_colors, domain = Alerts()$Effectifs_Estimatif_des_menages, bins = bins_adm1)
    
    req(Alerts)
    
    if(input$adm_level == "cercles"){
      leafletProxy("mymap")%>%
        clearControls()%>%
        clearMarkers() %>%
        clearShapes() %>%
        addCircles(data = reactive_polygons_adm2(),
                   lat = coordinates(reactive_polygons_adm2())[,2],
                   lng = coordinates(reactive_polygons_adm2())[,1],
                   radius = reactive_polygons_adm2()$sum_HH*5,
                   stroke = T, color = "#red", weight = 3, fillColor = "red", fillOpacity = 0.9,
                   label = mapply(function(x, y,z) {
                     HTML(sprintf("<div><b>%s </b> </div>
                                   <div>Ménages déplacés (alerte): %s</div>
                                   <div>Nombre d'alertes: %s </div>", htmlEscape(x),  htmlEscape(y),htmlEscape(z)
                     ))},
                     reactive_polygons_adm2()$admin2Name, reactive_polygons_adm2()$sum_HH,reactive_polygons_adm2()$nb_alerts,
                     SIMPLIFY = F),
                   labelOptions = labelOptions(noHide = F),
                   opacity = 0.9
                   )
        # addPolygons(data = reactive_polygons_adm2(),
        #             stroke = T, color = "white", weight = 1,
        #             # label = mapply(function(x, y,z) {
        #             #   HTML(sprintf("<div><b>%s </b> </div>
        #             #                <div>Ménages déplacés (alerte): %s</div>
        #             #                <div>Nombre d'alertes: %s </div>", htmlEscape(x),  htmlEscape(y),htmlEscape(z)
        #             #                ))},
        #             #   reactive_polygons_adm2()$admin2Name, reactive_polygons_adm2()$sum_HH,reactive_polygons_adm2()$nb_alerts,
        #             #   SIMPLIFY = F),
        #             # labelOptions = labelOptions(noHide = F),
        #             fillColor = "grey",
        #             fillOpacity = 0.1,
        # )%>%
        # addLegend("bottomright", pal = rrm_pal_adm2, values = Alerts()$Effectifs_Estimatif_des_menages,
        #           title = "Nombre de ménages recensés lors des alertes")
      
    }
    if(input$adm_level == "regions"){
      leafletProxy("mymap")%>%
        clearControls()%>%
        clearMarkers() %>%
        clearShapes() %>%
        addPolygons(data = reactive_polygons_adm1(),
                    stroke = T, color = "White", weight = 2,
                    label = mapply(function(x, y,z) {
                      HTML(sprintf("<div><b>%s </b> </div>
                                   <div>Ménages déplacés (alerte): %s</div>
                                   <div>Nombre d'alertes: %s </div>", htmlEscape(x),  htmlEscape(y),htmlEscape(z)
                      ))},
                      reactive_polygons_adm1()$admin1Name, reactive_polygons_adm1()$sum_HH,reactive_polygons_adm1()$nb_alerts, SIMPLIFY = F),
                    labelOptions = labelOptions(noHide = F),
                    fillColor = ~rrm_pal_adm1(reactive_polygons_adm1()$sum_HH),
                    fillOpacity = 0.9
        )%>%
        addLegend("bottomright", pal = rrm_pal_adm1, values = Alerts()$Effectifs_Estimatif_des_menages,
                  title = "Nombre de ménages recensés lors des alertes")
      }
    
  })
  
  reactive_alerts <- reactiveValues(alerts = NULL)
  
  observe({
    
    req(Alerts)
    
    if(input$adm_level == "cercles"){
      reactive_alerts$alerts<-Alerts() %>%
        filter(Date_evenement >= input$plot_date[1] & Date_evenement <= input$plot_date[2])%>%
        group_by(Pcode_Ad_2)%>%
        summarise(sum_HH =  sum(Effectifs_Estimatif_des_menages, na.rm = T), nb_alerts = n())%>%
        ungroup()%>%
        left_join(pcodes_adm2, by= "Pcode_Ad_2")%>%
        select(Admin1_Nam,Admin2_Nam, sum_HH, nb_alerts, Pcode_Ad_1,Pcode_Ad_2)%>%
        rename("Région d'accueil" = Admin1_Nam,
               "Cercle d'accueil" = Admin2_Nam,
               "Ménages affectés" = sum_HH,
               "Nombre Alertes" = nb_alerts,
               "PCode Région" = Pcode_Ad_1,
               "PCode Cercle" = Pcode_Ad_2
        )%>%
        datatable( options = list(dom = 'Bfrtip', autoWidth = TRUE, scrollY ="25vh", paging = FALSE,
                                  buttons = list(list(extend='copy', text ="Copier"), 
                                                 list(extend = 'csv', filename = paste0("RRM-Mali_",input$adm_level,"_",input$plot_date[1]," - ",input$plot_date[2])), 
                                                 list(extend = 'excel', filename = paste0("RRM-Mali_",input$adm_level,"_",input$plot_date[1]," - ",input$plot_date[2]))
                                  )
                                  ),
                   class = "compact", extensions=c("Buttons"), rownames= FALSE)
    }
    if(input$adm_level == "regions"){
      reactive_alerts$alerts <- Alerts() %>%
        filter(Date_evenement >= input$plot_date[1] & Date_evenement <= input$plot_date[2])%>%
        group_by(Pcode_Ad_1)%>%
        summarise(sum_HH =  sum(Effectifs_Estimatif_des_menages, na.rm = T), nb_alerts = n())%>%
        ungroup()%>%
        left_join(pcodes_adm1, by= "Pcode_Ad_1")%>%
        select(Admin1_Nam, sum_HH, nb_alerts, Pcode_Ad_1)%>%
        rename("Région d'accueil" = Admin1_Nam,
               "Ménages affectés" = sum_HH,
               "Nombre Alertes" = nb_alerts,
               "PCode Région" = Pcode_Ad_1,
        )%>%
        datatable(callback=JS('$(".buttons-csv").css("background","red"); 
                    $("a.buttons-print").css("background","green"); 
                    return table;'),
                  options = list(dom = 'Bfrtip', autoWidth = TRUE, scrollY ="25vh", paging = FALSE,
                                  buttons = list(list(extend = 'copy', text ="Copier"), 
                                                 list(extend = 'csv', filename = paste0("RRM-Mali_",input$adm_level,"_",input$plot_date[1]," - ",input$plot_date[2])), 
                                                 list(extend = 'excel', filename = paste0("RRM-Mali_",input$adm_level,"_",input$plot_date[1]," - ",input$plot_date[2]))
                                  )
        ),
        class = "compact", extensions=c("Buttons"), rownames= FALSE,
        )
    }
  })
  
  output$alertsTable <- renderDataTable(reactive_alerts$alerts, server = FALSE)
  
  user_created_map <- reactive({
    rrm_pal_adm2 <- colorBin(REACH_colors, domain = Alerts()$Effectifs_Estimatif_des_menages, bins = bins_adm2)
    
    rrm_pal_adm1 <- colorBin(REACH_colors, domain = Alerts()$Effectifs_Estimatif_des_menages, bins = bins_adm1)
    
    m <- basemap()
    
    title <- paste0("RRM-Mali - Carte par ",input$adm_level,": ",format(as.Date(input$plot_date[1], "%Y-%m-%d"),"%d %B %Y"), ""," - ",format(as.Date(input$plot_date[2],"%Y-%m-%d"), "%d %B %Y"))
    
    if(input$adm_level == "cercles"){
      m <- m%>%
        addCircles(data = reactive_polygons_adm2(), stroke=T, radius = reactive_polygons_adm1()$nb_alerts, color = "#FCE3E4")%>%
        addPolygons(data = reactive_polygons_adm2(),
                    stroke = T, color = "White", weight = 2,
                    label = mapply(function(x, y,z) {
                      HTML(sprintf("<div><b>%s </b> </div>
                                     <div>Ménages déplacés (alerte): %s</div>
                                     <div>Nombre d'alertes: %s </div>", htmlEscape(x),  htmlEscape(y),htmlEscape(z)
                      ))},
                      reactive_polygons_adm2()$admin2Name, reactive_polygons_adm2()$sum_HH,reactive_polygons_adm2()$nb_alerts,
                      SIMPLIFY = F),
                    labelOptions = labelOptions(noHide = F),
                    fillColor = ~rrm_pal_adm2(reactive_polygons_adm2()$sum_HH),
                    fillOpacity = 0.9
        )%>%
          addLegend("bottomright", pal = rrm_pal_adm2, values = Alerts()$Effectifs_Estimatif_des_menages,
                    title = "Nombre de ménages recensés lors des alertes")%>%
        addControl(title, position = "topleft")
    }
    if(input$adm_level == "regions"){
      m <- m%>%
        addPolygons(data = reactive_polygons_adm1(),
                    stroke = T, color = "White", weight = 2,
                    label = mapply(function(x, y,z) {
                      HTML(sprintf("<div><b>%s </b> </div>
                                   <div>Ménages déplacés (alerte): %s</div>
                                   <div>Nombre d'alertes: %s </div>", htmlEscape(x),  htmlEscape(y),htmlEscape(z)
                      ))},
                      reactive_polygons_adm1()$admin1Name, reactive_polygons_adm1()$sum_HH,reactive_polygons_adm1()$nb_alerts, SIMPLIFY = F),
                    labelOptions = labelOptions(noHide = F),
                    fillColor = ~rrm_pal_adm1(reactive_polygons_adm1()$sum_HH),
                    fillOpacity = 0.9
        )%>%
        addLegend("bottomright", pal = rrm_pal_adm1, values = Alerts()$Effectifs_Estimatif_des_menages,
                  title = "Nombre de ménages recensés lors des alertes")%>%
        addControl()
        
    }
    
    m
  })
  
  output$dl <- downloadHandler(
    
    filename <- function(){
      paste0("/RRM-Mali_",input$adm_level,"_",input$plot_date[1]," - ",input$plot_date[2], ".png")
      },
    
    content = function(file) {
      mapshot(user_created_map(), file=file)
    }
  )
  

}

shinyApp(ui, server)
