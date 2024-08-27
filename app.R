library(magrittr)
library(shiny)

#helps render the dropdown list
render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

#Define the user interface
ui <- fillPage(
  shinyjs::useShinyjs(),
  div(
    style = "height:100vh;position:relative;",
    leaflet::leafletOutput("map", height = "100%", width = "100%"),
    div(
      style = "position:absolute;top:0;right:0;width:min(80vw,600px);background-color:lightgray;padding:12px;",
      p(textOutput("instruction")),
      uiOutput("plot_input"),
      shinyjs::hidden(actionButton("save", "Save"))
    ),
    uiOutput("map_update")
  )
)

server <- function(input, output, session) {
  #sign into google sheets
  options(gargle_oauth_email = TRUE, gargle_oauth_cache = ".secrets")
  googlesheets4::gs4_auth(cache=".secrets", email="awiconferencedemoapp@gmail.com")

  #get the already entered data
  data_entry_sheet <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/17QnpQzieWcpm4q3XIs-qsQejL27E43iiCZEhbVZ9IiQ/edit?usp=sharing")

  #create a trigger to reload the map
  reload_map <- reactiveVal(1)

  #get entered data
  entered_parcels <- reactive({
    reload_map()
    data_entry_sheet %>%
      googlesheets4::read_sheet("Sheet1") %>%
      as.data.frame() %>%
      dplyr::mutate_all(as.character)
  })

  #join parcels to entered data
  parcels <- reactive({
    reload_map()
    readRDS("data/parcels.rds") %>%
      sf::st_sf() %>%
      dplyr::left_join(entered_parcels() %>%
                         as.data.frame(), by = c("P3_UID" = "P3_UID"))
  })

  #get apportionments
  apportionments <- readRDS("data/apports.rds")

  # Define the EPSG:27700 CRS
  crs27700 <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:27700",
    proj4def = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs",
    resolutions = c(38.10007620015241,
                    19.050038100076204,
                    9.525019050038102,
                    4.762509525019051,
                    2.3812547625095255,
                    1.1906273812547628,
                    0.5953136906273814,
                    0.24804737109474223,
                    0.14882842265684534), # Adjust based on the zoom levels available
    origin = c(-5220400.0, 4470200.0)
  )

  #map
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(
        options = leaflet::leafletOptions(crs = crs27700,
                                          zoomControl = FALSE)
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://maps2.bristol.gov.uk/server1/rest/services/base/1840s_tithe_wilts_128dpi/MapServer/tile/{z}/{y}/{x}"
      ) %>%
      leaflet::setView(lng = -1.9953126, lat = 51.351893, zoom = 1)
  })

  #respond to user clicking parcel on the map
  clicked_parcel_id <- reactiveVal(NULL)
  observeEvent(input$map_shape_click, {
    clicked_parcel_id(input$map_shape_click$id)

    #update map
    map <- leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("selected")

    if (!is.null(clicked_parcel_id())) {
      map %>%
        leaflet::addPolygons(
          data = parcels() %>%
            dplyr::filter(TITHE_ID == clicked_parcel_id()),
          weight = 1,
          group = "selected",
          color = "yellow",
          opacity = 0.9,
          fillOpacity = 0.1
        )
    }
  })

  #show/hide save button based on whether a parcel is clicked
  observeEvent(clicked_parcel_id(), {
    if (is.null(clicked_parcel_id())) {
      shinyjs::hide("save")
    } else {
      shinyjs::show("save")
    }
  }, ignoreNULL = F)

  #update the instruction text based on the clicked parcel
  output$instruction <- renderText({
    if (is.null(clicked_parcel_id())) {
      return("Click on a parcel to select plot codes.")
    } else {
      return(sprintf("You have selected parcel %s.", clicked_parcel_id()))
    }
  })

  #get the selected tithing (parish) to filter the apportionments
  selected_tithing <- reactive({
    if (is.null(clicked_parcel_id())) {
      return(NULL)
    }
    parcels() %>%
      dplyr::filter(TITHE_ID == clicked_parcel_id()) %>%
      dplyr::pull(tithing)
  })

  #plot dropdown
  output$plot_input <- renderUI({
    req(selected_tithing())

    tithing_apports <- apportionments %>%
      dplyr::filter(ref == selected_tithing())

    choices <- as.list(tithing_apports$ID)

    names(choices) <- lapply(1:nrow(tithing_apports), function(i) {
      tagList(
        tags$strong(tithing_apports$REF[i]),
        tags$div(tithing_apports$NAME[i]),
        tags$div(tithing_apports$LAND_USE[i])
      ) %>%
        paste0()
    }) %>%
      unname()

    selectizeInput(
      "plot_ref",
      "Choose all the plots that significantly overlap the selected parcel.",
      choices = choices,
      width = "400px",
      multiple = T,
      options = list(
        render = I(render),
        placeholder = "Select plot references...",
        maxItems = 10
      )
    )
  })

  #save the data
  observeEvent(input$save, {
    if (is.null(input$plot_ref)) {
      return()
    }
    entered_parcels() %>%
      dplyr::bind_rows(
        data.frame(
          P3_UID = parcels()$P3_UID[parcels()$TITHE_ID == clicked_parcel_id()],
          TITHE_REFS = paste(input$plot_ref, collapse = ", ")
        )
      ) %>%
      googlesheets4::write_sheet(data_entry_sheet, "Sheet1")
    reload_map(reload_map() + 1)
  })

  #update the map when the data changes
  output$map_update <- eventReactive(reload_map(), {
    map <- leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("done") %>%
      leaflet::clearGroup("not_done")

    not_done <- parcels() %>%
      dplyr::filter(is.na(TITHE_REFS))

    if (nrow(not_done) > 0) {
      map <- map %>%
        leaflet::addPolygons(
          data = not_done,
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          layerId = ~TITHE_ID,
          label = ~TITHE_ID,
          color = "#b364bd",
          fillColor = "#a2dba0",
          group = "done"
        )
    }

    done <- parcels() %>%
      dplyr::filter(!is.na(TITHE_REFS))

    if (nrow(done) > 0) {
      map <- map %>%
        leaflet::addPolygons(
          data = done,
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          layerId = ~TITHE_ID,
          label = ~TITHE_ID,
          color = "gray",
          group = "not_done"
        )
    }
    return("")
  })
}

shiny::shinyApp(ui, server)
