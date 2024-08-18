library(magrittr)
library(shiny)

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Tithe Data Entry"
  ),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Welcome",
                      tabName = "welcome"),
      shinydashboard::menuItem("Data Entry",
                      tabName = "data_entry"),
      shinydashboard::menuItem("Hints",
                      tabName = "hints"),
      shinydashboard::menuItem("About",
                      tabName = "about")
    )
  ),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinydashboard::tabItems(

      shinydashboard::tabItem(
        tabName = "welcome",
        shinydashboard::box(
          title = "Welcome to the data entry application.",
          p("This R Shiny web app shows how it is possible build a data entry application to gather data from historical maps."),
          h4("The Task"),
          p("The ancient woodland inventory maps woodland and wood pasture that has been in continuous existance since 1600 AD.
                 By taking part in this task you are helping to identify ancient woodlands that are missing from the current inventory.
                 Once included, greater protection will be afforded to these sites of irreplaceable ecological and historical value."),
          p("An important source of evidence for ancient status is the Tithe Maps, which were produced to calculate tax payments following the Tithe Commutation Act 1836.
                 Previously, tithes were a form of tax paid using the physical produce of the land. However, as part of economic industrialisation, this was changed to monetary payments.
                 To calculate these payments, maps were commissioned for each parish or tithing (subidivision of a parish).
                 These maps divided the land into plots, each with their own reference number referring to an entry in an associated document. This document, called the Apportionment,
                 contained information about specific plots required to calculate the new tax payments (including land-owner, area and land use).
                   The record of land use (coppice, woodland, arable, pasture etc.) is what we are most interested in. This is important evidence in establishing a woodland's ancient status."),
          p("Your task is to enter references for plots that correspond to a series of candidate woodland parcels.
                   These will be used to look up the land use for those woodland parcels, helping to determine which ones were wooded in the 1840's and therefore may be ancient."),
        )
      ),

      shinydashboard::tabItem(
        tabName = "data_entry",
        div(
          style = "height:calc(100vh - 80px);position:relative;",
                     leaflet::leafletOutput("map", height = "100%", width = "100%"),
          shinydashboard::box(
            title = "Enter plot references",
            width = 4,
            p(textOutput("instruction")),
            uiOutput("plot_input"),
            shinyjs::hidden(
              actionButton("save", "Save")
            ),
            tableOutput(
              "selected_plots"
            )
          ) %>%
            tagAppendAttributes(
              style = "position:absolute;top:0;right:0"
            )
        )
      ),

      shinydashboard::tabItem(
        tabName = "hints",
        shinydashboard::box(
          h4("Which plots do I choose?"),
          p("To determine which tithe plots correspond to a woodland parcel, you may need to see past some of the innaccuracies in the Tithe maps.
                 Example A shows how a parcel may have an offset from its corresponding plots (see the red arrows)"),
          img(src = "ExampleA.png", height = "100%", width = "100%"),
          p("In general, try to look for plots that match the shape of the parcel, even if they are slightly shifted away from the parcel on the map.
                   In most cases, if the plot is fully covered by the parcel, it should be included. plots that only partially overlap a parcel must cover at least a sixth of the parcel's area to be worth including."),
        ),
        shinydashboard::box(
          h4("Finding the plot reference when it is not shown on the map"),
          p("If the plot was much larger than the parcel, the plot reference might have been cropped out of the map.
                     To access an interactive version of the Tithe Maps, follow the instructions that appear when you select 'No, at least one plot reference unreadable or missing from the map' at step 4."),
        ),
        shinydashboard::box(
          h4("What to do if a plot has no reference"),
          p("Some plots may be missing a reference because they are included as part of an adjacent plot, with a Brace mark used to join them together, such as in Example B"),
          img(src = "ExampleB.png", height = "100%", width = "100%"),
        ),
        shinydashboard::box(
          h4("What count as tree symbols?"),
          p("A variety of markings are used to depict woody vegetation on the Tithe maps, ranging from small drawings of deciduous or coniferous trees to basic squiggles.
                 If there are any symbols inside the woodland parcel that could be trees, enter 'Some tree symbols' (see Example C).
                   If the tree symbols pretty much cover the parcel entirely (even if spaced far apart),
                   change this to 'Mostly or fully covered by tree symbols' (see Example D)."),
          img(src = "ExampleC.png", height = "100%", width = "100%"),
          img(src = "ExampleD.png", height = "100%", width = "100%"),
        )
      ),

      shinydashboard::tabItem(
        tabName = "about",
        shinydashboard::box(
          title = "About this app",
          h4("WSBRC"),
          p("WSBRC is the county's Local Environmental Records Centre, hosted by Wiltshire Wildlife Trust at their head office in Devizes."),
          a(href = "https://wsbrc-org-uk.stackstaging.com/", "Link to our website.", target="_blank"),
          br(),
          br(),
          h4("The Ancient Woodland Inventory"),
          p("The AWI is a dataset of woodlands considered to have been more or less continuously covered by trees since 1600 AD.
                   It was originally produced by hand in 1980s.
                   Since its publication, England’s AWI has helped protect those woodlands that it designates from damage or destruction.
                   However, the dataset is known to have significant errors and a large number of woodlands were excluded to save time.
                   Now Natural England and The Woodland Trust are funding Local Environmental Records Centres, such as WSBRC, to update the AWI in their areas.
                   The update makes use of modern GIS technology, as well as the wealth of historical documents available online, such as the Tithe maps and their Apportionments."),
          br(),
          h4("The Tithe Maps"),
          p("The Tithe maps are an incredibly detailed source of information about the landscape in the 1840s.
                   They were produced following the Tithe Commutation Act of 1836. This Act reformed an ancient system of taxation
                   where a tenth of the physical produce of the land was paid to the church or landowner, replacing it with a monetary tax based on land holding.
                   To calculate the payments, maps of each eligible parish were drawn up, dividing the land into plots with reference numbers.
                   These reference numbers referred to an entry in an associated document called the Tithe Apportionment.
                   The Apportionment gives the plot’s area, the names of the owner and occupier, and, most importantly for us,
                   a description of the land use – for example whether it was woodland, arable or pasture. This information helps us to determine
                   whether woodlands and wood pasture exisisted in the 1840s and therefore may be ancient.")
        ),

        shinydashboard::box(
          title = "Acknowledgements",
          "WSBRC's work to update the Ancient Woodland Inventory is funded by Natural England and The Woodland Trust.
                   Digitised tithes maps are provided by ",
          a(href = "https://wshc.org.uk", "Wiltshire and Swindon History Centre (WSHC)", target="_blank"),
          ". Apportionments were transcribed by ",
          a(href = "https://www.wiltshirefhs.co.uk", "Wiltshire Family History Society", target="_blank"), ".", br(), br(),
          "The project also benefits hugely from volunteers such as yourself. Your efforts allow us to gather more evidence and make the new Inventory as
                   accurate and authoritative as possible, so our ancient woodlands can be better conserved for the future."
        )
      )
    )


  )
)

server <- function(input, output, session) {
  #sign into google sheets
  options(gargle_oauth_email = TRUE, gargle_oauth_cache = ".secrets")
  googlesheets4::gs4_auth(cache=".secrets", email="awiconferencedemoapp@gmail.com")

  #get the already entered data from google sheets
  data_entry_sheet <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/17QnpQzieWcpm4q3XIs-qsQejL27E43iiCZEhbVZ9IiQ/edit?usp=sharing")
  entered_parcels <- data_entry_sheet %>%
    googlesheets4::read_sheet("Sheet1")

  reload_map <- reactiveVal(1)
  #get the parcels
  parcels <- reactive({
    reload_map()
    readRDS("data/parcels.rds") %>%
      dplyr::left_join(entered_parcels, by = c("TITHE_ID" = "TITHE_ID"))
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
        options = leaflet::leafletOptions(crs = crs27700)
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://maps2.bristol.gov.uk/server1/rest/services/base/1840s_tithe_wilts_128dpi/MapServer/tile/{z}/{y}/{x}"
      ) %>%
      leaflet::setView(lng = -1.9953126, lat = 51.351893, zoom = 1) %>%
      leaflet::addPolygons(
        data = parcels,
         weight = 1,
         fillOpacity = 0.5,
         layerId = ~TITHE_ID,
        label = ~TITHE_ID,
        color = "#b364bd",
        fillColor = "#a2dba0"
      )
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
          data = parcels %>%
            dplyr::filter(TITHE_ID == clicked_parcel_id()),
          weight = 1,
          group = "selected",
          color = "yellow",
          opacity = 0.9,
          fillOpacity = 0.1
        )
    }
  })

  #update the form based on the clicked parcel
  observeEvent(clicked_parcel_id(), {
    if (is.null(clicked_parcel_id())) {
      shinyjs::hide("plot_ref")
      shinyjs::hide("add_plot")
      return()
    }
    shinyjs::show("plot_ref")
    shinyjs::show("add_plot")
  }, ignoreNULL = F)

  output$instruction <- renderText({
    if (is.null(clicked_parcel_id())) {
      return("Click on a parcel to begin data entry.")
    } else {
      return(sprintf("You have selected parcel %s.", clicked_parcel_id()))
    }
  })

  selected_tithing <- reactive({
    if (is.null(clicked_parcel_id())) {
      return(NULL)
    }
    parcels %>%
      dplyr::filter(TITHE_ID == clicked_parcel_id()) %>%
      dplyr::pull(tithing)
  })

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


}

shiny::shinyApp(ui, server)
