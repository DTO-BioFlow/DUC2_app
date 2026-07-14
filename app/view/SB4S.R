# asc 14 Jul 2026: fix issue in mod_SB4S_server that rast assumes corner-values, not cell-centered

box::use(
  shiny[
    NS,
    moduleServer,
    observe
  ],
  bslib[layout_sidebar, sidebar],
  leaflet[
    leafletOutput,
    renderLeaflet,
    setView,
    addRasterImage,
    addLegend,
    leafletProxy,
    clearImages,
    clearControls
  ]
)

# ui ----------------------------------------------------------------------

mod_SB4S_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = "open",
      title = "SB4S simulation",
      mod_SB4S_sidebar_ui(ns("controls"))
    ),
    leafletOutput(ns("habitat_map"), height = 700)
  )
}

box::use(
  app / logic / maps[make_base_map],
  app / view / SB4S_sidebar[
    mod_SB4S_sidebar_ui,
    mod_SB4S_sidebar_server
  ],
  terra[rast, setValues],
  app / logic / SB4S / SB4S_api[flip_yaxis]
)
# server  -----------------------------------------------------------------

mod_SB4S_server <- function(
  id,
  biodata,
  habitat_data = NULL,
  sidebar_layers = NULL
) {
  
  moduleServer(id, function(input, output, session) {
    layer_selection <- mod_SB4S_sidebar_server(
      "controls"
    )

    # Initialize base map
    output$habitat_map <- renderLeaflet({
      make_base_map() |>
        setView(lat = 51.5, lng = 2.5, zoom = 8)
    })

    observe({
      map_hidden <- session$clientData[[
        paste0("output_", session$ns("habitat_map"), "_hidden")
      ]]
      if (!isFALSE(map_hidden)) {
        return(invisible(NULL))
      }

      proxy <- leafletProxy("habitat_map", session = session) |>
        clearImages() |>
        clearControls()

      desire <- layer_selection()
      #print(desire)


      if (desire$show_layer == FALSE) {
        return(invisible(NULL))
      }

      vnam = toString(desire$var)
      if (vnam == "sum") {vnam <- "N"}
      if (desire$time_index==0) {
          vnam <- paste0(vnam, "tavg")
	  xydata <- flip_yaxis(t(biodata[vnam][[1]]))  # NB transpose matrix
      } else {
          xytdata <- biodata[vnam][[1]]
          xydata  <- flip_yaxis(t(xytdata[,,desire$time_index]))  # NB transpose matrix
      }
     
      # --- fix issue that rast assumes corner-values, not cell-centered
      dx <- biodata$lon[2]-biodata$lon[1]
      dy <- biodata$lat[2]-biodata$lat[1]
      nx <- dim(biodata$lon)[1]
      ny <- dim(biodata$lat)[1]
      xmin <- min(biodata$lon)
      xmax <- max(biodata$lon)
      ymin <- min(biodata$lat)
      ymax <- max(biodata$lat)
      layer <- rast(nrows=ny, ncols=nx,
                    xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
		    vals=xydata)           
      #print(layer)
      
      proxy |>
        addRasterImage(
          layer,
          opacity = 0.75,
          layerId = "SB4S_raster"
        ) 
    })
  })
}
