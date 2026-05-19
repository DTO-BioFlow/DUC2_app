##################################################################################
##################################################################################

# Author: Emily T. Griffiths, Aarhus University
# Email: emilytgriffiths@ecos.au.dk
# Date: 2026-02-19
# Script Description: Controls the porpoise module within the DUC2 data viewer shiny application

##################################################################################
##################################################################################

box::use(
  shiny[
    NS,
    sliderInput,
    plotOutput,
    tableOutput,
    imageOutput,
    moduleServer,
    reactiveValues,
    reactiveVal,
    observeEvent,
    reactive,
    showNotification,
    renderTable,
    renderPlot,
    renderImage,
    req
  ],
  bslib[
    page_fillable,
    layout_columns,
    card,
    card_header,
    navset_card_tab,
    nav_panel
  ],
  leaflet[
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    colorNumeric,
    addMapPane,
    addPolygons,
    addCircleMarkers,
    addLegend,
    hideGroup,
    showGroup,
    pathOptions
  ],
  leaflet.extras[
    addDrawToolbar,
    drawRectangleOptions,
    drawShapeOptions,
    editToolbarOptions,
    selectedPathOptions
  ],
  sf[st_crs, st_bbox, st_as_sfc, st_transform, st_within],
  dplyr[slice_head, select, filter, mutate],
  viridisLite[viridis],
  app / logic / porpoise / diurnal_plot[diurnalPlot],
  app / logic / porpoise / dph_dpd_plot[DPHpDPDplot]
)

box::use(
  app / logic / maps[make_base_map]
)

mod_porpoise_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    layout_columns(
      card(card_header("Map"), leafletOutput(ns("map"), height = "650px")),
      navset_card_tab(
        nav_panel(
          title = "Detection Positive Hours per Day",
          plotOutput(ns("dph_plot"), height = "600px")
        ),
        nav_panel(
          title = "Daily Distribution of Detections",
          plotOutput(ns("diurnal_plot"), height = "650px")
        )
        # ,
        # nav_panel(title = 'Pressure Curve?', p('TBD'))
      ),
      card(card_header("Station Details"), tableOutput(ns("station_table"))),
      card(
        card_header("Settings"),
        sliderInput(
          ns("YMselect"),
          "Dates:",
          min = as.Date("2015-01-01", "%Y-%m-%d"),
          max = as.Date(Sys.Date(), "%Y-%m-%d"),
          value = c(
            as.Date("2015-01-01", "%Y-%m-%d"),
            as.Date(Sys.Date(), "%Y-%m-%d")
          )
        ),
        imageOutput(ns("logo"))
      ), ## Update this if older data is incorporated
      col_widths = c(6, 6, 8, 4),
      # row_heights = c(3,1)
    )
  )
}

porpoise_overlay_groups <- c(
  "SCANS IV Porpoise Density",
  "POD Location",
  "draw"
)

mod_porpoise_server <- function(
  id,
  SCANS_shape,
  POD_loc_sf,
  PAM_data,
  PAM_grd,
  POD_locations,
  selected_layers = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    # colors
    pal <- colorNumeric(
      palette = viridis(256),
      domain = SCANS_shape$density,
      na.color = NA
    )

    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())

    # ---- BUILD HP MAP
    porp_map <- make_base_map(lng = 3, lat = 51.5, zoom = 8) |>
      addMapPane("dataPane", zIndex = 410) |>
      # Density polygons (SCANS IV)
      addPolygons(
        data = SCANS_shape,
        fillColor = ~ pal(density),
        fillOpacity = 0.6,
        color = NA,
        weight = 0,
        group = "SCANS IV Porpoise Density",
        options = pathOptions(pane = "dataPane")
      ) |>
      # POD locations
      addCircleMarkers(
        data = POD_loc_sf,
        # lng = ~st_coordinates(.)[,1],
        # lat = ~st_coordinates(.)[,2],
        radius = 4,
        fillColor = "red",
        color = "red",
        fillOpacity = 1,
        stroke = FALSE,
        group = "POD Location",
        layerId = ~Station, # REQUIRED for selection logic
        options = pathOptions(pane = "dataPane")
      ) |>
      # Legend
      addLegend(
        position = "bottomright",
        pal = pal,
        values = SCANS_shape$density,
        title = "SCANS IV Porpoise Density",
        opacity = 1
      ) |>
      # boxing tool
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions(
          repeatMode = FALSE,
          shapeOptions = drawShapeOptions(
            color = "#2C7FB8",
            weight = 2,
            opacity = 0.9,
            fillOpacity = 0.15
          )
        ),
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      )

    # ---- Render the map ----
    output$map <- renderLeaflet({
      porp_map
    })

    observeEvent(selected_layers(), {
      proxy <- leafletProxy("map", session = session)
      selected <- selected_layers()
      matched <- intersect(selected, porpoise_overlay_groups)

      if (length(matched)) {
        proxy %>% hideGroup(porpoise_overlay_groups)
        proxy %>% showGroup(matched)
      }
    }, ignoreNULL = FALSE)

    # ---- Spatial edit/selection handling ----  for old map
    # edits <- callModule(editMod, "editor", porp_map)

    # ReactiveVal to hold the *current* rectangle bbox as an sfc
    selected_bbox_rv <- reactiveVal(NULL)

    rect_feature_to_bbox <- function(feature) {
      if (is.null(feature) || feature$geometry$type != "Polygon") {
        return(NULL)
      }
      coords <- feature$geometry$coordinates[[1]]
      lon <- vapply(coords, function(pt) pt[[1]], numeric(1))
      lat <- vapply(coords, function(pt) pt[[2]], numeric(1))
      # Build bbox sfc in EPSG:4326
      bb <- st_bbox(
        c(xmin = min(lon), ymin = min(lat), xmax = max(lon), ymax = max(lat)),
        crs = st_crs(4326)
      )
      st_as_sfc(bb)
    }

    observeEvent(input$map_draw_new_feature, {
      bb <- rect_feature_to_bbox(input$map_draw_new_feature)
      selected_bbox_rv(bb)
    })

    # If rectangle is edited: use the first edited rectangle (or expand to multi)
    observeEvent(input$map_draw_edited_features, {
      edited <- input$map_draw_edited_features
      if (
        is.null(edited) ||
          is.null(edited$features) ||
          length(edited$features) == 0
      ) {
        return()
      }
      # Take first polygon
      bb <- rect_feature_to_bbox(edited$features[[1]])
      selected_bbox_rv(bb)
    })

    # On delete: clear bbox
    observeEvent(input$map_draw_deleted_features, {
      selected_bbox_rv(NULL)
    })

    # Reactive expression for selected points  -- Old code

    # selected_points <- reactive({
    #   req(selected_points())
    #   selected_points()
    # })

    # Ensure POD points are in EPSG:4326 (safe even if already)
    pod_pts_4326 <- reactive({
      if (
        is.null(st_crs(POD_loc_sf)) ||
          is.na(st_crs(POD_loc_sf)$epsg) ||
          st_crs(POD_loc_sf)$epsg != 4326
      ) {
        st_transform(POD_loc_sf, 4326)
      } else {
        POD_loc_sf
      }
    })

    # Selected points (returns sf with 0 rows if no bbox)
    selected_points <- reactive({
      pts <- pod_pts_4326()
      bb <- selected_bbox_rv()

      if (is.null(bb)) {
        return(pts[0, ]) # nothing selected yet
      }

      # Use within against bbox (axis-aligned)
      idx <- st_within(pts, bb, sparse = FALSE)[, 1]
      pts[idx, ]
    })

    # Reactive expression for filtered data

    selected_data <- reactive({
      sp <- selected_points()

      if (nrow(sp) == 0) {
        return(
          slice_head(PAM_data, n = 0) |>
            select(Station, datetime, PPM)
        )
      }

      # req(selected_points())
      selected_groups <- unique(sp$Station)
      data <- PAM_data |>
        filter(Station %in% selected_groups) |>
        select(Station, datetime, PPM)

      if (length(unique(data$Station)) < 5) {
        showNotification(
          "Fewer than 5 stations selected. Data is not representative.",
          type = "warning"
        )
      }

      data
    })

    filtered_data <- reactive({
      req(selected_data())
      data <- selected_data() |>
        filter(datetime >= input$YMselect[1], datetime <= input$YMselect[2])

      if (length(unique(data$Station)) < 5) {
        showNotification(
          "Fewer than 5 stations selected. Data is not representative.",
          type = "warning"
        )
      }

      data
    })

    selectLocs <- reactive({
      req(filtered_data())
      selected_groups <- unique(filtered_data()$Station)
      POD_locations[POD_locations$Station %in% selected_groups, ]
    })

    # Table output for station details
    output$station_table <- renderTable({
      req(selectLocs())

      selectLocs() |>
        select(Station, long, lat) |>
        mutate(
          long = format(round(long, 4), nsmall = 4),
          lat = format(round(lat, 4), nsmall = 4)
        )
    })

    # Example output: print selected data structure
    output$dph_plot <- renderPlot({
      data <- filtered_data()
      req(nrow(data) > 0)
      # print(str(selected_data()))
      DPHpDPDplot(data)
    })

    output$diurnal_plot <- renderPlot({
      data <- filtered_data()
      locations <- selectLocs()
      req(nrow(data) > 0)
      req(any(data$PPM == 1, na.rm = TRUE))
      req(nrow(locations) > 0)
      # print(str(selected_data()))
      diurnalPlot(data, locations)
    })

    output$logo <- renderImage(
      {
        list(src = "assets/AU logo.png", height = "5%")
      },
      deleteFile = FALSE
    )
  })
}
