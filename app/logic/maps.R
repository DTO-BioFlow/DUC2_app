##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-02-03
# Script Name: ~/DUC2_viewer_acoustic_telemetry/R/map_environmental.R
# Script Description: make a base leaflet map, and a map with several environmental layers as overlaygroups

##################################################################################
##################################################################################

box::use(
  leaflet[
    leaflet,
    leafletProxy,
    setView,
    addMapPane,
    addTiles,
    tileOptions,
    addProviderTiles,
    addScaleBar,
    scaleBarOptions,
    addControl,
    addLayersControl,
    layersControlOptions,
    addWMSTiles,
    WMSTileOptions,
    clearGroup,
    hideGroup,
    removeControl
  ],
  leafem[addMouseCoordinates],
  leaflet.extras[addFullscreenControl],
  htmltools[HTML],
  htmlwidgets[onRender]
)

# 1. base layer map -------------------------------------------------------

make_base_map <- function(
  lng = 3,
  lat = 51.5,
  zoom = 8,
  arrow_src = "north_arrow.png"
) {
  north_arrow <-
    "<img src='https://www.clipartbest.com/cliparts/yTo/Lgr/yToLgryGc.png' style='width:35px;height:45px;'>"

  # ## TODO: change to file in .www/, not working for the moment
  # north_arrow <- sprintf(
  #   "<img src='%s' style='width:35px;height:45px;'>",
  #   arrow_src
  # )

  leaflet() |>
    setView(lng, lat, zoom = zoom) |>
    addMapPane("basePane", zIndex = 100) |>
    addTiles(
      group = "Open Street Map",
      options = tileOptions(pane = "basePane")
    ) |>
    addTiles(
      urlTemplate = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png",
      group = "EMODnet Bathymetry",
      options = tileOptions(pane = "basePane")
    ) |>
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron",
      options = tileOptions(pane = "basePane")
    ) |>
    addMouseCoordinates() |>
    addFullscreenControl() |>
    addScaleBar(
      position = "bottomleft",
      options = scaleBarOptions(maxWidth = 150, imperial = FALSE)
    ) |>
    addControl(
      html = north_arrow,
      position = "topleft",
      className = "fieldset {border: 0;}"
    ) |>
    addLayersControl(
      baseGroups = c(
        "CartoDB.Positron",
        "Open Street Map",
        "EMODnet Bathymetry"
      ),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomleft"
    )
}

# 2. environmental map ----------------------------------------------------

# --- helper: html for legend box ---
legend_control <- function(id, title, img_url, visible = FALSE) {
  paste0(
    '<details id="',
    id,
    '" style="background:white;padding:0px;border-radius:0px;display:',
    if (visible) "block" else "none",
    ';">',
    '<summary style="cursor:pointer;font-weight:600;">',
    title,
    "</summary>",
    '<img src="',
    img_url,
    '" />',
    "</details>"
  )
}

env_wms_legend_map <- function() {
  list(
    "Offshore Wind Farms (OWF)" = "legend-owf",
    "Submarine Power Cables (SPC)" = "legend-spc",
    "Marine Spatial Plans" = "legend-msp",
    "Sea convention polygons" = "legend-sea_conventions",
    "Shipwrecks" = "legend-shipwrecks",
    "Bathymetry (multicolor)" = "legend-bathy",
    "Seabed habitats" = "legend-seabedhabitats",
    "Seabed substrates" = "legend-seabedsubstrates"
  )
}

make_env_base_map <- function(base_map, wms_layers) {
  eez_legend <- "
  <div class='leaflet-control eez-legend'
       style='background:white;padding:0px 0px;border-radius:0px;'>
    <div style='display:flex;align-items:center;gap:0px;'>
      <span style='display:inline-block;width:34px;height:0px;
                   border-top:1px solid #000000;'></span>
      <span style='font-weight:600;'>Exclusive Economic Zone (EEZ) boundaries</span>
    </div>
  </div>
  "

  base_map() |>
    addMapPane("rasterPane", zIndex = 200) |>
    addMapPane("vectorPane", zIndex = 300) |>
    addMapPane("boundaryPane", zIndex = 400) |>
    addMapPane("markerPane", zIndex = 500) |>
    addWMSTiles(
      baseUrl = wms_layers$eez$wms_base,
      layers = wms_layers$eez$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        styles = "line_black",
        pane = "boundaryPane"
      ),
      group = "Exclusive Economic Zones (EEZ)"
    ) |>
    addControl(
      html = HTML(eez_legend),
      position = "topright",
      layerId = "legend-eez"
    )
}

env_wms_layer_specs <- function(wms_layers) {
  list(
    "Offshore Wind Farms (OWF)" = list(
      tiles = list(list(
        base_url = wms_layers$owf$wms_base,
        layers = wms_layers$owf$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          opacity = 1,
          pane = "boundaryPane"
        )
      )),
      legend = list(
        id = "legend-owf",
        title = "OWF status",
        img_url = wms_layers$owf$legend_link
      )
    ),
    "Submarine Power Cables (SPC)" = list(
      tiles = list(list(
        base_url = wms_layers$spc$wms_base,
        layers = wms_layers$spc$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          opacity = 1,
          pane = "boundaryPane"
        )
      )),
      legend = list(
        id = "legend-spc",
        title = "Cable owner",
        img_url = wms_layers$spc$legend_link
      )
    ),
    "Marine Spatial Plans" = list(
      tiles = list(list(
        base_url = wms_layers$msp$wms_base,
        layers = wms_layers$msp$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          pane = "vectorPane",
          opacity = 0.75
        )
      )),
      legend = list(
        id = "legend-msp",
        title = "Human Activities",
        img_url = wms_layers$msp$legend_link
      )
    ),
    "Sea convention polygons" = list(
      tiles = list(list(
        base_url = wms_layers$sea_conventions$wms_base,
        layers = wms_layers$sea_conventions$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          opacity = 1,
          pane = "vectorPane"
        )
      )),
      legend = list(
        id = "legend-sea_conventions",
        title = "Convention framework",
        img_url = wms_layers$sea_conventions$legend_link
      )
    ),
    "Seabed habitats" = list(
      tiles = list(list(
        base_url = wms_layers$seabedhabitats$wms_base,
        layers = wms_layers$seabedhabitats$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          pane = "rasterPane"
        )
      )),
      legend = list(
        id = "legend-seabedhabitats",
        title = "Habitat type",
        img_url = wms_layers$seabedhabitats$legend_link
      )
    ),
    "Seabed substrates" = list(
      tiles = list(list(
        base_url = wms_layers$seabedsubstrates$wms_base,
        layers = wms_layers$seabedsubstrates$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          pane = "rasterPane",
          opacity = 0.75
        )
      )),
      legend = list(
        id = "legend-seabedsubstrates",
        title = "Substrate type",
        img_url = wms_layers$seabedsubstrates$legend_link
      )
    ),
    "Bathymetry (multicolor)" = list(
      tiles = list(list(
        base_url = wms_layers$bathy_multicolor$wms_base,
        layers = wms_layers$bathy_multicolor$wms_layer_name,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          opacity = 0.5,
          pane = "rasterPane"
        )
      )),
      legend = list(
        id = "legend-bathy",
        title = "Depth",
        img_url = wms_layers$bathy_multicolor$legend_link
      )
    ),
    "Shipwrecks" = list(
      tiles = list(
        list(
          base_url = wms_layers$shipwrecks$wms_base,
          layers = wms_layers$shipwrecks$wms_layer_name,
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            pane = "markerPane"
          )
        ),
        list(
          base_url = wms_layers$shipwrecks_emodnet$wms_base,
          layers = wms_layers$shipwrecks_emodnet$wms_layer_name,
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            opacity = 0.5,
            pane = "markerPane"
          )
        )
      ),
      legend = list(
        id = "legend-shipwrecks",
        title = "Shipwrecks",
        img_url = wms_layers$shipwrecks_emodnet$legend_link
      )
    )
  )
}

add_env_wms_group <- function(map, group_name, group_spec) {
  for (tile_spec in group_spec$tiles) {
    map <- map |>
      addWMSTiles(
        baseUrl = tile_spec$base_url,
        layers = tile_spec$layers,
        options = tile_spec$options,
        group = group_name
      )
  }

  if (!is.null(group_spec$legend)) {
    map <- map |>
      addControl(
        html = HTML(legend_control(
          group_spec$legend$id,
          group_spec$legend$title,
          group_spec$legend$img_url,
          visible = TRUE
        )),
        position = "topright",
        layerId = group_spec$legend$id
      )
  }

  map
}

remove_env_wms_group <- function(map, group_name, group_spec) {
  map <- map |>
    clearGroup(group_name)

  if (!is.null(group_spec$legend)) {
    map <- map |>
      removeControl(group_spec$legend$id)
  }

  map
}

env_wms_overlay_sections <- function() {
  list(
    "Human Activities layers" = c(
      "Marine Spatial Plans",
      "Offshore Wind Farms (OWF)",
      "Submarine Power Cables (SPC)",
      "Sea convention polygons",
      "Shipwrecks"
    ),
    "Natural layers" = c(
      "Bathymetry (multicolor)",
      "Seabed substrates",
      "Seabed habitats"
    )
  )
}

flatten_env_wms_layers <- function(selected_layers) {
  if (is.null(selected_layers)) {
    return(character(0))
  }

  known_groups <- names(env_wms_legend_map())

  if (is.character(selected_layers)) {
    return(intersect(selected_layers, known_groups))
  }

  selected_groups <- unlist(
    selected_layers[c("human_activities", "natural")],
    use.names = FALSE
  )

  unique(intersect(selected_groups, known_groups))
}

update_env_wms_map <- function(
  map_id,
  session,
  wms_layers,
  selected_layers,
  cached_layers = character(0)
) {
  layer_specs <- env_wms_layer_specs(wms_layers)
  current_layers <- flatten_env_wms_layers(selected_layers)
  cached_layers <- unique(cached_layers)

  groups_to_remove <- setdiff(cached_layers, current_layers)
  groups_to_add <- setdiff(current_layers, cached_layers)

  proxy <- leafletProxy(mapId = map_id, session = session)

  for (group_name in groups_to_remove) {
    proxy <- remove_env_wms_group(
      map = proxy,
      group_name = group_name,
      group_spec = layer_specs[[group_name]]
    )
  }

  for (group_name in groups_to_add) {
    proxy <- add_env_wms_group(
      map = proxy,
      group_name = group_name,
      group_spec = layer_specs[[group_name]]
    )
  }

  invisible(current_layers)
}

# --- main factory: returns the environmental WMS leaflet map ---
make_env_wms_map <- function(
  base_map,
  wms_layers,
  hide_groups = c(
    "Bathymetry (multicolor)",
    "Seabed substrates",
    "Seabed habitats",
    "Sea convention polygons",
    "Marine Spatial Plans",
    "Submarine Power Cables (SPC)",
    "Shipwrecks"
  )
) {
  # TODO: make EEZ able to be toggled on/off
  # EEZ legend
  eez_legend <- "
  <div class='leaflet-control eez-legend'
       style='background:white;padding:0px 0px;border-radius:0px;'>
    <div style='display:flex;align-items:center;gap:0px;'>
      <span style='display:inline-block;width:34px;height:0px;
                   border-top:1px solid #000000;'></span>
      <span style='font-weight:600;'>Exclusive Economic Zone (EEZ) boundaries</span>
    </div>
  </div>
  "

  legend_map <- env_wms_legend_map()
  overlay_sections <- env_wms_overlay_sections()

  payload <- list(
    legends = legend_map,
    sections = overlay_sections
  )

  map_wms <- base_map() |>
    # panes
    addMapPane("rasterPane", zIndex = 200) |>
    addMapPane("vectorPane", zIndex = 300) |>
    addMapPane("boundaryPane", zIndex = 400) |>
    addMapPane("markerPane", zIndex = 500) |>
    # OWF
    addWMSTiles(
      baseUrl = wms_layers$owf$wms_base,
      layers = wms_layers$owf$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity = 1,
        pane = "boundaryPane"
      ),
      group = "Offshore Wind Farms (OWF)"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-owf",
        "OWF status",
        wms_layers$owf$legend_link
      )),
      position = "topright"
    ) |>
    # SPC
    addWMSTiles(
      baseUrl = wms_layers$spc$wms_base,
      layers = wms_layers$spc$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity = 1,
        pane = "boundaryPane"
      ),
      group = "Submarine Power Cables (SPC)"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-spc",
        "Cable owner",
        wms_layers$spc$legend_link
      )),
      position = "topright"
    ) |>
    # MSP
    addWMSTiles(
      baseUrl = wms_layers$msp$wms_base,
      layers = wms_layers$msp$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        pane = "vectorPane",
        opacity = 0.75
      ),
      group = "Marine Spatial Plans"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-msp",
        "Human Activities",
        wms_layers$msp$legend_link
      )),
      position = "topright"
    ) |>
    # Sea conventions
    addWMSTiles(
      baseUrl = wms_layers$sea_conventions$wms_base,
      layers = wms_layers$sea_conventions$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity = 1,
        pane = "vectorPane"
      ),
      group = "Sea convention polygons"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-sea_conventions",
        "Convention framework",
        wms_layers$sea_conventions$legend_link
      )),
      position = "topright"
    ) |>
    # Seabed habitats
    addWMSTiles(
      baseUrl = wms_layers$seabedhabitats$wms_base,
      layers = wms_layers$seabedhabitats$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        pane = "rasterPane"
      ),
      group = "Seabed habitats"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-seabedhabitats",
        "Habitat type",
        wms_layers$seabedhabitats$legend_link
      )),
      position = "topright"
    ) |>
    # Seabed substrates
    addWMSTiles(
      baseUrl = wms_layers$seabedsubstrates$wms_base,
      layers = wms_layers$seabedsubstrates$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        pane = "rasterPane",
        opacity = 0.75
      ),
      group = "Seabed substrates"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-seabedsubstrates",
        "Substrate type",
        wms_layers$seabedsubstrates$legend_link
      )),
      position = "topright"
    ) |>
    # Bathymetry
    addWMSTiles(
      baseUrl = wms_layers$bathy_multicolor$wms_base,
      layers = wms_layers$bathy_multicolor$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity = 0.5,
        pane = "rasterPane"
      ),
      group = "Bathymetry (multicolor)"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-bathy",
        "Depth",
        wms_layers$bathy_multicolor$legend_link
      )),
      position = "topright"
    ) |>
    # EEZ (VLIZ geoserver)
    addWMSTiles(
      baseUrl = wms_layers$eez$wms_base,
      layers = wms_layers$eez$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        styles = "line_black",
        pane = "boundaryPane"
      ),
      group = "Exclusive Economic Zones (EEZ)"
    ) |>
    # Shipwrecks
    addWMSTiles(
      baseUrl = wms_layers$shipwrecks$wms_base,
      layers = wms_layers$shipwrecks$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        pane = "markerPane"
      ),
      group = "Shipwrecks"
    ) |>
    # Shipwrecks EMODnet + legend
    addWMSTiles(
      baseUrl = wms_layers$shipwrecks_emodnet$wms_base,
      layers = wms_layers$shipwrecks_emodnet$wms_layer_name,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity = 0.5,
        pane = "markerPane"
      ),
      group = "Shipwrecks"
    ) |>
    addControl(
      html = HTML(legend_control(
        "legend-shipwrecks",
        "",
        wms_layers$shipwrecks_emodnet$legend_link
      )),
      position = "topright"
    ) |>
    hideGroup(hide_groups)

  map_wms |>
    # EEZ permanent legend
    addControl(html = HTML(eez_legend), position = "topright") |>
    # Layer control
    addLayersControl(
      baseGroups = c(
        "CartoDB.Positron",
        "Open Street Map",
        "EMODnet Bathymetry"
      ),
      overlayGroups = names(legend_map),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomleft"
    ) |>
    onRender(
      htmlwidgets::JS(
        "function(el, x, payload){",
        "var map = this;",
        "var legends  = (payload && payload.legends)  ? payload.legends  : {};",
        "var sections = (payload && payload.sections) ? payload.sections : {};",
        "var activeMarker = null;",
        "",
        "function norm(s){ return (s || '').replace(/\\s+/g,' ').trim(); }",
        "",
        "function showByLayer(layerName, visible){",
        "  var id = legends[layerName];",
        "  if(!id) return;",
        "  var node = document.getElementById(id);",
        "  if(!node) return;",
        "  node.style.display = visible ? 'block' : 'none';",
        "}",
        "",
        "function syncFromLayerControl(){",
        "  var inputs = el.querySelectorAll('.leaflet-control-layers-overlays input[type=checkbox]');",
        "  inputs.forEach(function(inp){",
        "    var label = inp.parentElement;",
        "    var name  = label ? norm(label.textContent) : null;",
        "    if(name && legends[name] !== undefined){",
        "      showByLayer(name, inp.checked);",
        "    }",
        "  });",
        "}",
        "",
        "function activeOverlayNames(){",
        "  var names = [];",
        "  var inputs = el.querySelectorAll('.leaflet-control-layers-overlays input[type=checkbox]');",
        "  inputs.forEach(function(inp){",
        "    if(inp.checked){",
        "      var label = inp.parentElement;",
        "      var name  = label ? norm(label.textContent) : null;",
        "      if(name) names.push(name);",
        "    }",
        "  });",
        "  return names;",
        "}",
        "",
        "function makeMarkerPopup(latlng){",
        "  var activeLayers = activeOverlayNames();",
        "  var html = '<div style=\"font-size:14px;line-height:1.4;\">' +",
        "    '<strong>Clicked location</strong><br/>' +",
        "    latlng.lat.toFixed(5) + ', ' + latlng.lng.toFixed(5) +",
        "    '</div><div style=\"margin-top:8px;\">' +",
        "    '<strong>Active overlay layers</strong><br/>' +",
        "    (activeLayers.length ? activeLayers.join('<br/>') : 'None') +",
        "    '</div>';",
        "  return html;",
        "}",
        "",
        "function updateMarkerPopup(){",
        "  if(!activeMarker) return;",
        "  activeMarker.setPopupContent(makeMarkerPopup(activeMarker.getLatLng()));",
        "}",
        "",
        "function addBaseHeadingOnce(){",
        "  var ctl = el.querySelector('.leaflet-control-layers');",
        "  if(!ctl) return;",
        "  if(ctl.querySelector('.base-heading')) return;",
        "  var base = ctl.querySelector('.leaflet-control-layers-base');",
        "  if(!base) return;",
        "  var hd = document.createElement('div');",
        "  hd.className = 'base-heading';",
        "  hd.style.textAlign = 'left';",
        "  hd.style.fontWeight = '600';",
        "  hd.style.margin = '0 0 6px 0';",
        "  hd.textContent = 'Background map';",
        "  base.prepend(hd);",
        "}",
        "",
        "function insertOverlaySectionHeadings(){",
        "  var ctl = el.querySelector('.leaflet-control-layers');",
        "  if(!ctl) return;",
        "  var overlays = ctl.querySelector('.leaflet-control-layers-overlays');",
        "  if(!overlays) return;",
        "  overlays.querySelectorAll('.overlay-section-heading').forEach(function(n){ n.remove(); });",
        "  var labelByName = {};",
        "  overlays.querySelectorAll('label').forEach(function(lab){",
        "    var name = norm(lab.textContent);",
        "    if(name) labelByName[name] = lab;",
        "  });",
        "  Object.keys(sections).forEach(function(sectionName){",
        "    var layers = sections[sectionName] || [];",
        "    var firstLabel = null;",
        "    for(var i=0; i<layers.length; i++){",
        "      var nm = layers[i];",
        "      if(labelByName[nm]){",
        "        firstLabel = labelByName[nm];",
        "        break;",
        "      }",
        "    }",
        "    if(!firstLabel) return;",
        "    var heading = document.createElement('div');",
        "    heading.className = 'overlay-section-heading';",
        "    heading.textContent = sectionName;",
        "    heading.style.fontWeight = '600';",
        "    heading.style.margin = '8px 0 4px 0';",
        "    heading.style.paddingTop = '6px';",
        "    heading.style.borderTop = '1px solid rgba(0,0,0,0.15)';",
        "    heading.style.textAlign = 'left';",
        "    overlays.insertBefore(heading, firstLabel);",
        "  });",
        "}",
        "",
        "function makeLegendsScrollable(){",
        "  var maxH = Math.max(120, Math.floor(el.getBoundingClientRect().height * 0.5));",
        "  var legendsNodes = el.querySelectorAll('details[id^=\"legend-\"]');",
        "  legendsNodes.forEach(function(d){",
        "    d.style.maxHeight = maxH + 'px';",
        "    d.style.overflowY = 'auto';",
        "    d.style.overflowX = 'hidden';",
        "    var sum = d.querySelector('summary');",
        "    if(sum){",
        "      sum.style.position = 'sticky';",
        "      sum.style.top = '0';",
        "      sum.style.background = 'white';",
        "      sum.style.zIndex = '1';",
        "    }",
        "  });",
        "}",
        "",
        "Object.keys(legends).forEach(function(layerName){",
        "  showByLayer(layerName, false);",
        "});",
        "",
        "requestAnimationFrame(function(){",
        "  requestAnimationFrame(function(){",
        "    addBaseHeadingOnce();",
        "    insertOverlaySectionHeadings();",
        "    makeLegendsScrollable();",
        "    syncFromLayerControl();",
        "  });",
        "});",
        "",
        "map.on('click', function(e){",
        "  if(activeMarker){",
        "    map.removeLayer(activeMarker);",
        "  }",
        "  activeMarker = L.marker(e.latlng, {pane: 'markerPane'}).addTo(map);",
        "  activeMarker.bindPopup(makeMarkerPopup(e.latlng)).openPopup();",
        "});",
        "",
        "map.on('overlayadd', function(e){ showByLayer(e.name, true); updateMarkerPopup(); });",
        "map.on('overlayremove', function(e){ showByLayer(e.name, false); updateMarkerPopup(); });",
        "",
        "map.on('layeradd layerremove', function(){",
        "  addBaseHeadingOnce();",
        "  insertOverlaySectionHeadings();",
        "  makeLegendsScrollable();",
        "  syncFromLayerControl();",
        "  updateMarkerPopup();",
        "});",
        "",
        "window.addEventListener('resize', function(){",
        "  makeLegendsScrollable();",
        "});",
        "}"
      ),
      data = payload
    )
}
