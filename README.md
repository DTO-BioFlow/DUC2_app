# Marine life habitat use in potential offshore infrastructure areas

An interactive RShiny application for visualizing acoustic telemetry data, passive acoustic detection and classification data, and environmental layers for the DTO-Bioflow Digital Use Case 2 (DUC2): Impact of Offshore Energy Installations on Marine Life.

![DTO-Bioflow logo](app/static/Logo_BIO-Flow2023_Final_Positive.png)

## 🎯 About

This application is part of the Horizon Europe-funded project **DTO-Bioflow** ([dto-bioflow.eu](https://dto-bioflow.eu), Grant ID 101112823, <https://doi.org/10.3030/101112823>). It provides interactive visualizations of:

-   **Acoustic telemetry data** from the [European Tracking Network](https://www.lifewatch.be/etn/)
-   **Passive Acoustic Monitoring (PAM) data** from the [European Tracking Network](https://www.lifewatch.be/cpod-data/)
-   **Species migration predictions** (European seabass)
-   **Environmental layers** via WMS services from [EDITO STAC catalog](https://viewer.dive.edito.eu/map?c=0,0,2.26&catalog=https:%2F%2Fapi.dive.edito.eu%2Fdata%2Fcatalogs)

🔗 Learn more: [DUC2 - Impact of Offshore Infrastructures](https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures)

## ✨ Features

### European Seabass (*Dicentrarchus labrax*)

-   Migration predictions (inside/outside offshore wind farms)
-   Acoustic telemetry detection data with interactive temporal charts
-   Environmental layer visualizations

### Harbour Porpoise (*Phocoena phocoena*)

-   Acoustic Density Data and Visualization
-   Environmental correlates

## 🚀 Installation

### Prerequisites

-   R (≥ 4.0)
-   RStudio (recommended)
-   Docker (optional, for containerized deployment)

### Required R Packages

``` r
install.packages(c(
  "shiny", "DT", "leaflet", "glue", "httr", "terra",
  "htmltools", "leaflet.minicharts", "leaflet.extras", "leafem",
  "tidyr", "RColorBrewer", "rstac", "purrr", "arrow",
  "dplyr", "lubridate"
))
```

Alternatively, use `renv` to restore the exact package environment:

``` r
renv::restore()
```

### Clone the Repository

``` bash
git clone https://github.com/your-org/DUC2_viewer.git
cd DUC2_viewer
```

## 📁 Project Structure

```         
DUC2_viewer/
├── app.R                         # Rhino entrypoint (calls rhino::app())
├── Dockerfile                    # Docker container configuration
├── renv.lock                     # R package dependency lock file
├── rhino.yml                     # Rhino project config
├── LICENSE                       # License file
├── README.md                     # This file
│
├── app/
│   ├── main.R                    # Main Rhino module (ui/server)
│   ├── logic/
│   │   └── legacy/               # Migrated app scripts (modules/helpers/config)
│   │       ├── 00_config.R       # Configuration (colors, URLs)
│   │       ├── DiurnalPlot.R
│   │       ├── DPHpDPDplot.R
│   │       ├── module_home.R
│   │       ├── module_porpoise.R
│   │       ├── seabass_module.R
│   │       ├── seabass_submodule_migration.R
│   │       ├── seabass_submodule_telemetry_data.R
│   │       ├── seabass_submodule_environmental_data.R
│   │       ├── maps.R
│   │       ├── EDITO_STAC_data.R
│   │       ├── helper_load_acoustic_telemetry_GAM_s3.R
│   │       └── helper_wrangle_acoustic_telemetry_data.R
│   └── static/                   # Static web assets
│       ├── app.css
│       ├── AU logo.png
│       ├── Logo_BIO-Flow2023_Final_Positive.png
│       ├── Logo_BIO-Flow2023_Final_Negative.png
│       ├── D_labrax_phylopic_CC0.png
│       ├── P_phocoena_phylopic_CC0.png
│       └── north_arrow.png
│   │
├── data/                         # Data files

│   ├── DTO_DUC2_PpData.Rdata     # Harbour Porpoise PAM data
│   ├── etn_sum_seabass.rds       # Seabass detection summaries
│   ├── etn_sum_seabass_monthyear_individual.rds       # Monthly individual summaries
│   ├── etn_sum_seabass_monthyear_individual_subset.rds # Subset for testing
│   ├── EDITO_STAC_layers_metadata.csv  # STAC environmental layer metadata
│   ├── TEL_animals.rds           # Tagged animal metadata telemetry
│   ├── TEL_deployments.rds       # Telemetry acoustic receiver deployments
│   └── TEL_detections.rds        # Raw telemetry detection data
│
└── tests/                        # Automated tests
```

### Key Files

| File | Purpose |
|-----------------------------|-------------------------------------------|
| `app.R` | Rhino app entrypoint |
| `app/main.R` | Main module with startup loader + top-level UI/server |
| `app/logic/legacy/00_config.R` | Stores configuration variables (colors, URLs, etc.) |
| `app/logic/legacy/module_*.R` | Main Shiny modules (home, porpoise) |
| `app/logic/legacy/seabass_*.R` | Seabass module and submodules |
| `app/logic/legacy/maps.R` | Map rendering functions (base map, environmental WMS) |
| `app/logic/legacy/helper_*.R` | Helper functions for data loading and processing |
| `app/logic/legacy/EDITO_STAC_data.R` | Functions to load STAC catalog metadata |
| `app/logic/legacy/DiurnalPlot.R` | Plotting function for PAM data |
| `app/logic/legacy/DPHpDPDplot.R` | Plotting function for PAM data |
| `Dockerfile` | Container configuration for deployment |
| `renv.lock` | Package dependency snapshot for reproducibility |

## 🏃 Running the Application

### From RStudio

1.  Open `app.R`
2.  Click "Run App" button

### From R Console

``` r
shiny::runApp()
```

### From Command Line

``` bash
R -e "shiny::runApp()"
```

### Using Docker

``` bash
docker build -t duc2-viewer .
docker run -p 3838:3838 duc2-viewer
```

Then navigate to `http://localhost:3838` in your browser.

## 🔧 Adding New Modules

This application uses a **modular architecture**. Each tab/feature is a self-contained module.

### Step 1: Create Module File

Create a new file in `app/logic/legacy/` (e.g., `app/logic/legacy/module_mynewspecies.R`):

``` r
# app/logic/legacy/module_mynewspecies.R

# UI Function
mod_mynewspecies_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("My New Species"),
    leafletOutput(ns("map"), height = 700)
  )
}

# Server Function
mod_mynewspecies_server <- function(id, 
                                     data           # Data parameter
                                     ) 
                                     {
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      make_base_map |>
        addMarkers(data = data, ~lon, ~lat)
    })
    
  })
}
```

### Step 2: Add Module to UI

In `app.R`, add a new tab panel:

``` r
# In app.R ui section
tabsetPanel(
  # ... existing tabs ...
  
  tabPanel(
    title = "My New Species",
    mod_mynewspecies_ui("mynewspecies")  # Call module UI
  )
)
```

### Step 3: Call Module Server

In `app.R` server function:

``` r
# In app.R server section
server <- function(input, output, session) {
  # ... existing modules ...
  
  mod_mynewspecies_server(
    "mynewspecies",
    data = my_species_data,           # Pass data
  )
}
```

### 🔑 Key Principles

1.  **Function Parameters**: Always pass functions and data as parameters to modules
2.  **Use Parameter Names**: Inside modules, use parameter names (e.g., `base_map_fun`), not global function names (e.g., `make_base_map`)
3.  **Namespace**: Use `ns <- NS(id)` in UI and wrap input/output IDs with `ns()`
4.  **Self-Contained**: Each module should be independent and reusable
5.  **File Organization**: Keep app scripts in `app/logic/legacy/`; use descriptive naming (e.g., `module_*.R`, `helper_*.R`)

### Example Module Call Chain

``` r
# In app/main.R startup loader (define once)
make_base_map <- function() { ... }

# In app/main.R (pass to module)
mod_seabass_server(
  "seabass",
  base_map_fun = make_base_map  # ← Global function name
)

# In app/logic/legacy/seabass_module.R (receive as parameter)
mod_seabass_server <- function(id, base_map_fun) {
  moduleServer(id, function(input, output, session) {
    
    # Pass to submodule
    mod_seabass_migration_server(
      "migration",
      base_map_fun = base_map_fun  # ← Use parameter name
    )
  })
}

# In app/logic/legacy/seabass_submodule_migration.R (use parameter)
mod_seabass_migration_server <- function(id, base_map_fun) {
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      make_base_map  # ← Call the function
    })
  })
}
```

## 📊 Data Sources

-   **Acoustic Telemetry**: [European Tracking Network (ETN)](https://www.lifewatch.be/etn/)
-   **Environmental Data**: EDITO STAC catalog (metadata in `data/EDITO_STAC_layers_metadata.csv`)
-   **Species Predictions**: GAM models from S3 storage

## 🤝 Contributing

### Development Workflow

1.  Create a feature branch: `git checkout -b feature/my-new-feature`
2.  Make changes following the modular structure
3.  Test the app locally
4.  Commit with clear messages: `git commit -m "Add new species module"`
5.  Push and create a pull request

### Code Style

-   Use `snake_case` for function and variable names
-   Place app scripts in `app/logic/legacy/`
-   Prefix helper functions with `helper_` (e.g., `helper_load_data.R`)
-   Prefix modules with `module_` (e.g., `module_home.R`)
-   Comment complex logic
-   Keep modules self-contained
-   Pass data and functions as parameters (avoid global dependencies)

### File Naming Conventions

-   **Configuration**: `00_config.R` (prefix with `00_` to load first)
-   **Modules**: `module_*.R` or `[species]_module.R`
-   **Submodules**: `[species]_submodule_*.R`
-   **Helpers**: `helper_*.R`
-   **Map functions**: `maps.R`

## 👥 Contributors

-   **Flanders Marine Institute (VLIZ)** [Marine Observation Centre, VLIZ](https://vliz.be/en/what-we-do/research/marine-observation-centre) - Lotte Pohl - [lotte.pohl\@vliz.be](mailto:lotte.pohl@vliz.be){.email}, Jo-Hannes Nowé - [johannes.nowe\@vliz.be](mailto:johannes.nowe@vliz.be){.email}

-   Aarhus University: Emily T. Griffiths, Mia Kronborg, Ellen Jacobs

-   Technical University of Denmark: Asbjørn Christensen

[Marine Observation Centre, VLIZ](https://vliz.be/en/what-we-do/research/marine-observation-centre)

## 🔗 Links

-   **DTO-Bioflow Project**: <https://dto-bioflow.eu>
-   **DUC2 Use Case**: <https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures>
-   **European Tracking Network**: <https://www.lifewatch.be/etn/>
-   **VLIZ**: <https://vliz.be/en>

------------------------------------------------------------------------

**Grant Information**: This project has received funding from the European Union's Horizon Europe research and innovation programme under grant agreement No. 101112823 (DTO-Bioflow).
