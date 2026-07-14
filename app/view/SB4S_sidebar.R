box::use(
  shiny[
    NS,
    moduleServer,
    radioButtons,
    selectInput,
    sliderInput,
    textOutput,
    uiOutput,
    renderUI,
    renderText,
    tags,
    observe,
    reactive,
    req,
    updateSliderInput,
    isolate
  ],
  bslib[input_switch],
  terra[nlyr]
)



mod_SB4S_sidebar_ui <- function(id, show_toggle = TRUE) {
  ns <- NS(id)

  tags$div(
    class = "SB4S-sidebar",
    input_switch(
      ns("show_layer"),    # sets input$show_layer
      "Show biomass distribution",
      value = FALSE
    ),
    selectInput(
      ns("var"),           # sets input$var
      "Variable",
      choices = c("F", "U", "S", "B", "sum"),
      selected = "sum"
    ),
    uiOutput(ns("week_picker")),
    sliderInput(
      ns("time_index"),     # sets input$time_index
      "week",
      min = 0,
      max = 52,
      value = 0,
      step = 1,
      ticks = FALSE
    ),
    tags$div(
      style = "text-align:center; font-weight:bold;",
      textOutput(ns("time_label"))
    )
  )
}


# --- resolve data layer key and timeframe ---

mod_SB4S_sidebar_server <- function(
  id,
  show_toggle = TRUE
) {
   moduleServer(id, function(input, output, session) {
      reactive({
      
      #print(input$show_layer, input$var, input$time_index)
      
      list(show_layer=input$show_layer,
           var=input$var,
	   time_index=input$time_index
      )
    })
    })
}
