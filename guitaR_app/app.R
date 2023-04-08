# Libraries ---------------------------------------------------------------
# shiny
library(shiny)
library(shinydashboard)
library(shinyjs)

# other
library(tidyverse)
library(DT)         # table display
library(tabr)

# Sources -----------------------------------------------------------------
# modules
source("mod-scales.R")
source("functions.R")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(

  ## Header ----
  dashboardHeader(title = "MyShinyApp",
                  tags$li(class = "dropdown",
                          style = "padding: 8px;")),

  ## Menu (to be rendered server-side) ----
  dashboardSidebar(collapsed = FALSE, sidebarMenuOutput("sidebar")),
  dashboardBody(shinyjs::useShinyjs(),
                tabItems(
                  tabItem("tab_scales", uiOutput("tab_scales"))
                ))
)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  ## UI Outputs ----------------------------------------------------------------
  # Reactive config
  config <- reactive({
    list(
      frets = input$frets,
      tuning = unlist(strsplit(input$tuning, ",")),
      notes_major = c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"),
      notes_minor = c("c", "d_", "d", "e_", "e", "f", "g_", "g", "a_", "a", "b_", "b"),
      note_label_size = input$note_label_size,
      note_point_size = input$note_point_size,
      base_size = input$base_size
      )
    })

  # Sidebar
  output$sidebar <- renderMenu({
    sidebarMenu(id = "tabs",
                menuItem("Scales", tabName = "tab_scales"),
                menuItem(
                  "Config",
                  tabName = "config",
                  sliderInput("frets", "Frets number", 3, 24, 15, 1),
                  textInput("tuning", "Guitar Tuning", "e,a,d,g,b,e"),
                  sliderInput("note_label_size", "Label size", 1, 20, 5, 1),
                  sliderInput("note_point_size", "Point size", 1, 20, 10, 1),
                  sliderInput("base_size", "Base size", 10, 30, 20, 1)
                )
                )
  })

  # Scales
  output$tab_scales <- renderUI({
    tabItem(tabName = "tab_scales", tab_scales_ui("mod_scales"))
  })
  tab_scales_server("mod_scales", config)

}

shiny::shinyApp(ui, server)
