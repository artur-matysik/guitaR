library(shiny)
library(colourpicker)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("GuitaR Shiny scales"),

  plotOutput("fretboard"),

  hr(),

  fluidRow(
    column(
      3,
      h4("Guitar setup"),
      sliderInput(
        "frets",
        "Number of frets:",
        min = 4,
        max = 30,
        value = 12,
        step = 1
      )

    ),
    column(
      3,
      h4("Scale setup"),
      checkboxInput("show_scale", "Show scale (markers)", TRUE),
      checkboxInput("show_scale_notes", "Show scale (notes)", TRUE),
      checkboxInput("show_root", "Marker root", TRUE),
      checkboxInput("show_3", "Marker 3rd", TRUE),
      checkboxInput("show_5", "Marker 5th", TRUE),
      selectInput(
        "root",
        "Root note",
        c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),
        selected = "A"
      ),
      selectInput(
        "scale_type",
        "Scale type",
        c(
          "Major" = "major",
          "Major pentatonic" = "major_pentatonic",
          "Minor natural" = "minor_natural",
          "Minor harmonic" = "minor_harmonic",
          "Minor pentatonic" = "minor_pentatonic"
        ),
        selected = "minor_pentatonic"
      )
    ),
    column(
      3,
      h4("Visual setup"),

      checkboxInput("show_all_notes", "Show all notes", FALSE),
      sliderInput("note_label_size", "Note character size", 2, 10, 5),
      sliderInput("note_point_size", "Note point size", 5, 20, 10, step = 1),
      sliderInput("base_size", "Base size", 10, 30, 20, step = 1)

    ),
    column(
      3,
      h4("Colors"),
      colourInput("color_root", "Marker root", value = "#FF0000"),
      colourInput("color_3", "Marker 3rd", value = "#FF7070"),
      colourInput("color_5", "Marker 5th", value = "#FCEDED")


    )
  )
))
