library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("GuitaR Shiny scales"),

  plotOutput("fretboard"),

  hr(),

  fluidRow(
    column(
      4,
      h4("Guitar setup"),
      sliderInput(
        "frets",
        "Number of frets:",
        min = 4,
        max = 30,
        value = 24
      )

    ),
    column(
      4,
      h4("Scale setup"),
      checkboxInput("show_scale", "Show scale (markers)", TRUE),
      checkboxInput("show_notes", "Show scale (notes)", TRUE),
      checkboxInput("show_root", "Show root", TRUE),
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
        )
      ),
      checkboxInput("show_notes", "Show notes", TRUE),

    ),
    column(
      4,
      h4("Visual setup"),

      checkboxInput("show_all_notes", "Show all notes", FALSE),
      sliderInput("base_size", "Base size", 20, 30, 25),
      sliderInput("note_label_size", "Note character size", 5, 10, 7),
      sliderInput("note_point_size", "Note point size", 10, 20, 15)

    )
  )
))
