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
      2,
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
      5,
      h4("Scale setup"),
      column(6,
             selectInput(
               "root",
               "Root note",
               c("C" = "C",
                 "C#/D♭" = "C#",
                 "D" = "D",
                 "D#/E♭" = "D#",
                 "E" = "E",
                 "F" = "F",
                 "F#/G♭" = "F#",
                 "G" = "G",
                 "G#/A♭" = "G#",
                 "A" = "A",
                 "A#/B♭" = "A#",
                 "B" = "B"),
               selected = "A"
             ),
             selectInput(
               "scale_type",
               "Scale type",
               c(
                 "Major" = "major",
                 "Minor natural" = "minor",
                 "Minor harmonic" = "minor_harmonic"
               ),
               selected = "major"
             ),
             checkboxInput("scale_type_pentatonic", "Pentatonic", FALSE),

             ),
      column(6,
             checkboxInput("show_scale", "Show scale (markers)", TRUE),
             checkboxInput("show_scale_notes", "Show scale (notes)", TRUE),
             checkboxInput("show_root", "Marker root", TRUE),
             checkboxInput("show_3", "Marker 3rd", TRUE),
             checkboxInput("show_5", "Marker 5th", TRUE),

      ),


    ),
    column(
      3,
      h4("Visual setup"),

      checkboxInput("show_all_notes", "Show all notes", FALSE),
      checkboxInput("show_non_pentatonic", "Show non-pentatonic", FALSE),
      sliderInput("note_label_size", "Note character size", 2, 10, 5),
      sliderInput("note_point_size", "Note point size", 5, 20, 10, step = 1)
      # sliderInput("base_size", "Base size", 10, 30, 20, step = 1)

    ),
    column(
      1,
      h4("Colors"),
      colourInput("color_root", "Marker root", value = "#FF0000"),
      colourInput("color_3", "Marker 3rd", value = "#FF7070"),
      colourInput("color_5", "Marker 5th", value = "#FCEDED")


    )
  )
))
