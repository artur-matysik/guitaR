# UI ----------------------------------------------------------------------
tab_scales_ui <- function(id) {
  fluidPage(# uiOutput(NS(id, "header")),
    plotOutput(NS(id, "scales"),  height = 300),
    hr(),
    fluidRow(
      uiOutput(NS(id, "selections_scale")),
      uiOutput(NS(id, "selections_notes")),
      uiOutput(NS(id, "selections_labels"))
    ))
}

# Server ------------------------------------------------------------------
tab_scales_server <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    # Functions ------------------
    fretboard <- reactive({
      df <- calc_fretboard(config()$frets, config()$tuning)
      # saveRDS(df, "fretboard.rds")
      df
    })

    # UI Outputs --------------------------------------------------------------
    ## Render header ----
    output$header <- renderUI({
      h3("Scales")
    })

    ## Render scale selections ----
    output$selections_scale <- renderUI({
      column(4,
        h4("Scale setup"),
        selectInput(
          NS(id, "select_scale"),
          "Select scale",
          choices = keys(),
          selected = "am",
          multiple = FALSE,
          width = 100
        )
      )
    })

    ## Render notes selections ----
    output$selections_notes <- renderUI({
      column(4,
        h4("Notes setup"),
        checkboxInput(NS(id, "show_scale_notes"), "Show notes in scale", T),
        checkboxInput(NS(id, "show_scale_key_note"), "Show key note", T),
        checkboxInput(NS(id, "show_scale_chord_notes"), "Show chord notes", T),
      )
    })

    ## Render labels selections ----
    output$selections_labels <- renderUI({
      column(
        4,
        h4("Label setup"),
        checkboxInput(NS(id, "show_note_label"), "Show note labels", T),
        selectInput(
          NS(id, "label_mode"),
          "Label",
          choices = c("As notes" = "note", "As scale index" = "scale"),
          selected = "note",
          multiple = FALSE
        ),
        # checkboxInput(NS(id, "show_all_notes"), "show_all_notes", F),
      )
    })

    ## Scales config -----
    config_scale <- reactive({
      # this is to prevent passing empty input at load
      req(input$select_scale)

      config_scale <- config()

      key_is_minor <-
        ifelse(str_detect(input$select_scale, "m"), TRUE, FALSE)


      if (key_is_minor) {
        config_scale$notes = config_scale$notes_minor
        config_scale$notes_display <-
          str_replace_all(toupper(config_scale$notes), "_", "b") #♭
      } else {
        config_scale$notes = config_scale$notes_major
        config_scale$notes_display <-
          str_replace_all(toupper(config_scale$notes), "'", "#") #♭
      }

      config_scale$key_is_minor <- key_is_minor
      config_scale$key <- input$select_scale
      # saveRDS(config_scale, "config_scale.rds")
      config_scale

    })

    ## Notes on fretboard ---
    fretboard_scale <- reactive({
      req(config_scale())
      req(fretboard())
      # req(input$show_all_notes)
      # req(input$show_scale_notes)
      # req(input$show_scale_key_note)
      # req(input$show_note_label)

      config_scale <- config_scale()

      show_all_notes = F # show all notes on fretboard
      show_scale_notes = input$show_scale_notes # show scale notes
      show_scale_key_note = input$show_scale_key_note # show key note
      show_note_label = input$show_note_label # show note labels

      # get scale
      if(config_scale$key_is_minor) {
        s <- scale_minor(key = config_scale$key, ignore_octave = TRUE)
        s <- flatten_sharp(s)
      } else {
        s <- scale_major(key = config_scale$key, ignore_octave = TRUE)
        s <- sharpen_flat(s)
      }

      fretboard_notes <- calc_fretboard_scale(fretboard(), config_scale)
      # saveRDS(fretboard_notes, "fretboard_notes.rds")

      # get intervals - notes
      scale_notes <- bind_cols(
        note_idx = 1:length(s),
        data.frame(note = as.character(s)),
        mainIntervals %>% filter(mmp_abb %in% scale_interval(rep("c", length(s)), s))
      )



      fretboard_notes_show <- fretboard_notes %>%
        left_join(scale_notes, by = c("fret_note" = "note")) %>%
        mutate(

          show_scale_notes = case_when(
            show_scale_notes ~ !is.na(mmp),
            TRUE ~ FALSE
          ),

          show_note_label = case_when(
            show_note_label & show_scale_notes ~ !is.na(mmp),
            show_note_label & input$show_scale_chord_notes ~ note_idx %in% c(1,3,5),
            show_note_label & show_scale_key_note ~ note_idx == 1,
            # show_note_label & show_all_notes ~ TRUE,
            TRUE ~ FALSE
          ),

          show_scale_key_note = case_when(
            show_scale_key_note | input$show_scale_chord_notes ~ note_idx == 1,
            TRUE ~ FALSE
          )
        )
      # saveRDS(fretboard_notes_show, "fretboard_notes_show.rds")
      fretboard_notes_show

    })

    ## Render fretboard ----
    output$scales <- renderPlot({
      req(fretboard_scale())
      req(fretboard())
      req(input$label_mode)

      note_label_size = config()$note_label_size
      note_point_size = config()$note_point_size
      base_size = config()$base_size

      theme_set(theme_minimal(base_size = base_size))
      fr <- draw_fretboard(fretboard())
      fretboard_notes_show <- fretboard_scale()
      # saveRDS(fretboard_notes_show, "fretboard_notes_show2.rds")
      fr <- fr +
        # # show label bakcground
        geom_point(data = fretboard_notes_show %>% filter(show_note_label & !show_scale_notes),
                   mapping = aes(x = fret_pos, y = string_idx),
                   shape = 16, size = note_point_size,
                   color = "white") +
        # show_scale_notes
        geom_point(data = fretboard_notes_show %>% filter(show_scale_notes ),
                   mapping = aes(x = fret_pos, y = string_idx),
                   shape = 21, size = note_point_size,
                   fill = "white", color = "black") +
        # show_scale_key_note
        geom_point(data = fretboard_notes_show %>% filter(show_scale_key_note),
                   mapping = aes(x = fret_pos, y = string_idx),
                   shape = 21, size = note_point_size,
                   fill = "red", color = "black")

      # show chord notes
      if(input$show_scale_chord_notes) {
        fr <- fr +
          geom_point(data = fretboard_notes_show %>% filter(note_idx %in% c(3, 5)),
                              mapping = aes(x = fret_pos, y = string_idx),
                              shape = 21, size = note_point_size,
                              fill = "yellow", color = "black")
      }

      # show note labels
      if(input$label_mode == "note") {
        fr <- fr +
          geom_text(data = fretboard_notes_show %>% filter(show_note_label),
                    mapping = aes(x = fret_pos, y = string_idx, label = note_display), size = note_label_size)
      } else {
        fr <- fr +
          geom_text(data = fretboard_notes_show %>% filter(show_note_label),
                    mapping = aes(x = fret_pos, y = string_idx, label = note_idx), size = note_label_size)
      }

      fr


    })
  }) # End of module server
} # End of server
