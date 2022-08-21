library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$fretboard <- renderPlot({

      # setup
      guitar_setup <- list(
        tuning = c("E", "A", "D", "G", "B", "E"),
        frets = input$frets,
        # notes: store in vector as ordered factor
        notes = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
      )

      # design fretboard
      calc_fretboard <- function(guitar_setup) {
        # calculate layout
        calc_fret_distance <- function(n, s = 1, k = 5.71584144995393e-2) {
          s * (1 - exp(-k * n))
        }

        fretboard_layout <- data.frame(
          fret_pos = calc_fret_distance(0:guitar_setup$frets),
          fret_num = 0:guitar_setup$frets
        ) %>% select(fret_num, fret_pos)

        # calculate markers
        calc_fret_marker_position <- function(n, pos) {
          mean(c(pos[n], pos[n+1]))
        }
        calc_fret_marker_position_v <- Vectorize(calc_fret_marker_position, "n")

        fretboard_markers <- bind_rows(
          # fret vs marker
          data.frame(marker = "single",
                     fret_num = c(3, 5, 7, 9, 15, 17, 19, 21),
                     marker_posY = max(length(guitar_setup$tuning) + 1) / 2),
          data.frame(marker = "double",
                     fret_num = c(12),
                     # c(-1, 1) * 1: * 1 can be a distance parameter here
                     marker_posY = max(length(guitar_setup$tuning) + 1) / 2 + c(-1, 1) * 1)) %>%
          # join with layout
          right_join(fretboard_layout, by = "fret_num") %>%
          arrange(fret_num) %>%
          # calc marker position X (fret)
          mutate(marker_posX = calc_fret_marker_position_v(fret_num, fret_pos)) %>%
          filter(!is.na(marker)) %>%
          select(fret_num, marker, marker_posX, marker_posY)

        left_join(fretboard_layout, fretboard_markers, by = "fret_num")
      }

      fretboard <- calc_fretboard(guitar_setup)

      # calculate notes on the fretboard
      calc_notes <- function(fretboard, guitar_setup) {
        calc_note <- function(
    fret = 10,
    tuning = "E") {


          if (fret >= 12)
          {
            fret = fret - (12 * floor(fret / 12))
          }
          note_idx <- which(guitar_setup$notes == tuning) + fret
          if(note_idx > 12) note_idx <- note_idx - 12
          guitar_setup$notes[note_idx]
        }

        calc_note_v <- Vectorize(calc_note)


        data.frame(
          fret_num = rep(0:guitar_setup$frets, each = length(guitar_setup$tuning)),
          string_idx = rep(1:length(guitar_setup$tuning), guitar_setup$frets + 1)
        ) %>% mutate(
          string_tune = guitar_setup$tuning[string_idx],
          fret_note = calc_note_v(fret_num, string_tune)
        ) %>%
          left_join(fretboard %>% select(fret_num, fret_pos), by = "fret_num") %>%
          distinct()

      }

      fretboard_notes <- calc_notes(fretboard, guitar_setup)



      # Generate scale ----------------------------------------------------------
      calc_scale <- function(root, type) {
        all_notes = data.frame(
          idx = 1:24,
          notes = rep(c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),2)
        )

        # form diatonic scales
        scale_pattern_major <- c(2, 2, 1, 2, 2, 2, 1)
        scale_pattern_minor_natural <- c(2, 1, 2, 2, 1, 2, 2)
        scale_pattern_minor_harmonic <- c(2, 1, 2, 2, 1, 3, 1)

        if(!str_detect(type, "pentatonic")) {
          scale_pattern <- case_when(
            type == "major" ~ scale_pattern_major,
            type == "minor_natural" ~ scale_pattern_minor_natural,
            type == "minor_harmonic" ~ scale_pattern_minor_harmonic
          )
          idx <- c(which(all_notes$notes==root)[1],  which(all_notes$notes==root)[1] + cumsum(scale_pattern))[1:7]

        } else {
          idx <- c(which(all_notes$notes==root)[1],  which(all_notes$notes==root)[1] + cumsum(scale_pattern_major))[1:7]
          if(str_detect(type, "major")) {
            idx <- idx[c(1, 2, 3, 5, 6)]
          } else if(str_detect(type, "minor")) {
            idx[3] <- idx[3] - 1
            idx[7] <- idx[7] - 1
            idx <- idx[c(1, 3, 4, 5, 7)]

          }
        }

        all_notes[idx,"notes"]

      }

      root = input$root
      type = input$scale_type
      show_all_notes <- input$show_all_notes
      show_scale_notes <- input$show_notes
      show_root <- input$show_root
      show_scale <- input$show_scale
      note_label_size <- input$note_label_size
      note_point_size <- input$note_point_size
      base_size <- input$base_size


      notes_show <- fretboard_notes %>%
        mutate(
          show_label = case_when(
            show_all_notes ~ TRUE,
            show_scale_notes ~ fret_note %in% calc_scale(root, type),

            TRUE ~ FALSE
          ),
          show_scale = case_when(
            show_scale ~ fret_note %in% calc_scale(root, type),
            TRUE ~ FALSE
          ),
          show_root = case_when(
            show_root ~  fret_note %in% root,
            TRUE ~ FALSE
          )
        )



      # Plot --------------------------------------------------------------------


      # plot empty fretboard
      fr <- ggplot(NULL) +
        # plot frets
        geom_vline(data = fretboard, aes(xintercept = fret_pos), alpha = 0.3) +
        geom_vline(xintercept = 0, alpha = 1, size = 2) +
        # plot strings
        geom_hline(
          yintercept = 1:length(guitar_setup$tuning),
          # string thickness
          size = 1 - seq(
            from = 0,
            to = 0.5,
            length.out = length(guitar_setup$tuning)
          )
        ) +
        # plot single markers
        geom_point(
          data = fretboard %>% filter(marker == "single"),
          aes(x = marker_posX, y = marker_posY),
          size = 5,
          alpha = 0.1
        ) +
        # plot double markers
        geom_point(
          data = fretboard %>% filter(marker == "double"),
          aes(x = marker_posX, y = marker_posY),
          size = 5,
          alpha = 0.1
        ) +
        # label strings
        scale_y_continuous(
          breaks = 1:length(guitar_setup$tuning),
          labels = guitar_setup$tuning,
          expand = c(0.07, 0.07)
        ) +
        # label frets
        scale_x_continuous(
          breaks = fretboard$fret_pos,
          labels = fretboard$fret_num,
          expand = c(.005, .005)
        ) +
        # plot labels
        labs(x = "fret", y = "string") +
        # theme
        theme_minimal(base_size = base_size) +
        theme(panel.grid.minor = element_blank())




      fr <- fr +
        # show label bakcground
        geom_point(data = notes_show %>% filter(show_label & !show_scale & !show_root),
                   mapping = aes(x = fret_pos, y = string_idx),
                   shape = 16, size = note_point_size,
                   color = "white") +
        # show scale
        geom_point(data = notes_show %>% filter(show_scale & !show_root),
                   mapping = aes(x = fret_pos, y = string_idx),
                   shape = 21, size = note_point_size,
                   fill = "white", color = "black") +
        # show root
        geom_point(data = notes_show %>% filter(show_scale & show_root),
                   mapping = aes(x = fret_pos, y = string_idx),
                   shape = 21, size = note_point_size,
                   fill = "red", color = "black") +
        # show note labels
        geom_text(data = notes_show %>% filter(show_label),
                  mapping = aes(x = fret_pos, y = string_idx, label = fret_note), size = note_label_size) +
        labs(title = paste(root, str_replace(type, "_", " ")))







      fr

    })

})
