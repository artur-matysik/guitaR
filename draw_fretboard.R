library(tidyverse)

# setup
guitar_setup <- list(
  tuning = c("E", "A", "D", "G", "B", "E"),
  frets = 23,
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
    fret = 18,
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
    fret_note = calc_note_v(fret_num, string_tune),
    interval = names(calc_note_v(fret_num, string_tune))
  ) %>%
    left_join(fretboard %>% select(fret_num, fret_pos), by = "fret_num") %>%
    distinct()

}

fretboard_notes <- calc_notes(fretboard, guitar_setup)



# Generate scale ----------------------------------------------------------
calc_scale <- function(root, type) {
  all_notes = data.frame(
    idx = 1:24,
    note = rep(c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),2)
    )

  # generate harmonic scale
  root <-  "A"
  type <- "minor_pentatonic"

  idx <- which(all_notes$note==root)[1] + 0:11

  scale_harmonic <- all_notes[idx,] %>%
    mutate(
      interval_num = c(1, 2, 2, 3, 3, 4, 4, 5, 6, 6, 7, 7),
      interval_name = c("P1", "m2", "M2", "m3", "M3", "P4", "A4", "P5", "m6", "M6", "m7", "M7"),
      interval_isMajor = c(T, F, T, F, T, T, F, T, F, T, F, T),
      interval_isMinor = c(T, F, T, T, F, T, F, T, T, F, T, F),
      interval_isMinorHarmonic =c(T, F, T, T, F, T, F, T, T, F, F, T)
    )

  scale_harmonic

  scale_harmonic %>% filter(
    case_when(
      str_detect(type, "major") ~ interval_isMajor,
      type == "minor_harmonic" ~ interval_isMinorHarmonic,
      str_detect(type, "minor") ~ interval_isMinor
    )
  ) %>%
    arrange(interval_num) %>%
    filter(
      case_when(str_detect(type, "pentatonic") ~ interval_num %in% c(1, 3, 4, 5, 7),
                TRUE ~ TRUE)
    ) %>%
    select(note, interval_num, interval_name)
}

calc_scale("A", "minor_harmonic")
calc_scale("A", "minor_natural")
calc_scale("G", "minor_natural")
calc_scale("A", "minor_pentatonic")
calc_scale("A", "major_pentatonic")


# Notes to show -----------------------------------------------------------

root = "A"
type = "minor_natural"
show_all_notes <- FALSE
show_scale_notes <- FALSE
show_root <- TRUE
show_3 <- TRUE
show_5 <- TRUE
show_scale <- TRUE
note_label_size <- 4
note_point_size <- 8


fretboard_notes_show <- fretboard_notes %>%
  left_join(calc_scale(root, type), by = c("fret_note" = "note")) %>%
  mutate(
    show_label = case_when(
      show_scale_notes & !show_all_notes ~ !is.na(interval_num),
      show_all_notes ~ TRUE,
      TRUE ~ FALSE
    ),
    show_scale = case_when(
      show_scale ~ !is.na(interval_num),
      TRUE ~ FALSE
    ),
    show_root = case_when(
      show_root ~  interval_num == 1,
      TRUE ~ FALSE
    ),
    show_3 = case_when(
      show_3 ~ interval_num == 3,
      TRUE ~ FALSE
    ),
    show_5 = case_when(
      show_5 ~ interval_num == 5,
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
  # labs(x = "fret", y = "string") +
  # theme
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        aspect.ratio=2/5
        )




# show label bakcground
fr + geom_point(data = fretboard_notes_show %>% filter(show_label & !show_scale),
           mapping = aes(x = fret_pos, y = string_idx),
           shape = 16, size = note_point_size,
           color = "white") +
  # show scale
  geom_point(data = fretboard_notes_show %>% filter(show_scale & !show_root),
             mapping = aes(x = fret_pos, y = string_idx),
             shape = 21, size = note_point_size,
             fill = "white", color = "black") +
  # show root
  geom_point(data = fretboard_notes_show %>% filter(show_scale & show_root),
             mapping = aes(x = fret_pos, y = string_idx),
             shape = 21, size = note_point_size,
             fill = "red", color = "black") +
  # show 3
  geom_point(data = fretboard_notes_show %>% filter(show_scale & show_3),
             mapping = aes(x = fret_pos, y = string_idx),
             shape = 21, size = note_point_size,
             fill = "green", color = "black") +
  # show 5
  geom_point(data = fretboard_notes_show %>% filter(show_scale & show_5),
             mapping = aes(x = fret_pos, y = string_idx),
             shape = 21, size = note_point_size,
             fill = "blue", color = "black") +
  # show note labels
  geom_text(data = fretboard_notes_show %>% filter(show_label),
            mapping = aes(x = fret_pos, y = string_idx, label = fret_note), size = note_label_size) +
  labs(title = paste(root, str_replace(type, "_", " ")))

