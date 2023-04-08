library(tidyverse)
# library(tabr)

# Input parameters ----
tuning <- c("e", "a", "d", "g", "b", "e")
frets <- 23
key <- "c" # see available keys: keys()

# Display
show_all_notes = FALSE # show all notes on fretboard
show_scale_notes = TRUE # show scale notes
show_scale_key_note = TRUE # show key note
show_note_label = TRUE # show note labels
note_label_size = 4
note_point_size = 8

# Setup ----
config <- list(
  tuning = tuning,
  frets = frets,
  key = key,
  notes_major = c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"),
  notes_minor = c("c", "d_", "d", "e_", "e", "f", "g_", "g", "a_", "a", "b_", "b")
)

# change flat/sharp notation based on key AND
# prepare note labels for display
key_is_minor <-
  ifelse(str_detect(config$key, "m"), TRUE, FALSE)
if (key_is_minor) {
  config$notes = config$notes_minor
} else {
  config$notes = config$notes_major
}
config$notes_display <- str_replace_all(toupper(config$notes), "_", "♭")
config$key_is_minor <- key_is_minor

## Fretboard ----

# design fretboard
calc_fretboard <- function(config) {
  # calculate layout
  calc_fret_distance <- function(n, s = 1, k = 5.71584144995393e-2) {
    s * (1 - exp(-k * n))
  }

  fretboard_layout <- data.frame(
    fret_pos = calc_fret_distance(0:config$frets),
    fret_num = 0:config$frets
  ) %>% select(fret_num, fret_pos)

  # calculate markers
  calc_fret_marker_position <- function(n, pos) {
    mean(c(pos[n], pos[n+1]))
  }
  calc_fret_marker_position_v <- Vectorize(calc_fret_marker_position, "n")

  fretboard_markers <- bind_rows(
    # fret vs marker
    data.frame(marker_double = FALSE,
               fret_num = c(3, 5, 7, 9, 15, 17, 19, 21),
               marker_posY = max(length(config$tuning) + 1) / 2),
    data.frame(marker_double = TRUE,
               fret_num = c(12),
               # c(-1, 1) * 1: * 1 can be a distance parameter here
               marker_posY = max(length(config$tuning) + 1) / 2 + c(-1, 1) * 1)) %>%
    # join with layout
    right_join(fretboard_layout, by = "fret_num") %>%
    arrange(fret_num) %>%
    # calc marker position X (fret)
    mutate(marker_posX = calc_fret_marker_position_v(fret_num, fret_pos)) %>%
    filter(!is.na(marker_double)) %>%
    select(fret_num, marker_double, marker_posX, marker_posY)

  left_join(fretboard_layout, fretboard_markers, by = "fret_num")
}

fretboard <- calc_fretboard(config)

# calculate notes on the fretboard
calc_notes <- function(fretboard, config) {

  calc_note <- function(fret, tuning) {
    # Get note on given string and fret position
    if (fret >= 12)
    {
      fret = fret - (12 * floor(fret / 12))
    }
    note_idx <- which(config$notes == tuning) + fret
    if (note_idx > 12)
      note_idx <- note_idx - 12
    config$notes[note_idx]
  }
  # vectorize
  calc_note_v <- Vectorize(calc_note)


  df <- data.frame(
    # data frame: all fret positions on every string
    fret_num = rep(0:config$frets, each = length(config$tuning)),
    string_idx = rep(1:length(config$tuning), config$frets + 1)
  ) %>% mutate(
    string_tune = config$tuning[string_idx],
    fret_note = calc_note_v(fret_num, string_tune),
    interval_idx = names(calc_note_v(fret_num, string_tune))
  ) %>%
    left_join(fretboard %>% select(fret_num, fret_pos) %>% distinct(),
              by = "fret_num") %>%
    distinct() %>%
    left_join(
      data.frame(fret_note = config$notes, note_display = config$notes_display),
      by = "fret_note"
    )
}


fretboard_notes <- calc_notes(fretboard, config)

# Generate scale ----------------------------------------------------------

# get scale
if(key_is_minor) {
  s <- scale_minor(key = config$key, ignore_octave = TRUE)
} else {
  s <- scale_major(key = config$key, ignore_octave = TRUE)
}

# get intervals - notes
scale_notes <- bind_cols(
  note_idx = 1:length(s),
  data.frame(note = as.character(s)),
  mainIntervals %>% filter(mmp_abb %in% scale_interval(rep("c", length(s)), s))
)

# Notes to show -----------------------------------------------------------

fretboard_notes_show <- fretboard_notes %>%
  left_join(scale_notes, by = c("fret_note" = "note")) %>%
  mutate(
    show_note_label = case_when(
      show_note_label & show_scale_notes & !show_all_notes ~ !is.na(mmp),
      show_note_label & show_all_notes ~ TRUE,
      TRUE ~ FALSE
    ),
    show_scale_notes = case_when(
      show_scale_notes ~ !is.na(mmp),
      TRUE ~ FALSE
    ),
    show_scale_key_note = case_when(
      show_scale_key_note ~  note_idx == 1,
      TRUE ~ FALSE
    )
  )

# Plot --------------------------------------------------------------------

# plot empty fretboard
fr <- ggplot(NULL) +
  # plot frets
  geom_vline(data = fretboard, aes(xintercept = fret_pos), alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 1, linewidth = 2) +
  # plot strings
  geom_hline(
    yintercept = 1:length(config$tuning),
    # string thickness
    size = 1 - seq(
      from = 0,
      to = 0.5,
      length.out = length(config$tuning)
    )
  ) +
  # plot single markers
  geom_point(
    data = fretboard %>% filter(!marker_double),
    aes(x = marker_posX, y = marker_posY),
    size = 5,
    alpha = 0.1
  ) +
  # plot double markers
  geom_point(
    data = fretboard %>% filter(marker_double),
    aes(x = marker_posX, y = marker_posY),
    size = 5,
    alpha = 0.1
  ) +
  # label strings
  scale_y_continuous(
    breaks = 1:length(config$tuning),
    labels = toupper(config$tuning),
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


fr


fr +
  # show label bakcground
  geom_point(data = fretboard_notes_show %>% filter(show_note_label & !show_scale_notes),
           mapping = aes(x = fret_pos, y = string_idx),
           shape = 16, size = note_point_size,
           color = "white") +
  # show_scale_key_note
  geom_point(data = fretboard_notes_show %>% filter(show_scale_notes & !show_scale_key_note),
             mapping = aes(x = fret_pos, y = string_idx),
             shape = 21, size = note_point_size,
             fill = "white", color = "black") +
  # show root
  geom_point(data = fretboard_notes_show %>% filter(show_scale_notes & show_scale_key_note),
             mapping = aes(x = fret_pos, y = string_idx),
             shape = 21, size = note_point_size,
             fill = "red", color = "black") +
  # show note labels
  geom_text(data = fretboard_notes_show %>% filter(show_note_label),
            mapping = aes(x = fret_pos, y = string_idx, label = note_display), size = note_label_size)

