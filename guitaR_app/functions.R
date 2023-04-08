# design fretboard
calc_fretboard <- function(frets = 12, tuning) {
  # calculate layout
  calc_fret_distance <- function(n, s = 1, k = 5.71584144995393e-2) {
    s * (1 - exp(-k * n))
  }

  fretboard_layout <- data.frame(
    fret_pos = calc_fret_distance(0:frets),
    fret_num = 0:frets
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
               marker_posY = max(length(tuning) + 1) / 2),
    data.frame(marker_double = TRUE,
               fret_num = c(12),
               # c(-1, 1) * 1: * 1 can be a distance parameter here
               marker_posY = max(length(tuning) + 1) / 2 + c(-1, 1) * 1)) %>%
    # join with layout
    right_join(fretboard_layout, by = "fret_num") %>%
    arrange(fret_num) %>%
    # calc marker position X (fret)
    mutate(marker_posX = calc_fret_marker_position_v(fret_num, fret_pos)) %>%
    filter(!is.na(marker_double)) %>%
    select(fret_num, marker_double, marker_posX, marker_posY)

  df <- left_join(fretboard_layout, fretboard_markers, by = "fret_num")
  attr(df, "tuning") <- tuning
  df
}


calc_note <- function(fret, tuning, notes) {
  # Get note on given string and fret position
  rs <- NULL
  for(i in 1:length(fret)) {
    fret_i <- fret[i]
    if (fret_i >= 12)
    {
      fret_i = fret_i - (12 * floor(fret_i / 12))
    }
    note_idx <- which(notes == tuning[i]) + fret_i
    if (note_idx > 12)
      note_idx <- note_idx - 12
    rs[i] <- notes[note_idx]
  }
  rs
}

# notes on fretboard (based on scale)
calc_fretboard_scale <- function(fretboard, config_scale) {

  df <- data.frame(
    # data frame: all fret positions on every string
    fret_num = rep(0:config_scale$frets, each = length(config_scale$tuning)),
    string_idx = rep(1:length(config_scale$tuning), config_scale$frets + 1)
  ) %>%

    mutate(
      string_tune = config_scale$tuning[string_idx],
      fret_note = calc_note(fret_num, string_tune, config_scale$notes),
      interval_idx = names(calc_note(fret_num, string_tune, config_scale$notes))
    ) %>%
    left_join(fretboard %>% select(fret_num, fret_pos) %>% distinct(),
              by = "fret_num") %>%
    distinct() %>%
    left_join(
      data.frame(fret_note = config_scale$notes, note_display = config_scale$notes_display),
      by = "fret_note"
    )

  df
}

# draw empty fretboard
draw_fretboard <- function(fretboard) {
  fr <- ggplot(NULL) +
    # plot frets
    geom_vline(data = fretboard, aes(xintercept = fret_pos), alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 1, linewidth = 2) +
    # plot strings
    geom_hline(
      yintercept = 1:length(attr(fretboard, "tuning")),
      # string thickness
      size = 1 - seq(
        from = 0,
        to = 0.5,
        length.out = length(attr(fretboard, "tuning"))
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
      breaks = 1:length(attr(fretboard, "tuning")),
      labels = toupper(attr(fretboard, "tuning")),
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
    # theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.title=element_blank(),
          # aspect.ratio=1/5
    )

  fr
}
