library(tidyverse)

# setup strings
strings <- 1:6
names(strings) <- c("E", "A", "D", "G", "B", "E")

# calculate frets distance
calc_fret_distance <- function(n, s = 1, k = 5.71584144995393e-2) {
  s * (1 - exp(-k * n))
}
frets <- calc_fret_distance(0:23)
names(frets) <- as.character(0:(length(frets)-1))

# markers
calc_fret_marker_position <- function(n, frets) {
  mean(c(frets[n], frets[n+1]))
}
calc_fret_marker_position_v <- Vectorize(calc_fret_marker_position, "n")

fret_markers_single <- c(3, 5, 7, 9, 15, 17, 19, 21) 
fret_markers_single <- fret_markers_single[fret_markers_single %in% names(frets)]
fret_markers_single_position <- calc_fret_marker_position_v(fret_markers_single, frets)

fret_markers_double <- c(12) 
fret_markers_double <- fret_markers_double[fret_markers_double %in% names(frets)]
fret_markers_double_position <- calc_fret_marker_position_v(fret_markers_double, frets)



# plot fret
ggplot(NULL, aes(y = strings)) +
  geom_hline(aes(yintercept = strings), size = 1 - seq(from = 0, to = 0.5, length.out = length(strings))) +
  geom_vline(aes(xintercept = frets), alpha = 0.3) +
  geom_point(aes(
    x = fret_markers_single_position,
    y = max(strings + 1) / 2),
    size = 5,
    alpha = 0.1) +
  geom_point(
    aes(x = fret_markers_double_position, y = max(strings + 1) / 2 + c(1,-1)),
    size = 5,
    alpha = 0.5
  ) +
  
  scale_y_continuous(breaks = strings, labels = names(strings), expand = c(0.03,0.03)) +
  scale_x_continuous(
    expand = c(0.005, 0),
    breaks = frets,
    labels = names(frets),
    limits = c(0, max(frets))
  ) +
  labs(x = "fret", y = "string") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
