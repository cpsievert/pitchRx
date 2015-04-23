library(pitchRx)
library(animation)
data(pitches)

relabel <- function(variable, value) {
  value <- sub("^R$", "Right-Handed Batter", value)
  sub("^L$", "Left-Handed Batter", value)
}

#figure 6
saveHTML(
  animateFX(pitches, avg.by="pitch_types", layer=list(facet_grid(pitcher_name~stand, labeller = relabel), theme_bw(), coord_equal())),
  outdir = getwd()
)
