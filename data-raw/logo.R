# Generate the hex sticker at man/figures/logo.png.
# Requires hexSticker and ggplot2 (not package dependencies; install ad hoc to
# regenerate). Run from the package root. The palette and layout match the
# abnoRmal sticker so the two read as one family.

hexagon <- function(cx, cy, r) {
  angle <- seq(30, 330, by = 60) * pi / 180
  data.frame(x = cx + r * cos(angle), y = cy + r * sin(angle))
}

# A honeycomb of package hexagons around a filled centre: finRes as the
# umbrella over the suite.
w <- sqrt(3)
centres <- data.frame(
  cx = c(0, w, -w, w / 2, -w / 2, w / 2, -w / 2),
  cy = c(0, 0, 0, 1.5, 1.5, -1.5, -1.5)
)
cells <- do.call(rbind, lapply(seq_len(nrow(centres)), function(i) {
  cell <- hexagon(centres$cx[i], centres$cy[i], 0.82)
  cell$id <- i
  cell
}))

chart <- ggplot2::ggplot(cells, ggplot2::aes(x, y, group = id)) +
  ggplot2::geom_polygon(ggplot2::aes(fill = id == 1), colour = "#F2A20C", linewidth = 1) +
  ggplot2::scale_fill_manual(values = c(`TRUE` = "#F2A20C", `FALSE` = "#0B2545"), guide = "none") +
  ggplot2::coord_fixed() +
  ggplot2::theme_void()

hexSticker::sticker(
  chart,
  package = "finRes",
  p_size = 20,
  p_color = "#FFFFFF",
  p_y = 1.4,
  s_x = 1, s_y = 0.75, s_width = 0.75, s_height = 0.75,
  h_fill = "#0B2545",
  h_color = "#08192E",
  filename = "man/figures/logo.png",
  dpi = 320
)
