library(hexSticker)
library(showtext)

# Try using Roboto Black specifically
font_add_google("Roboto", "roboto")
showtext_auto()

sticker(
  package = "",
  subplot = "tools/logo.png",
  p_color = "#000000",  # Pure black
  p_family = "roboto",
  p_fontface = "bold",
  s_x = 1, s_y = 1,
  s_width = 1, s_height = 1,
  h_fill = "white",
  h_color = "white",
  h_size = 1,
  p_x = 1, p_y = 1.2,
  spotlight = FALSE,
  l_x = 1, l_y = 1.2,
  l_alpha = 0.7,
  white_around_sticker = TRUE,
  filename = "man/figures/logo_runrgetics.png"
)
