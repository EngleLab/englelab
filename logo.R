library(hexSticker)
library(here)

sticker(here("man/figures",
             "eagle.png"),
        filename = here("man/figures", "logo.png"),
        package = "englelab",
        h_fill = "#004A85",
        h_color = "#EAAA00",
        p_color = "#FFFFFF",
        p_size = 8,
        s_width = .6,
        s_x = 1.0,
        s_y = .70)

