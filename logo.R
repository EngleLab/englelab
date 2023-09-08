library(hexSticker)
library(here)

sticker(here("man/figures",
             "awm_logo_small.jpeg"),
        filename = here("man/figures", "logo.png"),
        package = "englelab",
        h_fill = "#FFFFFF",
        h_color = "#FFB002",
        p_color = "#161D7A",
        p_size = 16,
        s_width = .45,
        s_x = 1.0,
        s_y = .80)

