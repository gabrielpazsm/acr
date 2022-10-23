# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

theme_ac <- function(){ ## Criando o tema padrão
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 0.8),
    axis.ticks.length = unit(0.20, "cm"),
    axis.line = element_line(color = "black", size = 0.75),
    axis.title.x = element_text(size = 20, face = "bold",  margin = unit(c(5, 0, 0, 0), "mm")),
    axis.title.y = element_text(size = 20, face = "bold",margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
    axis.text = element_text(color = "black", family = "serif", size = 16),
    axis.text.x = element_text(color = "black", family = "serif", size = 16, margin = unit(c(2.5, 0, 0, 0), "mm")),
    text = element_text(color = "black", family = "serif", size = 16)
  )
}
