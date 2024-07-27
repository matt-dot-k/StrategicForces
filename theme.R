require(ggplot2)

# Set ggplot theme
theme <- theme(
    axis.title.x = element_text(
        size = 14, face = "bold", angle = 0,
        margin = margin(t = 0.25, unit = "cm")),
    axis.title.y = element_text(
        size = 14, face = "bold", angle = 90,
        margin = margin(r = 0.25, unit = "cm")),
    axis.text = element_text(
        size = 12),
    axis.line = element_line(
        linewidth = 0.5, color = "#000000"),
    axis.ticks.length = unit(0.25, "cm"),
    plot.background = element_rect(
        fill = "#FFFFFF"),
    panel.background = element_rect(
        fill = "#FFFFFF"),
    legend.background = element_rect(
        fill = "#FFFFFF"),
    legend.key = element_rect(
        fill = "#FFFFFF"),
    panel.grid.major.y = element_line(
        linewidth = 0.3, color = "gray50"),
    panel.grid.minor.y = element_line(
        linewidth = 0.3, color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(
        linewidth = 1.0, color = "#000000", fill = NA),
    legend.title = element_text(
        size = 14, face = "bold"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.text = element_text(
        size = 12, face = "bold"),
    legend.position = "bottom"
)