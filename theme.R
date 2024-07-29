require(ggplot2)

# Set ggplot theme
theme <- theme(
    axis.title.x = element_text(size = 14, face = "bold", angle = 0,
                                margin = margin(t = 0.25, unit = "cm")),
    axis.title.y = element_text(size = 14, face = "bold", angle = 90,
                                margin = margin(r = 0.25, unit = "cm")),
    axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.line = element_line(linewidth = 0.5, color = "#000000"),
    axis.ticks.length = unit(0.25, "cm"),
    panel.border = element_rect(linewidth = 1.0, color = "#000000", fill = NA),
    legend.title = element_text(size = 14, face = "bold"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
)