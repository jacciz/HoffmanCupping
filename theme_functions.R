theme_coffee <- function(base_size = 12, include_legend = FALSE) {
  # TODO include legend? argument, include lines?
  # Source: 
  # https://github.com/Metropolitan-Council/councilR/blob/main/R/theme_council.R
  
  requireNamespace("rlang", quietly = TRUE)
  
  purrr::map(
    c(base_size),
    rlang:::check_number_decimal
  )
  
  font_sizes <-
    list(
      "title" = ggplot2::rel(1.2),
      "subtitle" = ggplot2::rel(1.1),
      "axis_title" = base_size,
      "axis_text" = ggplot2::rel(0.8),
      "legend_title" = base_size,
      "legend_text" = ggplot2::rel(0.8),
      "caption" = ggplot2::rel(0.8),
      "strip" = ggplot2::rel(0.8)
    )
  
  font <- "sans" # this is Arial via windowsFonts()
  half_line <- base_size / 2
  theme_classic() %+replace%
    theme(
      # --- Font ---
      text = element_text(
        family = font,
        size = base_size,
        # color = "#330066",
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = ggplot2::margin(),
        debug = FALSE
      ),
      
      # --- Axis ---
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = ggplot2::element_line(color = "grey92"),
      axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
      axis.text = element_text(
        family = font,
        size = font_sizes$axis_text,
        color = "grey30",
        margin = ggplot2::margin(t = 0.8 * half_line / 2)
      ),
      axis.text.x = element_text(vjust = 0.5),
      axis.text.y = element_text(hjust = 1),
      
      # --- Legend ---
      legend.background = ggplot2::element_blank(),
      legend.spacing = ggplot2::unit(2 * half_line, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = ggplot2::margin(half_line,
                                      half_line,
                                      half_line,
                                      half_line),
      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.position = "right",# or "none" ?
      legend.text = element_text(size = font_sizes$legend_text),
      legend.title = ggtext::element_markdown(size = font_sizes$legend_title, hjust = 0),
      
      # --- Faceting ---
      strip.background = element_rect(fill = "#CCD2D4", colour = "transparent"),
      strip.text  = element_text(
        size = font_sizes$strip,
        hjust = 0,
        margin = ggplot2::margin(0.4 * half_line,
                                 0.4 * half_line,
                                 0.4 * half_line,
                                 0.4 * half_line)
      ),
      
      # --- Panel lines ---
      panel.grid = ggplot2::element_line(colour = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(half_line, "pt"),
      panel.grid.major.y = ggplot2::element_line(linewidth = ggplot2::rel(1)),
      
      # --- Title and Subtitle ---
      # plot.title.position = "plot",
      plot.title = ggtext::element_markdown(
        family = font,
        lineheight = 1.1,
        size = font_sizes$title,
        hjust = 0,
        vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        family = font,
        size = font_sizes$subtitle,
        hjust = 0,
        vjust = 1,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.margin = ggplot2::margin(15, 15, 10, 15), #top, right, bottom, left
      # --- Caption ---
      plot.caption = element_text(
        family = font,
        hjust = 1,
        vjust = 1,
        face = "italic",
        size = font_sizes$caption,
        color = "grey30",
        margin = ggplot2::margin(t = half_line)
      ),
      plot.title.position = "panel",
      plot.caption.position =  "panel"
    )
}