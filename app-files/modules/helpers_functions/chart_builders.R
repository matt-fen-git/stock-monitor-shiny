# Chart construction helper functions

#' Get dynamic chart dimensions based on data size
get_chart_dimensions <- function(num_points) {
  height <- if (num_points < 50) 600 
            else if (num_points < 100) 700 
            else 800
  
  min_range <- if (num_points < 50) 24 * 3600 * 1000 * 3 
               else 24 * 3600 * 1000 * 7
  
  list(height = height, min_range = min_range)
}

#' Calculate panel configuration
get_panel_config <- function(selected_indicators) {
  num_panels <- 3
  if ("obv" %in% selected_indicators || "mfi" %in% selected_indicators) {
    num_panels <- num_panels + 1
  }
  if ("adx" %in% selected_indicators) num_panels <- num_panels + 1
  
  heights <- switch(as.character(num_panels),
                   "3" = c(3, 1, 1),
                   "4" = c(3, 1, 1, 1),
                   "5" = c(3, 1, 1, 1, 1),
                   c(3, 1, 1))
  
  list(num_panels = num_panels, heights = heights)
}
