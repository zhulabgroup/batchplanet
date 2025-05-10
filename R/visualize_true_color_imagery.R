#' Visualize Multi-Band Raster Data
#'
#' Creates an RGB composite visualization of a multi-band raster.
#'
#' @param path Character. Path to the raster file.
#' @param crop_shape Optional. An `sf` or `SpatVector` object to crop the raster.
#' @param brightness Numeric. Brightness multiplier (default: 1).
#' @param bands Character vector. Names of the RGB bands (default: c("red", "green", "blue")).
#' @param save_plot Logical. If TRUE, saves the plot.
#' @param plot_path Character. Directory in which to save the plot (default: "plots").
#' @param color_palette Character. The palette option for styling (default: "colorblind").
#'
#' @return A ggplot object displaying the RGB composite.
#' @export
visualize_true_color_imagery <- function(file, df_coordinates = NULL, brightness = 5) {
  ras <- terra::rast(file) %>%
    terra::project("EPSG:4326")

  df_ras <- ras %>%
    as.data.frame(xy = T) %>%
    as_tibble() %>%
    select(
      b = blue,
      g = green,
      r = red,
      x,
      y
    ) %>%
    mutate(across(c(r, g, b), ~ . * 0.0001 * brightness)) %>%
    mutate(across(c(r, g, b), ~ pmax(., 0))) %>%
    mutate(across(c(r, g, b), ~ pmin(., 1))) %>%
    mutate(rgb = rgb(r, g, b, maxColorValue = 1))

  p <- ggplot(df_ras) +
    geom_tile(aes(x = x, y = y, fill = rgb)) +
    scale_fill_identity() +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal()

  if (!is.null(df_coordinates)) {
    p <- p +
      geom_point(data = df_coordinates, aes(x = lon, y = lat), pch = 1, alpha = 0.8, color = "yellow")
  }

  return(p)
}

#' @export
visualize_true_color_imagery_batch <- function(dir, df_coordinates = NULL) {
  library(shiny)
  library(shinyWidgets)

  v_site <- list.dirs(file.path(dir, "raw"), recursive = F, full.names = F)
  # Helper to parse site and date from directory structure
  get_raster_metadata <- function(dir) {
    ls_df_metadata <- list()
    for (siteoi in v_site) {
      raster_files <- list.files(file.path(dir, "raw", siteoi), pattern = "\\SR_harmonized_clip.tif$", recursive = TRUE, full.names = TRUE)
      ls_df_metadata[[siteoi]] <- tibble(
        file = raster_files,
        date = str_extract(basename(file), "\\d{8}"), # Extract 8-digit date
        time = str_extract(basename(file), "(?<=_)\\d{6}") # Extract 6-digit time
      ) %>%
        mutate(datetime = as.POSIXct(paste(date, time), format = "%Y%m%d %H%M%S", tz = "UTC")) %>%
        mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) %>%
        select(-date, -time) %>%
        mutate(site = siteoi)
    }
    df_metadata <- bind_rows(ls_df_metadata)
    return(df_metadata)
  }

  raster_metadata <- get_raster_metadata(dir)

  ui <- fluidPage(
    titlePanel("PlanetScope true color imagery viewer"),

    # First row: site + brightness
    fluidRow(
      column(
        6,
        selectInput("site", "Select Site:", choices = v_site, width = "100%")
      ),
      column(
        6,
        sliderInput("brightness", "Brightness", min = 0, max = 10, value = 5, step = 0.5, width = "100%", ticks = F)
      )
    ),

    # Second row: time slider
    fluidRow(
      column(
        12,
        uiOutput("date_ui")
      )
    ),

    # Third row: plot
    fluidRow(
      column(
        12,
        plotOutput("raster_plot", height = "700px")
      )
    )
  )

  server <- function(input, output, session) {
    # Update date options based on selected site
    output$date_ui <- renderUI({
      v_datetime <- raster_metadata %>%
        filter(site == input$site) %>%
        arrange(datetime) %>%
        pull(datetime)

      shinyWidgets::sliderTextInput("datetime", "Select Date and Time:",
        choices = v_datetime,
        selected = v_datetime[1],
        grid = F,
        animate = animationOptions(
          interval = 1500,
          loop = FALSE
        ),
        width = "100%"
      )
      # selectInput("datetime", "Select Date and Time:", choices = v_datetime)
    })

    # Generate raster visualization
    output$raster_plot <- renderPlot({
      req(input$site, input$datetime)

      selected_file <- raster_metadata %>%
        filter(site == input$site, datetime == input$datetime) %>%
        pull(file) %>%
        first()

      if (!is.null(df_coordinates)) {
        df_coordinates_site <- df_coordinates %>% filter(site == input$site)
      }

      visualize_true_color_imagery(
        file = selected_file,
        df_coordinates = df_coordinates_site,
        brightness = input$brightness
      )
    })
  }

  shinyApp(ui, server)
}
