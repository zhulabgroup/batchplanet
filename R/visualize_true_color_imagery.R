#' Launch a Shiny App to Visualize True-Color Imagery
#'
#' Starts an interactive Shiny app for browsing and visualizing true-color PlanetScope imagery in a directory.
#'
#' @param dir Character. Base directory containing raw imagery (expects a `raw/` subdirectory).
#' @param df_coordinates Optional data frame. Point locations to overlay; must contain `lon`, `lat`, and `site`. Default: `NULL`.
#' @param cloud_lim Numeric. Maximum allowed cloud cover (0-1, default: 1). Only images with cloud cover <= cloud_lim are shown.
#'
#' @return None. Launches a Shiny app in the default web browser.
#'
#' @examples
#' \dontrun{
#' visualize_true_color_imagery_batch(dir = "alldata/PSdata/", df_coordinates = df_coordinates, cloud_lim = 0.1)
#' }
#'
#' @export
visualize_true_color_imagery_batch <- function(dir, df_coordinates = NULL, cloud_lim = 1) {
  library(shiny)
  library(shinyWidgets)

  v_site <- list.dirs(file.path(dir, "raw"), recursive = F, full.names = F)

  raster_metadata <- get_raster_metadata(dir)
  # Filter by cloud cover
  raster_metadata <- raster_metadata %>% filter(cloud_cover <= cloud_lim)

  init_brightness <- estimate_initial_brightness(raster_metadata)

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
        sliderInput("brightness", "Brightness", min = 0, max = 10, value = init_brightness, step = 0.5, width = "100%", ticks = F)
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
        brightness = input$brightness,
        global_brightness = init_brightness
      )
    })
  }

  shinyApp(ui, server)
}

# Helper to parse site and date from directory structure and extract cloud cover
get_raster_metadata <- function(dir) {
  ls_df_metadata <- list()
  for (siteoi in v_site) {
    raster_files <- list.files(file.path(dir, "raw", siteoi), pattern = "\\SR_harmonized_clip.tif$", recursive = TRUE, full.names = TRUE)
    meta_files <- gsub("SR_harmonized_clip.tif$", "metadata.json", raster_files)
    cloud_covers <- sapply(meta_files, function(mf) {
      if (file.exists(mf)) {
        meta <- tryCatch(jsonlite::fromJSON(mf), error = function(e) NULL)
        if (!is.null(meta) && !is.null(meta$properties$cloud_cover)) {
          as.numeric(meta$properties$cloud_cover)
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    })
    ls_df_metadata[[siteoi]] <- tibble(
      file = raster_files,
      date = str_extract(basename(file), "\\d{8}"), # Extract 8-digit date
      time = str_extract(basename(file), "(?<=_)\\d{6}"), # Extract 6-digit time
      cloud_cover = cloud_covers
    ) %>%
      mutate(datetime = as.POSIXct(paste(date, time), format = "%Y%m%d %H%M%S", tz = "UTC")) %>%
      mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) %>%
      select(-date, -time) %>%
      mutate(site = siteoi)
  }
  df_metadata <- bind_rows(ls_df_metadata)
  return(df_metadata)
}

estimate_initial_brightness <- function(raster_metadata) {
  # Sample up to 100 images to estimate average brightness (after cloud filter)
  if (nrow(raster_metadata) > 0) {
    sample_files <- raster_metadata %>% sample_n(min(100, nrow(raster_metadata)))
    brightness_vals <- c()
    for (i in seq_len(nrow(sample_files))) {
      file <- sample_files$file[i]
      ras <- tryCatch(terra::rast(file), error = function(e) NULL)
      if (!is.null(ras)) {
        ras <- terra::project(ras, "EPSG:4326")
        df_ras <- as.data.frame(ras, xy = TRUE)
        colnames(df_ras)[3:6] <- c("blue", "green", "red", "nir")
        if (nrow(df_ras) > 0) {
          mean_brightness <- mean((df_ras$red + df_ras$green + df_ras$blue) * 0.0001 / 3, na.rm = TRUE)
          brightness_vals <- c(brightness_vals, mean_brightness)
        }
      }
    }
    init_brightness <- ifelse(length(brightness_vals) > 0, round(mean(brightness_vals, na.rm = TRUE) * 10, 1), 5)
    init_brightness <- min(max(init_brightness, 0), 10)
  } else {
    init_brightness <- 5
  }
  return(init_brightness)
}
  
#' Visualize true-color raster imagery
#'
#' Reads a multi-band raster, converts the red, green, and blue bands to normalized RGB values, and creates a ggplot2 tile plot of the true-color composite. Optionally overlays point locations. Automatically normalizes image brightness to the global average (computed at app startup) for legibility, then applies the user brightness slider (0-10, default 5) as a multiplier.
#'
#' @param file Character. Path to a multi-band raster file (e.g., PlanetScope SR clip).
#' @param df_coordinates Optional data frame. Point locations to overlay; must contain `lon`, `lat`, and `site`. Default: `NULL`.
#' @param brightness Numeric. Brightness multiplier for RGB values (slider value, default: 5).
#' @param global_brightness Numeric. The global average brightness (from sampled images at app startup).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' visualize_true_color_imagery(
#'   file = "raw/SJER/20240501_SR_harmonized_clip.tif",
#'   df_coordinates = df_coordinates_SJER,
#'   brightness = 5,
#'   global_brightness = 0.5
#' )
#' }
#'
#' @export
visualize_true_color_imagery <- function(file, df_coordinates = NULL, brightness = 5, global_brightness = 0.5) {
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
    )

  # Calculate mean brightness (0-1 scale) for this image
  mean_brightness <- mean((df_ras$r + df_ras$g + df_ras$b) * 0.0001 / 3, na.rm = TRUE)
  # Avoid division by zero
  if (is.na(mean_brightness) || mean_brightness == 0) mean_brightness <- 0.01
  # Compute normalization multiplier to match global average
  auto_mult <- global_brightness / mean_brightness
  # User slider: 0-10, default 5, so scale is brightness/5
  user_mult <- brightness / 5
  final_mult <- auto_mult * user_mult

  df_ras <- df_ras %>%
    mutate(across(c(r, g, b), ~ . * 0.0001 * final_mult)) %>%
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
