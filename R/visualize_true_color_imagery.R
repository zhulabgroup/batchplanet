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

  raster_metadata <- get_raster_metadata(dir, v_site)

  # Filter by cloud cover
  raster_metadata <- raster_metadata %>% filter(cloud_cover <= cloud_lim)

  global_brightness_lookup <- lapply(unique(raster_metadata$site), function(s) {
    data <- raster_metadata %>% filter(site == s)
    brightness <- estimate_global_brightness(data)
    tibble(site = s, brightness = brightness)
  }) %>% bind_rows()

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
      req(input$site)

      raster_metadata_site <- raster_metadata %>%
        filter(site == input$site)

      v_datetime <- raster_metadata_site %>%
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
      req(input$site, input$datetime, input$brightness)

      raster_metadata_site <- raster_metadata %>%
        filter(site == input$site)

      global_brightness <- global_brightness_lookup %>%
        filter(site == input$site) %>%
        pull(brightness)

      df_coordinates_site <- if (!is.null(df_coordinates)) {
        df_coordinates %>% filter(site == input$site)
      } else {
        NULL
      }

      selected_file <- raster_metadata_site %>%
        filter(datetime == input$datetime) %>%
        pull(file) %>%
        first()

      visualize_true_color_imagery(
        file = selected_file,
        df_coordinates = df_coordinates_site,
        brightness = input$brightness,
        global_brightness = global_brightness
      )
    })
  }

  shinyApp(ui, server)
}

# Helper to parse site and date from directory structure and extract cloud cover
#' @export
get_raster_metadata <- function(dir, v_site) {
  ls_df_metadata <- list()
  for (siteoi in v_site) {
    raster_files <- list.files(file.path(dir, "raw", siteoi), pattern = "\\SR_harmonized_clip.tif$", recursive = TRUE, full.names = TRUE)
    meta_files <- list.files(file.path(dir, "raw", siteoi), pattern = "\\metadata.json$", recursive = TRUE, full.names = TRUE)
    cloud_covers <- sapply(meta_files, function(mf) {
      if (file.exists(mf)) {
        meta <- tryCatch(jsonlite::fromJSON(mf), error = function(e) NULL)
        if (!is.null(meta) && !is.null(meta$properties$cloud_cover)) {
          as.numeric(meta$properties$cloud_cover)
        } else {
          NA
        }
      } else {
        NA
      }
    })

    if (length(raster_files) == 0) {
      next # Skip if no raster files found for this site
    }

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

# Helper function to estimate global average brightness from sampled images
#' @export
estimate_global_brightness <- function(raster_metadata) {
  # Sample up to 100 images to estimate average brightness (after cloud filter)
  if (nrow(raster_metadata) > 0) {
    set.seed(42)
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
          if (mean_brightness <= 0.2) {
            brightness_vals <- c(brightness_vals, mean_brightness)
          }
        }
      }
    }
    global_brightness <- mean(brightness_vals, na.rm = TRUE)
    # # cap at 0.1 in case most images are still cloudy
    # global_brightness <- min(global_brightness, 0.1)
  } else {
    global_brightness <- NA
  }
  return(global_brightness)
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
#'   file = "alldata/PSdata/raw/SJER/20240501_SR_harmonized_clip.tif",
#'   df_coordinates = df_coordinates_SJER,
#'   brightness = 5,
#'   global_brightness = 0.05
#' )
#' }
#'
#' @export
visualize_true_color_imagery <- function(file, df_coordinates = NULL, brightness = 5, global_brightness = 0.05) {
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

  df_ras <- df_ras %>%
    mutate(across(c(r, g, b), ~ . * 0.0001)) %>%
    mutate(across(c(r, g, b), ~ . * brightness * 0.05 / global_brightness)) %>%
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
      geom_point(data = df_coordinates, aes(x = lon, y = lat), pch = 1, alpha = 0.8, color = "yellow") +
      coord_equal()
  }

  return(p)
}

#' Visualize coordinates from a data frame
#'
#' Creates a scatter plot of coordinate data.
#' @param df_coordinates Data frame containing longitude and latitude columns.
#'
#' @return A ggplot object displaying the coordinate points.
#'
#' @examples
#' \dontrun{
#' visualize_coordinates(df_coordinates)
#' }
#'
#' @export
visualize_coordinates <- function(df_coordinates) {
  # Validate required columns
  if (!all(c("lon", "lat") %in% names(df_coordinates))) {
    stop("Data frame must contain 'lon' and 'lat' columns.")
  }

  p <- ggplot(df_coordinates, aes(
    x = lon, y = lat,
    text = str_c("ID: ", id, "<br>Longitude: ", lon, "<br>Latitude: ", lat)
  )) +
    geom_point(size = 0.5) +
    labs(x = "Longitude", y = "Latitude")

  p <- apply_plot_style(p)

  plotly::ggplotly(p, tooltip = "text")
}
