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
#' visualize_true_color_imagery_batch(
#'   dir = "alldata/PSdata/",
#'   df_coordinates = df_coordinates,
#'   cloud_lim = 0.1
#' )
#' }
#'
#' @import ggplot2
#' @import shiny
#' @import shinyWidgets
#'
#' @export
visualize_true_color_imagery_batch <- function(dir, df_coordinates = NULL, cloud_lim = 1) {
  v_site <- list.dirs(file.path(dir, "raw"), recursive = F, full.names = F)

  raster_metadata <- get_raster_metadata(dir, v_site)

  # Filter by cloud cover
  raster_metadata <- raster_metadata |> dplyr::filter(cloud_cover <= cloud_lim)

  global_brightness_lookup <- lapply(unique(raster_metadata$site), function(s) {
    data <- raster_metadata |> dplyr::filter(site == s)
    brightness <- estimate_global_brightness(data)
    tibble::tibble(site = s, brightness = brightness)
  }) |> dplyr::bind_rows()

  ui <- fluidPage(
    # titlePanel("PlanetScope true color imagery viewer"),
    tags$style(HTML(".row { margin-bottom: -10px; } .shiny-plot-output { margin-top: -20px; }")),

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
        plotOutput("raster_plot", width = "auto", height = "600px")
      )
    )
  )

  server <- function(input, output, session) {
    # Update date options based on selected site
    output$date_ui <- renderUI({
      req(input$site)

      raster_metadata_site <- raster_metadata |>
        dplyr::filter(site == input$site)

      v_datetime <- raster_metadata_site |>
        dplyr::arrange(datetime) |>
        dplyr::pull(datetime)

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
    output$raster_plot <- renderPlot(
      {
        req(input$site, input$datetime, input$brightness)

        raster_metadata_site <- raster_metadata |>
          dplyr::filter(site == input$site)

        global_brightness <- global_brightness_lookup |>
          dplyr::filter(site == input$site) |>
          dplyr::pull(brightness)

        df_coordinates_site <- if (!is.null(df_coordinates)) {
          df_coordinates |> dplyr::filter(site == input$site)
        } else {
          NULL
        }

        selected_file <- raster_metadata_site |>
          dplyr::filter(datetime == input$datetime) |>
          dplyr::pull(file) |>
          dplyr::first()

        visualize_true_color_imagery(
          file = selected_file,
          df_coordinates = df_coordinates_site,
          brightness = input$brightness,
          global_brightness = global_brightness
        )
      },
      height = 500,
      width = function() {
        if (is.null(input$datetime)) {
          return(500)
        }
        selected_file <- raster_metadata |>
          dplyr::filter(site == input$site, datetime == input$datetime) |>
          dplyr::pull(file) |>
          dplyr::first()

        ras <- terra::rast(selected_file)
        ext <- terra::ext(ras)
        aspect_ratio <- (ext[2] - ext[1]) / (ext[4] - ext[3])
        return(as.numeric(500 * aspect_ratio))
      }
    )
  }

  shinyApp(ui, server)
}

# Helper to parse site and date from directory structure and extract cloud cover
get_raster_metadata <- function(dir, v_site) {
  ls_df_metadata <- list()
  for (siteoi in v_site) {
    raster_files <- list.files(file.path(dir, "raw", siteoi), pattern = "\\SR_harmonized_clip.tif$", recursive = TRUE, full.names = TRUE)
    meta_files <- list.files(file.path(dir, "raw", siteoi), pattern = "\\metadata.json$", recursive = TRUE, full.names = TRUE)

    # Throw an error if counts don't match
    if (length(raster_files) != length(meta_files)) {
      stop(sprintf(
        "Data mismatch for site '%s': Found %d raster files but %d metadata files. Every raster must have a corresponding metadata.json.",
        siteoi, length(raster_files), length(meta_files)
      ), call. = FALSE)
    }

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

    ls_df_metadata[[siteoi]] <- tibble::tibble(
      file = raster_files,
      date = stringr::str_extract(basename(file), "\\d{8}"), # Extract 8-digit date
      time = stringr::str_extract(basename(file), "(?<=_)\\d{6}"), # Extract 6-digit time
      cloud_cover = cloud_covers
    ) |>
      dplyr::mutate(datetime = as.POSIXct(paste(date, time), format = "%Y%m%d %H%M%S", tz = "UTC")) |>
      dplyr::mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) |>
      dplyr::select(-date, -time) |>
      dplyr::mutate(site = siteoi)
  }
  df_metadata <- dplyr::bind_rows(ls_df_metadata)
  return(df_metadata)
}

# Helper function to estimate global average brightness from sampled images
estimate_global_brightness <- function(raster_metadata) {
  # Sample up to 100 images to estimate average brightness (after cloud filter)
  if (nrow(raster_metadata) > 0) {
    set.seed(42)
    sample_files <- raster_metadata |> dplyr::sample_n(min(100, nrow(raster_metadata)))
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
#' @import ggplot2
#' @export
visualize_true_color_imagery <- function(file, df_coordinates = NULL, brightness = 5, global_brightness = 0.05) {
  ras <- terra::rast(file) |>
    terra::project("EPSG:4326")

  df_ras <- ras |>
    as.data.frame(xy = T) |>
    tibble::as_tibble() |>
    dplyr::select(
      b = blue,
      g = green,
      r = red,
      x,
      y
    )

  df_ras <- df_ras |>
    dplyr::mutate(across(c(r, g, b), ~ . * 0.0001)) |>
    dplyr::mutate(across(c(r, g, b), ~ . * brightness * 0.05 / global_brightness)) |>
    dplyr::mutate(across(c(r, g, b), ~ pmax(., 0))) |>
    dplyr::mutate(across(c(r, g, b), ~ pmin(., 1))) |>
    dplyr::mutate(rgb = rgb(r, g, b, maxColorValue = 1))

  p <- ggplot(df_ras) +
    geom_tile(aes(x = x, y = y, fill = rgb)) +
    scale_fill_identity() +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  if (!is.null(df_coordinates)) {
    p <- p +
      geom_point(data = df_coordinates, aes(x = lon, y = lat), pch = 1, alpha = 0.8, color = "red")
  }

  return(p)
}

#' Visualize coordinates from a data frame
#'
#' Creates a scatter plot of coordinate data.
#' @param df_coordinates Data frame containing longitude and latitude columns.
#'
#' @return An interactive `leaflet` map object when supported, a static `ggplot` object otherwise.
#'
#' @examples
#' \dontrun{
#' visualize_coordinates(df_coordinates)
#' }
#'
#' @import ggplot2
#' @export
visualize_coordinates <- function(df_coordinates) {
  # Validate required columns
  if (!all(c("lon", "lat") %in% names(df_coordinates))) {
    stop("Data frame must contain 'lon' and 'lat' columns.")
  }

  df_coordinates <- df_coordinates |> tidyr::drop_na(lat, lon)

  if (interactive()) {
    p <- leaflet::leaflet(df_coordinates) |>
      # leaflet::addTiles() |>   # Adds standard map background
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) |>
      leaflet::addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = 3,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.7,
        label = ~ paste0("ID: ", id, " (", lon, ", ", lat, ")")
      )
  } else { # static ggplot
    p <- ggplot(df_coordinates, aes(
      x = lon, y = lat,
      text = stringr::str_c("ID: ", id, "<br>Longitude: ", lon, "<br>Latitude: ", lat)
    )) +
      geom_point(size = 0.5, color = "blue", alpha = 0.7) +
      labs(x = "Longitude", y = "Latitude")

    p <- apply_plot_style(p)
  }

  p
}
