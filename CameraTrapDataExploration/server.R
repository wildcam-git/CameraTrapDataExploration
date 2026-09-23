#
#

server <- function(input, output, session) {
  
  
  # Upload Data ----------------------------------------------------------------------------------------------------------
  
  # Reactive expression to read in the data files provided by the user
  data_store <- reactiveValues(dfs = list())
  analysis_data_store <- reactiveValues(results = list())
  results_store <- reactiveValues(data = NULL)
  # An a flag to show when the results are ready for appropriate error messages
  results_ready <- reactiveVal(FALSE)
  button_clicked <- reactiveVal(FALSE)
  
  observeEvent(input$files, {
    
    # Ensure files exist
    req(input$files)
    
    # Show progress bar
    withProgress(message = 'Processing files', value = 0, {
      
      # List to store dataframes
      dfs <- list()
      
      incProgress(0.1, detail = "Reading uploaded files...")
      
      # Iterate through uploaded files
      for (i in seq_along(input$files$datapath)) {
        file_path <- input$files$datapath[i]
        file_name <- input$files$name[i]
        
        if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
          dfs[[file_name]] <- read_csv(file_path, show_col_types = FALSE)
        } else if (grepl("\\.zip$", file_name, ignore.case = TRUE)) {
          temp_dir <- file.path(tempdir(), paste0("unzipped_", i))
          dir.create(temp_dir, showWarnings = FALSE)
          unzip(file_path, exdir = temp_dir)
          
          csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
          
          if (length(csv_files) == 0) {
            message("No CSV files found in ZIP.")
          }
          
          for (csv in csv_files) {
            dfs[[basename(csv)]] <- read_csv(csv, show_col_types = FALSE)
          }
        }
      }
      
      incProgress(0.2, detail = "Merging image files...")
      
      # Merge Wildlife Insights image files
      images_data <- lapply(names(dfs)[grepl("^images_[0-9]", names(dfs))], function(name) dfs[[name]])
      
      if (length(images_data) > 0) {
        if (!"images.csv" %in% names(dfs)) {
          dfs[["images.csv"]] <- bind_rows(images_data)
        }
        dfs <- dfs[!grepl("^images_[0-9]", names(dfs))]
      }
      
      incProgress(0.3, detail = "Detecting data format...")
      
      # HOUSEKEEPING - process based on which files are present
      
      # Wildlife Insights format
      if (all(c("images.csv", "deployments.csv") %in% names(dfs)) &&
          !all(c("stations.csv", "tags.csv") %in% names(dfs)) &&
          !any(str_detect(names(dfs), "main_report.csv$"))) {
        
        incProgress(0.1, detail = "Processing Wildlife Insights format...")
        
        dfs[["deployments.csv"]]$project_id <- dfs[["projects.csv"]]$project_name[1] 
        dfs[["images.csv"]]$sp <- str_replace_all(paste(dfs[["images.csv"]]$genus, dfs[["images.csv"]]$species),"\\s+", ".")
        dfs[["images.csv"]]$timestamp <- ymd_hms(dfs[["images.csv"]]$timestamp)
        dfs[["images.csv"]] <- dfs[["images.csv"]] |>
          mutate(is_blank = coalesce(as.integer(is_blank),
                                     as.integer(common_name %in% c("Blank"))))
        
        dfs[["deployments.csv"]]$start_date <- ymd(dfs[["deployments.csv"]]$start_date)
        dfs[["deployments.csv"]]$end_date <- ymd(dfs[["deployments.csv"]]$end_date)
        dfs[["deployments.csv"]]$days <- interval(
          dfs[["deployments.csv"]]$start_date,
          dfs[["deployments.csv"]]$end_date
        ) / ddays(1)
        
        dfs[["images.csv"]] <- left_join(
          dfs[["images.csv"]],
          dfs[["deployments.csv"]][, c("deployment_id", "placename")],
          by = "deployment_id"
        )
        
        incProgress(0.3, detail = "Wildlife Insights processing complete!")
      }
      
      # WildTrax format
      if (any(str_detect(names(dfs), "main_report.csv$"))) {
        
        incProgress(0.1, detail = "Processing WildTrax format...")
        
        mr_idx <- grep("main_report.csv$", names(dfs))
        if (length(mr_idx) > 1) {
          dfs[["main_report.csv"]] <- dplyr::bind_rows(dfs[mr_idx])
          dfs <- dfs[-mr_idx[-1]]
        } else {
          names(dfs)[mr_idx] <- "main_report.csv"
        }
        
        incProgress(0.1, detail = "Creating deployments...")
        
        dfs[["deployments.csv"]] <- dfs[["main_report.csv"]] |>
          group_by(project_id, location, latitude, longitude) |>
          summarise(start_date = min(as.Date(image_date_time)),
                    end_date = max(as.Date(image_date_time))) |>
          mutate(deployment_id = paste0(location),
                 feature_type = "") |>
          select(project_id, deployment_id, placename = location, longitude, latitude, start_date, end_date, feature_type) |>
          ungroup() |>
          mutate(days = interval(start_date, end_date) / ddays(1))
        
        incProgress(0.1, detail = "Processing images...")
        
        dfs[["images.csv"]] <- dfs[["main_report.csv"]] |>
          filter(!species_common_name %in% c("STAFF/SETUP", "Human"),
                 !individual_count == "VNA") |>
          group_by(image_date_time) |>
          filter(row_number() == 1) |>
          ungroup() |>
          mutate(is_blank = ifelse(species_common_name == "NONE", 1, 0),
                 class = "Mammalia",
                 family = "",
                 genus = "",
                 order = "",
                 sp = str_replace_all(species_common_name, " |, ", "."),
                 individual_count = as.numeric(individual_count),
                 placename = location,
                 common_name = species_common_name) |>
          select(project_id,
                 deployment_id = location,
                 placename,
                 is_blank,
                 class,
                 species = species_common_name,
                 common_name,
                 sp,
                 timestamp = image_date_time,
                 number_of_objects = individual_count,
                 age = age_class,
                 sex = sex_class,
                 family, genus, order)
        
        reps <- c("main", "abstract", "image_report",
                  "image_set", "location", "megadetector",
                  "project", "tag", "definitions")
        
        dfs <- dfs[!str_detect(names(dfs), str_c(reps, collapse = "|"))]
        
        incProgress(0.1, detail = "WildTrax processing complete!")
      }
      
      # WildCo/Migrations format
      if (all(c("stations.csv", "tags.csv") %in% names(dfs))) {
        
        incProgress(0.1, detail = "Processing Migrations format...")
        
        tryCatch({
          
          if("images.csv" %in% names(dfs)) {
            dfs[["images_raw.csv"]] <- dfs[["images.csv"]]
            dfs[["images.csv"]] <- NULL
          }
          
          camera_checks <- dfs[["camera_checks.csv"]]
          stations <- dfs[["stations.csv"]]
          images_raw <- dfs[["images_raw.csv"]]
          tags <- dfs[["tags.csv"]]
          species <- dfs[["species.csv"]]
          
          incProgress(0.05, detail = "Creating deployments...")
          
          deployments_df <- camera_checks |>
            left_join(stations, by = c("station_id", "project_id")) |>
            rename(placename = station_id, feature_type = feature) |>
            mutate(
              project_id = as.character(project_id),
              start_date = ymd(substr(check_date, 1, 10)),
              end_date = ymd(substr(stop_date, 1, 10)),
              days = as.integer(difftime(end_date, start_date, units = "days")),
              deployment_id = paste0(placename, "_", start_date)
            ) |>
            arrange(placename, start_date)
          
          dfs[["deployments.csv"]] <- deployments_df
          
          rm(camera_checks, stations)
          gc()
          
          incProgress(0.05, detail = "Processing species tags...")
          
          tags_processed <- tags |>
            left_join(
              species |> select(species_id, order_taxo, family_taxo, genus_taxo, species_taxo, common_names),
              by = "species_id"
            )
          
          rm(tags, species)
          gc()
          
          incProgress(0.1, detail = paste("Processing", format(nrow(images_raw), big.mark=","), "images..."))
          
          chunk_size <- 50000
          n_rows <- nrow(images_raw)
          
          if(n_rows > chunk_size) {
            images_chunks <- list()
            n_chunks <- ceiling(n_rows / chunk_size)
            
            for(i in seq(1, n_rows, by = chunk_size)) {
              end_idx <- min(i + chunk_size - 1, n_rows)
              chunk_num <- ceiling(i / chunk_size)
              
              incProgress(0.1 / n_chunks, 
                          detail = paste("Processing images chunk", chunk_num, "of", n_chunks))
              
              chunk <- images_raw[i:end_idx, ] |>
                left_join(tags_processed, by = c("_id" = "image_id")) |>
                filter(!is.na(species_id)) |>
                mutate(
                  placename = station_id,
                  timestamp = ymd_hms(exif_timestamp),
                  sp = gsub("\\s+", "", paste0(genus_taxo, ".", species_taxo)),
                  number_of_objects = as.integer(coalesce(as.numeric(species_count), 1)),
                  image_date = as.Date(timestamp),
                  common_name = common_names,
                  genus = tolower(genus_taxo),
                  species = tolower(species_taxo),
                  order = tolower(order_taxo),
                  family = tolower(family_taxo)
                ) |>
                select(placename, timestamp, image_date, sp, number_of_objects, 
                       common_name, genus, species, order, family, age_category, sex)
              
              images_chunks[[length(images_chunks) + 1]] <- chunk
              gc()
            }
            
            images_processed <- bind_rows(images_chunks)
            rm(images_chunks)
            gc()
            
          } else {
            images_processed <- images_raw |>
              left_join(tags_processed, by = c("_id" = "image_id")) |>
              filter(!is.na(species_id)) |>
              mutate(
                placename = station_id,
                timestamp = ymd_hms(exif_timestamp),
                sp = gsub("\\s+", "", paste0(genus_taxo, ".", species_taxo)),
                number_of_objects = as.integer(coalesce(as.numeric(species_count), 1)),
                image_date = as.Date(timestamp),
                common_name = common_names,
                genus = tolower(genus_taxo),
                species = tolower(species_taxo),
                order = tolower(order_taxo),
                family = tolower(family_taxo)
              ) |>
              select(placename, timestamp, image_date, sp, number_of_objects, 
                     common_name, genus, species, order, family, age_category, sex)
          }
          
          rm(images_raw, tags_processed)
          gc()
          
          incProgress(0.1, detail = "Linking images to deployments...")
          
          require(data.table)
          
          images_dt <- as.data.table(images_processed)
          deployments_dt <- as.data.table(deployments_df)
          
          images_dt[, deployment_id := as.character(NA)]
          
          unique_stations <- unique(images_dt$placename)
          n_stations <- length(unique_stations)
          
          for(station_idx in seq_along(unique_stations)) {
            station <- unique_stations[station_idx]
            
            if(station_idx %% 10 == 0) {  # Update every 10 stations
              incProgress(0.1 / n_stations * 10, 
                          detail = paste("Linking station", station_idx, "of", n_stations))
            }
            
            station_deps <- deployments_dt[placename == station][order(start_date)]
            
            if(nrow(station_deps) == 0) next
            
            images_dt[placename == station, deployment_id := {
              dep_ids <- character(.N)
              
              for(i in seq_len(.N)) {
                img_date <- image_date[i]
                
                within_idx <- which(img_date >= station_deps$start_date & 
                                      img_date <= station_deps$end_date)
                
                if(length(within_idx) > 0) {
                  dep_ids[i] <- station_deps$deployment_id[within_idx[1]]
                } else if(img_date < station_deps$start_date[1]) {
                  dep_ids[i] <- station_deps$deployment_id[1]
                } else {
                  past_idx <- which(station_deps$end_date < img_date)
                  if(length(past_idx) > 0) {
                    dep_ids[i] <- station_deps$deployment_id[max(past_idx)]
                  } else {
                    dep_ids[i] <- station_deps$deployment_id[1]
                  }
                }
              }
              dep_ids
            }]
          }
          
          incProgress(0.05, detail = "Finalizing data structure...")
          
          dfs[["images.csv"]] <- images_dt |>
            as.data.frame() |>
            mutate(
              placename = as.character(placename),
              deployment_id = as.character(deployment_id),
              sp = as.factor(sp),
              common_name = as.factor(common_name),
              genus = as.factor(genus),
              species = as.factor(species),
              order = as.factor(order),
              family = as.factor(family),
              project_id = as.character(deployments_df$project_id[1]),
              class = "Mammalia",
              is_blank = 0L
            ) |>
            rename(age = age_category) |>
            select(project_id, deployment_id, placename, is_blank, class, order, 
                   family, genus, species, common_name, sp, timestamp, 
                   number_of_objects, age, sex)
          
          rm(images_dt, deployments_dt, images_processed)
          gc()
          
          incProgress(0.05, detail = "Migrations processing complete!")
          
          wildco_files <- c("camera_checks.csv", "stations.csv", "tags.csv", 
                            "species.csv", "images_raw.csv")
          dfs <- dfs[!names(dfs) %in% wildco_files]
          
          gc()
          
        }, error = function(e) {
          message("Migrations processing error: ", e$message)
          print(e)
          showNotification(paste("Error processing Migrations data:", e$message), 
                           type = "error", duration = NULL)
        })
      }
      
      incProgress(0.05, detail = "Upload complete!")
      
      # Store in reactive values
      isolate({
        if (length(dfs) > 0) {
          data_store$dfs <- dfs
        } else {
          data_store$dfs <- list()
        }
      })
      
    })  # Close withProgress
    
  })  # Close observeEvent
  
  # Clear data button
  observeEvent(input$clear_files, {
    
    data_store$dfs <- list()
    
    shinyjs::reset("files")
    
  })
  
  # Dropdown to select a file to preview
  output$selected_file <- renderUI({
    
    req(length(data_store$dfs) > 0)  # Ensure data exists
    
    selectInput("choice", "Select a File to Preview",
                choices = names(data_store$dfs),
                selected = names(data_store$dfs)[1])
    
  })
  
  # Render a preview of the selected file
  output$custom_data_preview <- renderDT({
    
    req(input$choice, data_store$dfs)
    
    DT::datatable(data_store$dfs[[input$choice]],
                  options = list(scrollX = TRUE))
    
  })
  
  
  # Reactive Map ------------------------------------------------------------------------
  output$map <- renderLeaflet({
    validate(
      need(data_store$dfs[["deployments.csv"]], 
           "Please upload deployment data to view this content")
    )
    #req(data_store())
    deployments<- data_store$dfs[["deployments.csv"]]
    locs <- deployments |>
      select(placename, longitude, latitude) |>
      distinct() |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
    #specify the station label 
    locs$label_text <- paste0(
      locs$placename, "<br>",
      "Lat: ", round(st_coordinates(locs)[,1], 5), "<br>",
      "Long: ", round(st_coordinates(locs)[,2], 5)
    )
    
    cam <- makeAwesomeIcon(
      icon = "camera",
      iconColor = "black",
      library = "ion",
      markerColor = "white"
    )
    
    locs |>
      leaflet() |>
      # Add a satellite image layer
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") |>  
      addProviderTiles(providers$Esri.WorldTopoMap, group="Base") |>    
      addAwesomeMarkers(label = ~lapply(label_text, HTML),
                        icon = cam) |>
      # add a layer control box to toggle between the layers
      addLayersControl(
        baseGroups = c("Satellite", "Base"),
        options = layersControlOptions(collapsed = FALSE)) |>
      #Add a scale bar
      addScaleBar(position = "topleft")
    
    
  })
  
  # Deployment check -----------------------------------------------------------
  
  output$deployment_dates <- renderPlotly({
    
    validate(
      need(data_store$dfs[["deployments.csv"]], 
           "Please upload deployment and image data to view this content")
    )
    
    # Call the saved data
    deployments <- data_store$dfs[["deployments.csv"]]
    images <- data_store$dfs[["images.csv"]]
    
    # Ensure character types (not factors)
    deployments$placename <- as.character(deployments$placename)
    deployments$deployment_id <- as.character(deployments$deployment_id)
    images$deployment_id <- as.character(images$deployment_id)
    
    # Create unique placenames and order them
    unique_places <- unique(deployments$placename)
    place_lookup <- setNames(seq_along(unique_places), unique_places)
    
    # Call the plot
    p <- plot_ly()
    
    # loop through each place name
    for(i in seq_along(unique_places)) {
      placename <- unique_places[i]
      
      # Subset the data to just that placename
      tmp <- deployments[deployments$placename == placename, ]
      # Order by date
      tmp <- tmp[order(tmp$start_date), ]
      
      # Loop through each deployment at that placename
      for(j in 1:nrow(tmp)) {
        # Add a box reflecting the first and last image for each deployment
        tmp_img <- images[images$deployment_id == tmp$deployment_id[j], ]
        
        # Only run if there are images
        if(nrow(tmp_img) > 0) {
          # Add an orange colour block to show when images were collected
          p <- add_trace(p,
                         x = c(min(tmp_img$timestamp), max(tmp_img$timestamp)),
                         y = c(i, i),
                         type = "scatter",
                         mode = "lines",
                         hovertext = paste0(tmp$deployment_id[j]),
                         color = I("orange"),
                         line = list(width = 8, opacity = 0.5),
                         showlegend = FALSE)
        }
        
        # Add a black line denoting the start and end periods of each deployment
        p <- add_trace(p, 
                       x = c(as.POSIXct(paste(tmp$start_date[j], "00:01:00"), format = "%Y-%m-%d %H:%M:%S"),
                             as.POSIXct(paste(tmp$end_date[j], "23:59:00"), format = "%Y-%m-%d %H:%M:%S")),
                       y = c(i, i), 
                       type = "scatter",
                       mode = "lines+markers", 
                       hovertext = tmp$deployment_id[j], 
                       color = I("black"),
                       marker = list(
                         symbol = c("circle", "triangle-left"),
                         size = 7,
                         color = c("black", "grey")),
                       showlegend = FALSE)
      }
    }
    
    # Add a categorical y axis
    p <- p %>% 
      layout(
        yaxis = list(
          ticktext = as.list(unique_places), 
          tickvals = as.list(1:length(unique_places)),
          tickmode = "array"),
        height = max(400, length(unique_places) * 20)
      ) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c('lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 
                                        'hoverCompareCartesian', 'zoomIn2d', 'zoomOut2d'))
    
    p
  })
  
  # Independent data creation  ---------------------------------------------------
  
  
  # Update the count dropdown dynamically based on available numeric inputs
  observe({
    # Get all integer columns
    integer_cols <- names(data_store$dfs[["images.csv"]])[
      sapply(data_store$dfs[["images.csv"]], function(col) is.numeric(col) && all(col == floor(col)))
    ]
    
    # Exclude specific columns
    integer_cols <- setdiff(integer_cols, c("project_id", "is_blank"))
    
    # Set default selection
    default_selection <- if("number_of_objects" %in% integer_cols) {
      "number_of_objects"
    } else if(length(integer_cols) > 0) {
      integer_cols[1]
    } else {
      NULL
    }
    
    updateSelectInput(session, "ind_count", 
                      choices = integer_cols,
                      selected = default_selection)
  })
  
  
  # Create waiter instance (for showing users that something is happening when they click on the button)
  w <- Waiter$new(id = "ind_run", html = spin_ellipsis(), color = "grey20")
  
  
  # Track when the button is clicked
  output$button_clicked <- reactive({
    button_clicked()
  })
  outputOptions(output, "button_clicked", suspendWhenHidden = FALSE)
  
  
  # When you click run on the ui, the following happens
  observeEvent(input$ind_run, {
    
    # Set button clicked flag
    button_clicked(TRUE)
    
    # Show spinner when processing starts
    w$show()
    
    # Run the create_ind_detect function on the imported data:
    # Ensure data exists
    req(length(data_store$dfs) > 0)
    
    # Convert threshold from chosen units to minutes
    unit_mult <- switch(input$ind_units,
                        sec = 1/60,
                        min = 1,
                        hr  = 60)
    
    thresh_minutes <- input$ind_thresh * unit_mult
    
    results <- create_ind_detect(deployments = data_store$dfs[["deployments.csv"]],
                                 images = data_store$dfs[["images.csv"]],
                                 threshold = thresh_minutes,   
                                 count_column = input$ind_count,
                                 include_classes = input$taxa_include,
                                 include_humans = input$include_humans,
                                 summaries = input$analysis_frames)
    
    # Subset results based on selected analysis frames
    frames <- input$analysis_frames
    if (is.null(frames)) frames <- character(0)
    
    # Store results in reactive value so they are accessible later!
    results_store$data <- results
    
    # Set the Results flag to TRUE
    results_ready(TRUE)
    
    # Hide spinner when processing is done
    w$hide()
    
    # Dropdown to select a file to preview
    output$selected_analysis_file <- renderUI({
      
      req(length(results_store$data) > 0)  # Ensure data exists
      
      selectInput("analysis_choice", "Select an analysis file to preview:",
                  choices = names(results_store$data),
                  selected = names(results_store$data)[1])
      
    })
    
    # Render a preview of the selected file
    output$custom_analysis_data_preview <- renderDT({
      
      req(input$analysis_choice, results_store$data)
      
      DT::datatable(results_store$data[[input$analysis_choice]],
                    options = list(scrollX = TRUE))
    })
    
    output$results_box <- renderUI({
      req(results_store$data)  # Only render when results exists
      
      box(
        title = "Independent data",
        width = 12,
        downloadButton("download_results", "Download All Datasets (.zip)"),
        p(),
        uiOutput("selected_analysis_file"),
        DTOutput("custom_analysis_data_preview")
        
      )
    })
    
    
    # Download a zipfile of the formatted data
    output$download_results <- downloadHandler(
      filename = function() {
        
        unit_mult <- switch(input$ind_units,
                            sec = 1/60,
                            min = 1,
                            hr  = 60)
        
        thresh_minutes <- input$ind_thresh * unit_mult
        
        paste0("ind_data_",  round(thresh_minutes, 2), "_mins_",
               format(Sys.Date(), "%Y%m%d"), ".zip")
        
      },
      content = function(file) {
        req(results_store$data)
        
        # Create a temporary directory
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        # Write each dataframe as a CSV file
        for (i in seq_along(results_store$data)) {
          csv_name <- paste0(names(results_store$data)[i], ".csv")
          csv_path <- file.path(temp_dir, csv_name)
          write.csv(results_store$data[[i]], csv_path, row.names = FALSE)
        }
        
        # Get list of files to zip
        csv_files <- list.files(temp_dir, full.names = TRUE)
        
        # Create the zip file
        zip::zip(zipfile = file, 
                 files = basename(csv_files),
                 root = temp_dir)
      },
      contentType = "application/zip"
    )
    
    
    
    ######################      
    # detection summaries -----------------------------------------------------------    
    
    
    # Create summary statistics output
    output$summary_stats <- renderUI({
      req(results_ready())
      
      # Calculate statistics
      total_stations <- nrow(results_store$data[["camera_locations"]])
      total_survey_nights <- sum(results_store$data[["independent_total_observations"]]$days)
      total_species <- nrow(results_store$data[["species_list"]])
      total_detections <- sum(results_store$data[["independent_total_observations"]][, results_store$data[["species_list"]]$sp])
      
      # Calculate nights per camera
      avg_nights <- mean(results_store$data[["independent_total_observations"]]$days)
      min_nights <- min(results_store$data[["independent_total_observations"]]$days)
      max_nights <- max(results_store$data[["independent_total_observations"]]$days)
      
      # Calculate date range - use row_lookup instead
      row_lookup <- results_store$data[["row_lookup"]]
      date_range <- paste(format(min(row_lookup$date), "%b %Y"), "to", format(max(row_lookup$date), "%b %Y"))
      
      fluidRow(
        column(12,
               box(
                 title = "Survey Summary Statistics",
                 width = NULL,
                 column(3, 
                        tags$div(style = "text-align: center;",
                                 h3(total_stations, style = "color: #3c8dbc; margin: 5px;"),
                                 p("Unique Stations")
                        )
                 ),
                 column(3,
                        tags$div(style = "text-align: center;",
                                 h3(round(total_survey_nights, 0), style = "color: #3c8dbc; margin: 5px;"),
                                 p("Camera trap days")
                        )
                 ),
                 column(3,
                        tags$div(style = "text-align: center;",
                                 h3(total_species, style = "color: #3c8dbc; margin: 5px;"),
                                 p("Species Detected")
                        )
                 ),
                 column(3,
                        tags$div(style = "text-align: center;",
                                 h3(total_detections, style = "color: #3c8dbc; margin: 5px;"),
                                 p("Total Independent Detections")
                        )
                 ),
                 column(6,
                        tags$div(style = "text-align: center; margin-top: 10px;",
                                 p(strong("Avg Days per Station: "), round(avg_nights, 1), 
                                   " (min: ", round(min_nights, 1), ", max: ", round(max_nights, 1), ")")
                        )
                 ),
                 column(6,
                        tags$div(style = "text-align: center; margin-top: 10px;",
                                 p(strong("Survey Period: "), date_range)
                        )
                 )
               )
        )
      )
    })
    
    # Reset filters button
    observeEvent(input$reset_filters, {
      updateNumericInput(session, "min_detections", value = 0)
      updateNumericInput(session, "max_detections", value = NA)
    })
    
    # Set up the error message if the independent data doesnt exist
    output$capture_summary_output <- renderUI({
      if (!results_ready()) {
        box(
          width = 12,
          h4("Independent data not yet created", style = "color: #777; text-align: center; padding: 20px;")
        )
      } else {
        tagList(
          uiOutput("summary_stats"),
          # Add filter controls
          fluidRow(
            column(12,
                   box(
                     width = 12,
                     title = "Filter Species by Detection Count",
                     collapsible = TRUE,
                     collapsed = FALSE,
                     fluidRow(
                       column(4,
                              numericInput("min_detections", 
                                           "Minimum Detections:", 
                                           value = 0, 
                                           min = 0, 
                                           step = 1)
                       ),
                       column(4,
                              numericInput("max_detections", 
                                           "Maximum Detections:", 
                                           value = NA, 
                                           min = 1, 
                                           step = 1)
                       ),
                       column(4,
                              br(),
                              actionButton("reset_filters", "Reset Filters", 
                                           icon = icon("undo"))
                       )
                     )
                   )
            )
          ),
          plotlyOutput(outputId = "capture_summary", height = "auto")
        )
      }
    })
    
    # Create the plotly plot
    output$capture_summary <- renderPlotly({
      req(results_ready())
      
      # Call the saved data
      deployments <- data_store$dfs[["deployments.csv"]]
      images <- data_store$dfs[["images.csv"]]
      
      long_obs <- results_store$data[["independent_total_observations"]] %>% 
        pivot_longer(cols=results_store$data[["species_list"]]$sp,
                     names_to="sp",
                     values_to = "count")
      
      tmp <- long_obs %>%
        group_by(sp) %>%
        summarise(count=sum(count))
      
      sp_summary <- left_join(results_store$data[["species_list"]], tmp)
      
      total_binary <- results_store$data[["independent_total_observations"]] %>%
        mutate(across(results_store$data[["species_list"]]$sp, ~+as.logical(.x)))
      
      long_bin <- total_binary %>% 
        pivot_longer(cols=results_store$data[["species_list"]]$sp, names_to="sp", values_to = "count")
      
      tmp <- long_bin %>% 
        group_by(sp) %>% 
        summarise(occupancy=sum(count)/nrow(results_store$data[["camera_locations"]]))
      
      sp_summary <- left_join(sp_summary, tmp)
      
      # Apply filters
      min_det <- if(is.null(input$min_detections)) 0 else input$min_detections
      max_det <- if(is.null(input$max_detections) || is.na(input$max_detections)) {
        Inf
      } else {
        input$max_detections
      }
      
      sp_summary <- sp_summary %>%
        filter(count >= min_det & count <= max_det)
      
      # Check if any species remain after filtering
      validate(
        need(nrow(sp_summary) > 0, 
             "No species match the current filter criteria. Please adjust the min/max values.")
      )
      
      sp_summary <- sp_summary[order(sp_summary$count),]
      
      # Calculate plot height
      plot_height <- max(400, nrow(sp_summary) * 20)
      
      yform <- list(categoryorder = "array", categoryarray = sp_summary$sp)
      xform <- list(title="Independent detections")
      
      fig1 <- plot_ly(x = sp_summary$count, y = sp_summary$sp, 
                      type = 'bar', orientation = 'h', name="count",
                      height = plot_height) %>% 
        layout(yaxis = yform, xaxis=xform) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c('lasso2d',  'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'zoomIn2d', 'zoomOut2d'))
      
      yform <- list(categoryorder = "array", categoryarray = sp_summary$sp, showticklabels=F)
      xform <- list(title="Proportion of stations detected")
      
      fig2 <- plot_ly(x = sp_summary$occupancy, y = sp_summary$sp, 
                      type = 'bar', orientation = 'h', name="occupancy",
                      height = plot_height) %>% 
        layout(yaxis = yform, xaxis=xform) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c('lasso2d',  'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'zoomIn2d', 'zoomOut2d'))
      
      subplot(nrows=1, fig1, fig2, titleX = T)
    })
    #############################
    # Temporal patterns -----------------------------------------------------------    
    # Code to show error message before independent data is created
    output$temporal_plots_output <- renderUI({
      if (!results_ready()) {
        box(
          width = 12,
          h4("Independent data not yet created", style = "color: #777; text-align: center; padding: 20px;")
        )
      } else {
        species_choices <- results_store$data[["species_list"]]$sp[order(results_store$data[["species_list"]]$sp)]
        
        tagList(
          fluidRow(
            column(width = 4, offset = 4,  # Center the period selector
                   selectInput("temporal_period", "Aggregation Period:",
                               choices = c("Day" = "day", "Week" = "week", "Month" = "month"),
                               selected = "month")
            )
          ),
          fluidRow(
            column(12, plotlyOutput("camera_effort_plot", height="700px"))  
          ),
          hr(),
          "You can also see the capture rate through time for each of the species included in the species list:",
          p(),
          fluidRow(
            column(width = 6, offset = 3,  # Center the species selector
                   selectInput("selected_species", "Select Species:",
                               choices = species_choices,
                               selected = species_choices[1])
            )
          ),
          fluidRow(
            column(12, plotlyOutput("species_trends_plot"))
          )
        )
      }
    })
    
    # Reactive aggregated temporal data
    temporal_summary <- reactive({
      req(input$temporal_period)
      
      sp_list <- results_store$data[["species_list"]]$sp
      
      # Use the appropriate pre-calculated dataframe based on selected period
      if (input$temporal_period == "day") {
        summary_data <- results_store$data[["independent_daily_observations"]]
        summary_data$period <- as.Date(summary_data$date)
        
        # Create complete date sequence
        date_range <- seq(min(summary_data$period), max(summary_data$period), by = "day")
        complete_periods <- data.frame(period = date_range)
        
      } else if (input$temporal_period == "week") {
        summary_data <- results_store$data[["independent_weekly_observations"]]
        
        # Parse week format manually to avoid ISOweek validation issues
        # Extract year and week from "YYYY-Wxx" format
        week_parts <- strsplit(summary_data$date, "-W")
        summary_data$year <- as.numeric(sapply(week_parts, `[`, 1))
        summary_data$week <- as.numeric(sapply(week_parts, `[`, 2))
        
        # Validate and fix week numbers
        summary_data$week <- pmin(summary_data$week, 53)  # Cap at 53
        summary_data$week <- pmax(summary_data$week, 1)   # Floor at 1
        
        # Calculate date as: start of year + (week - 1) * 7 days
        # Then adjust to the Monday of that week
        summary_data$period <- as.Date(paste0(summary_data$year, "-01-01")) + 
          (summary_data$week - 1) * 7
        
        # Adjust to Monday (ISO weeks start on Monday)
        days_from_monday <- (as.numeric(format(summary_data$period, "%u")) - 1)
        summary_data$period <- summary_data$period - days_from_monday
        
        # Create complete week sequence
        date_range <- seq(
          from = min(summary_data$period, na.rm = TRUE),
          to = max(summary_data$period, na.rm = TRUE),
          by = "week"
        )
        complete_periods <- data.frame(period = date_range)
        
      } else {
        summary_data <- results_store$data[["independent_monthly_observations"]]
        summary_data$period <- ym(summary_data$date)
        
        # Create complete month sequence
        date_range <- seq(min(summary_data$period), max(summary_data$period), by = "month")
        complete_periods <- data.frame(period = date_range)
      }
      
      # Aggregate by period first
      period_aggregated <- summary_data %>%
        group_by(period) %>%
        summarise(
          locs_active = n(),
          cam_days = sum(days, na.rm = TRUE),
          across(all_of(sp_list), sum, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Join with complete periods and fill NAs with 0
      result <- complete_periods %>%
        left_join(period_aggregated, by = "period") %>%
        mutate(
          locs_active = replace_na(locs_active, 0),
          cam_days = replace_na(cam_days, 0),
          across(all_of(sp_list), ~replace_na(.x, 0))
        )
      
      return(result)
    })
    
    # Static Plot 1: Number of Active Cameras Over Time
    output$camera_effort_plot <- renderPlotly({
      
      req(temporal_summary())
      
      # Pull in the aggregated data (already has zeros filled in)
      period_summary <- temporal_summary()
      sp_summary <- results_store$data[["species_list"]]
      
      # Calculate overall capture rate
      period_summary$all.sp <- rowSums(period_summary[, sp_summary$sp])
      # Avoid division by zero
      period_summary$all.cr <- ifelse(period_summary$cam_days > 0, 
                                      period_summary$all.sp / (period_summary$cam_days / 100),
                                      0)
      
      # Set x-axis title based on period
      x_title <- switch(input$temporal_period,
                        "day" = "Date",
                        "week" = "Week",
                        "month" = "Month")
      
      # Plot 1: Active cameras
      p1 <- plot_ly(period_summary, x = ~period, y = ~locs_active, type = "scatter", mode = "lines",
                    line = list(color = "blue"), name = "Number of Active Cameras") %>%
        layout(title = "Active Cameras Over Time",
               xaxis = list(title = x_title),
               yaxis = list(title = "Active Cameras", rangemode = "tozero")) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c('lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 
                                          'hoverCompareCartesian', 'zoomIn2d', 'zoomOut2d'))
      
      # Plot 2: Capture rates
      p2 <- plot_ly(period_summary, x = ~period, y = ~all.cr, type = "scatter", mode = "lines",
                    line = list(color = "darkred"), name = "Capture Rate per 100 days") %>%
        layout(title = "Overall Capture Rates (All Species)",
               xaxis = list(title = x_title),
               yaxis = list(title = "Capture Rate per 100 days", rangemode = "tozero")) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c('lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 
                                          'hoverCompareCartesian', 'zoomIn2d', 'zoomOut2d'))
      
      # Combine plots vertically using subplot()
      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>% layout(height = 700)
    })
    
    # Interactive Plot: Capture Rates for Selected Species
    output$species_trends_plot <- renderPlotly({
      req(input$selected_species, temporal_summary())
      
      # Pull in the aggregated data (already has zeros filled in)
      period_summary <- temporal_summary()
      sp_summary <- results_store$data[["species_list"]]
      
      # Wide to long for selected species
      long_df <- period_summary %>%
        pivot_longer(
          cols = sp_summary$sp,
          names_to = "sp",
          values_to = "Count"
        ) %>%
        mutate(CaptureRate = ifelse(cam_days > 0, Count / (cam_days / 100), 0))
      
      filtered_data <- long_df %>%
        filter(sp == input$selected_species)
      
      # Set x-axis title based on period
      x_title <- switch(input$temporal_period,
                        "day" = "Date",
                        "week" = "Week",
                        "month" = "Month")
      
      # Create ggplot object
      p <- ggplot(filtered_data, aes(x = period, y = CaptureRate)) +
        geom_line(color = "darkgreen", size = 1) +
        labs(title = paste("Capture Rate of", input$selected_species),
             x = x_title, y = "Capture Rate per 100 days") +
        ylim(0, NA) + 
        theme_minimal()
      
      # Convert to plotly
      ggplotly(p) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c('lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 
                                          'hoverCompareCartesian', 'zoomIn2d', 'zoomOut2d'))
    })
    ################# ----------------------------------------------------------------------------------------
    # Spatial patterns 
    
    # Code to show error message before independent data is created
    # Spatial plots output
    output$spatial_plots_output <- renderUI({
      if (!results_ready()) {
        box(
          width = 12,
          h4("Independent data not yet created", style = "color: #777; text-align: center; padding: 20px;")
        )
      } else {
        species_choices <- results_store$data[["species_list"]]$sp[order(results_store$data[["species_list"]]$sp)]
        
        tagList(
          # Overall capture rate map
          fluidRow(
            column(12, 
                   h3("Overall Capture Rate (All Species)"),
                   p("This map shows the total capture rate across all species at each camera location."),
                   downloadButton("download_overall_map_png", "Download Overall Map as PNG", icon = icon("camera")),
                   p(),
                   leafletOutput("overall_capture_map")
            )
          ),
          hr(),
          # Species-specific maps
          fluidRow(
            column(12, 
                   h3("Species-Specific Capture Rates"),
                   p("Select a species to view its capture rate distribution across camera locations.")
            )
          ),
          fluidRow(
            column(width = 6, offset = 3,  # Center the dropdown
                   selectInput("selected_species_map", "Select Species:",
                               choices = species_choices,
                               selected = species_choices[1])
            )
          ),
          fluidRow(
            column(12, 
                   downloadButton("download_map_png", "Download Species Map as PNG", icon = icon("camera")),
                   p(),
                   leafletOutput("capture_map"))
          ),
          hr(),
          fluidRow(
            column(12, titlePanel("Spatial co-occurences"))
          ),
          "The following plot shows the co-occurrence correlations between different species in your survey:",
          p(),
          fluidRow(
            column(12, 
                   plotOutput("corrplot"))
          )
        )
      }
    })
    
    # Overall capture rate map (all species combined)
    output$overall_capture_map <- renderLeaflet({
      
      wide_obs <- results_store$data[["independent_total_observations"]] 
      locs <- results_store$data[["camera_locations"]]
      sp_list <- results_store$data[["species_list"]]$sp
      
      total_obs <- left_join(wide_obs, locs)
      
      # Calculate overall capture rate (sum of all species)
      overall_cr <- total_obs %>% 
        mutate(
          total_detections = rowSums(select(., all_of(sp_list)), na.rm = TRUE),
          CaptureRate = total_detections / (days / 100)
        )
      
      # Calculate min and max for legend
      max_cr <- max(overall_cr$CaptureRate, na.rm = TRUE)
      min_cr <- min(overall_cr$CaptureRate, na.rm = TRUE)
      mid_cr <- min_cr + ((max_cr - min_cr)/2)
      
      # Create custom HTML legend
      legend_html <- paste0(
        '<div style="padding: 10px; background: white; border: 2px solid grey; border-radius: 5px;">',
        '<strong>All Species</strong><br>',
        '<strong>Capture Rate</strong><br>(per 100 days)<br><br>',
        '<svg width="120" height="80">',
        '<circle cx="15" cy="15" r="11" fill="rgba(0, 102, 204, 0.6)" />',
        '<text x="35" y="20" font-size="12">High: ', round(max_cr, 2), '</text>',
        '<circle cx="15" cy="40" r="7" fill="rgba(0, 102, 204, 0.6)" />',
        '<text x="35" y="45" font-size="12">Med: ', round(mid_cr, 2), '</text>',
        '<circle cx="15" cy="65" r="3" fill="rgba(0, 102, 204, 0.6)" />',
        '<text x="35" y="70" font-size="12">Low: ', round(min_cr, 2), '</text>',
        '</svg>',
        '</div>'
      )
      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addCircleMarkers(
          data = overall_cr,
          lng = ~longitude,
          lat = ~latitude,
          radius = ~(CaptureRate / max(CaptureRate, na.rm = TRUE) * 10) + 1,
          stroke = FALSE,
          fillOpacity = 0.6,
          popup = ~paste(placename, "<br>Total Detections:", total_detections, 
                         "<br>Capture Rate:", round(CaptureRate, 2))
        ) %>%   
        addScaleBar(position = "topleft") %>%
        addControl(html = legend_html, position = "bottomright")
    })
    
    # Download overall map button 
    output$download_overall_map_png <- downloadHandler(
      filename = function() {
        paste0("map_overall_", Sys.Date(), ".png")
      },
      content = function(file) {
        
        wide_obs <- results_store$data[["independent_total_observations"]] 
        locs <- results_store$data[["camera_locations"]]
        sp_list <- results_store$data[["species_list"]]$sp
        
        total_obs <- left_join(wide_obs, locs)
        
        overall_cr <- total_obs %>% 
          mutate(
            total_detections = rowSums(select(., all_of(sp_list)), na.rm = TRUE),
            CaptureRate = total_detections / (days / 100)
          )
        
        max_cr <- max(overall_cr$CaptureRate, na.rm = TRUE)
        min_cr <- min(overall_cr$CaptureRate, na.rm = TRUE)
        mid_cr <- min_cr + ((max_cr - min_cr)/2)
        
        legend_html <- paste0(
          '<div style="padding: 10px; background: white; border: 2px solid grey; border-radius: 5px;">',
          '<strong>All Species</strong><br>',
          '<strong>Capture Rate</strong><br>(per 100 days)<br><br>',
          '<svg width="120" height="80">',
          '<circle cx="15" cy="15" r="11" fill="rgba(0, 102, 204, 0.6)" />',
          '<text x="35" y="20" font-size="12">High: ', round(max_cr, 2), '</text>',
          '<circle cx="15" cy="40" r="7" fill="rgba(0, 102, 204, 0.6)" />',
          '<text x="35" y="45" font-size="12">Med: ', round(mid_cr, 2), '</text>',
          '<circle cx="15" cy="65" r="3" fill="rgba(0, 102, 204, 0.6)" />',
          '<text x="35" y="70" font-size="12">Low: ', round(min_cr, 2), '</text>',
          '</svg>',
          '</div>'
        )
        
        m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addCircleMarkers(
            data = overall_cr,
            lng = ~longitude,
            lat = ~latitude,
            radius = ~(CaptureRate / max(CaptureRate, na.rm = TRUE) * 10) + 1,
            stroke = FALSE,
            fillOpacity = 0.6
          ) %>%  
          addScaleBar(position = "topleft") %>%
          addControl(html = legend_html, position = "bottomright")
        
        temp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(m, temp_html, selfcontained = TRUE)
        webshot2::webshot(temp_html, file = file, vwidth = 1200, vheight = 800)
      }
    )
    
    
    output$capture_map <- renderLeaflet({
      
      req(input$selected_species_map) 
      
      wide_obs <- results_store$data[["independent_total_observations"]] 
      locs<- results_store$data[["camera_locations"]]
      
      total_obs <- left_join(wide_obs, locs)
      
      focal_cr <- total_obs %>% mutate(CaptureRate = total_obs[[input$selected_species_map]] / (days / 100))
      
      # Calculate min and max for legend
      max_cr <- max(focal_cr$CaptureRate, na.rm = TRUE)
      min_cr <- min(focal_cr$CaptureRate, na.rm = TRUE)
      mid_cr <- min_cr + ((max_cr - min_cr)/2)
      
      # Create custom HTML legend
      legend_html <- paste0(
        '<div style="padding: 10px; background: white; border: 2px solid grey; border-radius: 5px;">',
        '<strong>', input$selected_species_map, '</strong><br>',
        '<strong>Capture Rate</strong><br>(per 100 days)<br><br>',
        '<svg width="120" height="80">',
        '<circle cx="15" cy="15" r="11" fill="rgba(0, 102, 204, 0.6)" />',
        '<text x="35" y="20" font-size="12">High: ', round(max_cr, 2), '</text>',
        '<circle cx="15" cy="40" r="7" fill="rgba(0, 102, 204, 0.6)" />',
        '<text x="35" y="45" font-size="12">Med: ', round(mid_cr, 2), '</text>',
        '<circle cx="15" cy="65" r="3" fill="rgba(0, 102, 204, 0.6)" />',
        '<text x="35" y="70" font-size="12">Low: ', round(min_cr, 2), '</text>',
        '</svg>',
        '</div>'
      )
      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addCircleMarkers(
          data = focal_cr,
          lng = ~longitude,
          lat = ~latitude,
          radius = ~(CaptureRate / max(CaptureRate, na.rm = TRUE) * 10) + 1,
          stroke = FALSE,
          fillOpacity = 0.6,
          popup = ~paste(placename, "- Capture Rate:", round(CaptureRate, 2))
        ) %>%   
        addScaleBar(position = "topleft") %>%
        addControl(html = legend_html, position = "bottomright")
      
    })
    
    # Download map button 
    output$download_map_png <- downloadHandler(
      filename = function() {
        paste0("map_", input$selected_species_map, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(input$selected_species_map)
        
        wide_obs <- results_store$data[["independent_total_observations"]] 
        locs <- results_store$data[["camera_locations"]]
        total_obs <- left_join(wide_obs, locs)
        focal_cr <- total_obs %>% mutate(CaptureRate = total_obs[[input$selected_species_map]] / (days / 100))
        
        # Calculate min and max for legend
        max_cr <- max(focal_cr$CaptureRate, na.rm = TRUE)
        min_cr <- min(focal_cr$CaptureRate, na.rm = TRUE)
        mid_cr <- min_cr + ((max_cr - min_cr)/2)
        
        # Create custom HTML legend
        legend_html <- paste0(
          '<div style="padding: 10px; background: white; border: 2px solid grey; border-radius: 5px;">',
          '<strong>', input$selected_species_map, '</strong><br>',
          '<strong>Capture Rate</strong><br>(per 100 days)<br><br>',
          '<svg width="120" height="80">',
          '<circle cx="15" cy="15" r="11" fill="rgba(0, 102, 204, 0.6)" />',
          '<text x="35" y="20" font-size="12">High: ', round(max_cr, 2), '</text>',
          '<circle cx="15" cy="40" r="7" fill="rgba(0, 102, 204, 0.6)" />',
          '<text x="35" y="45" font-size="12">Med: ', round(mid_cr, 2), '</text>',
          '<circle cx="15" cy="65" r="3" fill="rgba(0, 102, 204, 0.6)" />',
          '<text x="35" y="70" font-size="12">Low: ', round(min_cr, 2), '</text>',
          '</svg>',
          '</div>'
        )
        
        m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addCircleMarkers(
            data = focal_cr,
            lng = ~longitude,
            lat = ~latitude,
            radius = ~(CaptureRate / max(CaptureRate, na.rm = TRUE) * 10) + 1,
            stroke = FALSE,
            fillOpacity = 0.6
          ) %>%  addScaleBar(position = "topleft") %>%
          addControl(html = legend_html, position = "bottomright")
        
        # Save to temp html first
        temp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(m, temp_html, selfcontained = TRUE)
        
        # Convert to PNG using webshot2
        webshot2::webshot(temp_html, file = file, vwidth = 1200, vheight = 800)
      }
    )
    
    
    #################
    # Spatial co-ocurance 
    
    
    
    output$corrplot <- renderPlot({
      
      total_obs <- results_store$data[["independent_total_observations"]] 
      tmp <- total_obs[,!(colnames(total_obs)%in% c("placename", "days"))]
      M <- cor(tmp[, colSums(tmp)>5])
      
      
      focal_cr <- total_obs %>% mutate(CaptureRate = total_obs[[input$selected_species_map]] / (days / 100))
      
      corrplot(M, method="color", 
               type="upper", 
               order="hclust",
               # addCoef.col = "black", # We suppress the coefs to make a cleaner plot
               tl.col="black", tl.srt=45, #Text label color and rotation
               diag=FALSE)
    })
    
    
    # Generate Word Report -------------------------------------------------------
    output$generate_report <- downloadHandler(
      filename = function() {
        req(results_ready())
        # Get project name from deployments
        project_name <- data_store$dfs[["deployments.csv"]]$project_id[1]
        paste0("Camera_Trap_Report_", project_name, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        req(results_ready())
        
        # Show loading spinner
        waiter_show(
          html = spin_fading_circles(),
          color = "rgba(0,0,0,0.8)"
        )
        
        # Ensure spinner hides even if there's an error
        on.exit(waiter_hide())
        
        # Get project name
        project_name <- data_store$dfs[["deployments.csv"]]$project_id[1]
        
        # Create a new Word document
        doc <- read_docx()
        
        # Add title with project name
        doc <- doc |>
          body_add_par(paste("Camera Trap Survey Report:", project_name), style = "heading 1") |>
          body_add_par(paste("Generated:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") |>
          body_add_par("", style = "Normal")
        
        # Add summary statistics
        total_stations <- nrow(results_store$data[["camera_locations"]])
        total_survey_nights <- sum(results_store$data[["independent_total_observations"]]$days)
        total_species <- nrow(results_store$data[["species_list"]])
        total_detections <- sum(results_store$data[["independent_total_observations"]][, results_store$data[["species_list"]]$sp])
        avg_nights <- mean(results_store$data[["independent_total_observations"]]$days)
        row_lookup <- results_store$data[["row_lookup"]]
        date_range <- paste(format(min(row_lookup$date), "%b %Y"), "to", format(max(row_lookup$date), "%b %Y"))
        
        doc <- doc |>
          body_add_par("Survey Summary", style = "heading 2") |>
          body_add_par(paste("Survey Period:", date_range), style = "Normal") |>
          body_add_par(paste("Unique Stations:", total_stations), style = "Normal") |>
          body_add_par(paste("Total Camera Trap Days:", round(total_survey_nights, 0)), style = "Normal") |>
          body_add_par(paste("Average Days per Station:", round(avg_nights, 1)), style = "Normal") |>
          body_add_par(paste("Species Detected:", total_species), style = "Normal") |>
          body_add_par(paste("Total Independent Detections:", total_detections), style = "Normal") |>
          body_add_par("", style = "Normal")
        
        # Add species list table with detections and occupancy
        doc <- doc |>
          body_add_par("Species Summary", style = "heading 2")
        
        # Calculate detections and occupancy for each species
        long_obs <- results_store$data[["independent_total_observations"]] %>% 
          pivot_longer(cols=results_store$data[["species_list"]]$sp,
                       names_to="sp",
                       values_to = "count")
        
        detections_summary <- long_obs %>%
          group_by(sp) %>%
          summarise(total_detections = sum(count))
        
        # Calculate occupancy (proportion of stations)
        total_binary <- results_store$data[["independent_total_observations"]] %>%
          mutate(across(results_store$data[["species_list"]]$sp, ~+as.logical(.x)))
        
        long_bin <- total_binary %>% 
          pivot_longer(cols=results_store$data[["species_list"]]$sp, names_to="sp", values_to = "count")
        
        occupancy_summary <- long_bin %>% 
          group_by(sp) %>% 
          summarise(prop_stations = sum(count)/total_stations)
        
        # Join with species list
        species_summary_table <- results_store$data[["species_list"]] |>
          left_join(detections_summary, by = "sp") |>
          left_join(occupancy_summary, by = "sp") |>
          select(common_name, genus, species, total_detections, prop_stations) |>
          mutate(
            common_name = substr(common_name, 1, 30),  # Truncate to 30 characters
            total_detections = round(total_detections, 2),
            prop_stations = round(prop_stations, 2)
          ) |>
          flextable() |>
          set_header_labels(
            common_name = "Common\nName", 
            genus = "Genus", 
            species = "Species",
            total_detections = "Independent\nDetections",
            prop_stations = "Proportion of\nStations"
          ) |>
          fontsize(size = 8, part = "all") |>  # Reduce font size by ~20% (10 to 8)
          autofit()
        
        doc <- doc |> body_add_flextable(species_summary_table)
        
        # Add survey location map
        doc <- doc |>
          body_add_par("", style = "Normal") |>
          body_add_par("Survey Locations", style = "heading 2")
        
        # Create static map
        
        deployments <- data_store$dfs[["deployments.csv"]]
        locs <- deployments |>
          select(placename, longitude, latitude) |>
          distinct()
        
        # Create sf object
        locs_sf <- st_as_sf(locs, coords = c("longitude", "latitude"), crs = 4326)
        
        # Create map with OSM basemap
        map_plot <- ggplot() +
          annotation_map_tile(type = "osm", zoomin = 0) +  # OpenStreetMap basemap
          geom_sf(data = locs_sf, color = "red", size = 3, shape = 17) +
          annotation_scale(location = "bl", width_hint = 0.3) +  # Scale bar
          annotation_north_arrow(location = "tr", which_north = "true",  # North arrow
                                 style = north_arrow_fancy_orienteering) +
          theme_minimal() +
          theme(text = element_text(size = 10))
        
        # Save and add to document
        temp_map <- tempfile(fileext = ".png")
        ggsave(temp_map, plot = map_plot, width = 6, height = 4.5, dpi = 300)
        doc <- doc |> body_add_img(temp_map, width = 6, height = 4.5)
        
        # Add detection summary plot
        doc <- doc |>
          body_add_par("", style = "Normal") |>
          body_add_par("Detection Summary", style = "heading 2")
        
        sp_summary <- results_store$data[["species_list"]] |>
          left_join(detections_summary, by = "sp") |>
          arrange(total_detections)
        
        # Create ggplot
        p1 <- ggplot(sp_summary, aes(x = reorder(sp, total_detections), y = total_detections)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          labs(x = NULL, y = "Independent Detections") +
          theme_minimal() +
          theme(text = element_text(size = 10))
        
        # Save plot temporarily and add to document
        temp_plot <- tempfile(fileext = ".png")
        ggsave(temp_plot, plot = p1, width = 6, height = max(3, nrow(sp_summary) * 0.2), dpi = 300)
        doc <- doc |> body_add_img(temp_plot, width = 6, height = max(3, nrow(sp_summary) * 0.2))
        
        # Add temporal patterns
        doc <- doc |>
          body_add_par("", style = "Normal") |>
          body_add_par("Temporal Patterns", style = "heading 2")
        
        mon_obs <- results_store$data[["independent_monthly_observations"]]
        sp_summary_full <- results_store$data[["species_list"]]
        
        mon_summary <- mon_obs %>%        
          group_by(date) %>%              
          summarise(locs_active=n(), cam_days=sum(days))   
        
        mon_summary_species <- mon_obs %>% 
          group_by(date) %>%  
          summarise(across(sp_summary_full$sp, sum, na.rm=TRUE))
        
        mon_summary <- left_join(mon_summary, mon_summary_species, by = "date")
        mon_summary$date <- ym(mon_summary$date)
        mon_summary$all.sp <- rowSums(mon_summary[, sp_summary_full$sp])
        mon_summary$all.cr <- mon_summary$all.sp/(mon_summary$cam_days/100)
        
        # Camera effort plot
        p2 <- ggplot(mon_summary, aes(x = date, y = locs_active)) +
          geom_line(color = "blue", size = 1) +
          labs(x = "Date", y = "Active Cameras", title = "Active Cameras Over Time") +
          theme_minimal() +
          theme(text = element_text(size = 10))
        
        temp_plot2 <- tempfile(fileext = ".png")
        ggsave(temp_plot2, plot = p2, width = 6, height = 3, dpi = 300)
        doc <- doc |> body_add_img(temp_plot2, width = 6, height = 3)
        
        # Capture rate plot
        p3 <- ggplot(mon_summary, aes(x = date, y = all.cr)) +
          geom_line(color = "darkred", size = 1) +
          labs(x = "Date", y = "Capture Rate per 100 days", title = "Overall Capture Rates") +
          theme_minimal() +
          theme(text = element_text(size = 10))
        
        temp_plot3 <- tempfile(fileext = ".png")
        ggsave(temp_plot3, plot = p3, width = 6, height = 3, dpi = 300)
        doc <- doc |> body_add_img(temp_plot3, width = 6, height = 3)
        
        # Save the document
        print(doc, target = file)
        
        # Clean up temp files
        unlink(c(temp_plot, temp_plot2, temp_plot3, temp_map))
        
        # Hide spinner (though on.exit will also do this)
        waiter_hide()
      }
    )
    
    
    
    # End of observeEvent(input$ind_run
  })
  
  ## Glossary ------------------------------------------------------------------------
  
  
  # Read glossary from Google Sheets
  glossary_data <- reactive({
    
    # Use googlesheets4 to read the sheet
    
    # Make it public access (no authentication needed)
    gs4_deauth()
    
    # Read the data
    tryCatch({
      read_sheet("https://docs.google.com/spreadsheets/d/1zY3-BS3u3LXj7apu8ybEzw2dgHDyuJyIVY-j4IYJBgY/edit?usp=sharing")
    }, error = function(e) {
      # Fallback data if Google Sheets fails
      data.frame(
        Term = c("Independent Detection", "Deployment", "Capture Rate"),
        Definition = c(
          "A detection event separated from the previous detection of the same species by a defined time threshold.",
          "A period during which a camera trap was active at a specific location.",
          "The number of independent detections per unit of survey effort (typically per 100 camera trap days)."
        )
      )
    })
  })
  
  
  # Render the table
  output$glossary_accordion <- renderUI({
    
    data <- glossary_data()
    
    # Filter based on search
    if (!is.null(input$glossary_search) && input$glossary_search != "") {
      search_term <- tolower(input$glossary_search)
      data <- data %>%
        filter(
          grepl(search_term, tolower(Term)) | 
            grepl(search_term, tolower(Definition))
        )
    }
    
    # Create accordion items
    accordion_items <- lapply(1:nrow(data), function(i) {
      box(
        title = data$Term[i],
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        data$Definition[i]
      )
    })
    
    do.call(tagList, accordion_items)
  })
  
  
  
}
