# UI

# Navigation -----------------------------------------------------------------------------------------------------------

header <- dashboardHeader(
  title = title,
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  tags$img(src = "https://raw.githubusercontent.com/mabecker89/CameraTrapDataExploration/refs/heads/main/imgs/wildcam_grey.png", width = "100%", style = "padding: 0px;"),
  sidebarMenu(
    id = "tabs",
    # Home tab
    menuItem("Home", tabName = "home", icon = icon("home")),
    # Upload camera trap data
    menuItem("File Upload", tabName = "upload", icon = icon("table")),
    # Initial data check
    menuItem("Data checking", tabName = "map", icon = icon("globe-americas")),
    # Analysis data creation
    menuItem("Independent Detections", tabName = "ind_detect", icon=icon("desktop")),
    # Analysis data exploration
    menuItem("Analysis Data Exploration", icon = icon("wpexplorer"),
             menuSubItem("Detection summaries", tabName = "capture"),
             menuSubItem("Temporal Patterns", tabName = "temporal"),
             menuSubItem("Spatial Patterns", tabName = "spatial_capture")
    #         menuSubItem("Covariate Plots", tabName = "covariate")
             ),
    menuItem("Glossary", tabName = "glossary", icon = icon("table")),
    menuItem("Static Report", tabName = "report", icon = icon("book"))
  )
)

# Welcome page ------------------------------------------------------------------------------------

ui_welcome <- fluidRow(
  
  column(
    width = 12,
    h2("Camera trap data exploration tool", style = "font-size: 130%"),
    
    "This app represents a tool to explore annotated camera trap data in a quick and no-code way.",
    p(),
    HTML("To use this tool, <b>you must follow two simple rules</b>:"),
    p(),
    "1. Use the tabs sequentially, as the output of a given tab typically depends on the tab previous to it.",
    p(),
    "2. Your data files must be either:",
    tags$ul(
      tags$li("Raw data exports directly from the Wildlife Insights, WildTrax, or Migrations platforms. This is usually a .zip file containing a set of csv files."),
      tags$li(HTML("A set of .csv files with the same names and formats as from those platforms. We have provided templates in <a href='https://drive.google.com/drive/folders/1ulLVZoxfunWIOrmNjXArtQmRaJVWpfD6?usp=sharing' target='_blank'>this folder</a>. It is essential the files names and at least the column headings shown are replicated (others can be present)."))
    ),
    p(),
    HTML("If you do not have any existing data but want to try the app, download an example dataset in Wildlife Insights format here: <a href='https://drive.google.com/file/d/1Ry-2sqWLkC0jEalRDPFsMRcHgyRs9WW6/view?usp=drive_link' target='_blank'> Wildlife Insights Example</a>. Note - some errors have been introduced into this dataset to highlight how issues may be detected."),
    p(),
    HTML("For feedback and feature requests, please post an issue at the <a href='https://github.com/wildcam-git/CameraTrapDataExploration/issues' target='_blank'>projects github page</a>."),
    p(),
    
    # Acknowledgments section with custom styling
    tags$div(
      style = "background-color: white; padding: 20px; border-radius: 5px; border: 1px solid #ddd; margin-top: 20px;",
      
      HTML(paste0(
        "This app has been authored by Christopher Beirne and Marcus Becker, based on the code in ",
        "<a href='https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/' target='_blank'>",
        "An Introduction to Camera Trap Data Management and Analysis in R</a>."
      )),
      p(),
      HTML(paste0(
        "The creation of this tool has been supported by ",
        "<a href='https://wildcams.ca/' target='_blank'>WildCAM</a> (Wildlife Cameras for Adaptive Management); ",
        "the BC Government; the <a href='https://wildlife.forestry.ubc.ca/' target='_blank'>Wildlife Coexistence Lab at UBC</a>; and NSERC."
      )),
      p(),
      tags$img(
        src = "https://raw.githubusercontent.com/mabecker89/CameraTrapDataExploration/refs/heads/main/imgs/merged%20logos.png",
        style = "max-width: 600px; height: auto;"
      )
    )
  )
)



# Upload Data ----------------------------------------------------------------------------------------------------------

# Match upload button to 'btn-primary' style
custom_style <- HTML("
      .btn {
        font-size: 16px;
      }

      .btn-default.btn-file {
        background-color: #3c8dbc;
        border-color: #367fa9;
        color: white;
      }

      .form-control {
        height: 36px;
        font-size: 16px;
      }

      #custom_upload_progress .progress-bar {
        font-size: 16px;
        color: black;
      }
      #custom_spatial_upload_progress .progress-bar {
        font-size: 16px;
      }
    ")

ui_custom_values <- fluidRow(
  
  column(
    width = 12,
    tags$head(tags$style(custom_style)),
    box(
      
      h2("Upload Your Data"), width = 12, style = "font-size: 130%",
      
      "Please upload your camera trap data files using the blue action buttom below.",
      p(),
      h3("We currently support either:"),
      tags$ul(
        tags$li(HTML("Raw data exports from the <b>Wildlife Insights</b>, <b>WildTrax</b>, and <b>Migrations</b> platforms."),
                tags$ul(
                  tags$li("Note: This is usually a .zip file containing multiple csv files.")
                )
        ),
        p(),
        tags$li(HTML("A set of .csv files with the same names and formats as from those platforms. We have provided templates in <a href='https://drive.google.com/drive/folders/1ulLVZoxfunWIOrmNjXArtQmRaJVWpfD6?usp=sharing' target='_blank'>this folder</a>. It is essential the files names and at least the column headings shown are replicated (others can be present)."))
       ),
      p(),
      HTML("If you are uploading WildTrax output, note that the <b>main report</b> is the essential file to use, and that it will be converted to Wildlife Insights equivalent (including column names)."),
      p(),
      "Uploaded data can be previewed using the dropdown list below.",
      p(),
      
     fileInput("files", 
               label = "Upload a ZIP folder or individual files (.csv or Excel):",
               multiple = TRUE,
               buttonLabel = "Upload Data",
               placeholder = "No file selected",
               accept = c(".csv", ".xlsx", ".zip"),
               width = "50%"),
     
     actionButton("clear_files", "Clear"),
     
     uiOutput("selected_file")
     
     ),
     
     box(h3("Data Preview"), 
         width = 12,
         DTOutput(outputId = "custom_data_preview"))
    
  )
)



# Map and camera check ----------------------------------------------------------------------------------------------------------

ui_map <- fluidRow(
  
  # Map of camera locations
  column(
      h2("Camera locations"), width = 12, style = "font-size: 130%",
         
         "The following map represents the locations of your cameras in your 'deployment' data.",
         p(),
         "You can hover over the individual icons to see the placenames and its GPS coordinates.",
         p(),
         "If a camera is projecting in an incorrect location, you will need to return to your data manager, update it there, then re-import your data.",
         p(),
         
         leafletOutput("map", width = "100%", height = map_height),  
         
  
  # Camera activity check
  box(h2("Camera activity"), width = 12, style = "font-size: 110%",
      
      "The following plot represents the periods of time there was a camera deployed at each placename.",
      p(),
      h3("Key:"),
      tags$ul(
        tags$li("Black dots represent each time a camera deployment starts"),
        tags$li("Grey triangles represent when a deployment ends"),
        tags$li("Black lines connect deployment start and end points (showing when the camera is active)"),
        tags$li("If you hover over the start/end points you will get its corresponding 'deployment_id' code and the date - useful for diagnosing mismatches"),
        tags$li("Orange blocks represent the period of time between the first and last image associated with each deployment.")),
      p(),
      HTML("<b>Important note 1</b> - if an orange block appears outside the camera activity periods (black lines) then there is a mismatch between your deployment dates and the image timestamps. Return to your camera trap data management software to address this issue then re-export your data."),
      p(),
      HTML("<b>Important note 2</b> - any image data falling outside of the black lines will be removed at the <i>'Generate independent detections'</i> step, and not feature in subsequent data summaries."),
      p(),
      plotlyOutput(outputId = "deployment_dates", height = "auto"))
  
  )

)

# Independent data creation ----------------------------------------------------------------------------------------------------------

ui_ind_detect <- fluidRow(
  
  # Map of camera locations
  column(
    h2("Independent data creation"), width = 12, style = "font-size: 130%",
    
    "This page creates and stores your independent data.",
    p(),
    
    # Taxonomic filters
    h3("Select which groups to include:"),
    checkboxGroupInput("taxa_include", 
                       label = NULL,
                       choices = c("Mammals" = "Mammalia",
                                   "Birds" = "Aves",
                                   "Reptiles" = "Reptilia",
                                   "Amphibians" = "Amphibia"),
                       selected = c("Mammalia", "Aves")),  # Mammals and birds selected by default
    
    checkboxInput("include_humans", "Include humans (Homo sapiens)", value = FALSE),
    
    p(),
    
    # Independence threshold
    
    h3("Define independence threshold:"),
    "Specify the time interval used to separate one independent detection from another.",
    p(),
    radioButtons(
      "ind_units",
      label = "Select time units:",
      choices = c("Seconds" = "sec",
                  "Minutes" = "min",
                  "Hours" = "hr"),
      inline = TRUE,
      selected = "min"
    ),
    numericInput(
      "ind_thresh",
      "Number of time units:",
      value = 30,
      min = 0.01,
      max = 1000
    ),
    helpText("Note: Most camera trappers use 30 minutes."),
    p(),
    
    "You also need to select the column which specifies your count data (note - only numerical columns are available for selection)",
    p(),
    # Select the "count" column. Only give the user numeric inputs as an option
    selectInput("ind_count", "Choose a animal count column:", choices = NULL),
    
    # Analysis data options
    h3("Select output datasets:"),
    checkboxGroupInput(
      "analysis_frames",
      label = "Independent detection summaries:",
      choices = c(
        "Total" = "total",
        "Monthly" = "monthly",
        "Weekly" = "weekly",
        "Daily" = "daily"
      ),
      selected = c("total", "monthly", "weekly", "daily")
    ),
    p(),
    
    # Make a button to run the independent detections
    p(),
    HTML("<b>Click the green button below to create your independent data:</b>"),
    p(),
    HTML("<i> This code should run quickly. 2 million images across 350 stations took 20 seconds in testing. </i>"),
    p(),
    actionButton("ind_run", "Generate independent detections", class = "btn-lg btn-success"),
    p(),
    "You can download your independent datasets once the code is finished running.",
    p(),
    uiOutput("selected_analysis_file"),
    
    uiOutput("results_box")
  )
)
 
# Detection summaries ----------------------------------------------------------------------------------------------------------


ui_capture <- fluidRow(
  
  # Detection summaries
  column(
    h2("Detection summaries"), width = 12, style = "font-size: 130%",
    
    "This page creates summaries of your independent detections.",
    p(),
    conditionalPanel(
      condition = "output.button_clicked == false",
      box(
        width = 12,
        h4("Independent detections not yet created.", style = "color: #777; text-align: center; padding: 20px;")
      )
    ),
    uiOutput("capture_summary_output")
  )
)


# Temporal patterns ----------------------------------------------------------------------------------------------------------


ui_temporal <- fluidRow(
  
  column(
    width = 12,
    titlePanel("Temporal trends"),
    "The following plots show how many cameras you have active over time (top panel) and the total capture rate irrespective of species (bottom panel).",
    p(),
    
    conditionalPanel(
      condition = "output.button_clicked == false",
      box(
        width = 12,
        h4("Independent detections not yet created.", style = "color: #777; text-align: center; padding: 20px;")
      )
    ),
    
    uiOutput("temporal_plots_output")
  )
)

# Spatial maps of captures  ----------------------------------------------------------------------------------------------------------

ui_report <- fluidRow(
  column(
    width = 12,
    titlePanel("Generate Report"),
    "Generate a comprehensive Word document report of your camera trap survey results. The report includes:",
    tags$ul(
      tags$li("Survey summary statistics"),
      tags$li("Complete species list"),
      tags$li("Detection summary charts"),
      tags$li("Temporal patterns of camera effort and capture rates")
    ),
    p(),
    
    conditionalPanel(
      condition = "output.button_clicked == false",
      box(
        width = 12,
        h4("Independent detections not yet created.", style = "color: #777; text-align: center; padding: 20px;")
      )
    ),
    
    conditionalPanel(
      condition = "output.button_clicked == true",
      box(
        width = 12,
        downloadButton("generate_report", "Generate Report (.docx)", 
                       icon = icon("file-word"),
                       class = "btn-primary btn-lg")
      )
    )
  )
)



# DTOutput("test_table")  
ui_spatial_caps <- fluidRow(
  
  column(
    width = 12,
    titlePanel("Capture Rate Maps"),
    "The following plots allow you to see where you accrued camera detections. Select from the drop down menu to vary the species.",
    p(),
    
    conditionalPanel(
      condition = "output.button_clicked == false",
      box(
        width = 12,
        h4("Independent detections not yet created.", style = "color: #777; text-align: center; padding: 20px;")
      )
    ),
    
    uiOutput("spatial_plots_output")
  )
)

# ----------------------------------------------------------------------------------------------------------
# Glossary

ui_glossary <- fluidRow(
  column(
    width = 12,
    titlePanel("Glossary"),
    "Key terms and definitions used throughout the Camera Trap Data Exploration application.",
    p(),
    textInput("glossary_search", "Search terms:", placeholder = "Type to filter..."),
    p(),
    uiOutput("glossary_accordion")
  )
)


    
# ----------------------------------------------------------------------------------------------------------

# Report
tabItem(tabName = "report",
        fluidRow(
          column(12,
                 box(
                   title = "Generate Static Report",
                   width = 12,
                   "Generate a static Word document report of your camera trap survey results. The report includes:",
                   tags$ul(
                     tags$li("Survey summary statistics"),
                     tags$li("Complete species list"),
                     tags$li("Detection summary charts"),
                     tags$li("Temporal patterns of camera effort and capture rates")
                   ),
                   p(),
                   conditionalPanel(
                     condition = "output.button_clicked",
                     downloadButton("generate_report", "Generate Report (.docx)", 
                                    icon = icon("file-word"),
                                    class = "btn-primary btn-lg")
                   ),
                   conditionalPanel(
                     condition = "!output.button_clicked",
                     h4("Please create independent detection data first", 
                        style = "color: #777; text-align: center; padding: 20px;")
                   )
                 )
          )
        )
)

#-----------------------------------------------------------------------------------


body <- dashboardBody(
  autoWaiter(),
  use_waiter(),
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  tabItems(
    tabItem("home", ui_welcome),
    tabItem("upload", ui_custom_values),
    tabItem("map", ui_map),
    tabItem("ind_detect",ui_ind_detect),
    tabItem("capture", ui_capture),
    tabItem("temporal", ui_temporal),
    tabItem("spatial_capture", ui_spatial_caps),
    tabItem("glossary", ui_glossary),
    tabItem("report", ui_report)
  )
)

dashboardPage(
  header = header,
  sidebar = sidebar,
  body =   body,
  title = title,
  skin = skin
)


