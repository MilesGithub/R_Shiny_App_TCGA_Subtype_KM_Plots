# --- Load Libraries ---

# List of packages to check and install

packages <- c("BiocManager", "shiny", "survival", "survminer", "ggplot2", "dplyr")

# Function to check if a package is installed
load_packages <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (pkg %in% rownames(installed.packages())) {
    } else if (pkg == "BiocManager") {
      install.packages("BiocManager")
      print("Installed BiocManager from CRAN")
    } else if (pkg %in% c("NMF", "SummarizedExperiment")) {
      if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
      BiocManager::install(pkg, update = FALSE, ask = FALSE)
      print(paste0("Installed from Bioconductor: ", pkg))
    } else {
      install.packages(pkg)
      print(paste0("Installed from CRAN: ", pkg))
    }
  }

  library(pkg, character.only = TRUE)
  print(paste0("Loaded: ", pkg))

}

invisible(lapply(packages, load_packages))
print("Required packages loaded.")

# --- Load Data ---
data_file <- 'data/NIHMS958212-supplement-2.csv'
if (!file.exists(data_file)) stop("Error: Data file not found.")
data<-read.delim(data_file, sep=",", stringsAsFactors = FALSE)

cols_to_keep <- c("TCGA.Study", "TCGA.Subtype", "Immune.Subtype", "OS", "OS.Time", "PFI", "PFI.Time")
data<-data[,colnames(data) %in% cols_to_keep]

data<-data[!is.na(data$OS),]
data<-data[!is.na(data$PFI),]
data<-data[!is.na(data$OS.Time),]
data<-data[!is.na(data$PFI.Time),]
data<-data[!is.na(data$TCGA.Study) & data$TCGA.Study != "",]
data<-data[!is.na(data$TCGA.Subtype) & data$TCGA.Subtype != "",]
data<-data[!is.na(data$Immune.Subtype) & data$Immune.Subtype != "",]

print("Data loaded and initial NA filtering complete.")

# --- Define UI ---
ui <- fluidPage(
  
  titlePanel("TCGA Subtype Kaplan-Meier Plots"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Use placeholder and ensure unique values AFTER NA filtering
      selectInput("study", "TCGA Study:",
                  choices = c('-'="NA", unique(data$TCGA.Study))),
      
      selectInput("subtype1", "TCGA Subtype 1:",
                  choices = NULL),
      
      selectInput("subtype2", "TCGA Subtype 2:",
                  choices = NULL),
      
      selectInput("immune_subtype1", "TCGA Immune Subtype 1:",
                  choices = NULL),
      
      selectInput("immune_subtype2", "TCGA Immune Subtype 2:",
                  choices = NULL),
      
      selectInput("metric", "Select Metric:",
                  choices = c("Overall Survival (OS)" = "OS",
                              "Progression-Free Interval (PFI)" = "PFI")),
      
      actionButton("submit", "Submit")
    ),

    mainPanel(
      plotOutput("kmPlot", height = "800px"),
      hr(),
      h4("Cox Proportional Hazards Model Summary"),
      helpText("Compares Group 1 vs Group 2 as defined by the selections."),
      verbatimTextOutput("coxSummary")
    )
  )
)

# --- Define Server Logic ---
server <- function(input, output, session) {
  
  # Reactive values to store the submitted inputs
  submitted_values <- reactiveValues(study = NULL, subtype1 = NULL, subtype2 = NULL, immune_subtype1 = NULL, immune_subtype2 = NULL, metric = NULL)
  
  # Reactive value to store Cox summary
  cox_output <- reactiveValues(summary = "Submit inputs to view Cox analysis.")
  
  # Update subtypes based on the selected study
  observeEvent(input$study, {
    # Use "NA" as the value for the placeholder
    req(input$study != "NA") # Check against the placeholder value
    
    # Get unique subtypes for the *selected* study from the pre-filtered data
    subtypes <- unique(data[data$TCGA.Study == input$study,]$TCGA.Subtype)
    immune_subtypes <- unique(data[data$TCGA.Study == input$study,]$Immune.Subtype)
    
    # Add "NA" option for explicit non-selection
    subtypes_with_na <- c('-'="NA", subtypes)
    immune_subtypes_with_na <- c('-'="NA", immune_subtypes)
    
    # Update dropdowns, default to "NA"
    updateSelectInput(session, "subtype1", choices = subtypes_with_na, selected = "NA")
    updateSelectInput(session, "subtype2", choices = subtypes_with_na, selected = "NA")
    updateSelectInput(session, "immune_subtype1", choices = immune_subtypes_with_na, selected = "NA")
    updateSelectInput(session, "immune_subtype2", choices = immune_subtypes_with_na, selected = "NA")
  }, ignoreNULL = TRUE) # ignoreNULL should be TRUE
  
  # When submit button is pressed, store the inputs in reactive values
  observeEvent(input$submit, {
    # Basic check: Ensure a study is selected
    if(is.null(input$study) || input$study == "NA") {
      showModal(modalDialog(title = "Input Error", "Please select a TCGA Study.", easyClose = TRUE))
      return()
    }
    # Store current selections
    submitted_values$study <- input$study
    submitted_values$subtype1 <- input$subtype1
    submitted_values$subtype2 <- input$subtype2
    submitted_values$immune_subtype1 <- input$immune_subtype1
    submitted_values$immune_subtype2 <- input$immune_subtype2
    submitted_values$metric <- input$metric
    
    # Reset Cox output when new submission occurs
    cox_output$summary <- "Processing..."
  })
  
  # Render Kaplan-Meier plot AND calculate Cox model
  output$kmPlot <- renderPlot({
    
    # Ensure that the inputs have been submitted AND study is not the placeholder
    req(submitted_values$study, submitted_values$study != "NA")
    # Check that at least one subtype or immune subtype is selected for each group (allow 'NA')
    req(submitted_values$subtype1, submitted_values$subtype2,
        submitted_values$immune_subtype1, submitted_values$immune_subtype2,
        submitted_values$metric)
    
    # --- Original Filtering Logic ---
    plot_data_base <- data[data$TCGA.Study == submitted_values$study, ]
    
    # Handle potential lack of data for the study
    if(nrow(plot_data_base) == 0) {
      cox_output$summary <- paste("No data found for study:", submitted_values$study)
      # Return empty plot or message plot
      plot.new(); title("No data for selected study")
      return()
    }
    
    plot_data_1 <- plot_data_base
    plot_data_2 <- plot_data_base
    
    plot_data_1$test <- 1
    plot_data_2$test <- 2
    
    # Filter Group 1
    if(!is.null(submitted_values$subtype1) && submitted_values$subtype1 != 'NA'){
      plot_data_1 <- plot_data_1[plot_data_1$TCGA.Subtype == submitted_values$subtype1, ]
    }
    if(!is.null(submitted_values$immune_subtype1) && submitted_values$immune_subtype1 != 'NA'){
      plot_data_1 <- plot_data_1[plot_data_1$Immune.Subtype == submitted_values$immune_subtype1, ]
    }
    
    # Filter Group 2
    if(!is.null(submitted_values$subtype2) && submitted_values$subtype2 != 'NA'){
      plot_data_2 <- plot_data_2[plot_data_2$TCGA.Subtype == submitted_values$subtype2, ]
    }
    if(!is.null(submitted_values$immune_subtype2) && submitted_values$immune_subtype2 != 'NA'){
      plot_data_2 <- plot_data_2[plot_data_2$Immune.Subtype == submitted_values$immune_subtype2, ]
    }
    
    # Combine the two filtered groups
    plot_data <- rbind(plot_data_1, plot_data_2)
    
    # Check if data exists after filtering
    if(nrow(plot_data) == 0) {
      cox_output$summary <- "No data remaining after applying subtype filters."
      plot.new(); title("No data for selected subtypes")
      return()
    }
    # Check if both groups (test=1 and test=2) have data
    if(length(unique(plot_data$test)) < 2) {
      cox_output$summary <- "Data found for only one of the specified groups after filtering. Cannot compare."
      # Proceed to plot the single group if desired, or stop
      plot.new(); title("Only one group found")
      return() # Stop here, cannot do comparison plot or Cox
    }
    
    # --- Original Type Conversion ---
    if("OS" %in% names(plot_data)) plot_data$OS <- suppressWarnings(as.numeric(as.character(plot_data$OS)))
    if("PFI" %in% names(plot_data)) plot_data$PFI <- suppressWarnings(as.numeric(as.character(plot_data$PFI)))
    if("OS.Time" %in% names(plot_data)) plot_data$OS.Time <- suppressWarnings(as.numeric(as.character(plot_data$OS.Time)))
    if("PFI.Time" %in% names(plot_data)) plot_data$PFI.Time <- suppressWarnings(as.numeric(as.character(plot_data$PFI.Time)))
    
    if (submitted_values$metric == "OS") {
      req_cols <- c("OS.Time", "OS")
    } else {
      req_cols <- c("PFI.Time", "PFI")
    }

    if (!all(req_cols %in% names(plot_data))) {
      missing_req <- req_cols[!req_cols %in% names(plot_data)]
      cox_output$summary <- paste("Error: Required column(s) missing after processing:", paste(missing_req, collapse=", "))
      plot.new(); title("Error: Missing required columns")
      return()
    }
    plot_data <- plot_data[complete.cases(plot_data[, req_cols]), ]
    
    # Ensure time is non-negative
    time_col_name <- req_cols[1]
    plot_data <- plot_data[plot_data[[time_col_name]] >= 0, ]
    
    # Check again if data remains after cleaning NAs and negative times
    if(nrow(plot_data) == 0) {
      cox_output$summary <- "No valid data (non-NA, non-negative time) remaining for the selected metric."
      plot.new(); title("No valid data remaining")
      return()
    }
    # Check again if both groups still have data
    if(length(unique(plot_data$test)) < 2) {
      cox_output$summary <- "Data for only one group remains after cleaning NAs/times. Cannot compare."
      plot.new(); title("Only one group remains after cleaning")
      return()
    }

    plot_data$test <- factor(plot_data$test)
    
    # --- Create survfit object ---
    fit <- NULL
    time_col <- NULL
    status_col <- NULL
    if (submitted_values$metric == "OS") {
      time_col <- "OS.Time"
      status_col <- "OS"
      fit <- tryCatch(survfit(Surv(OS.Time, OS) ~ test, data = plot_data),
                      error = function(e) NULL)
    } else { # PFI
      time_col <- "PFI.Time"
      status_col <- "PFI"
      fit <- tryCatch(survfit(Surv(PFI.Time, PFI) ~ test, data = plot_data),
                      error = function(e) NULL)
    }
    
    # Check if survfit succeeded
    if(is.null(fit)){
      cox_output$summary <- "Error creating survival fit object. Check data."
      plot.new(); title("Error in survfit")
      return()
    }
    
    # --- Cox Proportional Hazards Analysis ---
    cox_model <- NULL
    cox_summary_text <- "Cox model could not be fitted. Check data and event counts."
    if (!is.null(time_col) && !is.null(status_col)) {
      tryCatch({
        formula_cox <- as.formula(paste("Surv(", time_col, ",", status_col, ") ~ test"))
        cox_model <- coxph(formula_cox, data = plot_data)
        cox_summary_text <- capture.output(summary(cox_model))
        cox_summary_text <- paste(cox_summary_text, collapse = "\n")
      }, error = function(e) {
        cox_summary_text <<- paste("Cox model fitting error:", e$message)
        message("Cox model fitting error: ", e$message)
      })
    } else {
      cox_summary_text <- "Metric selection error, cannot determine time/status columns for Cox model."
    }
    cox_output$summary <- cox_summary_text
    
    # --- Output KM plot (Original logic) ---
    group1_label <- paste("Group 1 (Subtype:", submitted_values$subtype1, ", Immune:", submitted_values$immune_subtype1, ")")
    group2_label <- paste("Group 2 (Subtype:", submitted_values$subtype2, ", Immune:", submitted_values$immune_subtype2, ")")
    
    ggsurvplot(
      fit,
      data = plot_data,
      risk.table = TRUE,
      pval = TRUE,
      conf.int = TRUE,
      palette = c("#E69F00", "#56B4E9"),
      xlab = "Time (days)",
      ylab = paste(submitted_values$metric, "Probability"),
      legend.title = "Groups",
      legend.labs = c(group1_label, group2_label),
      risk.table.y.text.col = TRUE,
      risk.table.height = 0.2,
      risk.table.y.text = FALSE,
      ncensor.plot = TRUE,
      ncensor.plot.height = 0.2,
      surv.median.line = "hv",
      ggtheme = theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = "#f2f2f2", colour = "#f2f2f2", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#dbe3db"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#dbe3db"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
      )
    )
    
  })
  
  # --- Render Cox Model Summary ---
  output$coxSummary <- renderPrint({
    # Display the summary stored in the reactive value
    cat(cox_output$summary) 
  })
  
} # End server

# --- Run the Application ---
shinyApp(ui = ui, server = server)