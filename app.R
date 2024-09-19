# Load libraries
library(shiny)
library(survival)
library(survminer)
library(ggplot2)

# Load data
data<-read.delim('data/NIHMS958212-supplement-2.csv', sep=",")
data<-data[,colnames(data) %in% c("TCGA.Study", "TCGA.Subtype", "OS", "OS.Time", "PFI", "PFI.Time")]
data<-data[!is.na(data$TCGA.Subtype),]
data<-data[!is.na(data$OS),]
data<-data[!is.na(data$PFI),]
data<-data[!is.na(data$OS.Time),]
data<-data[!is.na(data$PFI.Time),]

# Define UI for the application
ui <- fluidPage(
  
  titlePanel("TCGA Subtype Kaplan-Meier Plots"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      selectInput("study", "Select TCGA Study:", 
                  choices = c('-',unique(data$TCGA.Study))),
      
      selectInput("subtype1", "Select First TCGA Subtype:", 
                  choices = NULL),
      
      selectInput("subtype2", "Select Second TCGA Subtype:", 
                  choices = NULL),
      
      selectInput("metric", "Select Metric:", 
                  choices = c("Overall Survival (OS)" = "OS", 
                              "Progression-Free Interval (PFI)" = "PFI")),
      
      actionButton("submit", "Submit")
    ),
    
    # Show a plot of the Kaplan-Meier estimate
    mainPanel(
      plotOutput("kmPlot", height = "800px")
    )
  )
)

# Define server logic required to draw a Kaplan-Meier plot
server <- function(input, output, session) {
  
  # Reactive values to store the submitted inputs
  submitted_values <- reactiveValues(study = NULL, subtype1 = NULL, subtype2 = NULL, metric = NULL)
  
  # Update subtypes based on the selected study
  observeEvent(input$study, {
    
    subtypes <- unique(data[data$TCGA.Study == input$study,]$TCGA.Subtype)
    updateSelectInput(session, "subtype1", choices = subtypes, selected = subtypes[1])
    updateSelectInput(session, "subtype2", choices = subtypes, selected = subtypes[2])
    
  })
  
  # When submit button is pressed, store the inputs in reactive values
  observeEvent(input$submit, {
    submitted_values$study <- input$study
    submitted_values$subtype1 <- input$subtype1
    submitted_values$subtype2 <- input$subtype2
    submitted_values$metric <- input$metric
  })
  
  # Render Kaplan-Meier plot based on the stored values after submit button is clicked
  output$kmPlot <- renderPlot({
    
    # Ensure that the inputs have been submitted
    req(submitted_values$study, submitted_values$subtype1, submitted_values$subtype2, submitted_values$metric)
    
    # Filter data based on the submitted study and subtypes
    plot_data <- data[data$TCGA.Study == submitted_values$study & data$TCGA.Subtype %in% c(submitted_values$subtype1, submitted_values$subtype2),]

    plot_data$TCGA.Study<-as.character(plot_data$TCGA.Study)
    plot_data$OS<-as.numeric(as.character(plot_data$OS))
    plot_data$PFI<-as.numeric(as.character(plot_data$PFI))
    plot_data$OS.Time<-as.numeric(as.character(plot_data$OS.Time))
    plot_data$PFI.Time<-as.numeric(as.character(plot_data$PFI.Time))
    
    plot_data<-plot_data[!is.na(plot_data$TCGA.Study),]
    
    # Create survfit object
    if (submitted_values$metric == "OS") {
      fit <- survfit(Surv(OS.Time, OS) ~ TCGA.Subtype, data = plot_data)
    } else {
      fit <- survfit(Surv(PFI.Time, PFI) ~ TCGA.Subtype, data = plot_data)
    }      
    
    # Output KM plot
    ggsurvplot(
      fit,
      data = plot_data,
      risk.table = TRUE,
      pval = TRUE,
      conf.int = TRUE,
      palette = c("#E69F00", "#56B4E9"),
      xlab = "Time (days)",
      risk.table.y.text.col = TRUE,
      risk.table.height = 0.2,
      risk.table.y.text = FALSE,
      ncensor.plot = TRUE,
      ncensor.plot.height = 0.2,
      surv.median.line = "hv",
      ggtheme = theme(
        legend.position = "none",
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
}

# Run the application 
shinyApp(ui = ui, server = server)
