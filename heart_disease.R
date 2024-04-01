library(shiny)
library(ggplot2)
library(ggcorrplot)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinythemes)
library(caret)



# anksjfnasfknasnkl

# Reading data and Creating a model ---------------------------------------


heart_data <- read.csv("heart.csv")


# Transforming data
heart_data$target <- as.factor(heart_data$target)
heart_data$sex <- as.numeric(heart_data$sex)
heart_data$cp <- as.numeric(heart_data$cp)
heart_data$restecg <- as.numeric(heart_data$restecg)
heart_data$exang <- as.numeric(heart_data$exang)
heart_data$slope <- as.numeric(heart_data$slope)
heart_data$thal <- as.numeric(heart_data$thal)

# Logistic Regression Model
set.seed(1024)
log_model <- train(target ~ ., data = heart_data, 
                        method = "glm", family = "binomial")




# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "Heart Disease Prediction"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dataset", tabName = "data_table", icon = icon("table")),
      menuItem("Data Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Prediction", tabName = "prediction", icon = icon("heartbeat")),
      menuItem("Model Accuracy", tabName = "accuracy", icon = icon("chart-line"))
      
    )
  ),
  
  
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              tags$style(type="text/css",
                         ".content-wrapper { 
                         background-image: url('https://hai.stanford.edu/sites/default/files/news/teaser-images/heart%20illustration.jpg'); 
                         background-size: cover; }"),
              fluidRow(
                valueBoxOutput("total_records"),
                valueBoxOutput("male_heart_disease"),
                valueBoxOutput("female_heart_disease")
              ),
             ),
      
     
      # Data Summary tab
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = "Heart Disease Distribution",
                  plotOutput("target_plot"),
                  background = "navy"
                ),
                
                box(
                  title = "Age vs Cholestrol",
                  plotOutput("chols"),
                  background = "navy"
                ),
                
                box(
                  title = "Age Distribution",
                  plotOutput("age_plot"),
                  background = "navy"
                ),
                
                box(
                  title = "Chest Pain Type Distribution",
                  plotOutput("cp_plot"),
                  background = "navy"
                )
              )),
      
      
      # Prediction tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = "Input Parameters",
                  numericInput("age", "Age", min = 0, max = 100, value = 50),
                  selectInput("sex", "Sex", c("Male", "Female")),
                  selectInput("cp", "Chest Pain Type", 
                              c("Typical Angina", "Atypical Angina", 
                                "Non-Anginal Pain", "Asymptomatic")),
                  numericInput("trestbps", "Resting Blood Pressure (mm Hg)", 
                               min = 0, max = 300, value = 120),
                  numericInput("chol", "Serum Cholesterol (mg/dL)", 
                               min = 0, max = 600, value = 200),
                  selectInput("fbs", "Fasting Blood Sugar > 120 mg/dL", 
                              c("No", "Yes")),
                  selectInput("restecg", "Resting Electrocardiographic Results", 
                              c("Normal", "ST-T Wave Abnormality", 
                                "Probable or Definite Left Ventricular Hypertrophy")),
                  numericInput("thalach", "Maximum Heart Rate Achieved", 
                               min = 0, max = 300, value = 150),
                  selectInput("exang", "Exercise Induced Angina", c("No", "Yes")),
                  numericInput("oldpeak", "ST Depression Induced by Exercise Relative to Rest", 
                               min = 0, max = 10, value = 1),
                  selectInput("slope", "Slope of the Peak Exercise ST Segment", 
                              c("Upsloping", "Flat", "Downsloping")),
                  numericInput("ca", "Number of Major Vessels (0-3) Colored by Flourosopy", 
                               min = 0, max = 3, value = 0),
                  selectInput("thal", "Thalassemia", c("Normal", "Fixed Defect", 
                                                       "Reversible Defect")),
                  actionButton("predict", "Predict"),
                  background = "navy"
                ),
                
                # Result box
                box(
                  title = "Prediction",
                  textOutput("result"),
                  background = "navy"
                )
              )),
      
      #Model Accuracy tab
      tabItem(tabName = "accuracy",
              fluidRow(
                box(
                  title = "Model Accuracy",
                  textOutput("logistic_accuracy"),
                  background = "navy"
                )
              )),
      
      #Original Data tab
      tabItem(tabName = "data_table",
              fluidRow(
                box(
                  width = 25, solidHeader = TRUE, status = "danger",
                  title = "Heart Disease Data",
                  dataTableOutput("data_table")
                )
              ))
    )
  )
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  prediction_result <- reactiveVal(NULL)
  
  #Summary statistics
  total_records <- nrow(heart_data)
  male_heart_disease <- sum(heart_data$sex == 1 & heart_data$target == 1)
  female_heart_disease <- sum(heart_data$sex == 0 & heart_data$target == 1)
  
  observeEvent(input$predict, {
    df <- data.frame(
      age = input$age,
      sex = ifelse(input$sex == "Male", 1, 0),
      cp = match(input$cp, c("Typical Angina", "Atypical Angina", 
                             "Non-Anginal Pain", "Asymptomatic")) - 1,
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = ifelse(input$fbs == "Yes", 1, 0),
      restecg = match(input$restecg, c("Normal", "ST-T Wave Abnormality", 
                                       "Probable or Definite Left Ventricular Hypertrophy")) - 1,
      thalach = input$thalach,
      exang = ifelse(input$exang == "Yes", 1, 0),
      oldpeak = input$oldpeak,
      slope = match(input$slope, c("Upsloping", "Flat", "Downsloping")) - 1,
      ca = input$ca,
      thal = match(input$thal, c("Normal", "Fixed Defect", "Reversible Defect")) - 1
    )
    
    log_pred <- predict(log_model, df, type = "prob")[, 2]
    
    prediction_result(paste0("Probability of having a heart disease is ", 
                             round(log_pred * 100, 2), "%"))
  })
  
  
  #----Output data----
  
  #Dataset
  output$data_table <- renderDataTable({
    heart_data
  })
  
  # Render value boxes
  output$total_records <- renderValueBox({
    valueBox(total_records, "Total Records", icon = icon("database"), color = "maroon")
  })
  
  output$male_heart_disease <- renderValueBox({
    valueBox(male_heart_disease, "Males with Heart Disease", icon = icon("male"), color = "maroon")
  })
  
  output$female_heart_disease <- renderValueBox({
    valueBox(female_heart_disease, "Females with Heart Disease", icon = icon("female"), color = "maroon")
  })
  
  
  #---Plots---
  output$target_plot <- renderPlot({
    labels <- c("No", "Yes")
    ggplot(heart_data, aes(x = target, fill = target)) +
      geom_bar() +
      labs(x = "Heart Disease", y = "Patient Count") +
      scale_x_discrete(label = labels) +
      scale_fill_manual(values=c("#ce4257", "#ce4257")) +
      theme(legend.position = "none")
  })
  
  output$chols <- renderPlot({
    ggplot(data = heart_data) + 
      geom_point(mapping = aes(x = age, y = chol, col = target)) +
      labs(x = "Age", y = "Cholestrol") +
      scale_color_manual(values=c("#ffbd00", "#ce4257")) +
      theme(legend.position = "none")
  })
  
  output$age_plot <- renderPlot({
    ggplot(heart_data, aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "#ce4257") +
      labs(title = "Age Distribution", x = "Age", y = "Frequency")
  })

  output$cp_plot <- renderPlot({
    chestpain <- as.factor(heart_data$cp)
    lab <- c("Typical Angina", "Atypical Angina", "Non-Anginal Pain", "Asymptomatic")
    ggplot(heart_data, aes(x = chestpain, fill = chestpain)) +
      geom_bar() +
      labs(x = "Chest Pain Type",  y = "Frequency") +
      scale_x_discrete(label = lab) +
      scale_fill_manual(values=c("#c31b4d", "#ce4257", "#ff0054", "#9e0059")) +
      theme(legend.position = "none")
  })
  
  
  #---Prediction---
  output$result <- renderText({
    prediction_result()
  })
  
  
  #---Model Accuracy
  output$logistic_accuracy <- renderText({
    paste0("The accuracy of the logistic regression model is: ", round(log_model$results$Accuracy * 100, 2), "%")
  })
  
  
  
}



shinyApp(ui, server)

