


## R Markdown



library(rsconnect)
rsconnect::deployApp("C:/Users/benji/OneDrive/Documents/Shiny App Code.R")

# app.R
# Shiny app: Predict depression trajectories (DepScore) across age
# using NHANES-like clean_data

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# --------------------------------------------------------------------
# 0. Load and prepare data
# --------------------------------------------------------------------



# Make sure variables are in the right type
clean_data <- clean_data |>
  filter(
    !is.na(DepScore),
    !is.na(Age),
    !is.na(Race1),
    !is.na(RIAGENDR),
    !is.na(HHIncome),
    !is.na(Work),
    !is.na(SleepHrsNight),
    !is.na(BMI)
  ) |>
  mutate(
    Race1    = factor(Race1),
    RIAGENDR = factor(RIAGENDR),
    HHIncome = factor(HHIncome),
    Work     = factor(Work)
    # BMI and Age and SleepHrsNight are treated as numeric
  )

age_min <- floor(min(clean_data$Age, na.rm = TRUE))
age_max <- ceiling(max(clean_data$Age, na.rm = TRUE))

# Fit the linear regression model once
dep_model <- lm(
  DepScore ~ Age + Race1 + RIAGENDR + HHIncome + Work + SleepHrsNight + BMI,
  data = clean_data
)

# --------------------------------------------------------------------
# 1. UI
# --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Depression Trajectories Across Age: Current vs Future Profile"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Your current profile"),
      selectInput(
        "race_now",
        "Race (Race1):",
        choices = levels(clean_data$Race1),
        selected = levels(clean_data$Race1)[1]
      ),
      selectInput(
        "gender_now",
        "Gender (RIAGENDR):",
        choices = levels(clean_data$RIAGENDR),
        selected = levels(clean_data$RIAGENDR)[1]
      ),
      selectInput(
        "income_now",
        "Household income (HHIncome):",
        choices = levels(clean_data$HHIncome),
        selected = levels(clean_data$HHIncome)[1]
      ),
      selectInput(
        "work_now",
        "Work status (Work):",
        choices = levels(clean_data$Work),
        selected = levels(clean_data$Work)[1]
      ),
      sliderInput(
        "sleep_now",
        "Sleep hours per night (SleepHrsNight):",
        min = floor(min(clean_data$SleepHrsNight, na.rm = TRUE)),
        max = ceiling(max(clean_data$SleepHrsNight, na.rm = TRUE)),
        value = round(mean(clean_data$SleepHrsNight, na.rm = TRUE)),
        step = 0.5
      ),
      sliderInput(
        "bmi_now",
        "BMI:",
        min = floor(min(clean_data$BMI, na.rm = TRUE)),
        max = ceiling(max(clean_data$BMI, na.rm = TRUE)),
        value = round(mean(clean_data$BMI, na.rm = TRUE)),
        step = 0.5
      ),
      
      tags$hr(),
      h3("Your future / goal profile"),
      helpText("Race stays the same as your current profile."),
      # Race is fixed to race_now; we won't show a separate race selector here
      
      selectInput(
        "gender_future",
        "Future gender (RIAGENDR):",
        choices = levels(clean_data$RIAGENDR),
        selected = levels(clean_data$RIAGENDR)[1]
      ),
      selectInput(
        "income_future",
        "Future HHIncome:",
        choices = levels(clean_data$HHIncome),
        selected = levels(clean_data$HHIncome)[1]
      ),
      selectInput(
        "work_future",
        "Future Work status:",
        choices = levels(clean_data$Work),
        selected = levels(clean_data$Work)[1]
      ),
      sliderInput(
        "sleep_future",
        "Future Sleep hours per night:",
        min = floor(min(clean_data$SleepHrsNight, na.rm = TRUE)),
        max = ceiling(max(clean_data$SleepHrsNight, na.rm = TRUE)),
        value = round(mean(clean_data$SleepHrsNight, na.rm = TRUE)),
        step = 0.5
      ),
      sliderInput(
        "bmi_future",
        "Future BMI:",
        min = floor(min(clean_data$BMI, na.rm = TRUE)),
        max = ceiling(max(clean_data$BMI, na.rm = TRUE)),
        value = round(mean(clean_data$BMI, na.rm = TRUE)),
        step = 0.5
      ),
      
      tags$hr(),
      h3("Age to inspect"),
      sliderInput(
        "age_slider",
        "Age:",
        min = age_min,
        max = age_max,
        value = round((age_min + age_max) / 2),
        step = 1
      )
    ),
    
    mainPanel(
      h3("Predicted depression score vs age"),
      plotOutput("traj_plot", height = "450px"),
      
      h3("Predicted scores at selected age"),
      verbatimTextOutput("pred_text"),
      
      h4("Model used"),
      verbatimTextOutput("model_print"),
      
      helpText("Note: This app uses a linear regression model fit to survey data to illustrate associations, not to provide medical advice or diagnosis.")
    )
  )
)

# --------------------------------------------------------------------
# 2. Server
# --------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Helper: build newdata for a given profile across ages
  make_profile_df <- function(profile, ages) {
    # profile is a list with elements:
    # race, gender, income, work, sleep, bmi
    
    data.frame(
      Age           = ages,
      Race1         = factor(profile$race,    levels = levels(clean_data$Race1)),
      RIAGENDR      = factor(profile$gender,  levels = levels(clean_data$RIAGENDR)),
      HHIncome      = factor(profile$income,  levels = levels(clean_data$HHIncome)),
      Work          = factor(profile$work,    levels = levels(clean_data$Work)),
      SleepHrsNight = profile$sleep,
      BMI           = profile$bmi
    )
  }
  
  # Reactive: profile lists
  profile_now <- reactive({
    list(
      race   = input$race_now,
      gender = input$gender_now,
      income = input$income_now,
      work   = input$work_now,
      sleep  = input$sleep_now,
      bmi    = input$bmi_now
    )
  })
  
  profile_future <- reactive({
    list(
      race   = input$race_now,  # race stays the same
      gender = input$gender_future,
      income = input$income_future,
      work   = input$work_future,
      sleep  = input$sleep_future,
      bmi    = input$bmi_future
    )
  })
  
  # Reactive: prediction data for plotting
  pred_data <- reactive({
    ages <- seq(age_min, age_max, by = 1)
    
    df_now    <- make_profile_df(profile_now(), ages)
    df_future <- make_profile_df(profile_future(), ages)
    
    df_now$Predicted    <- as.numeric(predict(dep_model, newdata = df_now))
    df_future$Predicted <- as.numeric(predict(dep_model, newdata = df_future))
    
    df_now$Scenario    <- "Current profile"
    df_future$Scenario <- "Future profile"
    
    bind_rows(df_now, df_future)
  })
  
  # Plot trajectories
  output$traj_plot <- renderPlot({
    df <- pred_data()
    
    ggplot(df, aes(x = Age, y = Predicted, colour = Scenario)) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = input$age_slider, linetype = "dashed") +
      geom_point(
        data = df |> filter(Age == input$age_slider),
        aes(x = Age, y = Predicted, colour = Scenario),
        size = 3
      ) +
      scale_colour_manual(values = c("Current profile" = "steelblue", "Future profile" = "firebrick")) +
      labs(
        x = "Age",
        y = "Predicted depression score (0b