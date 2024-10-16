#——————————————————————————————————————————Data Collection————————————————————————————————————————————
rm(list=ls())
df_bodyfat <- read.csv("BodyFat.csv", header = TRUE)
#————————————————————————————Check the consistency of DENSITY and BODYFAT—————————————————————————————
calculate_body_fat <- function(D) {
  a <- 1.10
  b <- 0.90
  B <- (1 / D) * (a * b / (a - b)) - (b / (a - b))
  return(B * 100)
}
df_bodyfat$calculated_bodyfat <- calculate_body_fat(df_bodyfat$DENSITY)
df_bodyfat$error <- df_bodyfat$BODYFAT - df_bodyfat$calculated_bodyfat
error_rows1 <- df_bodyfat[abs(df_bodyfat$error) > 2, ]
# print(error_rows1)
indices_to_replace <- c(48, 76)# Incorrect calculation of bodyfat
indices_to_na <- c(96, 182, 216)# Abnormal value of bodyfat
df_bodyfat$BODYFAT[indices_to_replace] <- error_rows1$calculated_bodyfat[error_rows1$IDNO %in% indices_to_replace]
df_bodyfat$BODYFAT[indices_to_na] <- NA
# df_bodyfat[indices_to_replace,]
# df_bodyfat[indices_to_na,]
df_bodyfat <- df_bodyfat[,1:17]
#—————————————————————————————Check the consistency of HEIGHT,WEIGHT,ADIPOSITY——————————————————————————
library(dplyr)  
calculate_bmi <- function(weight, height) {
  height_m <- height * 0.0254  
  weight_kg <- weight / 2.2     
  bmi <- weight_kg / (height_m^2)  
  return(bmi)
}
df_bodyfat$calculatedBMI <- mapply(calculate_bmi, df_bodyfat$WEIGHT, df_bodyfat$HEIGHT)
df_bodyfat$error <- df_bodyfat$ADIPOSITY - df_bodyfat$calculatedBMI
error_rows2 <- df_bodyfat[abs(df_bodyfat$error) > 0.5, ]
# print(error_rows2)
calculate_height_from_bmi <- function(weight, bmi) {
  weight_kg <- weight / 2.2     
  height_m <- sqrt(weight_kg / bmi)  
  height_inches <- height_m / 0.0254  
  return(height_inches)
}
calculate_weight_from_bmi <- function(height, bmi) {
  height_m <- height * 0.0254  
  weight_kg <- bmi * (height_m^2)  
  weight_lbs <- weight_kg * 2.2     
  return(weight_lbs)
}
indices_to_na_height <- c(42,163)# Wrong calculation of HEIGHT
indices_to_na_weight <- c(221)# Wrong calculation of WEIGHT
df_bodyfat$HEIGHT[indices_to_na_height] <- NA
df_bodyfat$WEIGHT[indices_to_na_weight] <- NA
df_bodyfat <- df_bodyfat %>%
  mutate(HEIGHT = ifelse(row_number() %in% indices_to_na_height,
                         calculate_height_from_bmi(WEIGHT, ADIPOSITY),
                         HEIGHT))#Impute the HEIGHT with function of WEIGHT and ADIPOSITY
df_bodyfat <- df_bodyfat %>%
  mutate(WEIGHT = ifelse(row_number() %in% indices_to_na_weight,
                         calculate_weight_from_bmi(HEIGHT, ADIPOSITY),
                         WEIGHT))#Impute the WEIGHT with function of HEIGHT and ADIPOSITY
# df_bodyfat[indices_to_na_height,]
# df_bodyfat[indices_to_na_weight,]
df_bodyfat <- df_bodyfat[,1:17]
#———————————————————————————Check outliers with IQR method.Check the abnormal BODYFAT_______________
df_bodyfat <- df_bodyfat[, !names(df_bodyfat) %in% c("IDNO", "DENSITY")]
find_outliers <- function(df) {
  outlier_results <- list() 
  for (col_name in colnames(df)) {
    Q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outlier_indices <- which(df[[col_name]] < lower_bound | 
                               df[[col_name]] > upper_bound | 
                               df[[col_name]] <= 0)
    if (length(outlier_indices) > 0) {
      outlier_values <- data.frame(
        Index = outlier_indices,
        Value = df[[col_name]][outlier_indices]
      )
      outlier_results[[col_name]] <- outlier_values 
    }
  }
  bodyfat_outlier_indices <- which(df$BODYFAT < 3 | df$BODYFAT > 40)
  if (length(bodyfat_outlier_indices) > 0) {
    bodyfat_outlier_values <- data.frame(
      Index = bodyfat_outlier_indices,
      Value = df$BODYFAT[bodyfat_outlier_indices]
    )
    outlier_results[["BODYFAT"]] <- rbind(outlier_results[["BODYFAT"]], bodyfat_outlier_values)
  }
  return(outlier_results)
}
outliers_result <- find_outliers(df_bodyfat)
# print(outliers_result)
#----------------------Find the index that only occurs once and define them as outliers-----------
outliers_summary <- data.frame(Index = integer(), Feature = character(), stringsAsFactors = FALSE)
for (feature_name in names(outliers_result)) {
  feature_data <- outliers_result[[feature_name]]
  feature_indexes <- feature_data$Index
  new_rows <- data.frame(Index = feature_indexes, Feature = feature_name, stringsAsFactors = FALSE)
  outliers_summary <- rbind(outliers_summary, new_rows)
}
outliers_summary <- outliers_summary[order(outliers_summary$Index), ]
index_counts <- table(outliers_summary$Index)
unique_indices <- as.numeric(names(index_counts[index_counts == 1]))
outliers_summary <- outliers_summary[outliers_summary$Index %in% unique_indices, ]
# outliers_summary
# df_bodyfat[unique_indices,]
#--------------------------------Impute with regression--------------------------------------------
for (i in 1:nrow(outliers_summary)) {
  index <- outliers_summary$Index[i]
  feature <- outliers_summary$Feature[i]
  df_bodyfat[index, feature] <- NA
}
impute_with_regression <- function(data, feature) {
  model <- lm(as.formula(paste(feature, "~ .")), data = data)
  predictions <- predict(model, newdata = data[is.na(data[[feature]]), ])
  data[is.na(data[[feature]]), feature] <- predictions
  return(data)
}
for (feature in unique(outliers_summary$Feature)) {
  df_bodyfat <- impute_with_regression(df_bodyfat, feature)
}
#————————————————————————————————————————————Define X and y————————————————————————————————————————
y<- df_bodyfat$BODYFAT
X <- df_bodyfat[, !names(df_bodyfat) %in% "BODYFAT"]
#——————————————————————————————————————————————Model———————————————————————————————————————————————
data <- cbind(y, X)
model <- lm(y ~ ABDOMEN + WEIGHT, data = data)
summary(model)

#——————————————————————————————————————————————Shiny———————————————————————————————————————————————
library(shiny)

# Define the linear regression model
predict_body_fat <- function(weight, abdomen) {
  # Linear regression formula
  intercept <- -40.42
  coef_abdomen <- 0.92
  coef_weight <- -0.14
  
  # Prediction calculation
  predicted_y <- intercept + coef_abdomen * abdomen + coef_weight * weight
  return(predicted_y)
}

ui <- fluidPage(
  titlePanel(
    div(
      h2("Body Fat Predictor", align = "center"),
      div("Group 7", style = "text-align: right; font-size: 1em; color: gray; margin-top: -10px;")
    )
  ),
  
  tabsetPanel(
    tabPanel("Predictor",
             fluidRow(
               column(width = 6, offset = 3,
                      div(style = "background-color: #f7f7f7; padding: 40px; border-radius: 8px; box-shadow: 0px 3px 8px rgba(0, 0, 0, 0.1);",
                          h4("Please input your Weight and Abdomen:", align = "center"),
                          numericInput("WEIGHT", "Weight (lbs):", value = NA, min = 50, max = 500, width = "100%"),
                          numericInput("ABDOMEN", "Abdomen (cm):", value = NA, min = 50, max = 200, width = "100%"),
                          actionButton("predict", "Predict Body Fat", class = "btn-primary", style = "width: 100%; background-color: #5b9bd5; border: none; margin-top: 20px;"),
                          textOutput("bodyfat_result"),
                          verbatimTextOutput("warning_message"),
                          div(style = "margin-top: 40px; font-size: 1.5em;",
                              h4("Prediction Result", align = "center"),
                              verbatimTextOutput("prediction", placeholder = TRUE)
                          ),
                          div(style = "margin-top: 40px;",
                              tags$img(src = "body_image.png", height = "300px", id = "bodyImage"),
                              uiOutput("highlight_output"),
                              tags$div(style = "font-size: 1.5em;", textOutput("category_text"))
                          )
                      )
               )
             )
    ),
    tabPanel("FAQ",
             fluidRow(
               column(width = 8, offset = 2,
                      h3("Frequently Asked Questions", align = "center"),
                      br(),
                      h4("1. What does this app do?"),
                      p("This app predicts your body fat percentage based on two inputs: your weight (in pounds) and abdomen circumference (in centimeters). The prediction is calculated using a regression model built from a dataset of body measurements."),
                      h4("2. How does the prediction model work?"),
                      p("The model uses a simple linear regression formula to estimate body fat percentage:"),
                      p("Body Fat % = -40.42 + 0.92 × Abdomen (cm) - 0.14 × Weight (lbs)"),
                      p("As your abdomen circumference increases, the predicted body fat percentage also increases, while higher weight slightly decreases the predicted percentage. The coefficients were determined through statistical analysis of a sample dataset."),
                      h4("3. What input values should I provide?"),
                      p("• Weight (lbs): Enter your weight in pounds. Typical adult weights range from 100 to 300 lbs, but values outside this range are accepted if needed."),
                      p("• Abdomen (cm): Measure the circumference of your abdomen at the level of your belly button. This measurement is usually between 60 cm and 150 cm for most adults."),
                      h4("4. What should I do if my predicted body fat percentage is high?"),
                      p("If your predicted body fat is higher than the recommended range, consider lifestyle changes such as improving your diet or increasing physical activity. Consult a healthcare professional for personalized advice."),
                      h4("5. What should I do if I encounter an error?"),
                      p("If you see an error message, it likely means that one or more required inputs are missing. Here’s how to resolve common errors:"),
                      p("• 'Error: Please provide a value for Weight.' This means the Weight input is missing. Make sure to enter your weight in pounds."),
                      p("• 'Error: Please provide a value for Abdomen.' This indicates that the Abdomen circumference input is missing. Be sure to enter the measurement in centimeters."),
                      p("• 'Error: Please provide both Weight and Abdomen values.' If you see this message, it means that both input fields are empty. Fill in both your weight and abdomen circumference to proceed with the prediction."),
                      h4("6. How can I visually interpret my predicted body fat?"),
                      p("You can use the contour plot below to understand where you fall within the body fat categories based on your weight and abdomen circumference measurements. Each contour represents a different body fat range, helping you quickly assess whether you are below athlete, in the athlete, fit, average, or obese category."),
                      tags$img(src = "contour_plot.png", alt = "Contour Plot", style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%;"),
                      h4("7.Contact Infomation:"),
                      p("Group7  rchen394@wisc.edu")
               )
             )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    inputs <- c(
      WEIGHT = input$WEIGHT,
      ABDOMEN = input$ABDOMEN
    )
    
    # Check if any inputs are missing
    if (any(is.na(inputs))) {
      output$prediction <- renderPrint({
        cat("Error: Please fill in all predictor values.")
      })
      # Clear previous highlight if there is an error
      output$highlight_output <- renderUI({ NULL })
    } else {
      # Predict body fat percentage using the regression model
      predicted_y <- predict_body_fat(weight = input$WEIGHT, abdomen = input$ABDOMEN)
      
      # Output the prediction
      output$prediction <- renderPrint({
        cat("Predicted Body Fat is", round(predicted_y, 2),"%")
      })
      
      # Check for abnormal body fat percentage
      if (predicted_y < 5 || predicted_y > 50) {
        output$warning_message <- renderText({
          paste("Warning: The predicted body fat percentage (", round(predicted_y, 2), "%) is outside the normal range!")
        })
      } else {
        output$warning_message <- renderText({ "" })
      }
      
      # Clear the highlight if the predicted value is less than 6
          output$highlight_output <- renderUI({
          if (predicted_y <= 5) {
            tags$div(style = "position: relative; top: -300px; left: 15px;",
                     tags$div(style = "border: 3px solid red; height: 300px; width: 100px; position: absolute;"))
          } else if (predicted_y > 5 && predicted_y <= 14) {
            tags$div(style = "position: relative; top: -300px; left: 110px;",
                     tags$div(style = "border: 3px solid red; height: 300px; width: 100px; position: absolute;"))
          } else if (predicted_y > 14 && predicted_y <= 18) {
            tags$div(style = "position: relative; top: -300px; left: 215px;",
                     tags$div(style = "border: 3px solid red; height: 300px; width: 100px; position: absolute;"))
          } else if (predicted_y > 18 && predicted_y <= 25) {
            tags$div(style = "position: relative; top: -300px; left: 310px;",
                     tags$div(style = "border: 3px solid red; height: 300px; width: 100px; position: absolute;"))
          } else {
            tags$div(style = "position: relative; top: -300px; left: 415px;",
                     tags$div(style = "border: 3px solid red; height: 300px; width: 100px; position: absolute;"))
          }
        })

      
      # Set category text based on prediction
      output$category_text <- renderText({
        if (predicted_y <= 6) {
          "You are in the Underweight category."
        } else if (predicted_y > 6 && predicted_y <= 14) {
          "You are in the Athlete category."
        } else if (predicted_y > 14 && predicted_y <= 18) {
          "You are in the Fit category."
        } else if (predicted_y > 18 && predicted_y <= 25) {
          "You are in the Average category."
        } else {
          "You are in the Obese category."
        }
        
      })
    }
  })
}

shinyApp(ui = ui, server = server)
