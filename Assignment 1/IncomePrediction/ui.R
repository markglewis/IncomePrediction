

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Income Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("Age","Age", value = "21", width = NULL, placeholder = NULL),
      selectInput("workclass", "Work Class:",
                  c("Private" = " Private",
                    "Self Employed Not inc" = " Self-emp-not-inc",
                    "Local Government" = " Local-gov",
                    "State Government" = " State-gov",
                    "Self Employed inc" = " Self-emp-inc",
                    "Federal Government" = " Federal-gov",
                    "Never-worked" = "  Never-worked",
                    "Without Pay" = " Without-pay")),
      selectInput("education", "Education:",
                  c("Highscool Graduate" = " HS-grad",
                    "College" = " Some-college",
                    "Bachelors" = " Bachelors",
                    "Masters" = " Masters",
                    "Assoc-voc" = " Assoc-voc",
                    "Doctorate" = " Doctorate",
                    "Assoc Academic" = " Assoc-acdm",
                    "Professional School" = " Prof-school",
                    "Masters" = " Masters")),
      selectInput("marStat", " Marital Status:",
                  c("Divorced" = " Divorced",
                    "Married" = " Married-civ-spouse",
                    "Married Absent Spouse" = " Married-spouse-absent",
                    "Never Married" = " Never-married",
                    "Separated" = " Separated",
                    "Widowed" = " Widowed")),
      selectInput("occupation", " Occupation",
                  c("Proffesional Specialty" = " Prof-specialty",
                    "Manegerial Exec" = " Exec-managerial",
                    "Adm-clerical" = " Adm-clerical",
                    "Craft-repair" = " Craft-repair",
                    "Machine Operation/Inspection" = " Machine-op-inspct",
                    "Sales" = " Sales",
                    "Armed-Forces" = " Armed-Forces",
                    "Farming-fishing" = " Farming-fishing",
                    "Handlers-cleaners" = " Handlers-cleaners",
                    "Protective services" = " Protective-serv",
                    "Private House Servent" = " Priv-house-serv",
                    "Transportation" = " Transport-moving",
                    "Tech-support" = " Tech-support",
                    "Other Services" = " Other-service")),
      selectInput("relationship", " Relationship",
                  c("Husband" = " Husband",
                    "No family" = " Not-in-family",
                    "Other relative" = " Other-relative",
                    "Own child" = " Own-child",
                    "Unmarried" = " Unmarried",
                    "Wife" = " Wife")),
      selectInput("Race", "Race",
                  c("White" = " White",
                    "African-American" = " Black",
                    "American Indian" = " Amer-Indian-Eskimo",
                    "Asian" = " Asian-Pac-Islander",
                    "Other" = " Other")),
      selectInput("Sex", "Sex",
                  c("Female" = " Female",
                    "Male" = " Male")),
      textInput("hrs-per-week","Hours of Work Per Week", value = "40", width = NULL, placeholder = NULL)
      
    ),
  
    
    mainPanel(
      actionButton("Submitbtn", "Submit"),
      textOutput("incomeOutput"),
      textOutput("inputOutput"),
      mainPanel("The C5.0 model was trained and tested with k-fold crossvalidation. With data from UCI
machine learning repository, Which can be found here: https://archive.ics.uci.edu/ml/datasets/Census+Income.
                ")
      
    )
  )
  
  
))
