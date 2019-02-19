#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(rpart)
library(e1071)
library(C50)

shinyServer(function(input, output) {
  c5model <- readRDS("c5model.rds")
  
  
    
    y <- eventReactive( input$Submitbtn,{
      userInput <- data.frame(age=c(as.numeric(input$Age)),
                              workclass=c(input$workclass),
                              education=c(input$education),
                              'education-num'=c(15),
                              marStat=c(input$marStat),
                              occupation=(input$occupation),
                              relationship=(input$relationship),
                              race=c(input$Race),
                              sex=c(input$Sex),
                              'hrs-per-week'=c(as.numeric(input$`hrs-per-week`)))
      p<-predict(c5model,userInput)
      })
    output$incomeOutput<- renderText({
      if (as.numeric(y())==2){"Your income is valued more than $50,000."} else {"Your income valued less than $50,000"}
      
      })
    
    x <- eventReactive(input$Submitbtn,{
      paste("Your values: ",as.character(input$Age),as.character(input$workclass),as.character(input$education),as.character(input$marStat),as.character(input$occupation),as.character(input$relationship),as.character(input$Race) ,as.character(input$Sex),as.character(input$`hrs-per-week`), sep=", ")
       
    })
    output$inputOutput<- renderText({x()})
  
})

#<- data.frame(age=c(as.numeric(28,39)),
#              workclass=c(" Private"," State-gov"),
#              education=c(" Bachelors"," Bachelors"),
#              'education-num'=c(15,15),
#              marStat=c(" Married-civ-spouse"," Never-married"),
#              occupation=c(" Prof-specialty", " Adm-clerical"),
#              relationship=c(" Wife"," Not-in-family"),
#              race=c(" Black"," White"),
#              sex=c(" Female"," Male"),
#              'hrs-per-week'=c(as.numeric(40,13)))

