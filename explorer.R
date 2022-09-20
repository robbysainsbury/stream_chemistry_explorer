#initializes functions and libraries
library(tidyverse)
library(lmtest)
library(GGally)
library(bestglm)
library(shiny)
source("helper_functions.R")#helper functions Robby wrote that make reading in/looking at the data easier

#reading in data
chemistry.alldata = readInData("../../Data/raw/data_tiger/")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Megafire Burn Data"),
    fluidRow(
      column(3,
        fluidRow(
          column(5, #left input col
            #date inputs
            dateInput("startDate", label = "Start Date", width="90%", value = "2018-01-01"),
            
            #drop down menu for selecting x variable
            selectInput("xVar", "Explanatory Variable", width="90%", list(
              `Basic` = basicVars,
              `SCAN` = SCANVars,
              `YSI` = YSIVars)
            ),
            
            selectInput("smoothLine", "Plot Lines?", width="90%", list(
              "None" = " none", "One" = "one", "Burn Level" = "burnlevel"))
          ),
          column(5, #right input col
            dateInput("endDate", label = "End Date", width="90%", value = "2023-01-01"),
            
            #drop down menu for selecting y variable
            selectInput("yVar", "Response Variable", width="90%", list(
              `SCAN` = SCANVars,
              `YSI` = YSIVars)),
              
            #drop down menu for selecting color grouping
            selectInput("groupBy", "Group By", width="90%", list(
              "None" = " none", "Burn Level" = "burnlevel"))
          )
        ),
        
        #Note: Creating the drop down menus for variable selection
        #First argument is the name of the variable, then the label being displayed.
        #The lists each have titles that appear, you can set it so the first "____" is a label 
        #and the actual value is second with an equal sign between them
        
        
        #remove NAs drop down 
        checkboxInput("removeNAs", "Remove *Group* NAs?",TRUE),
        
        actionButton("export","Export the current data set?"),
        actionButton("export","Export the current figure?")
        
      ),
        #output the message 
        #textOutput("varMessage"),
        #under that output the scatter plot
      column(9,
        plotOutput("plot",height = "600px")
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # error checking, not needed anymore
    # output$varMessage <- renderText({
    #   paste("You chose", input$xVar, "and", input$yVar)
    #   })
    
    output$plot <- renderPlot({
      #resetting working data frame to chemistry.alldata between the dates selected 
      #(the default range should be around all of the samples)
      chemistry.working <- createTimeTable(chemistry.alldata,input$startDate,input$endDate)
      #Removes NAs in Burn Level if asked to
      if(input$removeNAs == TRUE){
        chemistry.working <- chemistry.working %>% filter(!is.na(BurnLevel))
      }
      
      if(input$xVar %in% qualXvars){ #box plot
        #getting the column for each of the vars, the pull function allows me to send in a string for the name of the col 
        xCol=pull(chemistry.working,input$xVar)
        yCol=pull(chemistry.working,input$yVar)
        #setting up the axis limits for the y var
        yLims=getLimitsIQR(yCol)
        
        #making the box plot
        ggplot(data=chemistry.working, aes_string(input$xVar,input$yVar,fill=input$xVar)) +
          geom_boxplot() +
          coord_cartesian(ylim = yLims)+
          xlab(getLabel(input$xVar))+ylab(getLabel(input$yVar))+
          labs(title=paste(getLabel(input$yVar), " Explained by ",getLabel(input$xVar),sep = ""))

      }
      else{ #scatter plot
        #getting the column for each of the vars, the pull function allows me to send in a string for the name of the col 
        xCol=pull(chemistry.working,input$xVar)
        yCol=pull(chemistry.working,input$yVar)
        #setting up the axis limits for the y var
        xLims=getLimitsIQR(xCol)
        yLims=getLimitsIQR(yCol)
        #making the scatter plot
        scatterPlot <- ggplot(data=chemistry.working)+ 
          geom_point(aes_string(input$xVar,input$yVar,color="BurnLevel"))+
          coord_cartesian(xlim = xLims, ylim = yLims)+
          scale_color_manual(values=burnLevelColors)+
          xlab(getLabel(input$xVar))+ylab(getLabel(input$yVar))+
          labs(title=paste(getLabel(input$yVar), " Explained by ",getLabel(input$xVar),sep = ""))
        
        #checking if they want a line of best fit and how they want it
        #one line being added
        if(input$smoothLine == "one"){
          scatterPlot <- scatterPlot + geom_smooth(aes_string(input$xVar,input$yVar),se=FALSE, color="black")
        }
        #three lines, divided by burnlevel, being added
        if(input$smoothLine == "burnlevel"){
          scatterPlot <- scatterPlot + geom_smooth(aes_string(input$xVar,input$yVar,color="BurnLevel"),se=FALSE)
        }
        scatterPlot
        }
    })
}      

?geom_smooth()


# Run the application 
shinyApp(ui = ui, server = server)
