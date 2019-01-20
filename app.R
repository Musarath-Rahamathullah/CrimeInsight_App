## Crime Analysis Projecct

# loading libaries
library(reshape)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(shiny)
library(sqldf)
library(shinyjs)
library(ggplot2)

#read the dataset
mydata.d = read.csv("./crimedata1.csv")
mydata = as.data.frame(mydata.d[,c(2:4,148:149)])

data.scatter = mydata.d[,c(4,133,135,137,139,141,143,145,147,150)]


meanviolentCrime = round(mean(mydata$ViolentCrimesPerPop),0)
#print(meanviolentCrime)
meanNonviolentCrime = round(mean(mydata$nonViolPerPop),0)
#print(meanNonviolentCrime)

# defining states of US
state_choices = sort(unique(as.character(mydata$state))) 
state_choices = c("NONE",state_choices)

# UI code
ui <- fluidPage(
    
                      
          # Application title
          titlePanel("CRIME RATE"),
                      
          sidebarPanel(
                        
                selectInput("statesI", "Select state:", choices = state_choices,selected= NULL,multiple = FALSE),
                uiOutput("countyO"),
                        
                actionButton("submit", "SUBMIT")
                        
                ),
          # Items to show in the Main Panel
          mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Summary", plotOutput("hist"),tableOutput("table")),
                        tabPanel("Plot", plotOutput("scatter"))
                       
                      )
             
             
                  )
  
  
   
  )

server<-function(input, output) {

  
  
  # Drop down  for counties
  output$countyO<- renderUI({
      
    #print(input$statesI)
    sta <- input$statesI
 
    # If missing input, return to avoid error later in function
    if(sta == "NONE")
    {
      # Create the empty list 
      selectInput("countyI", "Select County/City:", choices = NA)
      
    }
    
    else
    {
  
      # Get the data set with the appropriate name
      
      mydata1 = mydata[mydata$state == input$statesI,] 
      #print(mydata1)
      
      # Create the list 
      selectInput("countyI", "Select County/City:", choices = sort(unique(as.character(mydata1$Communityname))),selected= NULL,multiple = FALSE)
      
      }
    
    })
  
    
  
  # Output the data
    
    #graph/plot output
    output$hist <- renderPlot({
      
      # call function 
      text_reactive()
      
   })
    
    text_reactive <- eventReactive(input$submit,{
      # clean up table on new submit
      tbl.data = c()
      output$table = renderTable(tbl.data)
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      #  it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "In progress.....", value = 0)
      
      # function to Increment the progress bar, and update the detail text.
      prog  <- function(x)
      {
        progress$inc(1/x)
      }  
      
      # call the progress function
      prog(8)
      
      # validata the inputs
      validate(
        need(input$statesI != 'NONE','ERROR:Select a state!'),
        need(input$countyI != 'NA','ERROR:Choose your city')
      )
      
      # call the progress function
      prog(8)
      
      if(input$statesI == "NONE")
          
          return()
          
      else
        {
          if (is.null(input$countyI))
          return()
    
          else
          { 
            # call the progress function
            prog(8)
            
            # mean crime rate of the state
            mean.VC.State = round(mean(mydata$ViolentCrimesPerPop[mydata$state == input$statesI]),0)
            #print(mean.VC.State)
            mean.NVC.State = round(mean(mydata$nonViolPerPop[mydata$state == input$statesI]),0)
            #print(mean.NVC.State)
            
            # call the progress function
            prog(8)
            
            # crime rate of the city
            county.violentcrime = round(mydata$ViolentCrimesPerPop[mydata$state == input$statesI & mydata$Communityname == input$countyI],0)
            #print(county.violentcrime)
            county.Nonviolentcrime = round(mydata$nonViolPerPop[mydata$state == input$statesI & mydata$Communityname == input$countyI],0)
            #print(county.Nonviolentcrime)
         
            
            # call the progress function
            prog(8)
            
            names = c("Violent","Nonviolent")
            dataviolent = c(county.violentcrime,mean.VC.State,meanviolentCrime) 
            datanonviolent = c(county.Nonviolentcrime,mean.NVC.State,meanNonviolentCrime)
            
            data = data.frame(rbind(dataviolent,datanonviolent), names)
            
            colnames(data) = c("CityCrimeRate","StateAvgRate","CountryAvgRate","Crimes")
            rownames(data) = c()
            
            data = melt(data)
            #print(data)
         
            data = data.frame(data)
            
            # call the progress function
            prog(8)
         
            #print(data)
            # plot the crime rate in the given community
             g = ggplot(data,aes(Crimes,value))+geom_bar(aes(fill=variable),position = "dodge",stat = "identity")+
              ggtitle(paste("Crime Rate of",input$countyI,"in",input$statesI))+ylab("No of crimes")+xlab("Crime Type")+
              theme(plot.title = element_text(hjust = 0.5))
            print(g)
            
            # call the progress function
            prog(8)
            
            tbl.data = data.frame(violent = dataviolent,Nonviolent = datanonviolent)
            rownames(tbl.data) = c("CityCrimeRate","StateAvgRate","CountryAvgRate")
            
            # table output
            output$table = renderTable(tbl.data,border = TRUE,hover = TRUE,rownames = TRUE)
            
            # call the progress function
            prog(8)
            
            
          
          }
        
        }
      
    })
    
  # plot for distribution of crime types  
   output$scatter<- renderPlot({
     
     if(input$statesI == 'NONE')
     
       g1 = c("No state selected")
       
     
      else
      {
        #print(head(data.scatter))
        data.scatter = data.scatter[data.scatter$state == input$statesI,-1]
        #print(head(data.scatter))
        data.scatter = melt(data.scatter,id = "TotalCrimeperPOP")
        #print(head(data.scatter))
        
        g1 = ggplot(data.scatter,aes(y=log(TotalCrimeperPOP),x = log(value),col = variable))+geom_point()+
          ggtitle(paste("Distribution of Crime Types in",input$statesI))
      }
      
      print(g1) 
      
    
  })
}
  
  
  


shinyApp(ui=ui,server = server)

