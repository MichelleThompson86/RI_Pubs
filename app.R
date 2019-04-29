
library(DT)
library(shiny)
library(ggplot2)

data = read.csv("C:/Users/mthompson/Dropbox/MishiT/R/RI_Pubs/RI_Pubs.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
# Application title
   titlePanel(title = div(img(src="Field_Logo_Blue_x1.jpg", height = 75, width = 75, position = "left"),
              "Publications from Rapid Inventory Data"
                          )),
   
# Sidebar panel for inputs ----
   sidebarPanel(
     

     # Button
    # downloadButton("downloadData", "Download"), 
     
     plotOutput("distPlot"), width = 6), 

# Create a new Row in the UI for selectInputs
     fluidRow(     
     selectInput("Year",
                 "Year:",
                 c("All",
                   unique(as.character(data$Year)))), 
     selectInput("RI",
                 "RI:",
                 c("All",
                   unique(as.character(data$RI))))
     
   ),
 
# Show a plot of the generated distribution
#mainPanel(
     #    plotOutput("distPlot")
    #  ),   

      
DT::dataTableOutput("mytable")
      

   )


# Define a server for the Shiny app
server <-function(input, output) {
  
output$mytable = DT::renderDataTable({
    datatable(data, options = list(pageLength = 50))
  })

output$distPlot <- renderPlot({

    # plot
    ggplot(data)   +
      geom_point(aes(x=Year, y=cumulative, colour = "black")) + 
      geom_line(aes(x=Year, y=cumulative, colour = "black"))+
      geom_bar(data = data, aes(x=Year), stat = "count",
               fill="green", 
               alpha = .2)+
    scale_color_identity(name = "",
                         breaks = c("black"),
                         labels = c("Cumulative"),
                         guide = "legend")+
      labs(x="Year", y="Count")+
      theme_bw()+
      theme(
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=60, size = 14, hjust =1),
        axis.text.y=element_text(size = 12),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_text(size = 16))+
      theme(
        legend.title=element_text(size=18),
        legend.text=element_text(size=14), 
        legend.position ="bottom"
      )+
      scale_x_continuous(breaks=2000:2019
      )+
      scale_y_continuous(breaks = seq(0, 130, by = 20))+
      ggtitle("All Publications")
    
     }) 
    
 # })
}

shinyApp(ui, server)



