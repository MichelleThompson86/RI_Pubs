library(DT)
library(shiny)
library(ggplot2)
library(rlang)
library(tibble)
library(viridis)

data = read.csv("RI_Pubs_stats.csv")
data1 = read.csv("RI_Pubs.csv")
data2 = read.csv("groups.csv")
data3 = read.csv("RI.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(title = div(img(src="Field_Logo_Blue_x1.jpg", height = 75, width = 75, position = "left"),
                           "Publications from Rapid Inventory Data")),
    
      sidebarPanel(
        h3(">130 publications"),
        h3(">65 new species described"),
        plotOutput("distPlot"),
        
        #Button
        downloadButton("downloadData", "Download table")), 
      
        # Main panel for displaying outputs ----
        mainPanel(
    
        # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Category", plotOutput("plot1")),
                      tabPanel("RI", plotOutput("plot2")))),
             
      
    basicPage(
      DT::dataTableOutput("mytable")
      
  ))



# Define a server for the Shiny app
server <-function(input, output) {
  
  output$mytable = DT::renderDataTable({
    datatable(data1, options = list(pageLength = 50))
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
  
   #Category plot
   output$plot1 <-
   renderPlot({
   ggplot(data = data2, aes(x=Group, y = Number))   +
      geom_bar(stat = "identity",
               fill="blue", 
               alpha = .2)+
      labs(x="Category", y="Count")+
      theme_bw()+
      theme(
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=60, size = 16, hjust =1),
        axis.text.y=element_text(size = 12),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_text(size = 16))+
      theme(
        legend.title=element_text(size=18),
        legend.text=element_text(size=14) 
      )+
       ggtitle("Number of publications by category")+
       theme(plot.title = element_text(size = 18))
   })
    
   #RI plot
   output$plot2 <-
   renderPlot({
     data3$RI=as.factor(data3$RI)
     legend_ord <- levels(with(data3, reorder(RI, -Count)))
     ggplot(data3, aes(x=reorder(RI,-Count,sum), y=Count, fill="blue"))+
       geom_bar (stat = "identity") +
              theme_minimal()+
       labs(x="RI", y="Count")+
       theme_bw()+
       theme(
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         panel.grid.major.y=element_blank(),
         panel.grid.minor.y=element_blank(),
         axis.text.x=element_text(angle=60, size = 16, hjust =1),
         axis.text.y=element_text(size = 12),
         axis.title.y=element_text(size = 16),
         axis.title.x=element_text(size = 16))+
       theme(
         legend.title=element_text(size=18),
         legend.text=element_text(size=14) 
       )+
       theme(plot.title = element_text(size = 18))+
       ggtitle("Number of publications by RI")+
       scale_fill_discrete(breaks= legend_ord)
 
      
   
  })
  

}

shinyApp(ui, server)