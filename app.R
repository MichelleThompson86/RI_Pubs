library(DT)
library(shiny)
library(ggplot2)
library(rlang)
library(tibble)
library(dplyr)


data = read.csv("RI_Pubs_stats.csv")
data1= read.csv("RI_Pubs.csv")
data2 = read.csv("groups.csv")
data3 = read.csv("RI.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title = div(img(src="Field_Logo_Blue_x1.jpg", height = 75, width = 75, position = "left"),
                         "Measuring the Scientific Impact of Rapid Inventory Data - Publications from RI Data")),
  
  sidebarPanel(
    h3("> 130 publications"),
    h3("> 65 new species described"),
    plotOutput("distPlot"),
    
    #Button
    downloadButton("downloadData", "Download table")
  ),
  

  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Taxonomic group", plotOutput("plot1")),
                tabPanel("RI", plotOutput("plot2")),
                tabPanel("New species", plotOutput("plot3")))),
  
  
  basicPage(
    DT::dataTableOutput("mytable"))
  )



# Define a server for the Shiny app
server <-function(input, output) {
  data1.1 <- data1 %>%
  mutate(DOI = paste0("<a href='",data1$DOI,"' target='_blank'>",data1$DOI,"</a>"))
 
  output$mytable = DT::renderDataTable({
  datatable(data1.1, options = list(pageLength = 50), escape = FALSE)
  })
  

  output$downloadData <- downloadHandler(
    
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
    filename = function() {
      paste("RI_pubs", ".csv", sep = "")
    },

    content = function(file) {
    write.csv(data1, file, row.names = FALSE)
    }
  )
    

  
  output$distPlot <- renderPlot({
    
    # cumulative plot
    ggplot(data)   +
      geom_point(aes(x=Year, y=cumulative, colour = "black")) + 
      geom_line(aes(x=Year, y=cumulative, colour = "black"))+
      geom_bar(data = data, aes(x=Year), stat = "count",
               fill="black", 
               alpha = .8)+
      scale_color_identity(name = "",
                           breaks = c("black"),
                           labels = c("Cumulative publications"),
                           guide = "legend")+
      labs(x="Year", y="Count")+
      theme_bw()+
      theme(
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=60, size = 14, hjust =1),
        axis.text.y=element_text(size = 13),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_text(size = 16))+
      theme(
        legend.title=element_text(size=18),
        legend.text=element_text(size=14), 
        legend.position ="bottom"
      )+
      scale_x_continuous(breaks=seq (2000,2020, by =2)
      )+
      scale_y_continuous(breaks = seq(0, 130, by = 20))+
      ggtitle("All Publications")
    
    
  }) 

  
  #Group plot
  output$plot1 <-
    renderPlot({
      data3$RI=as.factor(data3$RI)
      legend_ord <- levels(with(data2, reorder(Group, -Number)))
      ggplot(data = data2, aes(x=reorder(Group, -Number, sum), y = Number))   +
        geom_bar(stat = "identity",
                 fill="blue1", 
                 alpha = .9)+
        labs(x="Group", y="Count")+
        geom_text(aes(label = Number),       # add counts above bars
          stat='identity', 
          vjust = -0.2,   #nudge up to label above bar
          size=6)+
        theme_bw()+
        theme(
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          axis.text.x=element_text(angle=60, size = 16, hjust =1),
          axis.text.y=element_text(size = 13),
          axis.title.y=element_text(size = 16),
          axis.title.x=element_text(size = 16))+
        theme(
          legend.title=element_text(size=18),
          legend.text=element_text(size=14) 
        )+
        expand_limits(y = max(data2$Number * 1.05))+ #auto set so that bar labels don't get cutoff
        ggtitle("Number of publications by taxonomic group")+
        theme(plot.title = element_text(size = 18))
    })
  
  #RI plot
  output$plot2 <-
    renderPlot({
      data3$RI=as.factor(data3$RI)
      legend_ord <- levels(with(data3, reorder(RI, -Count)))
      ggplot(data3, aes(x=reorder(RI,-Count,sum), y=Count))+
        geom_bar (stat = "identity", fill = "blue1") +
        theme_minimal()+
        labs(x="RI", y="Count")+
        theme_bw()+
        theme(
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          axis.text.x=element_text(angle=60, size = 16, hjust =1),
          axis.text.y=element_text(size = 13),
          axis.title.y=element_text(size = 16),
          axis.title.x=element_text(size = 16))+
        theme(
          legend.title=element_text(size=18),
          legend.text=element_text(size=14) 
        )+
        scale_y_continuous(breaks=seq(0,20, by =2)
        )+
        theme(plot.title = element_text(size = 18))+
        ggtitle("Number of publications by rapid inventory")+
        scale_fill_discrete(breaks= legend_ord)
   
       })
  

  
}

shinyApp(ui, server)