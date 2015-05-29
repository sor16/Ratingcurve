library(shiny)
library(ggplot2)
        Sys.setlocale("LC_ALL", "is_IS")

        shinyUI(fluidPage(
            titlePanel('Linear Regression'),
            sidebarLayout(
                sidebarPanel(
                    fileInput('file1', 'Choose file'),
                   # radioButtons('skali', 'Skali',
                    #             c(Lograskali="log",
                    #               Raunskali="raun")),
                   #checkboxGroupInput("checkbox1",label="Raunskali",value=FALSE), 
                   #checkboxInput("checkbox2", label = "Lograskali",value=FALSE), 
                   #checkboxInput("checkbox4", label = "Leifarit a raunskala",value=FALSE),
                   #checkboxInput("checkbox3", label = "Leifarit a logskala",value=FALSE), 
                   #checkboxInput("checkbox5", label = "Tafla",value=FALSE),
                   checkboxGroupInput("checkbox", label = "Output",choices=list("Raunskali"="raun","Lograskali"="log",
                                    "Leifarit a logskala"="leiflog", "Leifarit a raunskala"="leifraun", "Tafla"="tafla") ,selected = NULL),
                   actionButton("go", label="Submit"),
                   br(),
                   br(),
                    textInput("nafn","Name of river"),
                    radioButtons('format','Document format',c('PDF','HTML','Word'),inline=TRUE),
                    downloadButton('downloadReport')
                    
                ),
                #list of outputs
                mainPanel(
                    textOutput('text'),
                    plotOutput('plotraun'),
                    plotOutput('plotlog'),
                    plotOutput('plotleifraun'),
                    plotOutput('plotleiflog'),
                    tableOutput('table')
                    
                    
                )
            )
        ))