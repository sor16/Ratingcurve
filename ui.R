library(shiny)
library(ggplot2)
        Sys.setlocale("LC_ALL", "is_IS")

        shinyUI(fluidPage(
            titlePanel('Linear Regression'),
            sidebarLayout(
                sidebarPanel(
                    fileInput('file1', 'Choose file'),
        
                    radioButtons('skali', 'Skali',
                                 c(Lograskali="log",
                                   Raunskali="raun")),
                    checkboxInput("checkbox", label = "Leifarit", value = FALSE),
                    textInput("nafn","Name of river"),
                    radioButtons('format','Document format',c('PDF','HTML','Word'),inline=TRUE),
                    downloadButton('downloadReport')
                    
                ),
                mainPanel(
                    textOutput('text'),
                    plotOutput('contents'),
                    plotOutput('residual')
                    
                    
                )
            )
        ))