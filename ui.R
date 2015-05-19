library(shiny)
library(ggplot2)
        Sys.setlocale("LC_ALL", "is_IS")

        shinyUI(fluidPage(
            titlePanel('Línulegt aðhvarf'),
            sidebarLayout(
                sidebarPanel(
                    fileInput('file1', 'Veldu gögn'),
                    #tags$hr(),
        
                    radioButtons('skali', 'Skali',
                                 c(Lograskali="log",
                                   Raunskali="raun")),
                    radioButtons('model', 'Líkan',
                                 c(Líkan1="lik1",
                                   Líkan2="lik2")),
                    checkboxInput("checkbox", label = "Leifarit", value = FALSE)
                    
                ),
                mainPanel(
                    plotOutput('contents'),
                    plotOutput('residual')
                    
                )
            )
        ))