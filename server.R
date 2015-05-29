library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
    data<-eventReactive(input$go,{
        inFile <- input$file1
   
        if (is.null(inFile)){
            return(NULL)
        }
        #output$text<-renderPrint({
        #    inFile$type
        #})
        
        if (inFile$type =="text/plain"){
            qvdata=read.table(inFile$datapath,skip=3,sep="|")
            qvdata=qvdata[,c(2:4,7)]
            qvdata=data.frame(lapply(qvdata, as.character), stringsAsFactors=FALSE)
            qvdata[,3:4]=apply(qvdata[,c(3,4)],2, function(x) as.numeric(gsub(",",".",x)))
            names(qvdata)=c("Date","Time","H","Q")
        }
        else if(inFile$type=="text/csv"){
            qvdata=read.table(inFile$datapath,skip=16)
            qvdata=qvdata[,c(1:3,6)]
            qvdata=data.frame(lapply(qvdata, as.character), stringsAsFactors=FALSE)
            qvdata[,3:4]=apply(qvdata[,c(3,4)],2, function(x) as.numeric(gsub(",",".",x)))
            names(qvdata)=c("Date","Time","H","Q")
            
        }
        else {
            qvdata=read.table(inFile$datapath)
            names(qvdata)=c("H","Q")
        }
        
        qvdata$Hlog=log(qvdata$H)
        qvdata$Qlog=log(qvdata$Q)
        ybar=mean(qvdata$Hlog)
        xbar=mean(qvdata$Qlog)
        ydif=qvdata$Hlog-ybar
        xdif=qvdata$Qlog-xbar
        Sxy=sum(xdif*ydif)
        Sxx=sum(xdif*xdif)
        b=Sxy/Sxx
        a=ybar-b*xbar
        qvdata$fit=a+b*qvdata$Qlog
        
        t=seq(0,max(qvdata$Qlog)+1,0.01)
        return(list("qvdata"=qvdata,"a"=a,"b"=b,"t"=t))
    })
    output$text<-renderPrint({input$checkbox})
    
plotratingcurve <- eventReactive(input$go,{
    plotlist=data()
    rclog=NULL
    rcraun=NULL
    rcleiflog=NULL
    rcleifraun=NULL
         if(!is.null(plotlist$qvdata)) {
            t=plotlist$t
            a=plotlist$a
            b=plotlist$b
            abline=a+b*t
            
            if ("raun" %in% input$checkbox){
                curve=data.frame(x=seq(0,max(plotlist$qvdata$Qlog),0.01))
                curve$fit=a+b*curve$x
                curve=exp(curve)
                rcraun=ggplot(plotlist$qvdata,aes(Q,H))+geom_point()+theme_bw()+stat_smooth(mapping=aes(x,fit),data=curve)+
                    ggtitle("Water level (h) vs Discharge(Q) Log Scale")
            }
            if("log" %in% input$checkbox){
                  rclog=ggplot(plotlist$qvdata,aes(Qlog,Hlog))+geom_point(col="red")+theme_bw()+geom_abline(intercept=a,slope=b)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")+ylab("log(H)")+xlab("log(Q)")
              
            }
            if("leiflog" %in% input$checkbox){
                                 rcleiflog=ggplot(plotlist$qvdata,aes(Qlog,Hlog-fit))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                                      xlab("log(Q)")+ylab(expression(epsilon[i]))
            }
            
            if ("leifraun" %in% input$checkbox){
            rcleifraun=ggplot(plotlist$qvdata,aes(Q,H-exp(fit)))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                                      xlab("Q")+ylab(expression(epsilon[i]))
            }
            return(list("rclog"=rclog,"rcraun"=rcraun, "rcleifraun"=rcleifraun,"rcleiflog"=rcleiflog))
        } 
    
     
  })

  
# plotresidual<- reactive({
#       plotlist=data()
#       t=plotlist$t
#       if(!is.null(plotlist$qvdata)) {
#           if(input$checkbox==TRUE){
#               
#               if(input$skali=='log' ){
#                   residual=ggplot(plotlist$qvdata,aes(Qlog,Hlog-fit))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
#                       xlab("log(Q)")+ylab(expression(epsilon[i]))
#               }
#               else {
#                   residual=ggplot(plotlist$qvdata,aes(Q,H-exp(fit)))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
#                       xlab("Q")+ylab(expression(epsilon[i]))
#               }
#               return(residual)
#           }   
#       }
#       
#   })
output$plotraun<-renderPlot({
    if(!is.null(plotratingcurve()$rcraun))
        plotratingcurve()$rcraun     
})
output$plotlog<-renderPlot({
    if(!is.null(plotratingcurve()$rclog))
    plotratingcurve()$rclog    
})
output$plotleifraun<-renderPlot({
    if(!is.null(plotratingcurve()$rcleifraun))
        plotratingcurve()$rcleifraun       
})
output$plotleiflog<-renderPlot({
    if(!is.null(plotratingcurve()$rcleiflog))
        plotratingcurve()$rcleiflog     
})
output$table <- renderTable({
    
    if(input$checkbox5==TRUE)
    data()$qvdata
})

    output$downloadReport <- downloadHandler(
        filename = function() {
            paste(input$file1$name, sep=".", switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
        },
        
        content = function(file) {
            src <- normalizePath('myreport.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'myreport.Rmd')
            
            library(rmarkdown)
            out <- render('myreport.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
  
})
