library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
    data<-reactive({
        inFile <- input$file1
   
        if (is.null(inFile)){
            return(NULL)
        }
        qvdata=read.table(inFile$datapath)
        names(qvdata)=c("H","Q")
        qvdata$Hlog=log(qvdata[,1])
        qvdata$Qlog=log(qvdata[,2])
        xbar=mean(qvdata$Hlog)
        ybar=mean(qvdata$Qlog)
        xdif=qvdata$Hlog-xbar
        ydif=qvdata$Qlog-ybar
        Sxy=sum(xdif*ydif)
        Sxx=sum(xdif*xdif)
        b=Sxy/Sxx
        a=ybar-b*xbar
        qvdata$fit=a+b*qvdata$Hlog
        
        #xbar2=mean(qvdata$H)
        #ybar2=mean(qvdata$Q)
        #xdif2=qvdata$H-xbar2
        #ydif2=qvdata$Q-ybar2
        #Sxy2=sum(xdif2*ydif2)
        #Sxx2=sum(xdif2*xdif2)
        #b2=Sxy2/Sxx2
        #a2=ybar2-b2*xbar2
        #qvdata$fit2=a2+b2*qvdata$H
        return(list("qvdata"=qvdata,"a"=a,"b"=b))
    })
    
    
  output$contents <- renderPlot({
    plotlist=data()
         if(!is.null(plotlist$qvdata)) {
            t=seq(0,2,0.01)
            #t2=seq(0,7.4,0.01)
            a=plotlist$a
            b=plotlist$b
            abline=a+b*t
            #a2=plotlist$a2
            #b2=plotlist$b2
            #abline2=a2+b2*t2
          
            if(input$skali=='log' && input$model =='lik2'){
                  ggplot(plotlist$qvdata,aes(Hlog,Qlog))+geom_point(col="red")+theme_bw()+geom_abline(intercept=a,slope=b)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")+xlab("log(H)")+ylab("log(Q)")
              
            }
            else if (input$skali=='log' && input$model =='lik1'){
                  with(plotlist$qvdata,plot(Hlog,Qlog,type="p",pch=20,col="red",xlab="Hlog",ylab="Qlog",main="Water level (h) vs Discharge(Q) Log Scale"))
                  lines(t,abline,col="black")
            }
            else if (input$model =='lik2'){
                  curve=data.frame(x=seq(0,1.2,0.01))
                  curve$fit=a+b*curve$x
                  curve=exp(curve)
                  ggplot(plotlist$qvdata,aes(H,Q))+geom_point()+theme_bw()+stat_smooth(mapping=aes(x,fit),data=curve)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")
            }
            else{
                  with(plotlist$qvdata,plot(H,Q,type="p",pch=20,col="black",xlab="h",ylab="Q",main="Water level (h) vs Discharge(Q) real scale")) 
                  lines(exp(t),exp(abline),col="blue")
            }
        } 
     
      
  })
  
output$residual<- renderPlot({
      plotlist=data()
      if(!is.null(plotlist$qvdata)) {
          if(input$checkbox==TRUE){
              
              if(input$skali=='log' && input$model =='lik1'){
                  with(plotlist$qvdata, plot(Hlog,Qlog-fit,pch=20,col="red",main="Residuals",ylab=expression(epsilon[i]),xlab="log(H)"))
                  with(plotlist$qvdata,lines(Hlog,rep(0,nrow(plotlist$qvdata)),col="black"))
              }
              else if(input$skali=='log' && input$model =='lik2'){
                  ggplot(plotlist$qvdata,aes(Hlog,Qlog-fit))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                      xlab("log(H)")+ylab(expression(epsilon[i]))
              }
              else if(input$skali=='raun' && input$model =='lik1'){
                  with(plotlist$qvdata, plot(H,Q-exp(fit),pch=20,col="red",main="Residuals",ylab=expression(epsilon[i]),xlab="H"))
                  with(plotlist$qvdata,lines(H,rep(0,nrow(plotlist$qvdata)),col="black"))
              }
              else{
                  ggplot(plotlist$qvdata,aes(H,Q-exp(fit)))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                      xlab("H")+ylab(expression(epsilon[i]))
              }
          }
      }
  })
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste('my-report', sep = '.', switch(
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
