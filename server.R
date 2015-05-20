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
    
    
  output$contents <- renderPlot({
    plotlist=data()
         if(!is.null(plotlist$qvdata)) {
            t=plotlist$t
            a=plotlist$a
            b=plotlist$b
            abline=a+b*t
          
            if(input$skali=='log' && input$model =='lik2'){
                  ggplot(plotlist$qvdata,aes(Qlog,Hlog))+geom_point(col="red")+theme_bw()+geom_abline(intercept=a,slope=b)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")+ylab("log(H)")+xlab("log(Q)")
              
            }
            else if (input$skali=='log' && input$model =='lik1'){
                  with(plotlist$qvdata,plot(Qlog,Hlog,type="p",pch=20,col="red",ylab="Hlog",xlab="Qlog",main="Water level (h) vs Discharge(Q) Log Scale"))
                  lines(t,abline,col="black")
            }
            else if (input$model =='lik2'){
                  curve=data.frame(x=seq(0,max(plotlist$qvdata$Qlog),0.01))
                  curve$fit=a+b*curve$x
                  curve=exp(curve)
                  ggplot(plotlist$qvdata,aes(Q,H))+geom_point()+theme_bw()+stat_smooth(mapping=aes(x,fit),data=curve)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")
            }
            else{
                  with(plotlist$qvdata,plot(Q,H,type="p",pch=20,col="black",xlab="Q",ylab="H",main="Water level (h) vs Discharge(Q) real scale")) 
                  lines(exp(t),exp(abline),col="blue")
            }
        } 
     
      
  })
  
output$residual<- renderPlot({
      plotlist=data()
      t=plotlist$t
      if(!is.null(plotlist$qvdata)) {
          if(input$checkbox==TRUE){
              
              if(input$skali=='log' && input$model =='lik1'){
                  with(plotlist$qvdata, plot(Qlog,Hlog-fit,pch=20,col="red",main="Residuals",ylab=expression(epsilon[i]),xlab="log(Q)"))
                  with(plotlist$qvdata,lines(t,rep(0,length(t)),col="black"))
              }
              else if(input$skali=='log' && input$model =='lik2'){
                  ggplot(plotlist$qvdata,aes(Qlog,Hlog-fit))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                      xlab("log(Q)")+ylab(expression(epsilon[i]))
              }
              else if(input$skali=='raun' && input$model =='lik1'){
                  with(plotlist$qvdata, plot(Q,H-exp(fit),pch=20,col="red",main="Residuals",ylab=expression(epsilon[i]),xlab="Q"))
                  with(plotlist$qvdata,lines(exp(t),rep(0,length(t)),col="black"))
              }
              else{
                  ggplot(plotlist$qvdata,aes(Q,H-exp(fit)))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                      xlab("Q")+ylab(expression(epsilon[i]))
              }
          }
      }
  })
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste(input$nafn, format(Sys.Date(),"%d-%m-%Y"), sep=".", switch(
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
