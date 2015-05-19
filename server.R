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
        qvdata$b=Sxy/Sxx
        qvdata$a=ybar-qvdata$b*xbar
        qvdata$fit=a+b*qvdata$Hlog
        
        xbar2=mean(qvdata$H)
        ybar2=mean(qvdata$Q)
        xdif2=qvdata$H-xbar2
        ydif2=qvdata$Q-ybar2
        Sxy2=sum(xdif2*ydif2)
        Sxx2=sum(xdif2*xdif2)
        qvdata$b2=Sxy2/Sxx2
        qvdata$a2=ybar2-qvdata$b2*xbar2
        qvdata$fit2=a2+b2*qvdata$H
        qvdata
    })
    
    
  output$contents <- renderPlot({
    qvdata=data()
         if(!is.null(qvdata)) {
            t=seq(0,2,0.01)
            abline=qvdata$a+qvdata$b*t
            abline2=qvdata$a2+qvdata$b2*exp(t)
          
            if(input$skali=='log' && input$model =='lik2'){
                  ggplot(qvdata,aes(Hlog,Qlog))+geom_point(col="red")+theme_bw()+geom_abline(intercept=a,slope=b)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")+xlab("log(H)")+ylab("log(Q)")
              
            }
            else if (input$skali=='log' && input$model =='lik1'){
                  plot(qvdata$Hlog,qvdata$Qlog,type="p",pch=20,col="red",xlab="Hlog",ylab="Qlog",main="Water level (h) vs Discharge(Q) Log Scale")
                  lines(t,abline,col="black")
            }
            else if (input$model =='lik2'){
                  curve=data.frame(x=seq(0,1.2,0.01))
                  curve$fit=a+b*curve$x
                  curve=exp(curve)
                  ggplot(qvdata,aes(H,Q))+geom_point()+theme_bw()+stat_smooth(mapping=aes(x,fit),data=curve)+
                  ggtitle("Water level (h) vs Discharge(Q) Log Scale")
            }
            else{
                  plot(qvdata$H,qvdata$Q,type="p",pch=20,col="black",xlab="h",ylab="Q",main="Water level (h) vs Discharge(Q) real scale") 
                  lines(exp(t),exp(abline),col="blue")
            }
        } 
     
      
  })
  
output$residual<- renderPlot({
      qvdata=data()
      if(!is.null(qvdata)) {
          if(input$checkbox==TRUE){
              
              if(input$skali=='log' && input$model =='lik1'){
                  plot(qvdata$Hlog,qvdata$Qlog-qvdata$fit,pch=20,col="red",main="Residuals",ylab=expression(epsilon[i]),xlab="log(H)")
                  lines(qvdata$Hlog,rep(0,nrow(qvdata)),col="black")
              }
              else if(input$skali=='log' && input$model =='lik2'){
                  ggplot(qvdata,aes(Hlog,Qlog-fit))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                      xlab("log(H)")+ylab(expression(epsilon[i]))
              }
              else if(input$skali=='raun' && input$model =='lik1'){
                  plot(qvdata$H,qvdata$Q-qvdata$fit2,pch=20,col="red",main="Residuals",ylab=expression(epsilon[i]),xlab="H")
                  lines(qvdata$H,rep(0,nrow(qvdata)),col="black")
              }
              else{
                  ggplot(qvdata,aes(H,Q-fit2))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
                      xlab("H")+ylab(expression(epsilon[i]))
              }
          }
      }
  })
  
})
