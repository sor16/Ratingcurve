regression<-function(file){    
    qvdata=read.table(file)
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
    t=seq(0,2,0.01)
    abline=a+b*t
    plot(qvdata$Hlog,qvdata$Qlog,type="p",pch=20,col="red",xlab="log(h)",ylab="log(Q)",main="Water level (h) vs Discharge(Q) Log Scale")
    lines(t,abline,col="black")
    plot(qvdata$H,qvdata$Q,type="p",pch=20,col="red",xlab="h",ylab="Q",main="Water level (h) vs Discharge(Q) real scale") 
    lines(exp(t),exp(abline),col="black")
    #leifarit
    qvdata$fit=a+b*qvdata$Hlog
    plot(qvdata$Hlog,qvdata$Qlog-qvdata$fit,pch=20,col="red")
    lines(qvdata$Hlog,rep(0,nrow(qvdata)),col="black")
    
    log=ggplot(qvdata,aes(Hlog,Qlog))+geom_point(col="red")+theme_bw()+geom_abline(intercept=a,slope=b)+ggtitle("Water level (h) vs Discharge(Q) Log Scale")
    curve=data.frame(x=seq(0,1.2,0.01))
    curve$fit=a+b*curve$x
    curve=exp(curve)
    real=ggplot(qvdata,aes(H,Q))+geom_point(col="red")+theme_bw()+stat_smooth(mapping=aes(x,fit),data=curve) +
          ggtitle("Water level (h) vs Discharge(Q) Log Scale")
    #Leifarit
    ggplot(qvdata,aes(Hlog,Qlog-fit))+geom_point(col="red")+theme_bw()+geom_abline(intercept=0,slope=0)+ggtitle("Residuals")+
        xlab("log(H)")+ylab(expression(epsilon[i]))

}