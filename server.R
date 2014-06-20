library(ggplot2)
dfun_limit <- function(fun, min, max, mn, std) {
  function(x) {
    y <- fun(x, mean=mn, sd=std)
    y[x<min | x>max]<-NA
    return(y)
  }
}

dfun_limit_df <- function(fun, min, max,df) {
  function(x) {
    y <- fun(x,df=df)
    y[x<min | x>max]<-NA
    return(y)
  }
}

dfun_limit_df2 <- function(fun, min, max,df1, df2) {
  function(x) {
    y <- fun(x,df1=df1, df2=df2)
    y[x<min | x>max]<-NA
    return(y)
  }
}


shinyServer( function(input, output) 
 {
  output$distribution<-renderPrint({input$distr})
  output$tail<-renderPrint({input$Tail})
  output$Z<-renderPrint({input$Z})
  

  output$newp <- renderPlot({
    if (input$distr==1){ffun<-dnorm
    if(input$Tail=="2"){
    p<-ggplot(data.frame(x=c(-input$lim, input$lim)), aes(x=x)) + 
    stat_function(fun=dfun_limit(ffun,-input$lim, input$Z,  input$mean, input$sd), geom="area", fill="red", alpha=0.2) + 
      stat_function(fun=ffun, arg = list(mean = input$mean, sd=input$sd ))
    } else if(input$Tail=="1"){
      p<-ggplot(data.frame(x=c(-input$lim, input$lim)), aes(x=x)) + 
        stat_function(fun=dfun_limit(ffun, input$Z, input$lim,  input$mean, input$sd), geom="area", fill="red", alpha=0.2) + 
        stat_function(fun=ffun, arg = list(mean = input$mean, sd=input$sd ))
    } 
    else if(input$Tail=="3"){
      p<-ggplot(data.frame(x=c(-input$lim, input$lim)), aes(x=x)) + 
        stat_function(fun=dfun_limit(ffun,-input$lim, input$Z_low,  input$mean, input$sd), geom="area", fill="red", alpha=0.2) + 
        stat_function(fun=dfun_limit(ffun, input$Z_hig, input$lim,  input$mean, input$sd), geom="area", fill="red", alpha=0.2) + 
        stat_function(fun=ffun, arg = list(mean = input$mean, sd=input$sd ))
    }  
    else if(input$Tail=="4"){
      p<-ggplot(data.frame(x=c(-input$lim, input$lim)), aes(x=x)) + 
        stat_function(fun=dfun_limit(ffun,input$Z_low, input$Z_hig,  input$mean, input$sd), geom="area", fill="red", alpha=0.2) + 
        stat_function(fun=ffun, arg = list(mean = input$mean, sd=input$sd ))
    }      
    
    } else if (input$distr==2){
    
      if(input$Tail_t=="1"){
        p<-ggplot(data.frame(x=c(-input$lim_t, input$lim_t)), aes(x=x)) + stat_function(fun = dt, args = list(df = input$df_t), geom="line")+
          stat_function(fun=dfun_limit_df(dt, input$t,input$lim_t, input$df_t), geom="area", fill="red", alpha=0.2)
    
      } else if(input$Tail_t=="2"){
      p<-ggplot(data.frame(x=c(-input$lim_t, input$lim_t)), aes(x=x)) + stat_function(fun = dt, args = list(df = input$df_t), geom="line")+
        stat_function(fun=dfun_limit_df(dt, -input$lim_t,input$t, input$df_t), geom="area", fill="red", alpha=0.2)
      } else if(input$Tail_t=="3"){
        p<-ggplot(data.frame(x=c(-input$lim_t, input$lim_t)), aes(x=x)) + stat_function(fun = dt, args = list(df = input$df_t), geom="line")+
          stat_function(fun=dfun_limit_df(dt, input$t_hig,input$lim_t, input$df_t), geom="area", fill="red", alpha=0.2)+
          stat_function(fun=dfun_limit_df(dt, -input$lim_t,input$t_low, input$df_t), geom="area", fill="red", alpha=0.2)
      }  else if(input$Tail_t=="4"){
        p<-ggplot(data.frame(x=c(-input$lim_t, input$lim_t)), aes(x=x)) + stat_function(fun = dt, args = list(df = input$df_t), geom="line")+
          stat_function(fun=dfun_limit_df(dt, input$t_low,input$t_hig, input$df_t), geom="area", fill="red", alpha=0.2)
      }
      
    } else if (input$distr==3){
      if(input$Tail_f=="1"){
      p<-ggplot(data.frame(x=c(0, input$lim_f)), aes(x=x)) + stat_function(fun = df, args = list(df1 = input$df_f_1, df2 = input$df_f_2), geom="line")+
        stat_function(fun=dfun_limit_df2(df, input$f, input$lim_f, input$df_f_1,input$df_f_2 ), geom="area", fill="red", alpha=0.2)
      } else if(input$Tail_f=="2"){
        p<-ggplot(data.frame(x=c(0, input$lim_f)), aes(x=x)) + stat_function(fun = df, args = list(df1 = input$df_f_1, df2 = input$df_f_2), geom="line")+
          stat_function(fun=dfun_limit_df2(df, 0, input$f, input$df_f_1,input$df_f_2 ), geom="area", fill="red", alpha=0.2)
      } else if(input$Tail_f=="4"){
        p<-ggplot(data.frame(x=c(0, input$lim_f)), aes(x=x)) + stat_function(fun = df, args = list(df1 = input$df_f_1, df2 = input$df_f_2), geom="line")+
          stat_function(fun=dfun_limit_df2(df, input$f_low, input$f_hig, input$df_f_1,input$df_f_2 ), geom="area", fill="red", alpha=0.2)
      
        
        } else if(input$Tail_f=="3"){
        p<-ggplot(data.frame(x=c(0, input$lim_f)), aes(x=x)) + stat_function(fun = df, args = list(df1 = input$df_f_1, df2 = input$df_f_2), geom="line")+
          stat_function(fun=dfun_limit_df2(df, input$f_hig, input$lim, input$df_f_1,input$df_f_2 ), geom="area", fill="red", alpha=0.2)+
          stat_function(fun=dfun_limit_df2(df, 0, input$f_low, input$df_f_1,input$df_f_2 ), geom="area", fill="red", alpha=0.2)
      }
      
    } else if (input$distr==4){
      if(input$Tail_chi=="1"){
      p<-ggplot(data.frame(x=c(0, input$lim_chi)), aes(x=x)) + stat_function(fun = dchisq, args = list(df = input$df_chi), geom="line")+
        stat_function(fun=dfun_limit_df(dchisq, input$chi, input$lim_chi,input$df_chi ), geom="area", fill="red", alpha=0.2)
      } else if(input$Tail_chi=="2"){
        p<-ggplot(data.frame(x=c(0, input$lim_chi)), aes(x=x)) + stat_function(fun = dchisq, args = list(df = input$df_chi), geom="line")+
          stat_function(fun=dfun_limit_df(dchisq, 0, input$chi,input$df_chi ), geom="area", fill="red", alpha=0.2)
      }else if(input$Tail_chi=="3"){
        p<-ggplot(data.frame(x=c(0, input$lim_chi)), aes(x=x)) + stat_function(fun = dchisq, args = list(df = input$df_chi), geom="line")+
          stat_function(fun=dfun_limit_df(dchisq, input$chi_hig, input$lim_chi,input$df_chi ), geom="area", fill="red", alpha=0.2)+
          stat_function(fun=dfun_limit_df(dchisq, 0, input$chi_low,input$df_chi ), geom="area", fill="red", alpha=0.2)
      }else if(input$Tail_chi=="4"){
        p<-ggplot(data.frame(x=c(0, input$lim_chi)), aes(x=x)) + stat_function(fun = dchisq, args = list(df = input$df_chi), geom="line")+
          stat_function(fun=dfun_limit_df(dchisq, input$chi_low, input$chi_hig,input$df_chi ), geom="area", fill="red", alpha=0.2)
      }
       
      
    }
    
    
    print(p)
  
    })

  
  
  
  
  
  
  
  
  
  output$p_value<-renderPrint(
    if (input$distr==1){
    if(input$Tail==1){
      pnorm(input$Z,input$mean,input$sd,lower.tail=FALSE)
    } else if(input$Tail==2){
      pnorm(input$Z,input$mean,input$sd,lower.tail=TRUE)
    } else if(input$Tail==3){
      pnorm(input$Z_low,input$mean,input$sd,lower.tail=TRUE)+ pnorm(input$Z_hig,input$mean,input$sd,lower.tail=FALSE)
    } else if(input$Tail==4){
      1-pnorm(input$Z_low,input$mean,input$sd,lower.tail=TRUE)- pnorm(input$Z_hig,input$mean,input$sd,lower.tail=FALSE)
    }
  
    } else if (input$distr==2){
      if(input$Tail_t==1){
        pt(input$t,input$df_t,lower.tail=FALSE)
      } else if(input$Tail_t==2){
        pt(input$t,input$df_t,lower.tail=TRUE)
      } else if(input$Tail_t==3){
        pt(input$t_hig,input$df_t,lower.tail=FALSE)+ pt(input$t_low,input$df_t,lower.tail=TRUE)
      } else if(input$Tail_t==4){
        1-pt(input$t_hig,input$df_t,lower.tail=FALSE)- pt(input$t_low,input$df_t,lower.tail=TRUE)        
      }
      
    }  else if (input$distr==3){
      if(input$Tail_f==1){
        pf(input$f,input$df_f_1,input$df_f_2,lower.tail=FALSE)
      } else if(input$Tail_f==2){
        pf(input$f,input$df_f_1,input$df_f_2,lower.tail=TRUE)
      } else if(input$Tail_f==3){
        pf(input$f_hig,input$df_f_1,input$df_f_2,lower.tail=FALSE)+pf(input$f_low,input$df_f_1,input$df_f_2,lower.tail=TRUE)
      } else if(input$Tail_f==4){
        1- pf(input$f_hig,input$df_f_1,input$df_f_2,lower.tail=FALSE)-pf(input$f_low,input$df_f_1,input$df_f_2,lower.tail=TRUE)
      }
      
    } else if (input$distr==4){
      if(input$Tail_chi==1){
        pchisq(input$chi, input$df_chi, lower.tail=FALSE)
      } else if(input$Tail_chi==2){
        pchisq(input$chi, input$df_chi, lower.tail=TRUE)        
      } else if(input$Tail_chi==3){
        pchisq(input$chi_hig, input$df_chi, lower.tail=FALSE) + pchisq(input$chi_low, input$df_chi, lower.tail=TRUE)
      } else if(input$Tail_chi==4){
        1-pchisq(input$chi_hig, input$df_chi, lower.tail=FALSE) - pchisq(input$chi_low, input$df_chi, lower.tail=TRUE)                  
      }
      
    }
    
    )
  
  
  
}
)