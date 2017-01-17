library(mcr)
library(ggplot2)
x = c(0.1000001, 0.2000001, 0.3001, 0.4000001, 0.500001)
y = c(0.1, 0.2, 0.3, 0.4, 0.6001)

my_result = mcreg(x,y,method.reg = "LinReg",nsamples=100)

MCResult.getCoefficients(my_result)

bias <- calcResponse(my_result,alpha=0.05,x.levels=seq(0,0.5,0.01))

MCResult.plot(my_result,ci.area=T,ci.border = T,xlim=c(0,0.5),identity=F)

#FIle input
#http://stackoverflow.com/questions/22272571/data-input-via-shinytable-in-r-shiny-application

ggplot()+
  geom_line(data=data.frame(bias),aes(x=X,y=Y))+
  geom_line(data=data.frame(bias),aes(x=X,y=Y.LCI),color="red")+
  geom_ribbon(data=data.frame(bias),aes(x=X, ymax=Y.UCI, ymin=Y.LCI), fill="pink", alpha=0.5)