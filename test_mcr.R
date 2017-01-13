library(mcr)

x = c(0.1000001, 0.2000001, 0.3001, 0.4000001, 0.500001)
y = c(0.1, 0.2, 0.3, 0.4, 0.5001)

my_result = mcreg(x,y,method.reg = "LinReg")

MCResult.plot(my_result)

#FIle input
#http://stackoverflow.com/questions/22272571/data-input-via-shinytable-in-r-shiny-application