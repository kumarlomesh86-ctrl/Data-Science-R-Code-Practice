library(sampleSelection)
data("Mroz87")
summary(Mroz87)
res=lm(wage~age+I(age^2)+educ+city,data=Mroz87)
summary(res)
res=lm(wage~age+I(age^2)+lfp+city,data=Mroz87)
summary(res)
res=lm(wage~age+I(age^2)+lfp+educ+city,data=Mroz87)
res
res = selection(lfp~age+I(age^2)+faminc+kids5+educ,wage~exper+I(exper^2)+educ+city,data=Mroz87,method="2step")
res
