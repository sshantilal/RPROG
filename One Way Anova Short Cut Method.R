#ONE WAY ANOVA SHORT-CUT METHOD
A=c(8,10,12,8,7)
B=c(12,11,9,14,4)
C=c(18,12,16,6,8)
D=c(13,9,12,16,15)
Wheat_production=data.frame(A,B,C,D)
n=sum(length(A),length(B),length(C),length(D),0)
T=sum(Wheat_production)
CF=T^2/n
ss_total=sum((Wheat_production)^2)-CF 
ss_bet=sum((sum(Wheat_production$A))^2/length(A),(sum(Wheat_production$B))^2/length(B),(sum(Wheat_production$C))^2/length(C),(sum(Wheat_production$D))^2/length(D))-CF
ss_within=ss_total-ss_bet
Source_of_variation=c("Between Samples","Within Samples","Total")
Sum_of_Squares=c(ss_bet,ss_within,ss_total)
Degree_of_freedom=c(ncol(Wheat_production)-1,n-ncol(Wheat_production),n-1)
Mean_Squares=c(Sum_of_Squares[1:2]/Degree_of_freedom[1:2],0)
F_ratio=c(Mean_Squares[1]/Mean_Squares[2],0,0)
ANOVA_table=data.frame(Source_of_variation,Sum_of_Squares,Degree_of_freedom,Mean_Squares,F_ratio)
#DIRECT METHOD
Wheat<-c(rep('A',nrow(Wheat_production)),rep('B',nrow(Wheat_production)),rep('C',nrow(Wheat_production)),rep('D',nrow(Wheat_production)))
Plot<-c(Wheat_production$A,Wheat_production$B,Wheat_production$C,Wheat_production$D)
is.factor(Wheat)
Wheat<-factor(Wheat)
Wheat_production.aov<-aov(Plot~Wheat)
summary(Wheat_production.aov)
