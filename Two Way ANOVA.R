#TWO WAY ANOVA
Fertilizers=c("W","X","Y","Z")
A=c(6,7,3,8)
B=c(5,5,3,7)
C=c(5,4,3,4)
Wheat_production=data.frame(Fertilizers,A,B,C)
n=sum(length(A),length(B),length(C),0)
T=sum(Wheat_production[,2:4])
CF=T^2/n
ss_total=sum((Wheat_production[,2:4])^2)-CF 
ss_bet_col=sum((sum(Wheat_production$A))^2/length(A),(sum(Wheat_production$B))^2/length(B),(sum(Wheat_production$C))^2/length(C))-CF
ss_bet_row=sum((sum(Wheat_production[1,2:4]))^2/3,(sum(Wheat_production[2,2:4]))^2/3,(sum(Wheat_production[3,2:4]))^2/3,(sum(Wheat_production[4,2:4]))^2/3)-CF
ss_residual=ss_total-ss_bet_col-ss_bet_row
Source_of_variation=c("Between Columns","Between rows","Residual","Total")
Sum_of_Squares=c(ss_bet_col,ss_bet_row,ss_residual,ss_total)
Degree_of_freedom=c(ncol(Wheat_production)-2,nrow(Wheat_production)-1,(ncol(Wheat_production)-2)*(nrow(Wheat_production)-1),n-1)
Mean_Squares=c(Sum_of_Squares[1:3]/Degree_of_freedom[1:3],0)
F_ratio=c(Mean_Squares[1]/Mean_Squares[3],Mean_Squares[2]/Mean_Squares[3],0,0)
ANOVA_table=data.frame(Source_of_variation,Sum_of_Squares,Degree_of_freedom,Mean_Squares,F_ratio)

