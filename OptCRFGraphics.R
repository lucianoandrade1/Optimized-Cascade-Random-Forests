x<-data.frame(mopsoCredit$objfnvalues[order(mopsoCredit$objfnvalues[1:6,1]),], type="MOPSO")
y<-data.frame(nsgar2Credit$value[order(nsgar2Credit$value[1:8,1]),], type="NSGA-II")

w<-data.frame(cbind(x[4,1],x[4,2]),type="Decision maker choice")
colnames(w)<-c("X1","X2","type")

df <- rbind(x,y,w)

library(ggplot2)
ggplot(df)+geom_line(aes(X1,X2,shape=type, colour=type))+geom_point(aes(X1,X2,shape=type,colour=type,size=type))+
scale_color_manual(values=c("black", "blue", "red", "green")) +
scale_shape_manual(values=c(17, 15, 19, 17))+
scale_size_manual(values=c(3, 3, 5, 5))+
ggtitle("Credit Card Fraud Detection Dataset") +
xlab("Pos Pred Value") + ylab("Neg Pred Value") +
theme_grey(base_size = 22)+
theme(plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.27, 0.2))
