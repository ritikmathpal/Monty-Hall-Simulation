#monty hall simulation
library(magrittr)
library(dplyr)

#monty door choosing to put a car behind
monty.setup<-function(){
  out=sample(1:3,1)
  return(out)
}

#contestant choosing the first door
contestent.setup<-function(){
  out=sample(1:3,1)
  return(out)
}

#monty opens a door with goat behind
monty.first_open<-function(monty.in,contestent.in){
  remaining_doors=setdiff(1:3,c(monty.in,contestent.in))
  if(length(remaining_doors)==1){
    return(remaining_doors)
  }
  else{
    out=sample(remaining_doors,1)
    return(out)
  }
  
}
monty.first_open(1,2)

#monty asks the contestant weather they would like to switch doors
#assuming the contestant always switches
monty.second_open<-function(monty.in,contestent.in,switch.in){
  if(switch.in==1){
    contestent.new_in=setdiff(1:3,c(contestent.in,monty.first_open(monty.in,contestent.in)))
  }
  else{
    contestent.new_in=contestent.in
  }
  
  if(contestent.new_in==monty.in){
    out=1
  }
  else{
    out=0
  }
  return(c(contestent.new_in,out))
}

#single simulation
single.sim<-function(monty.in,contestent.in,switch.in){
  first_open=monty.first_open(monty.in,contestent.in)
  result=monty.second_open(monty.in,contestent.in,switch.in)
  return(c(monty.in,contestent.in,first_open,switch.in,result))
}

test_df=data.frame(t(single.sim(monty.in = monty.setup(),contestent.in = contestent.setup(),1)))
names_vec=c("correct door|","contesent first choice|","monty first open|","switching T/F|","contestent new choice|","Win T/F|")
colnames(test_df)=names_vec
test_df

#many replicates
switch_df=data.frame(t(replicate(1000,single.sim(monty.in = monty.setup(),contestent.in = contestent.setup(),1))))
stay_df=data.frame(t(replicate(1000,single.sim(monty.in = monty.setup(),contestent.in = contestent.setup(),0))))
final_df=rbind(stay_df,switch_df)
names(final_df)=names_vec
final_df




#summary
summary=data_frame(c("stick","switch"),tapply(final_df$`Win T/F|`,INDEX = final_df$`switching T/F|`,mean))
summary$'win percentage'=summary$`tapply(...)`*100
colnames(summary)=c('strategy',"win prob","win percentage")  

#barplot
barplot(height = summary$`win prob`,names.arg = c("stick","switch"),xlab="strategy",ylab="probability of winning",col=c("red","green"),main="Switchig vs sticking")

#animations
switch_df=final_df[final_df$`switching T/F|`==1,]
stick_df=final_df[final_df$`switching T/F|`==0,]

library(animation)
x11()

Sys.sleep(5)
plot(1:1000,type="n",ylim=c(0,1),xlab="trials",ylab="win probability",main="stick vs switch")
abline(h=0.333,col="red",lwd=2)
abline(h=0.667,col="green",lwd=2)
legend('topright',legend=c("switch","stick"),col=c("green","red"),pch=19)
for(i in seq(2,nrow(final_df),5)){
  points(i,mean(switch_df$`Win T/F|`[1:i]),col=c("green"),pch=19)
  points(i,mean(stick_df$`Win T/F|`[1:i]),col="red",pch=19)
  dev.flush()
  Sys.sleep(0.1)
}

