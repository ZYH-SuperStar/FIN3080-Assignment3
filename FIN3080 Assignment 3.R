library(fpp3)
library(readxl)
stock <- read_excel("~/Downloads/Monthly%20Stock%20Price%20%20Returns211957954/TRD_Mnth.xlsx")
View(stock)   
Q1_stock <- stock[,c(1,2,4)]
colnames(Q1_stock) <- c('code','time','true_return')
Q1_stock <- Q1_stock %>% mutate(sort_return = 1)
Q1_stock <- Q1_stock %>% mutate(month=yearmonth(time)) %>% select(-time)
i=1
while (i <442803){
  a = Q1_stock[i,4]
  if (Q1_stock[i+1,4]==a+1 & Q1_stock[i,1]==Q1_stock[i+1,1]){
    Q1_stock[i+1,3] = Q1_stock[i,2]
  }
  i=i+1
}
Q1_stock <- Q1_stock %>% filter(is.na(true_return) ==FALSE & is.na(sort_return) ==FALSE) 
#按照上个月的return分组
a=yearmonth('2012 Jan')
b <- Q1_stock %>% filter(month==a)
b <-b %>% mutate(tercile = ntile(sort_return, 10))
b <- b %>% filter(is.na(sort_return)==FALSE) %>% group_by(tercile,month) %>% summarise(return=mean(true_return))
a=a+1
while (a <=yearmonth('2022 Dec')){
  c <- Q1_stock %>% filter(month==a)
  c <-c %>% mutate(tercile = ntile(sort_return, 10))
  c <- c %>% filter(is.na(sort_return)==FALSE) %>% group_by(tercile,month) %>% summarise(return=mean(true_return))
  b <- full_join(b,c)
  a=a+1
}
#累积回报率绘图
e=b
i=1
while (i<1321){
  l=e[i,'return']
  e[i+10,'return'] = (e[i+10,'return']+1)*(l+1)-1
  i=i+1
}
ggplot(data=e,aes(x=month,y=return,color=tercile))+geom_line()

e_bar <- e %>% group_by(tercile) %>% summarise(return=mean(return))
ggplot(data=e_bar,aes(x=tercile,y=return))+geom_bar(stat = 'identity')

e_1 <- e %>% filter(tercile == 1)
e_2 <- e %>% filter(tercile == 2)
e_3 <- e %>% filter(tercile == 3)
e_4 <- e %>% filter(tercile == 4)
e_5 <- e %>% filter(tercile == 5)
e_6 <- e %>% filter(tercile == 6)
e_7 <- e %>% filter(tercile == 7)
e_8 <- e %>% filter(tercile == 8)
e_9 <- e %>% filter(tercile == 9)
e_10 <- e %>% filter(tercile == 10)
ggplot()+geom_line(data=e_1,aes(x=month,y=return),color="#FF80FF",alpha=1)+
  geom_line(data=e_2,aes(x=month,y=return),color="#FF99FF",alpha=1)+
  geom_line(data=e_3,aes(x=month,y=return),color="#FFB3FF",alpha=1)+
  geom_line(data=e_4,aes(x=month,y=return),color="#FFCCFF",alpha=1)+
  geom_line(data=e_5,aes(x=month,y=return),color="#FFE6FF",alpha=1)+
  geom_line(data=e_6,aes(x=month,y=return),color="#E6FFFF",alpha=1)+
  geom_line(data=e_7,aes(x=month,y=return),color="#CCFFFF" ,alpha=1)+
  geom_line(data=e_8,aes(x=month,y=return),color="#B3FFFF",alpha=1)+
  geom_line(data=e_9,aes(x=month,y=return),color="#99FFFF",alpha=1)+
  geom_line(data=e_10,aes(x=month,y=return),color="#80FFFF",alpha=1)+
  theme_bw()+labs(title='Cumulative Returns Sorted by Last Month Return',y='Return',x='Month')


#Q2
library(readxl)
roe <- read_excel("~/Downloads/Earning%20Capacity225717095/FI_T5.xlsx")
View(roe)
colnames(roe) <- c('code','name','time','type','roeTTM','roeB','roeC','roeTTM')
roe <- roe %>% filter(type=='A')
roe <- roe %>% select(-name,-type)%>% mutate(quarter=yearquarter(time)+1) %>% select(-time)
Q2_stock <- stock[,c(1,2,4)]
colnames(Q2_stock) <- c('code','time','return')
Q2_stock <- Q2_stock %>% mutate(q_return = 1)
Q2_stock <- Q2_stock %>% mutate(quarter=yearquarter(time)) %>% select(-time)
i=1
while (i <442803){
  j=i
  a = Q2_stock[i,2]
  while (Q2_stock[i+1,4]==Q2_stock[i,4] & Q2_stock[i,1]==Q2_stock[i+1,1] & i <442803){
    a=a*(1+Q2_stock[i+1,2])
    i=i+1
  }
  for (k in (j:i)){
    Q2_stock[k,3] = a
  }
  i=i+1
}
Q2_stock <- Q2_stock %>% group_by(code,quarter) %>% summarise(return=mean(q_return))
roeA <- roe %>% select(code,roeA,quarter) %>% filter(is.na(roeA)==FALSE) %>% group_by(code,quarter) %>% summarise(roeA=mean(roeA))
Q2 <- left_join(Q2_stock,roeA)
Q2 <- Q2 %>% filter(is.na(roeA)==FALSE & is.na(return)==FALSE)
#按照上个季度的ROE分组
a=yearquarter('2012 Q2')
Q2 <- Q2 %>% ungroup()
b <- Q2 %>% filter(quarter==a)
b <-b %>% mutate(tercile = ntile(roeA, 10))
b <- b %>% filter(is.na(roeA)==FALSE) %>% group_by(tercile,quarter) %>% summarise(return=mean(return))
a=a+1
while (a <=yearquarter('2022 Q4')){
  c <- Q2 %>% filter(quarter==a)
  c <-c %>% mutate(tercile = ntile(roeA, 10))
  c <- c %>% filter(is.na(roeA)==FALSE) %>% group_by(tercile,quarter) %>% summarise(return=mean(return))
  b <- full_join(b,c)
  a=a+1
}
#累积回报率绘图
e=b
i=1
while (i<431){
  l=e[i,'return']
  e[i+10,'return'] = (e[i+10,'return']+1)*(l+1)-1
  i=i+1
}
ggplot(data=e,aes(x=quarter,y=return,color=tercile))+geom_line()

e_1 <- e %>% filter(tercile == 1)
e_2 <- e %>% filter(tercile == 2)
e_3 <- e %>% filter(tercile == 3)
e_4 <- e %>% filter(tercile == 4)
e_5 <- e %>% filter(tercile == 5)
e_6 <- e %>% filter(tercile == 6)
e_7 <- e %>% filter(tercile == 7)
e_8 <- e %>% filter(tercile == 8)
e_9 <- e %>% filter(tercile == 9)
e_10 <- e %>% filter(tercile == 10)
ggplot()+geom_line(data=e_1,aes(x=quarter,y=return),color="#FF80FF",alpha=1)+
  geom_line(data=e_2,aes(x=quarter,y=return),color="#FF99FF",alpha=1)+
  geom_line(data=e_3,aes(x=quarter,y=return),color="#FFB3FF",alpha=1)+
  geom_line(data=e_4,aes(x=quarter,y=return),color="#FFCCFF",alpha=1)+
  geom_line(data=e_5,aes(x=quarter,y=return),color="#FFE6FF",alpha=1)+
  geom_line(data=e_6,aes(x=quarter,y=return),color="#E6FFFF",alpha=1)+
  geom_line(data=e_7,aes(x=quarter,y=return),color="#CCFFFF" ,alpha=1)+
  geom_line(data=e_8,aes(x=quarter,y=return),color="#B3FFFF",alpha=1)+
  geom_line(data=e_9,aes(x=quarter,y=return),color="#99FFFF",alpha=1)+
  geom_line(data=e_10,aes(x=quarter,y=return),color="#80FFFF",alpha=1)+
  theme_bw()+labs(title='Cumulative Returns Sorted by Last ROE-A',y='Return',x='quarter')

#Use TTm
roeTTM <- roe %>% select(code,roeTTM,quarter) %>% filter(is.na(roeTTM)==FALSE) %>% group_by(code,quarter) %>% summarise(roeTTM=mean(roeTTM))
Q2B <- left_join(Q2_stock,roeTTM)
Q2B <- Q2B %>% filter(is.na(roeTTM)==FALSE & is.na(return)==FALSE)
#按照上个季度的ROE分组
a=yearquarter('2012 Q2')
Q2B <- Q2B %>% ungroup()
b <- Q2B %>% filter(quarter==a)
b <-b %>% mutate(tercile = ntile(roeTTM, 10))
b <- b %>% filter(is.na(roeTTM)==FALSE) %>% group_by(tercile,quarter) %>% summarise(return=mean(return))
a=a+1
while (a <=yearquarter('2022 Q4')){
  c <- Q2B %>% filter(quarter==a)
  c <-c %>% mutate(tercile = ntile(roeTTM, 10))
  c <- c %>% filter(is.na(roeTTM)==FALSE) %>% group_by(tercile,quarter) %>% summarise(return=mean(return))
  b <- full_join(b,c)
  a=a+1
}
#累积回报率绘图
e=b
i=1
while (i<431){
  l=e[i,'return']
  e[i+10,'return'] = (e[i+10,'return']+1)*(l+1)-1
  i=i+1
}
ggplot(data=e,aes(x=quarter,y=return,color=tercile))+geom_line()

e_1 <- e %>% filter(tercile == 1)
e_2 <- e %>% filter(tercile == 2)
e_3 <- e %>% filter(tercile == 3)
e_4 <- e %>% filter(tercile == 4)
e_5 <- e %>% filter(tercile == 5)
e_6 <- e %>% filter(tercile == 6)
e_7 <- e %>% filter(tercile == 7)
e_8 <- e %>% filter(tercile == 8)
e_9 <- e %>% filter(tercile == 9)
e_10 <- e %>% filter(tercile == 10)
ggplot()+geom_line(data=e_1,aes(x=quarter,y=return),color="#FF80FF",alpha=1)+
  geom_line(data=e_2,aes(x=quarter,y=return),color="#FF99FF",alpha=1)+
  geom_line(data=e_3,aes(x=quarter,y=return),color="#FFB3FF",alpha=1)+
  geom_line(data=e_4,aes(x=quarter,y=return),color="#FFCCFF",alpha=1)+
  geom_line(data=e_5,aes(x=quarter,y=return),color="#FFE6FF",alpha=1)+
  geom_line(data=e_6,aes(x=quarter,y=return),color="#E6FFFF",alpha=1)+
  geom_line(data=e_7,aes(x=quarter,y=return),color="#CCFFFF" ,alpha=1)+
  geom_line(data=e_8,aes(x=quarter,y=return),color="#B3FFFF",alpha=1)+
  geom_line(data=e_9,aes(x=quarter,y=return),color="#99FFFF",alpha=1)+
  geom_line(data=e_10,aes(x=quarter,y=return),color="#80FFFF",alpha=1)+
  theme_bw()+labs(title='Cumulative Returns Sorted by Last ROE-ttm',y='Return',x='quarter')

