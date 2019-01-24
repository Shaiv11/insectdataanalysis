
library(data.table)
library(ggplot2)
library(grDevices)


setwd("C:/Users/Shaival/Desktop/Internships & Courses/Blackcoffer")

#data has been converted to a .csv file and rearranged accordingly
dat <- read.csv("data factorial expe.csv")



#data cleaning 

dat<- dat[,-c(1,2,8,10,17,18,24,25,31,32,38,39,45,46,51,53,54,60,69,70,76,77,83,84,90,91,97,98,104,105,111,112,118,120,121,127,128,134,135,141,142,148,149,155,156,162,163)]
dat <- dat[,-c(7,48,49)]
dat <- dat[-c(81,82),]

season_1 <- dat[,1:41]
season_2 <- dat[,42:82]
season_3 <- dat[,83:116]


#analysis

se<- function(x){
  sd(x)/length(x)
}



#DBM

mean_s1_dbm <- c()
mean_s2_dbm <- c()
mean_s3_dbm <- c()
for (i in 1:80) {
  y <- ((season_1$wk1[i]+season_1$wk2[i]+season_1$wk3[i]+season_1$wk4[i])/4)
  mean_s1_dbm<- c(mean_s1_dbm,y)

  
  x <- ((season_2$wk1.7[i]+season_2$wk2.7[i]+season_2$wk3.7[i]+season_2$wk4.7[i])/4)
  mean_s2_dbm <- c(mean_s2_dbm,x)
  
  
  z <- ((season_3$wk1.14[i]+season_3$wk2.14[i]+season_3$wk3.14[i]+season_3$wk4.14[i])/4)
  mean_s3_dbm <- c(mean_s3_dbm, z)
  
  }

s_s1_dbm<-c()
s_s2_dbm<-c()
s_s3_dbm<-c()
for (i in 1:80) {
s1_dbm <- se(mean_s1_dbm)
s2_dbm <- se(mean_s2_dbm)
s3_dbm <- se(mean_s3_dbm)
s_s1_dbm <- append(s_s1_dbm, s1_dbm)
s_s2_dbm <- append(s_s2_dbm, s2_dbm)
s_s3_dbm <- append(s_s3_dbm, s3_dbm)
}
  
DBM_Planting <- data.frame(Insect = "DBM" ,Plant_Combinations=dat$Plant_DBM_1, Mean_Season1 = mean_s1_dbm, Mean_Season2= mean_s2_dbm, Mean_Season3 = mean_s3_dbm, StdError_Season1 = s_s1_dbm, StdError_Season2 = s_s2_dbm, StdError_Season3 = s_s3_dbm)




#Aphid   

mean_s1_aphid <- c()
mean_s2_aphid <- c()
mean_s3_aphid <- c()
for (i in 1:80) {
  a <- ((season_1$wk1.1[i]+season_1$wk2.1[i]+season_1$wk3.1[i]+season_1$wk4.1[i])/4)
  mean_s1_aphid<- c(mean_s1_aphid,a)
 
  
  b <- ((season_2$wk1.8[i]+season_2$wk2.8[i]+season_2$wk3.8[i]+season_2$wk4.8[i])/4)
  mean_s2_aphid <- c(mean_s2_aphid,b)
 
  c <- ((season_3$wk1.15[i]+season_3$wk2.15[i]+season_3$wk3.15[i]+season_3$wk4.15[i])/4)
  mean_s3_aphid <- c(mean_s3_aphid, c)

}

s_s1_a<-c()
s_s2_a<-c()
s_s3_a<-c()
for (i in 1:80) {
  s1_a <- se(mean_s1_aphid)
  s2_a <- se(mean_s2_aphid)
  s3_a <- se(mean_s3_aphid)
  s_s1_a <- append(s_s1_a, s1_a)
  s_s2_a <- append(s_s2_a, s2_a)
  s_s3_a <- append(s_s3_a, s3_a)
}


Aphid_Planting <- data.frame(Insect = "Aphid" ,Plant_Combinations=dat$Plant_Aphid_1,Mean_Season1 = mean_s1_aphid, Mean_Season2= mean_s2_aphid,Mean_Season3 = mean_s3_aphid, StdError_Season1 = s_s1_a, StdError_Season2 = s_s2_a, StdError_Season3 = s_s3_a)



#Whitefly 

mean_s1_wf <- c()
mean_s2_wf <- c()
mean_s3_wf <- c()
for (i in 1:80) {
  p <- ((season_1$wk1.2[i]+season_1$wk2.2[i]+season_1$wk3.2[i]+season_1$wk4.2[i])/4)
  mean_s1_wf<- c(mean_s1_wf,p)

  
  q <- ((season_2$wk1.9[i]+season_2$wk2.9[i]+season_2$wk3.9[i]+season_2$wk4.9[i])/4)
  mean_s2_wf <- c(mean_s2_wf,q)
  
  r <- ((season_3$wk1.16[i]+season_3$wk2.16[i]+season_3$wk3.16[i]+season_3$wk4.16[i])/4)
  mean_s3_wf <- c(mean_s3_wf, r)
}

s_s1_w<-c()
s_s2_w<-c()
s_s3_w<-c()
for (i in 1:80) {
  s1_w <- se(mean_s1_wf)
  s2_w <- se(mean_s2_wf)
  s3_w <- se(mean_s3_wf)
  s_s1_w <- append(s_s1_w, s1_w)
  s_s2_w <- append(s_s2_w, s2_w)
  s_s3_w <- append(s_s3_w, s3_w)
}

WF_Planting <- data.frame(Insect = "Whitefly" ,Plant_Combinations=dat$Plant_DBM_1, Mean_Season1 = mean_s1_wf, Mean_Season2= mean_s2_wf, Mean_Season3 = mean_s3_wf, StdError_Season1 = s_s1_w, StdError_Season2 = s_s2_w, StdError_Season3 = s_s3_w)


#hellula

mean_s1_hel <- c()
mean_s2_hel <- c()
mean_s3_hel <- c()
for (i in 1:80) {
  l <- ((season_1$wk1.3[i]+season_1$wk2.3[i]+season_1$wk3.3[i]+season_1$wk4.3[i])/4)
  mean_s1_hel<- c(mean_s1_hel,l)

  
  x <- ((season_2$wk1.10[i]+season_2$wk2.10[i]+season_2$wk3.10[i]+season_2$wk4.10[i])/4)
  mean_s2_hel <- c(mean_s2_hel,x)
  
  z <- ((season_3$wk1.17[i]+season_3$wk2.17[i]+season_3$wk3.17[i]+season_3$wk4.17[i])/4)
  mean_s3_hel <- c(mean_s3_hel, z)
  
}

s_s1_h<-c()
s_s2_h<-c()
s_s3_h<-c()
for (i in 1:80) {
  s1_h <- se(mean_s1_hel)
  s2_h <- se(mean_s2_hel)
  s3_h <- se(mean_s3_hel)
  s_s1_h <- append(s_s1_h, s1_h)
  s_s2_h <- append(s_s2_h, s2_h)
  s_s3_h <- append(s_s3_h, s3_h)
}


Hellula_Planting <- data.frame(Insect = "Hellula" ,Plant_Combinations=dat$Plant_DBM_1, Mean_Season1 = mean_s1_hel, Mean_Season2= mean_s2_hel, Mean_Season3 = mean_s3_hel, StdError_Season1 = s_s1_h, StdError_Season2 = s_s2_h, StdError_Season3 = s_s3_h)


#spider

mean_s1_s <- c()
mean_s2_s <- c()
mean_s3_s <- c()
for (i in 1:80) {
  t <- ((season_1$wk1.4[i]+season_1$wk2.4[i]+season_1$wk3.4[i]+season_1$wk4.4[i])/4)
  mean_s1_s<- c(mean_s1_s,t)
  
  o <- ((season_2$wk1.11[i]+season_2$wk2.11[i]+season_2$wk3.11[i]+season_2$wk4.11[i])/4)
  mean_s2_s <- c(mean_s2_s,o)
  
  m <- ((season_3$wk1.18[i]+season_3$wk2.18[i]+season_3$wk3.14[i]+season_3$wk4.18[i])/4)
  mean_s3_s <- c(mean_s3_s, m)
}

s_s1_s<-c()
s_s2_s<-c()
s_s3_s<-c()
for (i in 1:80) {
  s1_s <- se(mean_s1_s)
  s2_s <- se(mean_s2_s)
  s3_s <- se(mean_s3_s)
  s_s1_s <- append(s_s1_s, s1_s)
  s_s2_s <- append(s_s2_s, s2_s)
  s_s3_s <- append(s_s3_s, s3_s)
}


Spider_Planting <- data.frame(Insect = "Spider" ,Plant_Combinations=dat$Plant_DBM_1, Mean_Season1 = mean_s1_s, Mean_Season2= mean_s2_s, Mean_Season3 = mean_s3_s, StdError_Season1 = s_s1_s, StdError_Season2 = s_s2_s, StdError_Season3 = s_s3_s)


#ladybird

mean_s1_l <- c()
mean_s2_l <- c()
mean_s3_l <- c()
for (i in 1:80) {
  d <- ((season_1$wk1.5[i]+season_1$wk2.5[i]+season_1$wk3.5[i]+season_1$wk4.5[i])/4)
  mean_s1_l<- c(mean_s1_l,d)
  
  e <- ((season_2$wk1.12[i]+season_2$wk2.12[i]+season_2$wk3.12[i]+season_2$wk4.12[i])/4)
  mean_s2_l <- c(mean_s2_l,e)
  
  f <- ((season_3$wk1.19[i]+season_3$wk2.19[i]+season_3$wk3.19[i]+season_3$wk4.19[i])/4)
  mean_s3_l <- c(mean_s3_l, f)
}

s_s1_l<-c()
s_s2_l<-c()
s_s3_l<-c()
for (i in 1:80) {
  s1_l <- se(mean_s1_l)
  s2_l <- se(mean_s2_l)
  s3_l <- se(mean_s3_l)
  s_s1_l <- append(s_s1_l, s1_l)
  s_s2_l <- append(s_s2_l, s2_l)
  s_s3_l <- append(s_s3_l, s3_l)
}


LB_Planting <- data.frame(Insect = "Ladybird" ,Plant_Combinations=dat$Plant_DBM_1, Mean_Season1 = mean_s1_l, Mean_Season2= mean_s2_l, Mean_Season3 = mean_s3_l, StdError_Season1 = s_s1_l, StdError_Season2 = s_s2_l, StdError_Season3 = s_s3_l)



#hoverfly

mean_s1_h <- c()
mean_s2_h <- c()
for (i in 1:80) {
  aa <- ((season_1$wk1.6[i]+season_1$wk2.6[i]+season_1$wk3.6[i]+season_1$wk4.6[i])/4)
  mean_s1_h<- c(mean_s1_h,aa)
  
  bb <- ((season_2$wk1.13[i]+season_2$wk2.13[i]+season_2$wk3.13[i]+season_2$wk4.13[i])/4)
  mean_s2_h <- c(mean_s2_h,bb)
}

s_s1_ho<-c()
s_s2_ho<-c()
for (i in 1:80) {
  s1_ho <- se(mean_s1_h)
  s2_ho <- se(mean_s2_h)
  s_s1_ho <- append(s_s1_ho, s1_ho)
  s_s2_ho <- append(s_s2_ho, s2_ho)
}

  
  Hoverfly_Planting <- data.frame(Insect = "Hoverfly" ,Plant_Combinations=dat$Plant_DBM_1, Mean_Season1 = mean_s1_h, Mean_Season2= mean_s2_h, StdError_Season1 = s_s1_ho, StdError_Season2 = s_s2_ho, StdError_Season3 = "NA")

  
  
  
  write.csv(DBM_Planting, "DBM_Planting.csv")
  write.csv(Aphid_Planting, "Aphid_Planting.csv")
  write.csv(Hellula_Planting, "Hellula_Planting.csv")
  write.csv(WF_Planting, "Whitefly_Planting.csv")
  write.csv(Spider_Planting, "Spider_Planting.csv")
  write.csv(LB_Planting, "Ladybird_Planting.csv")
  write.csv(Hoverfly_Planting, "Hoverfly_Planting.csv")

  
  
  #WeeklyMeans
  
    #DBM
    
  d_1_1 <- mean(season_1$wk1)
  d_2_1<- mean(season_1$wk2)
  d_3_1 <- mean(season_1$wk3)
  d_4_1<- mean(season_1$wk4)
  
  dbm1<- data.frame(Insect = "DBM", Mean_Week1 = d_1_1, Mean_Week2 = d_2_1, Mean_Week3 = d_3_1, Mean_Week4 = d_4_1, Season = "1")
  
  d_1_2 <- mean(season_2$wk1.7)
  d_2_2<- mean(season_2$wk2.7)
  d_3_2 <- mean(season_2$wk3.7)
  d_4_2 <- mean(season_2$wk4.7)
  
  dbm2<- data.frame(Insect = "DBM", Mean_Week1 = d_1_2, Mean_Week2 = d_2_2, Mean_Week3 = d_3_2, Mean_Week4 = d_4_2, Season = "2")

  d_1_3 <- mean(season_3$wk1.14)
  d_2_3 <- mean(season_3$wk2.14)
  d_3_3 <- mean(season_3$wk3.14)
  d_4_3 <- mean(season_3$wk4.14)
  
  dbm3<- data.frame(Insect = "DBM", Mean_Week1 = d_1_3, Mean_Week2 = d_2_3, Mean_Week3 = d_3_3, Mean_Week4 = d_4_3, Season = "3")
  
  
  
  #Aphid
  
  a_1_1 <- mean(season_1$wk1.1)
  a_2_1<- mean(season_1$wk2.1)
  a_3_1 <- mean(season_1$wk3.1)
  a_4_1<- mean(season_1$wk4.1)
  
  a1<- data.frame(Insect = "Aphid", Mean_Week1 = a_1_1, Mean_Week2 = a_2_1, Mean_Week3 = a_3_1, Mean_Week4 = a_4_1, Season = "1")
  
  a_1_2 <- mean(season_2$wk1.8)
  a_2_2<- mean(season_2$wk2.8)
  a_3_2 <- mean(season_2$wk3.8)
  a_4_2 <- mean(season_2$wk4.8)
  
  a2<- data.frame(Insect = "Aphid", Mean_Week1 = a_1_2, Mean_Week2 = a_2_2, Mean_Week3 = a_3_2, Mean_Week4 = a_4_2, Season = "2")
  
  a_1_3 <- mean(season_3$wk1.15)
  a_2_3 <- mean(season_3$wk2.15)
  a_3_3 <- mean(season_3$wk3.15)
  a_4_3 <- mean(season_3$wk4.15)
  
  a3<- data.frame(Insect = "Aphid", Mean_Week1 = a_1_3, Mean_Week2 = a_2_3, Mean_Week3 = a_3_3, Mean_Week4 = a_4_3, Season = "3")

  
  #Whitefly
  
  w_1_1 <- mean(season_1$wk1.2)
  w_2_1<- mean(season_1$wk2.2)
  w_3_1 <- mean(season_1$wk3.2)
  w_4_1<- mean(season_1$wk4.2)
  
  w1<- data.frame(Insect = "Whitefly", Mean_Week1 = w_1_1, Mean_Week2 = w_2_1, Mean_Week3 = w_3_1, Mean_Week4 = w_4_1, Season = "1")
  
  w_1_2 <- mean(season_2$wk1.9)
  w_2_2<- mean(season_2$wk2.9)
  w_3_2 <- mean(season_2$wk3.9)
  w_4_2 <- mean(season_2$wk4.9)
  
  w2<- data.frame(Insect = "Whitefly", Mean_Week1 = w_1_2, Mean_Week2 = w_2_2, Mean_Week3 = w_3_2, Mean_Week4 = w_4_2, Season = "2")
  
  w_1_3 <- mean(season_3$wk1.16)
  w_2_3 <- mean(season_3$wk2.16)
  w_3_3 <- mean(season_3$wk3.16)
  w_4_3 <- mean(season_3$wk4.16)
  
  w3<- data.frame(Insect = "Whitefly", Mean_Week1 = w_1_3, Mean_Week2 = w_2_3, Mean_Week3 = w_3_3, Mean_Week4 = w_4_3, Season = "3")

  
  
  #Hellula
  
  h_1_1 <- mean(season_1$wk1.3)
  h_2_1<- mean(season_1$wk2.3)
  h_3_1 <- mean(season_1$wk3.3)
  h_4_1<- mean(season_1$wk4.3)
  
  h1<- data.frame(Insect = "Hellula", Mean_Week1 = h_1_1, Mean_Week2 = h_2_1, Mean_Week3 = h_3_1, Mean_Week4 = h_4_1, Season = "1")
  
  h_1_2 <- mean(season_2$wk1.10)
  h_2_2<- mean(season_2$wk2.10)
  h_3_2 <- mean(season_2$wk3.10)
  h_4_2 <- mean(season_2$wk4.10)
  
  h2<- data.frame(Insect = "Hellula", Mean_Week1 = h_1_2, Mean_Week2 = h_2_2, Mean_Week3 = h_3_2, Mean_Week4 = h_4_2, Season = "2")
  
  h_1_3 <- mean(season_3$wk1.17)
  h_2_3 <- mean(season_3$wk2.17)
  h_3_3 <- mean(season_3$wk3.17)
  h_4_3 <- mean(season_3$wk4.17)
  
  h3<- data.frame(Insect = "Hellula", Mean_Week1 = h_1_3, Mean_Week2 = h_2_3, Mean_Week3 = h_3_3, Mean_Week4 = h_4_3, Season = "3")

  
  
  #Spider
  
  s_1_1 <- mean(season_1$wk1.4)
  s_2_1<- mean(season_1$wk2.4)
  s_3_1 <- mean(season_1$wk3.4)
  s_4_1<- mean(season_1$wk4.4)
  
  s1<- data.frame(Insect = "Spider", Mean_Week1 = s_1_1, Mean_Week2 = s_2_1, Mean_Week3 = s_3_1, Mean_Week4 = s_4_1, Season = "1")
  
  s_1_2 <- mean(season_2$wk1.11)
  s_2_2<- mean(season_2$wk2.11)
  s_3_2 <- mean(season_2$wk3.11)
  s_4_2 <- mean(season_2$wk4.11)
  
  s2<- data.frame(Insect = "Spider", Mean_Week1 = s_1_2, Mean_Week2 = s_2_2, Mean_Week3 = s_3_2, Mean_Week4 = s_4_2, Season = "2")
  
  s_1_3 <- mean(season_3$wk1.18)
  s_2_3 <- mean(season_3$wk2.18)
  s_3_3 <- mean(season_3$wk3.18)
  s_4_3 <- mean(season_3$wk4.18)
  
  s3<- data.frame(Insect = "Spider", Mean_Week1 = s_1_3, Mean_Week2 = s_2_3, Mean_Week3 = s_3_3, Mean_Week4 = s_4_3, Season = "3")

  
  
  #ladybird
  
  
  l_1_1 <- mean(season_1$wk1.5)
  l_2_1<- mean(season_1$wk2.5)
  l_3_1 <- mean(season_1$wk3.5)
  l_4_1<- mean(season_1$wk4.5)
  
  l1<- data.frame(Insect = "Ladybird", Mean_Week1 = l_1_1, Mean_Week2 = l_2_1, Mean_Week3 = l_3_1, Mean_Week4 = l_4_1, Season = "1")
  
  l_1_2 <- mean(season_2$wk1.12)
  l_2_2<- mean(season_2$wk2.12)
  l_3_2 <- mean(season_2$wk3.12)
  l_4_2 <- mean(season_2$wk4.12)
  
  l2<- data.frame(Insect = "Ladybird", Mean_Week1 = l_1_2, Mean_Week2 = l_2_2, Mean_Week3 = l_3_2, Mean_Week4 = l_4_2, Season = "2")
  
  l_1_3 <- mean(season_3$wk1.19)
  l_2_3 <- mean(season_3$wk2.19)
  l_3_3 <- mean(season_3$wk3.19)
  l_4_3 <- mean(season_3$wk4.19)
  
  l3<- data.frame(Insect = "Ladybird", Mean_Week1 = l_1_3, Mean_Week2 = l_2_3, Mean_Week3 = l_3_3, Mean_Week4 = l_4_3, Season = "3")

  
  
  #hoverfly
  
  
  ho_1_1 <- mean(season_1$wk1.6)
  ho_2_1<- mean(season_1$wk2.6)
  ho_3_1 <- mean(season_1$wk3.6)
  ho_4_1<- mean(season_1$wk4.6)
  
  ho1<- data.frame(Insect = "Hoverfly", Mean_Week1 = ho_1_1, Mean_Week2 = ho_2_1, Mean_Week3 = ho_3_1, Mean_Week4 = ho_4_1, Season = "1")
  
  ho_1_2 <- mean(season_2$wk1.13)
  ho_2_2<- mean(season_2$wk2.13)
  ho_3_2 <- mean(season_2$wk3.13)
  ho_4_2 <- mean(season_2$wk4.13)
  
  ho2<- data.frame(Insect = "Hoverfly", Mean_Week1 = ho_1_2, Mean_Week2 = ho_2_2, Mean_Week3 = ho_3_2, Mean_Week4 = ho_4_2, Season = "2")

  
  Weekly_Means <- rbind(dbm1,dbm2,dbm3,a1,a2,a3,w1,w2,w3,h1,h2,h3,s1,s2,s3,l1,l2,l3,ho1,ho2)
  
  write.csv(Weekly_Means, "Weekly_Means.csv")  

  
  
  
  #Exploratory Analysis 
  
  ggplot(dat, aes(Plant_DBM_1,Marketable.yield_1)) + geom_point() + ggtitle("Marketable Yield for Season 1")
  ggplot(dat, aes(Plant_DBM_1,mkt_2)) + geom_point() + ggtitle("Marketable Yield for Season 2")
  ggplot(dat, aes(Plant_DBM_1,mkt_3)) + geom_point() + ggtitle("Marketable Yield for Season 3")
  
  ggplot(Weekly_Means, aes(Insect, Mean_Week1)) + geom_point(aes(color = factor(Season)))
  ggplot(Weekly_Means, aes(Insect, Mean_Week2)) + geom_point(aes(color = factor(Season)))
  ggplot(Weekly_Means, aes(Insect, Mean_Week3)) + geom_point(aes(color = factor(Season)))
  ggplot(Weekly_Means, aes(Insect, Mean_Week4)) + geom_point(aes(color = factor(Season)))

  
  
  #Repeated Measure Analysis
  
    #DBM
  
   par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4, main= "Week 4")
  mtext("Season 1 for DBM", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.7, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.7, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.7, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.7, main= "Week 4")
  mtext("Season 2 for DBM", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_3$Plant_dbm_3, season_3$wk1.14, main= "Week 1")
  plot(season_3$Plant_dbm_3, season_3$wk2.14, main= "Week 2")
  plot(season_3$Plant_dbm_3, season_3$wk3.14, main= "Week 3")
  plot(season_3$Plant_dbm_3, season_3$wk4.14, main= "Week 4")
  mtext("Season 3 for DBM", outer = TRUE, cex = 1, line = -1)
  
  
  #Aphid 
  
  par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1.1, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2.1, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3.1, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4.1, main= "Week 4")
  mtext("Season 1 for Aphid", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.8, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.8, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.8, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.8, main= "Week 4")
  mtext("Season 2 for Aphid", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_3$Plant_dbm_3, season_3$wk1.15, main= "Week 1")
  plot(season_3$Plant_dbm_3, season_3$wk2.15, main= "Week 2")
  plot(season_3$Plant_dbm_3, season_3$wk3.15, main= "Week 3")
  plot(season_3$Plant_dbm_3, season_3$wk4.15, main= "Week 4")
  mtext("Season 3 for Aphid", outer = TRUE, cex = 1, line = -1)
  
  
  #Whitefly
  
  par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1.2, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2.2, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3.2, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4.2, main= "Week 4")
  mtext("Season 1 for Whitefly", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.9, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.9, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.9, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.9, main= "Week 4")
  mtext("Season 2 for Whitefly", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_3$Plant_dbm_3, season_3$wk1.16, main= "Week 1")
  plot(season_3$Plant_dbm_3, season_3$wk2.16, main= "Week 2")
  plot(season_3$Plant_dbm_3, season_3$wk3.16, main= "Week 3")
  plot(season_3$Plant_dbm_3, season_3$wk4.16, main= "Week 4")
  mtext("Season 3 for Whitefly", outer = TRUE, cex = 1, line = -1)
  
  
  #Hellula
  
  
  par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1.3, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2.3, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3.3, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4.3, main= "Week 4")
  mtext("Season 1 for Hellua", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.10, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.10, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.10, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.10, main= "Week 4")
  mtext("Season 2 for Hellua", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_3$Plant_dbm_3, season_3$wk1.17, main= "Week 1")
  plot(season_3$Plant_dbm_3, season_3$wk2.17, main= "Week 2")
  plot(season_3$Plant_dbm_3, season_3$wk3.17, main= "Week 3")
  plot(season_3$Plant_dbm_3, season_3$wk4.17, main= "Week 4")
  mtext("Season 3 for Hellula", outer = TRUE, cex = 1, line = -1)
  
    
  
  #Spider
  
  par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1.4, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2.4, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3.4, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4.4, main= "Week 4")
  mtext("Season 1 for Spider", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.11, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.11, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.11, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.11, main= "Week 4")
  mtext("Season 2 for Spider", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_3$Plant_dbm_3, season_3$wk1.18, main= "Week 1")
  plot(season_3$Plant_dbm_3, season_3$wk2.18, main= "Week 2")
  plot(season_3$Plant_dbm_3, season_3$wk3.18, main= "Week 3")
  plot(season_3$Plant_dbm_3, season_3$wk4.18, main= "Week 4")
  mtext("Season 3 for Spider", outer = TRUE, cex = 1, line = -1)
  
  
  
  #Ladybird
  
  par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1.5, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2.5, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3.5, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4.5, main= "Week 4")
  mtext("Season 1 for Ladybird", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.12, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.12, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.12, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.12, main= "Week 4")
  mtext("Season 2 for Ladybird", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_3$Plant_dbm_3, season_3$wk1.19, main= "Week 1")
  plot(season_3$Plant_dbm_3, season_3$wk2.19, main= "Week 2")
  plot(season_3$Plant_dbm_3, season_3$wk3.19, main= "Week 3")
  plot(season_3$Plant_dbm_3, season_3$wk4.19, main= "Week 4")
  mtext("Season 3 for Ladybird", outer = TRUE, cex = 1, line = -1)
  
  
  #Hoverfly
  
  par(mfrow=c(2,2))
  plot(season_1$Plant_DBM_1, season_1$wk1.6, main= "Week 1")
  plot(season_1$Plant_DBM_1, season_1$wk2.6, main= "Week 2")
  plot(season_1$Plant_DBM_1, season_1$wk3.6, main= "Week 3")
  plot(season_1$Plant_DBM_1, season_1$wk4.6, main= "Week 4")
  mtext("Season 1 for Hoverfly", outer = TRUE, cex = 1, line = -1)
  
  par(mfrow=c(2,2))
  plot(season_2$Plant_DBM_2, season_2$wk1.13, main= "Week 1")
  plot(season_2$Plant_DBM_2, season_2$wk2.13, main= "Week 2")
  plot(season_2$Plant_DBM_2, season_2$wk3.13, main= "Week 3")
  plot(season_2$Plant_DBM_2, season_2$wk4.13, main= "Week 4")
  mtext("Season 2 for Hoverfly", outer = TRUE, cex = 1, line = -1)
  
  
  
  #End
  