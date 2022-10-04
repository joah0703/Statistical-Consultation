install.packages('foreign')
library(foreign)

toeic = read.spss('F:\\2021\\통계분석 상담\\토익특강결과.sav', to.data.frame=TRUE)
#write.csv(toeic, file = 'F:\\2021\\통계분석 상담\\토익특강결과.csv')

sum(is.na(toeic)) #NA값 없음
str(toeic)
colnames(toeic)


levels(toeic$Year)   #"2018" "2019" "2020" "2021"
levels(toeic$Program)    #"TOEIC Basic" "TOEIC Intermediate" "TOEIC Advanced" 

toeic2 = transform(toeic, year_cat = ifelse(Year==2018, 1, ifelse(Year==2019, 2, ifelse(Year==2020, 3, 4))),
                          program_cat = ifelse(Program=="TOEIC Basic", 1, ifelse(Program=="TOEIC Intermediate", 2, 3)))



##### 1. mean & plot
year2018 = toeic2[which(toeic2$year_cat==1),]
sum(year2018$program_cat==1) #128
sum(year2018$program_cat==2) #79
sum(year2018$program_cat==3) #24

mean(year2018[which(year2018$program_cat==1), "First"]) #291.0156
mean(year2018[which(year2018$program_cat==2), "First"]) #529.8734
mean(year2018[which(year2018$program_cat==3), "First"]) #479.1667

mean(year2018[which(year2018$program_cat==1), "Final"]) #468.3984
mean(year2018[which(year2018$program_cat==2), "Final"]) #652.4684
mean(year2018[which(year2018$program_cat==3), "Final"]) #610.2083

mean(year2018[which(year2018$program_cat==1), "Improve"]) #177.3828
mean(year2018[which(year2018$program_cat==2), "Improve"]) #122.5949
mean(year2018[which(year2018$program_cat==3), "Improve"]) #131.0417


###
year2019 = toeic2[which(toeic2$year_cat==2),]
sum(year2019$program_cat==1) #89
sum(year2019$program_cat==2) #46
sum(year2019$program_cat==3) #17

mean(year2019[which(year2019$program_cat==1), "First"]) #292.1348
mean(year2019[which(year2019$program_cat==2), "First"]) #474.8913
mean(year2019[which(year2019$program_cat==3), "First"]) #492.9412

mean(year2019[which(year2019$program_cat==1), "Final"]) #517.191
mean(year2019[which(year2019$program_cat==2), "Final"]) #617.1739
mean(year2019[which(year2019$program_cat==3), "Final"]) #641.7647

mean(year2019[which(year2019$program_cat==1), "Improve"]) #225.0562
mean(year2019[which(year2019$program_cat==2), "Improve"]) #142.2826
mean(year2019[which(year2019$program_cat==3), "Improve"]) #148.8235


###
year2020 = toeic2[which(toeic2$year_cat==3),]
sum(year2020$program_cat==1) #
sum(year2020$program_cat==2) #
sum(year2020$program_cat==3) #

mean(year2020[which(year2020$program_cat==1), "First"]) #
mean(year2020[which(year2020$program_cat==2), "First"]) #
mean(year2020[which(year2020$program_cat==3), "First"]) #

mean(year2020[which(year2020$program_cat==1), "Final"]) #
mean(year2020[which(year2020$program_cat==2), "Final"]) #
mean(year2020[which(year2020$program_cat==3), "Final"]) #

mean(year2020[which(year2020$program_cat==1), "Improve"]) #
mean(year2020[which(year2020$program_cat==2), "Improve"]) #
mean(year2020[which(year2020$program_cat==3), "Improve"]) #


###
year2021 = toeic2[which(toeic2$year_cat==4),]
sum(year2021$program_cat==1) #
sum(year2021$program_cat==2) #
sum(year2021$program_cat==3) #

mean(year2021[which(year2021$program_cat==1), "First"]) #
mean(year2021[which(year2021$program_cat==2), "First"]) #
mean(year2021[which(year2021$program_cat==3), "First"]) #

mean(year2021[which(year2021$program_cat==1), "Final"]) #
mean(year2021[which(year2021$program_cat==2), "Final"]) #
mean(year2021[which(year2021$program_cat==3), "Final"]) #

mean(year2021[which(year2021$program_cat==1), "Improve"]) #
mean(year2021[which(year2021$program_cat==2), "Improve"]) #
mean(year2021[which(year2021$program_cat==3), "Improve"]) #


library(ggplot2) 
library(dplyr)
data_sum = toeic %>% group_by(Year, Program) %>% summarise(mean=mean(First))
ggplot(data=data_sum, aes(x=Program,y=mean))+geom_point(aes(col=Year,shape=Year))+geom_line(aes(group=Year,col=Year,lty=Year))
ggplot(data=data_sum, aes(x=Year,y=mean))+geom_point(aes(col=Program,shape=Program))+geom_line(aes(group=Program,col=Program,lty=Program))

data_sum2 = toeic %>% group_by(Year, Program) %>% summarise(mean=mean(Final))
ggplot(data=data_sum2, aes(x=Program,y=mean))+geom_point(aes(col=Year,shape=Year))+geom_line(aes(group=Year,col=Year,lty=Year))
ggplot(data=data_sum2, aes(x=Year,y=mean))+geom_point(aes(col=Program,shape=Program))+geom_line(aes(group=Program,col=Program,lty=Program))

data_sum3 = toeic %>% group_by(Year, Program) %>% summarise(mean=mean(Improve))
ggplot(data=data_sum3, aes(x=Program,y=mean))+geom_point(aes(col=Year,shape=Year))+geom_line(aes(group=Year,col=Year,lty=Year))
ggplot(data=data_sum3, aes(x=Year,y=mean))+geom_point(aes(col=Program,shape=Program))+geom_line(aes(group=Program,col=Program,lty=Program))


##### 2. two-way ANOVA
# https://rfriend.tistory.com/136
#mean_toeic = read.csv("F:\\2021\\통계분석 상담\\평균도표.csv")

toeic2$year_cat = as.factor(toeic2$year_cat)
toeic2$program_cat = as.factor(toeic2$program_cat)
str(toeic2)
head(toeic2)

# first score
aov_model = aov(First ~ year_cat + program_cat + year_cat:program_cat, toeic2)
summary(aov_model)
one_aov = aov(First ~ year_cat, toeic2)
summary(one_aov)
one_aov2 = aov(First ~ program_cat, toeic2)
summary(one_aov2)

# final score
aov_model2 = aov(Final ~ year_cat + program_cat + year_cat:program_cat, toeic2)
summary(aov_model2)
one_aov3 = aov(Final ~ year_cat, toeic2)
summary(one_aov3)
one_aov4 = aov(Final ~ program_cat, toeic2)
summary(one_aov4)

# Improve
aov_model3 = aov(Improve ~ year_cat + program_cat + year_cat:program_cat, toeic2)
summary(aov_model3)
one_aov5 = aov(Improve ~ year_cat, toeic2)
summary(one_aov5)
one_aov6 = aov(Improve ~ program_cat, toeic2)
summary(one_aov6)


### Bonferroni test
#by year(First)
pairwise.wilcox.test(toeic$First, toeic$Year, p.adjust='bonferroni')
#by program (First)
pairwise.wilcox.test(toeic$First, toeic$Program, p.adjust='bonferroni')

#by year(Final)
pairwise.wilcox.test(toeic$Final, toeic$Year, p.adjust='bonferroni')
#by program (Final)
pairwise.wilcox.test(toeic$Final, toeic$Program, p.adjust='bonferroni')

#by year(Improve)
pairwise.wilcox.test(toeic$Improve, toeic$Year, p.adjust='bonferroni')
#by program (Improve)
pairwise.wilcox.test(toeic$Improve, toeic$Program, p.adjust='bonferroni')


##### 3. 
#https://sysiphe0.tistory.com/5
toeic3 = transform(toeic, class = ifelse(Year==2018 | Year==2019, "off-line", "on-line"))
toeic3$class = as.factor(toeic3$class)
str(toeic3)

class_avg = toeic3 %>% group_by(class, Program) %>% summarise(mean=mean(Final))
ggplot(data=class_avg, aes(x=Program,y=mean))+geom_point(aes(col=class,shape=class))+geom_line(aes(group=class,col=class,lty=class))+ggtitle("Final score")
ggplot(data=class_avg, aes(x=class,y=mean))+geom_point(aes(col=Program,shape=Program))+geom_line(aes(group=Program,col=Program,lty=Program))+ggtitle("Final score")
#ggplot(data=class_avg, aes(x=class, y=mean, group=class))+geom_boxplot()

class_avg2 = toeic3 %>% group_by(class, Program) %>% summarise(mean=mean(Improve))
ggplot(data=class_avg2, aes(x=Program,y=mean))+geom_point(aes(col=class,shape=class))+geom_line(aes(group=class,col=class,lty=class))+ggtitle("Improve")
ggplot(data=class_avg2, aes(x=class,y=mean))+geom_point(aes(col=Program,shape=Program))+geom_line(aes(group=Program,col=Program,lty=Program))+ggtitle("Improve")


install.packages('lawstat')
library(lawstat)

## final score
# total
shapiro.test(toeic3$Final)
#hist(toeic3$Final, breaks=100)
wilcox.test(Final ~ class, data=toeic3, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

# TOEIC Basic
toeic_b = toeic3[which(toeic3$Program=="TOEIC Basic"),]
shapiro.test(toeic_b$Final)
wilcox.test(Final ~ class, data=toeic_b, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

# TOEIC Intermediate
toeic_i = toeic3[which(toeic3$Program=="TOEIC Intermediate"),]
shapiro.test(toeic_i$Final)
wilcox.test(Final ~ class, data=toeic_i, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

# TOEIC Advanced
toeic_a = toeic3[which(toeic3$Program=="TOEIC Advanced"),]
shapiro.test(toeic_a$Final)
levene.test(toeic_a$Final, toeic_a$class, location="mean")
t.test(toeic_a[which(toeic_a$class=="off-line"), "Final"], toeic_a[which(toeic_a$class=="on-line"), "Final"], var.equal = TRUE)


## Improve
# total
shapiro.test(toeic3$Improve)
wilcox.test(Improve ~ class, data=toeic3, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

# TOEIC Basic
shapiro.test(toeic_b$Improve)
wilcox.test(Improve ~ class, data=toeic_b, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

# TOEIC Intermediate
shapiro.test(toeic_i$Improve)
wilcox.test(Improve ~ class, data=toeic_i, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

# TOEIC Advanced
shapiro.test(toeic_a$Improve)
levene.test(toeic_a$Improve, toeic_a$class, location="mean")
t.test(toeic_a[which(toeic_a$class=="off-line"), "Improve"], toeic_a[which(toeic_a$class=="on-line"), "Improve"], var.equal = TRUE)



toeic_off = toeic3[toeic3$class=="off-line",]
toeic_on = toeic3[toeic3$class=="on-line",]

# off-line / final
aov_model = aov(Final ~ Program, toeic_off)
summary(aov_model)
pairwise.wilcox.test(toeic_off$Final, toeic_off$Program, p.adjust='bonferroni')

# off-line / improve
aov_model2 = aov(Improve ~ Program, toeic_off)
summary(aov_model2)
pairwise.wilcox.test(toeic_off$Improve, toeic_off$Program, p.adjust='bonferroni')

# on-line / final
aov_model3 = aov(Final ~ Program, toeic_on)
summary(aov_model3)
pairwise.wilcox.test(toeic_on$Final, toeic_on$Program, p.adjust='bonferroni')

# on-line / improve
aov_model4 = aov(Improve ~ Program, toeic_on)
summary(aov_model4)
pairwise.wilcox.test(toeic_on$Improve, toeic_on$Program, p.adjust='bonferroni')
