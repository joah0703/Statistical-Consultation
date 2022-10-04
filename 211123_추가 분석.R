#install.packages('agricolae')
library(foreign)
library(agricolae)
library(lawstat)
toeic = read.spss('F:\\2021\\통계분석 상담\\토익특강결과.sav', to.data.frame=TRUE)

toeic2 = transform(toeic, year_cat = ifelse(Year==2018, 1, ifelse(Year==2019, 2, ifelse(Year==2020, 3, 4))),
                   program_cat = ifelse(Program=="TOEIC Basic", 1, ifelse(Program=="TOEIC Intermediate", 2, 3)))
str(toeic2)
#toeic2$year_cat = as.factor(toeic2$year_cat)
#toeic2$program_cat = as.factor(toeic2$program_cat)


#######cor.test
cor(toeic2[-c(1,2)])
cor.test(toeic2$First, toeic2$year_cat)
cor.test(toeic2$First, toeic2$program_cat)
cor.test(toeic2$Final, toeic2$year_cat)
cor.test(toeic2$Final, toeic2$program_cat)
cor.test(toeic2$Improve, toeic2$year_cat)
cor.test(toeic2$Improve, toeic2$program_cat)


#######one-way ANOVA & bonferroni, Scheffe
### program
basic = toeic[which(toeic$Program == "TOEIC Basic"),]
inter = toeic[which(toeic$Program == "TOEIC Intermediate"),]
adv = toeic[which(toeic$Program == "TOEIC Advanced"),]

plot(1:length(inter[which(inter$Year==2018),'Improve']),inter[which(inter$Year==2018),'Improve'], type='l')
lines(inter[which(inter$Year==2019),'Improve'], type='l', col=2)
lines(inter[which(inter$Year==2020),'Improve'], type='l', col=3)
lines(inter[which(inter$Year==2021),'Improve'], type='l', col=2)

aov_first1 = aov(First ~ Year, basic)
summary(aov_first1)

aov_first2 = aov(First ~ Year, inter)
summary(aov_first2)
pairwise.wilcox.test(inter$First, inter$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_first2, "Year", alpha = 0.05, group=TRUE, console=TRUE)
       
aov_first3 = aov(First ~ Year, adv)
summary(aov_first3)                    
pairwise.wilcox.test(adv$First, adv$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_first3, "Year", alpha = 0.05, group=TRUE, console=TRUE)

##
aov_final1 = aov(Final ~ Year, basic)
summary(aov_final1)
pairwise.wilcox.test(basic$Final, basic$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_final1, "Year", alpha = 0.05, group=TRUE, console=TRUE)

aov_final1 = aov(Final ~ Year, inter)
summary(aov_final1)
pairwise.wilcox.test(inter$Final, inter$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_final1, "Year", alpha = 0.05, group=TRUE, console=TRUE)

aov_final1 = aov(Final ~ Year, adv)
summary(aov_final1)
pairwise.wilcox.test(adv$Final, adv$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_final1, "Year", alpha = 0.05, group=TRUE, console=TRUE)

##
aov_final1 = aov(Improve ~ Year, basic)
summary(aov_final1)
pairwise.wilcox.test(basic$Improve, basic$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_final1, "Year", alpha = 0.05, group=TRUE, console=TRUE)

aov_final1 = aov(Improve ~ Year, inter)
summary(aov_final1)
pairwise.wilcox.test(inter$Improve, inter$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_final1, "Year", alpha = 0.05, group=TRUE, console=TRUE)

aov_final1 = aov(Improve ~ Year, adv)
summary(aov_final1)
pairwise.wilcox.test(adv$Improve, adv$Year, p.adjust='bonferroni')
comparison = scheffe.test(aov_final1, "Year", alpha = 0.05, group=TRUE, console=TRUE)


### year
y18 = toeic[which(toeic$Year == 2018),]
y19 = toeic[which(toeic$Year == 2019),]
y20 = toeic[which(toeic$Year == 2020),]
y21 = toeic[which(toeic$Year == 2021),]


aov_mod = aov(First ~ Program, y18)
summary(aov_mod)
pairwise.wilcox.test(y18$First, y18$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(First ~ Program, y19)
summary(aov_mod)
pairwise.wilcox.test(y19$First, y19$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(First ~ Program, y20)
summary(aov_mod)
pairwise.wilcox.test(y20$First, y20$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(First ~ Program, y21)
summary(aov_mod)
pairwise.wilcox.test(y21$First, y21$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

##
aov_mod = aov(Final ~ Program, y18)
summary(aov_mod)
pairwise.wilcox.test(y18$Final, y18$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(Final ~ Program, y19)
summary(aov_mod)
pairwise.wilcox.test(y19$Final, y19$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(Final ~ Program, y20)
summary(aov_mod)
pairwise.wilcox.test(y20$Final, y20$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(Final ~ Program, y21)
summary(aov_mod)
pairwise.wilcox.test(y21$Final, y21$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

##
aov_mod = aov(Improve ~ Program, y18)
summary(aov_mod)
pairwise.wilcox.test(y18$Improve, y18$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(Improve ~ Program, y19)
summary(aov_mod)
pairwise.wilcox.test(y19$Improve, y19$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(Improve ~ Program, y20)
summary(aov_mod)
pairwise.wilcox.test(y20$Improve, y20$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)

aov_mod = aov(Improve ~ Program, y21)
summary(aov_mod)
pairwise.wilcox.test(y21$Improve, y21$Program, p.adjust='bonferroni')
comparison = scheffe.test(aov_mod, "Program", alpha = 0.05, group=TRUE, console=TRUE)


####### t-test
toeic3 = transform(toeic, class = ifelse(Year==2018 | Year==2019, "off-line", "on-line"))
toeic3$class = as.factor(toeic3$class)

toeic_b = toeic3[which(toeic3$Program=="TOEIC Basic"),]
toeic_i = toeic3[which(toeic3$Program=="TOEIC Intermediate"),]
toeic_a = toeic3[which(toeic3$Program=="TOEIC Advanced"),]

## one sided
shapiro.test(toeic_b$Final)
wilcox.test(Final ~ class, data=toeic_b, alternative = c("greater"), mu = 0, conf.int = FALSE, conf.level = 0.95)
wilcox.test(Improve ~ class, data=toeic_b, alternative = c("greater"), mu = 0, conf.int = FALSE, conf.level = 0.95)
t.test(toeic_a[which(toeic_a$class=="off-line"), "Final"], toeic_a[which(toeic_a$class=="on-line"), "Final"], var.equal = TRUE, alternative = c("greater"))
t.test(toeic_a[which(toeic_a$class=="off-line"), "Improve"], toeic_a[which(toeic_a$class=="on-line"), "Improve"], var.equal = TRUE, alternative = c("greater"))

wilcox.test(Final ~ class, data=toeic_i, alternative = c("greater"), mu = 0, conf.int = FALSE, conf.level = 0.95)
wilcox.test(Improve ~ class, data=toeic_i, alternative = c("greater"), mu = 0, conf.int = FALSE, conf.level = 0.95)


## two sided
toeic_off = toeic3[which((toeic3$class=="off-line") & (toeic3$Program!="TOEIC Intermediate")),]
toeic_on = toeic3[which((toeic3$class=="on-line") & (toeic3$Program!="TOEIC Intermediate")),]


shapiro.test(toeic_off$Final)
wilcox.test(Final ~ Program, data=toeic_off, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

shapiro.test(toeic_on$Final)
wilcox.test(Final ~ Program, data=toeic_on, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

shapiro.test(toeic_off$Improve)
wilcox.test(Improve ~ Program, data=toeic_off, alternative = c("two.sided"), mu = 0, conf.int = FALSE, conf.level = 0.95)

shapiro.test(toeic_on$Improve)
levene.test(toeic_on$Improve, toeic_on$Program, location="mean")
t.test(toeic_on[which(toeic_on$Program=="TOEIC Basic"), "Improve"], toeic_on[which(toeic_on$Program=="TOEIC Advanced"), "Improve"], var.equal = FALSE)

