##

setwd("D:/wen_sung/psychology/Machine_learning/final_report")

##install.packages("pacman")
##if you has installed yet, just skip it.

##library packages
pacman::p_load(intsvy,tidyverse,plyr,ltm,gridExtra,coefplot)


##change folder to your TIMSS 8th grade data directory
##give you the variable description, run if necessary
timssg8.var.label(folder= 'D:/wen_sung/psychology/Machine_learning/final_report/TIMSS/data/timss2011/timss8')

##read data
##I select all countries, but you can use country=c() to select countries you are interested in 
dta <- timssg8.select.merge(folder='D:/wen_sung/psychology/Machine_learning/final_report/TIMSS/data/timss2011/timss8',
                            student=c("BSMMAT01","BSMMAT02","BSMMAT03","BSMMAT04","BSMMAT05",
                                      "BSBG04","BSBG05A","BSBG05B","BSBG05C","BSBG05D","BSBG05E", 
                                      "BSDGEDUP","ITSEX","BSBGHER","ITBIRTHM","ITBIRTHY","BSBG06A",
                                      "BSBG06B","BSBG09B","BSBG10A","BSBG10B","BSBG10C","BSBG11A",
                                      "BSBG11D","BSBM14A","BSBM14B","BSBM14C","BSBM14D","BSBM14E",
                                      "BSBM14F","BSBM15A","BSBM15B","BSBM15C","BSBM15D","BSBM15E",
                                      "BSBM16A","BSBM16B","BSBM16C","BSBM16D","BSBM16E","BSBM16F",
                                      "BSBM16G","BSBM16H","BSBM16I","BSBM16J","BSBM16K","BSBM16L",
                                      "BSBM16M","BSBM16N","BSBM20A","BSBM20B","BSBM33AA","BSBM33BA",
                                      "BSBGSLM","BSBGSLM","BSDGSLM","BSBGSVM","BSDGSVM","BSBGSCM",
                                      "BSDGSCM","BSBGEML","BSDGEML","BSDMWKHW","BSDMLOWP"),
                            school=c("BCBGMRS","BCBGEAS","BCDG03","BCDGCMP","BCBG09BA","BCBG09BB",
                                     "BCBG09BC","BCBG09BD","BCBG09BE","BCBG09BF","BCBG13A","BCBG13B",
                                     "BCBG13C","BCBG13D","BCBG15A","BCBG16A","BCDGMRS"),
                            math.teacher = c("BTBG05A","BTBG05F","BTBM17A","BTBM17B","BTBM18A","BTBM18B",
                                             "BTBM18C","BTBM18D","BTBM18E","BTBM19A","BTBM19B","BTBM19C",
                                             "BTBM19D","BTBM19E","BTBM19F","BTBM19I","BTBM19J","BTBM19K",
                                             "BTBM20A","BTBM20B","BTBM20C","BTBM20D","BTBM21A","BTBM21BA",
                                             "BTBM21BB","BTBM21BC","BTBM21BD"))
##find variables contains too many NAs
##which(apply(is.na(dta),2,sum) > 40000)


##delete variables containing too many NAs 

dta1 <- dta[,c("IDCNTRYL","IDSCHOOL","IDCLASS","IDSTUD","HOUWGT",
               "BSMMAT01","BSMMAT02","BSMMAT03","BSMMAT04","BSMMAT05",
               "BSBG04","BSBG05A","BSBG05B","BSBG05C","BSBG05D","BSBG05E", 
               "ITSEX","BSBGHER","ITBIRTHM","ITBIRTHY","BSBG06A",
               "BSBG06B","BSBG10A","BSBG10B","BSBG10C","BSBG11A",
               "BSBG11D","BSBM14A","BSBM14B","BSBM14C","BSBM14D","BSBM14E",
               "BSBM14F","BSBM15A","BSBM15B","BSBM15C","BSBM15D","BSBM15E",
               "BSBM16A","BSBM16B","BSBM16C","BSBM16D","BSBM16E","BSBM16F",
               "BSBM16G","BSBM16H","BSBM16I","BSBM16J","BSBM16K","BSBM16L",
               "BSBM16M","BSBM16N","BSBM20A","BSBM20B","BSBM33AA","BSBM33BA",
               "BSBGSLM","BSBGSLM","BSDGSLM","BSBGSVM","BSDGSVM","BSBGSCM",
               "BSDGSCM","BSBGEML","BSDGEML","BSDMWKHW","BSDMLOWP","BCBGMRS","BCBGEAS","BCDGCMP","BCBG09BA","BCBG09BB",
               "BCBG09BC","BCBG09BD","BCBG09BE","BCBG09BF","BCBG13A","BCBG13B",
               "BCBG13C","BCBG13D","BCBG15A","BCBG16A","BCDGMRS","BTBG05A","BTBG05F","BTBM17A","BTBM17B","BTBM18A","BTBM18B",
               "BTBM18C","BTBM18D","BTBM18E","BTBM19A","BTBM19B","BTBM19C",
               "BTBM19D","BTBM19E","BTBM19F","BTBM19I","BTBM19J","BTBM19K",
               "BTBM20A","BTBM20B","BTBM20C","BTBM20D","BTBM21A")]



##use subjects who do not have NAs

dta1 <- dta1[complete.cases(dta1),]

##sampling(because it's a practice, I use 1/3 data in order to save time)

idx_fold <- sample(1:14, nrow(dta1), replace = T) 
dta1 <- dta1%>% filter(idx_fold %in% c(1,2,3,4,5)) 

##rename variable names
names(dta1) <- c("AREA","IDSCHOOL","IDCLASS","IDSTUD","HOUWGT",
                 "MAT01","MAT02","MAT03","MAT04","MAT05",
                 "nbooks","computer","desk","book","room","net", 
                 "EDUP","gender","HomeERES",
                 "MathRS","EMPHAS","StuBack","ComputerForIns")


##write table and next time can read this table to facilitate work
write.csv(x = dta1,file = "TIMSS_2011_8th.csv")


##data management
dta <- dta1[complete.cases(dta1),]

##coerce variables to factor and rename factor levels
dta$EDUP <- as.factor(dta$EDUP)
levels(dta$EDUP) <- c("UnivOrHG","PostSec", "SeniorH", "JuniorH","PrimaryOrBL")
dta$gender <- factor(dta$gender, levels =c("GIRL", "BOY")) 
dta$nbooks <- factor(dta$nbooks,levels =  c("0-10 BOOKS","11-25 BOOKS","26-100 BOOKS","101-200 BOOKS","MORE THAN 200"))

dta$book <- (dta$book=="YES")
dta$computer <- (dta$computer=="YES")
dta$net <- (dta$net=="YES")
dta$desk <- (dta$desk=="YES")
dta$room <- (dta$room=="YES")

#Forth way to measure family resource


m1 <- lm(MAT01 ~ EDUP-1, data = dta)

m2 <- lm(MAT01 ~ EDUP+nbooks-1, data = dta)

dta$resource <- 0
dta[dta$nbooks=="0-10 BOOKS",]$resource <- 0
dta[dta$nbooks=="11-25 BOOKS",]$resource <- coef(m2)['nbooks11-25 BOOKS']
dta[dta$nbooks=="26-100 BOOKS",]$resource <- coef(m2)['nbooks26-100 BOOKS']
dta[dta$nbooks=="101-200 BOOKS",]$resource <- coef(m2)['nbooks101-200 BOOKS']
dta[dta$nbooks=="MORE THAN 200",]$resource <- coef(m2)['nbooksMORE THAN 200']


######## some data visualization to explore relation between variables###


##ggplot-A

ggplot(data = dta, aes(x = EDUP, y = MAT01)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(dta$MAT01) , linetype = 'dotted') +
  geom_hline(yintercept = mean(dta$MAT01)- .5*sd(dta$MAT01) , linetype = 'dotted') +
  geom_hline(yintercept = mean(dta$MAT01)- 1*sd(dta$MAT01) , linetype = 'dotted') +
  geom_hline(yintercept = mean(dta$MAT01)+ .5*sd(dta$MAT01) , linetype = 'dotted') +
  labs(x = 'Parents Education', y = 'Mean Math scores') +
  theme(text = element_text(size=16)) +
  theme_bw()+
  coord_flip()

##ggplot-B

ggplot(data = dta, aes(group = EDUP, y = MAT01, x = resource)) +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = EDUP, y = MAT01, x =resource), method = 'lm', se = F) +
  facet_grid( . ~  EDUP) +
  labs(x = 'Family Education Resource', y = 'Math Scores')+
  theme_bw()

##ggplot-C

dta$IDSCHOOL <- as.factor(dta$IDSCHOOL)

ggplot(data = dta,aes(x = HomeERES,y = MAT01))+
  geom_point()+
  stat_smooth(method = "lm")+
  labs(x = "Home educational resource",y = "Math Score")+
  theme_bw()

##plot

dta$AREA <- as.factor(dta$AREA)

dta_HomeERES <- data.frame(with(dta,tapply(HomeERES,AREA,mean)))
dta_HomeERES$AREA <- rownames(dta_HomeERES)
dta_HomeERES$AREA <- as.factor(dta_HomeERES$AREA)
dta_MAT01 <- data.frame(with(dta,tapply(MAT01,AREA,mean)))
dta_MAT01$AREA <- rownames(dta_MAT01)
dta_MAT01$AREA <- as.factor(dta_MAT01$AREA)

dta1 <- left_join(x = dta_HomeERES,y = dta_MAT01,by = "AREA")
colnames(dta1) <- c("HomeERES","AREA","MAT01")

with(dta1,plot(HomeERES,MAT01,xlab = "Home educational resource",ylab = "Math Score",type = "n"))
r1 <- lm(MAT01~HomeERES,data = dta1)
abline(a = coef(r1)[1],b = coef(r1)[2],lty = 2)
text(x = dta1$HomeERES,y = dta1$MAT01,labels = dta1$AREA)














