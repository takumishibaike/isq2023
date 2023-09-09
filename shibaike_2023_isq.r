library(stargazer)
library(ggplot2)
library(ggrepel)
library(glmmTMB)

rm(list=ls()) 

setwd("/Users/Takumi Shibaike/Desktop/replication/")
d <- read.csv("mammal.csv", header = T)

##### Create variables #####

d$ngos <- d$us_ngo + d$uk_ngo + d$aus_ngo + d$jpn_ngo
d$cute <- d$us_cute + d$uk_cute + d$aus_cute + d$jpn_cute
d$salience <- d$eng_news + d$jpn_news

# Create a binary variable showing animals live in the sample countries.
for (i in 1:length(d[,1])){
    if(sum(d[i,3:6])>0){
        d$g_north[i] <- 1
    }
    else{d$g_north[i] <- 0}
}

# List of species explicitedly listed by WWF
wwf_priority <- c("antelope","saiga","argali","elephant","bonobo","gorilla","chimpanzee","orangutan","leopard","panda","polar bear","rhino","tiger","whale","dolphin","kangaroo","wallaby")

for (i in 1:length(d[,1])){
    if(any(wwf_priority==d$name[i])){
        d$wwf_priority[i]=1
    }
    else{d$wwf_priority[i]=0}
}

##### Figures and Tables in the main text #####

### Figure 2
ggplot(d, aes(x= category, y= salience))+
    geom_point() +
    geom_label_repel(label=subset(d, salience > 10)$name,
                     data         = subset(d, salience > 10),
                     nudge_y       = 1,
                     size          = 4,
                     box.padding   = 0,
                     point.padding = 1,
                     force         = 1,
                     segment.size  = 0.2,
                     direction     = "x") + theme_bw() +labs(x="Endangeredness", y="Issue salience") + theme(text = element_text(size = 16))

### Figure 3
ggplot(d, aes(x= ngos, y= wwf))+
    geom_point() + geom_label_repel(label=subset(d, wwf > 50 | ngos > 10)$name,
                                    data         = subset(d, wwf > 50 |ngos > 10),
                                    nudge_x       = 1,
                                    size          = 4,
                                    box.padding   = 0,
                                    point.padding = 1,
                                    force         = 1,
                                    segment.size  = 0.2, 
                                    direction     = "y") +
     theme_bw() +labs(x="Specialist NGO desnity", y="WWF priority") + theme(text = element_text(size = 16))

### Figure 4
pangolin <- read.csv("pangolin.csv")
ggplot(pangolin, aes(x=year,y=pangolin, colour="black")) + geom_point(color="black") + geom_line(color="black") + theme_bw() + labs(x="Year", y="Number of articles") + theme(text = element_text(size = 16)) 

### Results for Table 2.
d$log_salience = log(d$salience+1) # log transformation.
d$count_salience = round(d$salience*100) # round it for NB.

m1 <- glmmTMB(count_salience ~ wwf + g_north + category + cute, data=d, family = nbinom2)
m2 <- glmmTMB(count_salience ~ wwf + ngos + g_north + category + cute, family = nbinom2, data=d)
m3 <- glmmTMB(count_salience ~ wwf + ngos + us + uk + aus + jpn + category + cute, family = nbinom2, data=d)
m4 <- glmmTMB(count_salience ~ wwf + ngos + wwf_priority + g_north + category + cute, family = nbinom2, data=d)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

###### Appendix #####

### Appendix C
wildlife = read.csv("wildlife.csv", header = T) # articles examined in the paper.
pet_abuse = read.csv("pet_abuse.csv", header = T) # articles with "pet abuse".
death_penalty = read.csv("death_penalty.csv", header = T) # articles with "death penalty".

t.test(pet_abuse$pet_abuse, wildlife$wildlife)
t.test(death_penalty$death_penalty, wildlife$wildlife)
t.test(death_penalty$death_penalty, pet_abuse$pet_abuse)

### Appendix D
stargazer(d[,c("salience","wwf","ngos","g_north","us","uk","jpn","aus","category","cute")])

### Appendix E
cordata = d[,c("category","wwf","ngos","salience")]
stargazer(cor(cordata)) 

### Appendix F
ggplot(d, aes(salience))+geom_histogram(binwidth = 1,alpha=0.5) + ylab("Number of species") + xlab("Issue salience")+ theme_bw() + theme(text = element_text(size = 16))


### Results for Appendix H.

l1 <- lm(log_salience ~ wwf + g_north + category + cute, data=d)
l2 <- lm(log_salience ~ ngos + wwf + g_north + category + cute, data=d)
l3 <- lm(log_salience ~ ngos + wwf + us + uk + aus + jpn + category + cute, data=d)


summary(l1)
summary(l2)
summary(l3)

### Results for Appendix I (first table).

m5 <- glmmTMB(count_salience ~ wwf + wwf_priority + g_north + category + cute, data=d, family = nbinom2)
m6 <- glmmTMB(count_salience ~ wwf + ngos + wwf_priority + g_north + category + cute, family = nbinom2, data=d)
m7 <- glmmTMB(count_salience ~ wwf + ngos + wwf_priority + us + uk + aus + jpn + category + cute, family = nbinom2, data=d)

summary(m5)
summary(m6)
summary(m7)

### Results for Appendix I (second table).

# Create a new dataset without priority species.
d_nopriority <- d
for (i in wwf_priority){
    row_to_delete <- which(i == d_nopriority$name)
    d_nopriority <- d_nopriority[-row_to_delete,]
}

m8 <- glmmTMB(count_salience ~ wwf + g_north + category + cute, data=d_nopriority, family = nbinom2)
m9 <- glmmTMB(count_salience ~ wwf + ngos + g_north + category + cute, family = nbinom2, data=d_nopriority)
m10 <- glmmTMB(count_salience ~ wwf + ngos + us + uk + aus + jpn + category + cute, family = nbinom2, data=d_nopriority)

summary(m8)
summary(m9)
summary(m10)


### Results for Appendix J.

m11 <- glmmTMB(count_salience ~ wwf_priority + ngos*wwf_priority + g_north + category + cute, data=d, family = nbinom2)
m12 <- glmmTMB(count_salience ~ ngos + wwf_priority + ngos*wwf_priority + g_north + category + cute, family = nbinom2, data=d)
m13 <- glmmTMB(count_salience ~ ngos + wwf_priority + ngos*wwf_priority + us + uk + aus + jpn + category + cute, family = nbinom2, data=d)

summary(m11)
summary(m12)
summary(m13)

### Marginal effects for Appendix K.
mod = m2
wwfdata = data.frame(wwf = seq(0,262,length.out=203),
                     ngos = mean(d$ngos),
                     g_north = mean(d$g_north),
                     category = mean(d$category),
                     cute = mean(d$cute))
ngodata <- data.frame(wwf = mean(d$wwf),
                      ngos = seq(0,32,length.out=203),
                      g_north = mean(d$g_north),
                      category = mean(d$category),
                      cute = mean(d$cute))

newdata = ngodata
pred = data.frame(predict(mod, newdata=newdata, se.fit = T))
names(pred)[1:2] = c("xb","se")
pred$lambda = exp(pred$xb) 
pred$lowerb = exp(pred$xb - 1.96*pred$se) # 95% confidence interval.
pred$upperb = exp(pred$xb + 1.96*pred$se) # 95% confidence interval.
pred$wwf = wwfdata$wwf
pred$ngos = ngodata$ngos
fixef(mod)
pred$mfx_ngos = pred$lambda*0.12993
pred$mfx_wwf = pred$lambda*0.01314
mean(pred$mfx_ngos)
mean(pred$mfx_wwf)
dev.off()
ggplot(pred, aes(x=wwf, y=lambda)) + 
    geom_line(col="black", size=1) +
    geom_line(aes(y=lowerb), lty=2) +
    geom_line(aes(y=upperb), lty=2) +
    xlab("WWF priority") + ylab("Issue salience") + ylim(0,1500)+ theme_bw()+ theme(text = element_text(size = 16))
ggplot(pred, aes(x=ngos, y=lambda)) + 
    geom_line(col="black", size=1) +
    geom_line(aes(y=lowerb), lty=2) +
    geom_line(aes(y=upperb), lty=2) +
    xlab("Specialist NGO density") + ylab("Issue salience") + ylim(0,1500) + theme_bw() + theme(text = element_text(size = 16))