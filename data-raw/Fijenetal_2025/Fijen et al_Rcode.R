#read data
data <- read.csv("data.csv", sep = ',')
data$round <- as.factor(data$round)
options(scipen = 999)
#load libraries
library(ggplot2)
library(visreg)
library(gridExtra)
library(lemon)
library(reshape2)
library(DHARMa)
library(glmmTMB)
library(performance)

############# Statistics on transect level
##### Wild bees
m1 <- glmmTMB(log10(ab.pol.wildbees.snh) ~ snh*mfc.flower + log.av.flow.cov + av.flow.richness + (1|round) + (1|landscape), data, 
              na.action = na.fail, REML = F)
m1b <- glmmTMB(log10(ab.pol.wildbees.snh) ~ snh + log.av.flow.cov + av.flow.richness + mfc.flower + (1|round)  + (1|landscape), data, 
               na.action = na.fail, REML = F)
anova(m1,m1b) #interaction NS, so m1b is model.
plot(simulateResiduals(m1b)) #all good
ranef(m1b) #apparently very little variation captured, hence singular fit-warning when using lmer
check_collinearity(m1b)
hist(resid(m1b))
summary(m1b)

#testing significance of main effects using lrt
m1ba <- glmmTMB(log10(ab.pol.wildbees.snh) ~ log.av.flow.cov + av.flow.richness+ mfc.flower+ (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)
m1bb <- glmmTMB(log10(ab.pol.wildbees.snh) ~ snh + av.flow.richness+ mfc.flower+ (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)
m1bc <- glmmTMB(log10(ab.pol.wildbees.snh) ~ snh + log.av.flow.cov+ mfc.flower+ (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)
m1bd <- glmmTMB(log10(ab.pol.wildbees.snh) ~ snh + log.av.flow.cov+ av.flow.richness+ (1|round) + (1|landscape), data, 
                na.action = na.fail, REML = F,  control=glmmTMBControl(optimizer=nlminb,
                                                                       optArgs=list(method="CG")))
anova(m1b, m1ba) #snh
anova(m1b, m1bb) #flowcov
anova(m1b, m1bc) #richness
anova(m1b, m1bd) #mfc due to convergence error no AIC, but p-value seems robust. 

##### Hoverflies
m2 <- glmmTMB(log10(ab.pol.hoverflies.snh+1) ~ snh*mfc.flower + log.av.flow.cov + av.flow.richness + (1|round)+ (1|landscape), data, 
              na.action = na.fail, REML = F)
m2b <- glmmTMB(log10(ab.pol.hoverflies.snh+1) ~ mfc.flower + log.av.flow.cov + av.flow.richness + snh + (1|round) + (1|landscape), data, 
               na.action = na.fail, REML = F) 
anova(m2,m2b) # interaction marginally not significant, so m2b is model. Do check graphs (supplement)
ranef(m2b) #variance captured
plot(simulateResiduals(m2b)) #Is okish - red, but visually ok. 
check_collinearity(m2b)
hist(resid(m2b))
summary(m2b)

#testing significance of main effects using lrt
m2bb <- glmmTMB(log10(ab.pol.hoverflies.snh+1) ~ log.av.flow.cov + snh + av.flow.richness + (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)
m2bc <- glmmTMB(log10(ab.pol.hoverflies.snh+1) ~ mfc.flower + log.av.flow.cov + snh + (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)
m2bd <- glmmTMB(log10(ab.pol.hoverflies.snh+1) ~ mfc.flower + log.av.flow.cov + av.flow.richness + (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)
m2be <- glmmTMB(log10(ab.pol.hoverflies.snh+1) ~ mfc.flower + snh + av.flow.richness + (1|round)+ (1|landscape), data, 
                na.action = na.fail, REML = F)

anova(m2b, m2bd) #snh
anova(m2b, m2be) #flower cover
anova(m2b, m2bc) #richness
anova(m2b, m2bb) #mfc


#### plots bees on transect level
visreg.m1b <- visreg(m1b, xvar = 'snh', trans = function(x) 10^x)
p1 <- ggplot(visreg.m1b$fit, aes(x = snh, y = visregFit)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m1b$res, aes(y = visregRes)) +  
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'N.S.', x = 70, y = 60, size = 6) +
  scale_y_continuous(limits = c(0,60)) +
  theme_bw() + labs(x = 'SNH in landscape (%)', 
                    y = "Wild bee density in SNH (#/150m2)",
                    tag = "(a)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) ) 
p1

visreg.m1bb <- visreg(m1b, xvar = 'log.av.flow.cov', trans = function(x) 10^x, 
                      xtrans = function(x) 10^x)
p1b <- ggplot(visreg.m1bb$fit, aes(x = log.av.flow.cov, y = visregFit)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m1bb$res, aes(y = visregRes)) +  
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'p<0.001', x = 35, y = 60, size = 6) +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_log10()+
  theme_bw() + labs(x = 'Flower cover (%)', 
                    y = "Wild bee density in SNH (#/150m2)",
                    tag = "(b)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) ) 
p1b

visreg.m1bc <- visreg(m1b, xvar = 'av.flow.richness', trans = function(x) 10^x)
p1c <- ggplot(visreg.m1bc$fit, aes(x = av.flow.richness, y = visregFit)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m1bc$res, aes(y = visregRes)) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'N.S.', x = 30, y = 60, size = 6) +
  scale_y_continuous(limits = c(0,60)) +
  theme_bw() + labs(x = 'Flower richness', 
                    y = "Wild bee density in SNH (#/150m2)",
                    tag = "(c)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) ) 
p1c

visreg.m1bd <- visreg(m1b, xvar = 'mfc.flower', trans = function(x) 10^x)
summary(m1b)
p1d <- ggplot(visreg.m1bd$fit, aes(x = mfc.flower, y = visregFit, fill = mfc.flower)) +
  geom_bar(data=visreg.m1bd$fit, aes(y = visregFit), stat = "summary", fun = "mean") +
  geom_errorbar(aes(ymin=visregLwr, ymax=visregUpr), width=.2) +
  annotate("text", label = 'N.S.', x = 2.4, y = 15, size = 6) +
  theme_bw() + labs(x = 'Mass-flowering crop flowering', 
                    y = "Wild bee density in SNH (#/150m2)",
                    tag = "(d)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        legend.position = 'none',
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_fill_manual(values = c("no" = "snow4", "yes" = "snow3"))

p1d

#Figure 1
grid.arrange(nrow = 2, p1, p1b, p1c, p1d)

# plots hoverflies
visreg(m2b)
str(data$round)
visreg.m2bb <- visreg(m2b, xvar = 'snh', trans = function(x) 10^x-1,
                      overlay = T, cond = list(round = '4', landscape = 'L10')) #set to random effects estimation closest to 0
p2a <- ggplot(visreg.m2bb$fit, aes(x = snh, y = visregFit)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m2bb$res, aes(y = visregRes)) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'p=0.01', x = 65, y = 22, size = 6) +
  scale_y_continuous() +
  theme_bw() + labs(x = 'SNH in landscape (%)', 
                    y = "Hoverfly density in SNH (#/150m2)",
                    tag = "(a)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) ) 
p2a

visreg.m2b <- visreg(m2b, xvar = 'log.av.flow.cov', trans = function(x) 10^x+1,
                     xtrans = function(x) 10^x, overlay = T, cond = list(round = '4', landscape = 'L10')) #set to random effects estimation closest to 0
p2b <- ggplot(visreg.m2b$fit, aes(x = log.av.flow.cov, y = visregFit)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m2b$res, aes(y = visregRes)) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'p<0.001', x = 30, y = 33, size = 6) +
  theme_bw() + labs(x = 'Flower cover (%)', 
                    y = "Hoverfly density in SNH (#/150m2)",
                    tag = "(b)") +
  scale_x_log10()+
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) ) 
p2b

visreg.m2bc <- visreg(m2b, xvar = 'av.flow.richness', trans = function(x) 10^x,
                      overlay = T, cond = list(round = '4', landscape = 'L10')) #set to random effects estimation closest to 0
p2c <- ggplot(visreg.m2bc$fit, aes(x = av.flow.richness, y = visregFit)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m2bc$res, aes(y = visregRes)) +  
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'p<0.001', x = 29, y = 22, size = 6) +
  #  scale_y_continuous(limits = c(0,60)) +
  theme_bw() + labs(x = 'Flower richness', 
                    y = "Hoverfly density in SNH (#/150m2)",
                    tag = "(c)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) ) 
p2c

visreg.m2bd <- visreg(m2b, xvar = 'mfc.flower', trans = function(x) 10^x,
                      overlay = T, cond = list(round = '4', landscape = 'L10')) #set to random effects estimation closest to 0
p2d <- ggplot(visreg.m2bd$fit, aes(x = mfc.flower, y = visregFit, fill = mfc.flower)) +
  geom_bar(data=visreg.m2bd$fit, aes(y = visregFit), stat = "summary", fun = "mean") +
  geom_errorbar(aes(ymin=visregLwr, ymax=visregUpr), width=.2) +
  annotate("text", label = 'p<0.001', x = 2.35, y = 9.5, size = 6) +
  theme_bw() + labs(x = 'Mass-flowering crop flowering', 
                    y = "Hoverfly density in SNH (#/150m2)",
                    tag = "(d)") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        legend.position = 'none',
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_fill_manual(values = c("no" = "snow4", "yes" = "snow3"))
p2d

#Figure 2
grid.arrange(nrow = 2, p2a, p2b, p2c, p2d)

######## predict to landscape levels ####
# First set the basic parameters
q1.snh <- min(data$snh)
q2.snh <- 5
q3.snh <- 10
q4.snh <- 15
q5.snh <- 20
q6.snh <- 25
q7.snh <- 30
q8.snh <- 35
q9.snh <- 40
q10.snh <- 45
q11.snh <- 50
q12.snh <- 55
q13.snh <- 60
q14.snh <- 65
q15.snh <- 70

q1.log.av.flow.cov <- min(data$log.av.flow.cov)
q2.log.av.flow.cov <- min(data$log.av.flow.cov) + 1*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q3.log.av.flow.cov <- min(data$log.av.flow.cov) + 2*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q4.log.av.flow.cov <- min(data$log.av.flow.cov) + 3*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q5.log.av.flow.cov <- min(data$log.av.flow.cov) + 4*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q6.log.av.flow.cov <- min(data$log.av.flow.cov) + 5*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q7.log.av.flow.cov <- min(data$log.av.flow.cov) + 6*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q8.log.av.flow.cov <- min(data$log.av.flow.cov) + 7*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q9.log.av.flow.cov <- min(data$log.av.flow.cov) + 8*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q10.log.av.flow.cov <- min(data$log.av.flow.cov) + 9*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q11.log.av.flow.cov <- min(data$log.av.flow.cov) + 10*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q12.log.av.flow.cov <- min(data$log.av.flow.cov) + 11*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q13.log.av.flow.cov <- min(data$log.av.flow.cov) + 12*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q14.log.av.flow.cov <- min(data$log.av.flow.cov) + 13*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q15.log.av.flow.cov <- min(data$log.av.flow.cov) + 14*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q16.log.av.flow.cov <- min(data$log.av.flow.cov) + 15*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q17.log.av.flow.cov <- min(data$log.av.flow.cov) + 16*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q18.log.av.flow.cov <- min(data$log.av.flow.cov) + 17*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q19.log.av.flow.cov <- min(data$log.av.flow.cov) + 18*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)
q20.log.av.flow.cov <- min(data$log.av.flow.cov) + 19*((max(data$log.av.flow.cov)-min(data$log.av.flow.cov))/19)

q1.av.flow.richness <- min(data$av.flow.richness)
q2.av.flow.richness <- min(data$av.flow.richness) + 1*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q3.av.flow.richness <- min(data$av.flow.richness) + 2*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q4.av.flow.richness <- min(data$av.flow.richness) + 3*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q5.av.flow.richness <- min(data$av.flow.richness) + 4*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q6.av.flow.richness <- min(data$av.flow.richness) + 5*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q7.av.flow.richness <- min(data$av.flow.richness) + 6*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q8.av.flow.richness <- min(data$av.flow.richness) + 7*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q9.av.flow.richness <- min(data$av.flow.richness) + 8*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q10.av.flow.richness <- min(data$av.flow.richness) + 9*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q11.av.flow.richness <- min(data$av.flow.richness) + 10*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q12.av.flow.richness <- min(data$av.flow.richness) + 11*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q13.av.flow.richness <- min(data$av.flow.richness) + 12*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q14.av.flow.richness <- min(data$av.flow.richness) + 13*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q15.av.flow.richness <- min(data$av.flow.richness) + 14*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q16.av.flow.richness <- min(data$av.flow.richness) + 15*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q17.av.flow.richness <- min(data$av.flow.richness) + 16*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q18.av.flow.richness <- min(data$av.flow.richness) + 17*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q19.av.flow.richness <- min(data$av.flow.richness) + 18*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)
q20.av.flow.richness <- min(data$av.flow.richness) + 19*((max(data$av.flow.richness)-min(data$av.flow.richness))/19)

#calculations for examples in MS
10^q17.log.av.flow.cov - 10^q9.log.av.flow.cov #EU - wild bees
q17.av.flow.richness - q9.av.flow.richness #EU - wild bees
10^q13.log.av.flow.cov - 10^q9.log.av.flow.cov #EU - hoverflies
q13.av.flow.richness - q9.av.flow.richness #EU - hoverflies
10^q12.log.av.flow.cov - 10^q9.log.av.flow.cov #working lands - wild bees
q12.av.flow.richness - q9.av.flow.richness #working lands - wild bees
10^q11.log.av.flow.cov - 10^q9.log.av.flow.cov #working lands - hoverflies
q11.av.flow.richness - q9.av.flow.richness #working lands - hoverflies

# wild bee predictions
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q1.log.av.flow.cov,
                      av.flow.richness = q1.av.flow.richness,
                      mfc.flower = 'no',
                      round = NA)
extrap.0 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q2.log.av.flow.cov,
                      av.flow.richness = q2.av.flow.richness,
                      mfc.flower = 'no',
                      round = NA)
extrap.5 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q3.log.av.flow.cov,
                      av.flow.richness = q3.av.flow.richness,
                      mfc.flower = 'no',
                      round = NA)
extrap.10 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q4.log.av.flow.cov,
                      av.flow.richness = q4.av.flow.richness,
                      mfc.flower = 'no',
                      round = NA)
extrap.15 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q5.log.av.flow.cov,
                      av.flow.richness = q5.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.20 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q6.log.av.flow.cov,
                      av.flow.richness = q6.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.25 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q7.log.av.flow.cov,
                      av.flow.richness = q7.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.30 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q8.log.av.flow.cov,
                      av.flow.richness = q8.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.35 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q9.log.av.flow.cov,
                      av.flow.richness = q9.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.40 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q10.log.av.flow.cov,
                      av.flow.richness = q10.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.45 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q11.log.av.flow.cov,
                      av.flow.richness = q11.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.50 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q12.log.av.flow.cov,
                      av.flow.richness = q12.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.55 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q13.log.av.flow.cov,
                      av.flow.richness = q13.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.60 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q14.log.av.flow.cov,
                      av.flow.richness = q14.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.65 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q15.log.av.flow.cov,
                      av.flow.richness = q15.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.70 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q16.log.av.flow.cov,
                      av.flow.richness = q16.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.75 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q17.log.av.flow.cov,
                      av.flow.richness = q17.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.80 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q18.log.av.flow.cov,
                      av.flow.richness = q18.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.85 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q19.log.av.flow.cov,
                      av.flow.richness = q19.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.90 <- predict(m1b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q20.log.av.flow.cov,
                      av.flow.richness = q20.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.95 <- predict(m1b, newdata, re.form=NA)


bees.matrix <- t(matrix(c(extrap.95, extrap.90, extrap.85, extrap.80,
                          extrap.75, extrap.70, extrap.65, extrap.60,
                          extrap.55, extrap.50, extrap.45, extrap.40,
                          extrap.35, extrap.30, extrap.25, extrap.20,
                          extrap.15, extrap.10, extrap.5, extrap.0), nrow = 15))

bees.matrix <- 10^bees.matrix
bees.matrix[,1] <- (q1.snh/100)*(750^2*pi)*bees.matrix[,1]/150
bees.matrix[,2] <- (q2.snh/100)*(750^2*pi)*bees.matrix[,2]/150
bees.matrix[,3] <- (q3.snh/100)*(750^2*pi)*bees.matrix[,3]/150
bees.matrix[,4] <- (q4.snh/100)*(750^2*pi)*bees.matrix[,4]/150
bees.matrix[,5] <- (q5.snh/100)*(750^2*pi)*bees.matrix[,5]/150
bees.matrix[,6] <- (q6.snh/100)*(750^2*pi)*bees.matrix[,6]/150
bees.matrix[,7] <- (q7.snh/100)*(750^2*pi)*bees.matrix[,7]/150
bees.matrix[,8] <- (q8.snh/100)*(750^2*pi)*bees.matrix[,8]/150
bees.matrix[,9] <- (q9.snh/100)*(750^2*pi)*bees.matrix[,9]/150
bees.matrix[,10] <- (q10.snh/100)*(750^2*pi)*bees.matrix[,10]/150
bees.matrix[,11] <- (q11.snh/100)*(750^2*pi)*bees.matrix[,11]/150
bees.matrix[,12] <- (q12.snh/100)*(750^2*pi)*bees.matrix[,12]/150
bees.matrix[,13] <- (q13.snh/100)*(750^2*pi)*bees.matrix[,13]/150
bees.matrix[,14] <- (q14.snh/100)*(750^2*pi)*bees.matrix[,14]/150
bees.matrix[,15] <- (q15.snh/100)*(750^2*pi)*bees.matrix[,15]/150

bees.matrix <- round(bees.matrix)
x.axis.names <- c('0.35%', '5%', '10%', '15%', '20%', '25%', '30%', '35%', '40%', '45%',
                  '50%', '55%', '60%', '65%', '70%')
y.axis.names <- c('Q20', 'Q19', 'Q18', 'Q17', 'Q16', 'Q15', 'Q14', 'Q13', 'Q12', 'Q11',
                  'Q10', 'Q9', 'Q8', 'Q7', 'Q6', 'Q5', 'Q4', 'Q3', 'Q2', 'Q1')
colnames(bees.matrix) <- x.axis.names
rownames(bees.matrix) <- y.axis.names

bees.longData<-melt(bees.matrix)
bees.longData$Var1 <- factor(bees.longData$Var1, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9', 'Q10',
                                                            'Q11', 'Q12', 'Q13', 'Q14', 'Q15', 'Q16', 'Q17', 'Q18', 'Q19', 'Q20'))
##### Calculating arrow-angle across the grid for figure 3
bees.longData$increase.up <- 0
bees.longData$increase.right <- 0
bees.longData$angle.matrix <- 0

for(i in 1:19) {
  for(j in 1:14) { 
    #increase up
    bees.longData$increase.up[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                      bees.longData$Var2 == levels(bees.longData$Var2)[j])] <- bees.longData$value[which(bees.longData$Var1 == levels(bees.longData$Var1)[i+1] &
                                                                                                                           bees.longData$Var2 == levels(bees.longData$Var2)[j])] -
      bees.longData$value[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                  bees.longData$Var2 == levels(bees.longData$Var2)[j])]
    #increase right
    bees.longData$increase.right[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                         bees.longData$Var2 == levels(bees.longData$Var2)[j])] <- bees.longData$value[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                                                                                                              bees.longData$Var2 == levels(bees.longData$Var2)[j+1])] -
      bees.longData$value[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                  bees.longData$Var2 == levels(bees.longData$Var2)[j])]
    
    
    #angle.matrix
    bees.longData$angle.matrix[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                       bees.longData$Var2 == levels(bees.longData$Var2)[j])] <-  bees.longData$increase.up[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                                                                                                                   bees.longData$Var2 == levels(bees.longData$Var2)[j])]/sum(bees.longData$increase.up[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                                                                                                                                                                                                               bees.longData$Var2 == levels(bees.longData$Var2)[j])], 
                                                                                                                                                                                             bees.longData$increase.right[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                                                                                                                                                                                                                  bees.longData$Var2 == levels(bees.longData$Var2)[j])])*0.5 +
      bees.longData$increase.right[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                           bees.longData$Var2 == levels(bees.longData$Var2)[j])]/sum(bees.longData$increase.up[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                                                                                                                       bees.longData$Var2 == levels(bees.longData$Var2)[j])], 
                                                                                                     bees.longData$increase.right[which(bees.longData$Var1 == levels(bees.longData$Var1)[i] &
                                                                                                                                          bees.longData$Var2 == levels(bees.longData$Var2)[j])])*0.0                                                                                                      
    print(i)
    print(j)
  }
}
bees.longData$angle.matrix <- bees.longData$angle.matrix*pi
bees.longData$angle.matrix[bees.longData$Var2 == '70%'] <- 0.5*pi
bees.longData$angle.matrix[bees.longData$Var2 == '70%' & bees.longData$Var1 == "Q20" ] <- NA
bees.longData$angle.matrix[bees.longData$Var1 == "Q20"] <- NA
bees.longData$angle.matrix[bees.longData$Var2 == "70%"] <- NA
bees.longData$speed <- 0.5
bees.longData$snh <- as.numeric(bees.longData$Var2)*5-5

#calculating threshold for different step-ratios
bees.longData$qual.quan.ratio.1 <- bees.longData$increase.up/bees.longData$increase.right
bees.longData$qual.quan.ratio.2 <- NA
bees.longData$qual.quan.ratio.3 <- NA

for(i in 1:length(levels(unique(bees.longData$Var2)))) {
  t <- bees.longData[bees.longData$Var2 == unique(bees.longData$Var2)[i],]
  
  for(j in 20:3) {
    t$qual.quan.ratio.2[j] <- (t$increase.up[j] + t$increase.up[j-1])/t$increase.right[j]
  }
  for(k in 20:4) {
    t$qual.quan.ratio.3[k] <- (t$increase.up[k] + t$increase.up[k-1] + + t$increase.up[k-2])/t$increase.right[k]
  }
  bees.longData$qual.quan.ratio.2[bees.longData$Var2 == unique(bees.longData$Var2)[i]] <- t$qual.quan.ratio.2
  bees.longData$qual.quan.ratio.3[bees.longData$Var2 == unique(bees.longData$Var2)[i]] <- t$qual.quan.ratio.3
  
}

#Same for hoverflies
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q1.log.av.flow.cov,
                      av.flow.richness = q1.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.0 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q2.log.av.flow.cov,
                      av.flow.richness = q2.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.5 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q3.log.av.flow.cov,
                      av.flow.richness = q3.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.10 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q4.log.av.flow.cov,
                      av.flow.richness = q4.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.15 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q5.log.av.flow.cov,
                      av.flow.richness = q5.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.20 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q6.log.av.flow.cov,
                      av.flow.richness = q6.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.25 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q7.log.av.flow.cov,
                      av.flow.richness = q7.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.30 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q8.log.av.flow.cov,
                      av.flow.richness = q8.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.35 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q9.log.av.flow.cov,
                      av.flow.richness = q9.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.40 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q10.log.av.flow.cov,
                      av.flow.richness = q10.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.45 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q11.log.av.flow.cov,
                      av.flow.richness = q11.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.50 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q12.log.av.flow.cov,
                      av.flow.richness = q12.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.55 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q13.log.av.flow.cov,
                      av.flow.richness = q13.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.60 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q14.log.av.flow.cov,
                      av.flow.richness = q14.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.65 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q15.log.av.flow.cov,
                      av.flow.richness = q15.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.70 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q16.log.av.flow.cov,
                      av.flow.richness = q16.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.75 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q17.log.av.flow.cov,
                      av.flow.richness = q17.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.80 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q18.log.av.flow.cov,
                      av.flow.richness = q18.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.85 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q19.log.av.flow.cov,
                      av.flow.richness = q19.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.90 <- predict(m2b, newdata, re.form=NA)
newdata <- data.frame(snh=c(q1.snh, q2.snh, q3.snh, q4.snh, q5.snh,
                            q6.snh, q7.snh, q8.snh, q9.snh, q10.snh,
                            q11.snh, q12.snh, q13.snh, q14.snh, q15.snh),
                      log.av.flow.cov = q20.log.av.flow.cov,
                      av.flow.richness = q20.av.flow.richness,
                      mfc.flower = 'no', round = NA)
extrap.95 <- predict(m2b, newdata, re.form=NA)


syrphids.matrix <- t(matrix(c(extrap.95, extrap.90, extrap.85, extrap.80,
                              extrap.75, extrap.70, extrap.65, extrap.60,
                              extrap.55, extrap.50, extrap.45, extrap.40,
                              extrap.35, extrap.30, extrap.25, extrap.20,
                              extrap.15, extrap.10, extrap.5, extrap.0), nrow = 15))

syrphids.matrix <- 10^syrphids.matrix-1
syrphids.matrix[,1] <- (q1.snh/100)*(750^2*pi)*syrphids.matrix[,1]/150
syrphids.matrix[,2] <- (q2.snh/100)*(750^2*pi)*syrphids.matrix[,2]/150
syrphids.matrix[,3] <- (q3.snh/100)*(750^2*pi)*syrphids.matrix[,3]/150
syrphids.matrix[,4] <- (q4.snh/100)*(750^2*pi)*syrphids.matrix[,4]/150
syrphids.matrix[,5] <- (q5.snh/100)*(750^2*pi)*syrphids.matrix[,5]/150
syrphids.matrix[,6] <- (q6.snh/100)*(750^2*pi)*syrphids.matrix[,6]/150
syrphids.matrix[,7] <- (q7.snh/100)*(750^2*pi)*syrphids.matrix[,7]/150
syrphids.matrix[,8] <- (q8.snh/100)*(750^2*pi)*syrphids.matrix[,8]/150
syrphids.matrix[,9] <- (q9.snh/100)*(750^2*pi)*syrphids.matrix[,9]/150
syrphids.matrix[,10] <- (q10.snh/100)*(750^2*pi)*syrphids.matrix[,10]/150
syrphids.matrix[,11] <- (q11.snh/100)*(750^2*pi)*syrphids.matrix[,11]/150
syrphids.matrix[,12] <- (q12.snh/100)*(750^2*pi)*syrphids.matrix[,12]/150
syrphids.matrix[,13] <- (q13.snh/100)*(750^2*pi)*syrphids.matrix[,13]/150
syrphids.matrix[,14] <- (q14.snh/100)*(750^2*pi)*syrphids.matrix[,14]/150
syrphids.matrix[,15] <- (q15.snh/100)*(750^2*pi)*syrphids.matrix[,15]/150

syrphids.matrix <- round(syrphids.matrix)
x.axis.names <- c('0.35%', '5%', '10%', '15%', '20%', '25%', '30%', '35%', '40%', '45%',
                  '50%', '55%', '60%', '65%', '70%')
y.axis.names <- c('Q20', 'Q19', 'Q18', 'Q17', 'Q16', 'Q15', 'Q14', 'Q13', 'Q12', 'Q11',
                  'Q10', 'Q9', 'Q8', 'Q7', 'Q6', 'Q5', 'Q4', 'Q3', 'Q2', 'Q1')
colnames(syrphids.matrix) <- x.axis.names
rownames(syrphids.matrix) <- y.axis.names

hoverflies.longData<-melt(syrphids.matrix)
hoverflies.longData$Var1 <- factor(hoverflies.longData$Var1, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9', 'Q10',
                                                                        'Q11', 'Q12', 'Q13', 'Q14', 'Q15', 'Q16', 'Q17', 'Q18', 'Q19', 'Q20'))
mean(hoverflies.longData$value)
ggplot(hoverflies.longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=(value)), interpolate = T) + 
  geom_text(label = prettyNum(hoverflies.longData$value, big.mark = ','), size = 3.5) +
  scale_fill_gradient2(name = 'Individuals', 
                       limits = c(1,230000),
                       low="#b2182b", mid = '#f7f7f7', high="#2166ac", 
                       midpoint = 30553) +
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality (flower availability)",
       title = 'Hoverflies')+
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) 


hoverflies.longData$increase.up <- 0
hoverflies.longData$increase.right <- 0
hoverflies.longData$angle.matrix <- 0

for(i in 1:19) {
  for(j in 1:14) { 
    #increase up
    hoverflies.longData$increase.up[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                            hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])] <- hoverflies.longData$value[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i+1] &
                                                                                                                                                   hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])] -
      hoverflies.longData$value[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                        hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])]
    #increase right
    hoverflies.longData$increase.right[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                               hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])] <- hoverflies.longData$value[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                                                                                                                      hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j+1])] -
      hoverflies.longData$value[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                        hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])]
    
    
    #angle.matrix
    hoverflies.longData$angle.matrix[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                             hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])] <-  hoverflies.longData$increase.up[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                                                                                                                           hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])]/sum(hoverflies.longData$increase.up[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                                                                                                                                                                                                                                         hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])], 
                                                                                                                                                                                                                                 hoverflies.longData$increase.right[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                                                                                                                                                                                                                                            hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])])*0.5 +
      hoverflies.longData$increase.right[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                 hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])]/sum(hoverflies.longData$increase.up[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                                                                                                                               hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])], 
                                                                                                                       hoverflies.longData$increase.right[which(hoverflies.longData$Var1 == levels(hoverflies.longData$Var1)[i] &
                                                                                                                                                                  hoverflies.longData$Var2 == levels(hoverflies.longData$Var2)[j])])*0.0                                                                                                      
    print(i)
    print(j)
  }
}
hoverflies.longData$angle.matrix <- hoverflies.longData$angle.matrix*pi
hoverflies.longData$angle.matrix[hoverflies.longData$Var2 == '70%'] <- 0.5*pi
hoverflies.longData$angle.matrix[hoverflies.longData$Var2 == '70%' & hoverflies.longData$Var1 == "Q20" ] <- NA

hoverflies.longData$speed <- 0.5

hoverflies.longData$qual.quan.ratio.1 <- hoverflies.longData$increase.up/hoverflies.longData$increase.right
hoverflies.longData$qual.quan.ratio.2 <- NA
hoverflies.longData$qual.quan.ratio.3 <- NA


for(i in 1:length(levels(unique(hoverflies.longData$Var2)))) {
  t <- hoverflies.longData[hoverflies.longData$Var2 == unique(hoverflies.longData$Var2)[i],]
  
  for(j in 20:3) {
    t$qual.quan.ratio.2[j] <- (t$increase.up[j] + t$increase.up[j-1])/t$increase.right[j]
  }
  for(k in 20:4) {
    t$qual.quan.ratio.3[k] <- (t$increase.up[k] + t$increase.up[k-1] + + t$increase.up[k-2])/t$increase.right[k]
  }
  hoverflies.longData$qual.quan.ratio.2[hoverflies.longData$Var2 == unique(hoverflies.longData$Var2)[i]] <- t$qual.quan.ratio.2
  hoverflies.longData$qual.quan.ratio.3[hoverflies.longData$Var2 == unique(hoverflies.longData$Var2)[i]] <- t$qual.quan.ratio.3
  
}
hoverflies.longData$snh <- as.numeric(hoverflies.longData$Var2)*5-5
hoverflies.longData$angle.matrix[hoverflies.longData$Var1 == "Q20"] <- NA
hoverflies.longData$angle.matrix[hoverflies.longData$Var2 == "70%"] <- NA


#Figure 3
pwildbees <- ggplot(bees.longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=(value)), interpolate = TRUE) + 
  scale_fill_gradient2(name = 'Individuals', 
                       limits = c(1,230000),
                       low="#b2182b", mid = '#f7f7f7', high="#2166ac", 
                       midpoint = 30553) +
  geom_spoke(aes(angle = angle.matrix, radius = speed),  lineend = 'round',
             linejoin = 'bevel', size = 1.5, colour = 'coral4', alpha = 0.7,
             arrow = arrow(length = unit(.1, 'inches'), type = 'open', angle = 45))+
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality (flower availability)",
       title = 'Wild bees')+
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=12), legend.title=element_text(size=14),
        legend.key.width = unit(2,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) 
pwildbees


psyrphids <- ggplot(hoverflies.longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=(value)), interpolate = TRUE) + 
  scale_fill_gradient2(name = 'Individuals', 
                       limits = c(1,350000),
                       low="#b2182b", mid = '#f7f7f7', high="#2166ac", 
                       midpoint = 21301) +
  geom_spoke(aes(angle = angle.matrix, radius = speed),  lineend = 'round',
             linejoin = 'bevel', size = 1.5, colour = 'coral4', alpha = 0.7,
             arrow = arrow(length = unit(.1, 'inches'), type = 'open', angle = 45))+
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality (flower availability)",
       title = 'Hoverflies')+
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=12), legend.title=element_text(size=14),
        legend.key.width = unit(2,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) 
psyrphids

pwildbees.no.legend <- pwildbees + labs(tag = '(a)') 
psyrphids2 <- psyrphids + labs(tag = '(b)')
grid_arrange_shared_legend(pwildbees.no.legend, psyrphids2,  nrow = 1)

# Figure 4
ratio.bees1 <- ggplot(bees.longData, aes(x = snh, y = qual.quan.ratio.1, colour = Var1)) +
  geom_line(size = 2) + geom_hline(yintercept=1, linetype = 'dashed')+
  annotate('text', x = 75, y = 1, label = expression(" <-- quality - quantity -->"),
           angle = 270, size = 6) +
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality:Habitat quantity ratio",
       title = 'Wild bees',
       tag = '(a)') +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        legend.position = 'none',
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_color_manual(name = 'Flower availability', values=c('#f0f921', '#f7e225', '#fccd25', '#feb72d', '#fca338',
                                                            '#f79044', '#f07f4f', '#e76e5b', '#dd5e66', '#d14e72',
                                                            '#c5407e', '#b6308b', '#a72197', '#9511a1', '#8305a7',
                                                            '#6e00a8', '#5901a5', '#43039e', '#2c0594', '#0d0887'))

ratio.bees1

ratios <- subset(bees.longData, Var1=="Q1")
ratios2 <- ratios
ratios3 <- ratios
ratios$ratio <- c("1One")
ratios2$ratio <- c("2Two")
ratios2$qual.quan.ratio.1 <- ratios2$qual.quan.ratio.2
ratios3$ratio <- c("3Three")
ratios3$qual.quan.ratio.1 <- ratios3$qual.quan.ratio.3
ratiosall <- rbind(ratios,ratios2,ratios3)
ratiosall$ratio <- as.factor(ratiosall$ratio)

ratio.bees.1.3 <- ggplot(ratiosall) +
  geom_line(aes(x = snh, y = qual.quan.ratio.1, linetype = ratio), size = 2) + 
  scale_linetype_manual(values=c("solid","dashed","dotted"),labels=c("1:1","2:1","3:1"),
                        name="Step ratio")+
  geom_hline(yintercept=1, linetype = 'dashed')+
  annotate('text', x = 75, y = 1, label = " <-- quality - quantity -->",
           angle = 270, size = 6) +
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality:Habitat quantity ratio",
       title = 'Wild bees - improvement assumptions',
       tag = '(c)') +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(2,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8),
        legend.position=c(0.25,0.8)) + 
  scale_y_continuous(limits = c(-1,5)) 
ratio.bees.1.3

ratio.syrphids <- ggplot(hoverflies.longData, aes(x = snh, y = qual.quan.ratio.1, colour = Var1)) +
  geom_line(size = 1) + geom_hline(yintercept=1, linetype = 'dashed')+
  annotate('text', x = 75, y = 1, label = expression(" <-- quality - quantity -->"),
           angle = 270, size = 6) +
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality:Habitat quantity ratio",
       title = 'Hoverflies',
       tag = '(b)') +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=10), legend.title=element_text(size=12),
        legend.key.width = unit(1,"cm"),
        legend.key.size = unit(0.8, 'lines'),
        legend.spacing.x = unit(0.5, 'cm'),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_color_manual(name = 'Flower availability', values=c('#f0f921', '#f7e225', '#fccd25', '#feb72d', '#fca338',
                                                            '#f79044', '#f07f4f', '#e76e5b', '#dd5e66', '#d14e72',
                                                            '#c5407e', '#b6308b', '#a72197', '#9511a1', '#8305a7',
                                                            '#6e00a8', '#5901a5', '#43039e', '#2c0594', '#0d0887'))
ratio.syrphids

grid.arrange(ratio.bees1,ratio.syrphids,ratio.bees.1.3,widths=c(0.3,0.4,0.3))

#Figure S1
visreg.m2bb.inter <- visreg(m2, xvar = 'snh', by = 'mfc.flower', trans = function(x) 10^x,
                            overlay = T, cond = list(round = '4', landscape = 'Leek21')) #set to random effects estimation closest to 0visreg.m2bb.inter$fit
p2a.inter <- ggplot(visreg.m2bb.inter$fit, aes(x = snh, y = visregFit, colour = mfc.flower)) +
  geom_line(size = 2) +
  geom_point(data=visreg.m3bb.inter$res, aes(y = visregRes)) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha = 0.4) +
  annotate("text", label = 'p=0.06', x = 65, y = 22, size = 6) +
  scale_y_continuous() +
  scale_color_manual(name = 'MFC flowering', values = c("#1f78b4", '#33a02c')) +
  theme_bw() + labs(x = 'SNH in landscape (%)', 
                    y = "Hoverfly density in SNH (#/150m2)",
                    tag = "") +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=14) )
p2a.inter

#Figure S2
ratio.syrphids.1.3 <- ggplot(hoverflies.longData) +
  geom_line(aes(x = snh, y = qual.quan.ratio.1, colour = Var1), size = 2, linetype = 'solid') + 
  geom_line(aes(x = snh, y = qual.quan.ratio.2, colour = Var1), size = 2, linetype = 'dashed') + 
  geom_line(aes(x = snh, y = qual.quan.ratio.3, colour = Var1), size = 2, linetype = 'dotted') + 
  geom_hline(yintercept=1, linetype = 'dashed')+
  annotate('text', x = 75, y = 1, label = expression(" <-- quality - quantity -->"),
           angle = 270, size = 6) +
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality:Habitat quantity ratio",
       title = 'Hoverflies - improvement assumptions') +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_y_continuous(limits = c(-2,10)) +
  scale_color_manual(name = 'Flower availability', values=c('#f0f921', '#f7e225', '#fccd25', '#feb72d', '#fca338',
                                                            '#f79044', '#f07f4f', '#e76e5b', '#dd5e66', '#d14e72',
                                                            '#c5407e', '#b6308b', '#a72197', '#9511a1', '#8305a7',
                                                            '#6e00a8', '#5901a5', '#43039e', '#2c0594', '#0d0887'))
ratio.syrphids.1.3

#Figure S3
bee.individuals <- ggplot(bees.longData, aes(x = snh, y = value, colour = Var1)) +
  geom_line(size = 1) +
  labs(x="Habitat quantity (%SNH)", 
       y="Individuals",
       title = 'Wild bees',
       tag = '(a)') +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_color_manual(name = 'Flower availability', values=c('#f0f921', '#f7e225', '#fccd25', '#feb72d', '#fca338',
                                                            '#f79044', '#f07f4f', '#e76e5b', '#dd5e66', '#d14e72',
                                                            '#c5407e', '#b6308b', '#a72197', '#9511a1', '#8305a7',
                                                            '#6e00a8', '#5901a5', '#43039e', '#2c0594', '#0d0887'))
bee.individuals

syrphid.individuals <- ggplot(hoverflies.longData, aes(x = snh, y = value, colour = Var1)) +
  geom_line(size = 1) +
  labs(x="Habitat quantity (%SNH)", 
       y="Individuals",
       title = 'Hoverflies',
       tag = '(b)') +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) +
  scale_color_manual(name = 'Flower availability', values=c('#f0f921', '#f7e225', '#fccd25', '#feb72d', '#fca338',
                                                            '#f79044', '#f07f4f', '#e76e5b', '#dd5e66', '#d14e72',
                                                            '#c5407e', '#b6308b', '#a72197', '#9511a1', '#8305a7',
                                                            '#6e00a8', '#5901a5', '#43039e', '#2c0594', '#0d0887'))
syrphid.individuals
grid_arrange_shared_legend(bee.individuals, syrphid.individuals,  nrow = 1)

#Figure S4
ggplot(bees.longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=(value)), interpolate = TRUE) + 
  geom_text(label = prettyNum(bees.longData$value, big.mark = ','), 
            nudge_y = -0.2, nudge_x = -0.1, size = 3) +
  scale_fill_gradient2(name = 'Individuals', 
                       limits = c(1,230000),
                       low="#b2182b", mid = '#f7f7f7', high="#2166ac", 
                       midpoint = 30553) +
  labs(x="Habitat quantity (%SNH)", 
       y="Habitat quality (flower availability)",
       title = 'Wild bees')+
  theme_bw() +
  theme(axis.title = element_text(size=16),
        title = element_text(size = 12),
        axis.text = element_text(size=12, colour="#000000"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width = unit(1.5,"cm"),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size=8) ) 

