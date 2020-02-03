setwd("")

library(dplyr)
library(psych)
library(lme4)
library(ggplot2)
library(tidyr)

# Data from 2 sessions: 
spanish <- read.csv(file= "ReuseData_2019_05_10.csv", header = TRUE, sep=",")
spanish <- spanish %>%
  dplyr::select(-c(X))
summary(spanish)

# Data from Session 1: 
entrain <- read.csv(file= "EntrainData_2019_05_10.csv", header = TRUE, sep=",")
entrain <- entrain %>%
  dplyr::select(-c(X))
summary(entrain)

########################################
#              ANALYSES                #
########################################


## 1. LEXICAL ENTRAINMENT

conditions_entrain <- entrain %>% 
  dplyr::group_by(condition) %>% 
  dplyr::summarise(no_rows = length(condition) / 21)
#ingroup = 80 vs outgroup = 81

alignbyitem <- entrain %>%
  dplyr::group_by(target, condition, freq1) %>%
  mutate(FirstPartner = ifelse(as.character(condition) %in% c("SS", "SL"), "IngroupPartner", "OutgroupPartner")) %>%
  dplyr::group_by(target, FirstPartner, freq1) %>% 
  dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE))
alignbyitem

alignbyitem_spread <- alignbyitem %>% 
  tidyr::spread(FirstPartner, alignbyitem)
alignbyitem_spread$IngroupPartner <- alignbyitem_spread$IngroupPartner*100/80
alignbyitem_spread$OutgroupPartner <- alignbyitem_spread$OutgroupPartner*100/81

alignbyitem_long <- alignbyitem_spread %>%
  tidyr::gather(Response, Frequency, freq1:OutgroupPartner)
alignbyitem_long

alignbycond <- alignbyitem_long %>%
  dplyr::group_by(Response) %>%
  dplyr::summarise(Entrain.m = mean(Frequency, na.rm=TRUE),
                   Entrain.sd = sd(Frequency, na.rm=TRUE),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
alignbycond

alignbyitem2 <- entrain %>%
  dplyr::group_by(ID, condition) %>%
  mutate(FirstPartner = ifelse(as.character(condition) %in% c("SS", "SL"), "IngroupPartner", "OutgroupPartner"), 
         SecondPartner = ifelse(as.character(condition) %in% c("SS", "LL"), "Same", "Different")) %>%
  dplyr::group_by(ID, FirstPartner, SecondPartner) %>% 
  dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE)*100/n()) %>%
  dplyr::group_by(FirstPartner, SecondPartner) %>%
  dplyr::summarise(mean = mean(alignbyitem), 
                   sd = sd(alignbyitem), 
                   N = n(),
                   se = sd/sqrt(n()))
alignbyitem2[5,] =c("Pretest", "Pretest", 4.81, 6.06, 21, 1.32)
alignbyitem2$mean <- as.numeric(alignbyitem2$mean)
alignbyitem2$se <- as.numeric(alignbyitem2$se)
alignbyitem2$SecondPartner <-as.factor(alignbyitem2$SecondPartner)
alignbyitem2$FirstPartner <-as.factor(alignbyitem2$FirstPartner)
summary(alignbyitem2)

alignbyitem3 <- entrain %>%
  dplyr::group_by(ID, condition) %>%
  mutate(FirstPartner = ifelse(as.character(condition) %in% c("SS", "SL"), "IngroupPartner", "OutgroupPartner"), 
         SecondPartner = ifelse(as.character(condition) %in% c("SS", "LL"), "Same", "Different")) %>%
  dplyr::group_by(ID, SecondPartner) %>% 
  dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE)*100/n()) %>%
  dplyr::group_by(SecondPartner) %>%
  dplyr::summarise(mean = mean(alignbyitem), 
                   sd = sd(alignbyitem), 
                   N = n(),
                   se = sd/sqrt(n()))

print(levels(alignbyitem2$SecondPartner))
alignbyitem2$SecondPartner <- factor(alignbyitem2$SecondPartner, 
                                        levels(alignbyitem2$SecondPartner)[c(3,1,2)])      

plot1 <- ggplot(subset(alignbyitem2, FirstPartner != "Pretest"),
       aes(x = FirstPartner, y = as.numeric(as.character(mean)), fill=SecondPartner))+
  geom_bar(stat = "identity",position = "dodge", width = .8)+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),
                position=position_dodge(width=0.8),
                width=0.1)+
  geom_hline(aes(yintercept = subset(alignbyitem2, FirstPartner == "Pretest")$mean),linetype=8)+
  scale_linetype_manual(name = "", values = c(2))+
  theme_bw() +
  theme(
    legend.position=c(0.5, 0.95),
    legend.direction = "horizontal",
    axis.line= element_line(), 
    axis.title.y = element_text(color="black", size=16, face = "bold"), 
    axis.title.x = element_text(color="black", size=16, face = "bold"), 
    axis.text.x = element_text(color="black", size=16),
    axis.text.y = element_text(color="black", size=16),
    legend.title=element_text(color="black", size=16, face = "bold"), 
    legend.text=element_text(color="black", size=16),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(color="black", size=16)) +
  expand_limits(y=c(0,60)) + 
  scale_x_discrete( breaks = c("IngroupPartner", "OutgroupPartner"), 
                    labels=c("Same-Community", "Other-Community"))+
  labs(y="Use of Disfavoured Terms in Session 1 (%)",
       x="First Partner's Community")+
  scale_fill_manual(values=c("grey70", "grey30"), 
                    name  ="Second Partner's \nCommunity",
                    breaks=c("Same", "Different"),
                    labels=c("Same", "Different"))

plot1

# Lexical Entrainment Effect 
alignbyitem_total <- entrain %>%
dplyr::group_by(target, freq1) %>%
dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE) *100 / 161)

wilcox.test(as.numeric(alignbyitem_total$freq1), 
            as.numeric(alignbyitem_total$alignbyitem), 
            paired=TRUE, 
            exact = FALSE) #Alignment Effect 

# Effect of First Partner?

entrain$aligncode <- as.numeric(as.character(entrain$aligncode))

entrain_stats_ID = entrain %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(mean = mean(aligncode, na.rm=TRUE)) %>%
  dplyr::summarise(Entrain.m = mean(mean, na.rm=TRUE)*100,
                   Entrain.sd = sd(mean, na.rm=TRUE)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
entrain_stats_ID

entrain_items = entrain %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(mean = mean(aligncode)) %>%
  dplyr::summarise(Entrain.m = mean(mean, na.rm=TRUE)*100,
                   Entrain.sd = sd(mean, na.rm=TRUE)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
entrain_items

entrain_items_groups = entrain %>%
  dplyr::mutate(round1 = ifelse(partner %in% c("in"), "Same Community", "Different Community")) %>%
  dplyr::group_by(partner,target,round1) %>%
  dplyr::summarise(mean = mean(aligncode)) %>%
  dplyr::group_by(round1) %>%
  dplyr::summarise(Entrain.m = mean(mean, na.rm=TRUE)*100,
                   Entrain.sd = sd(mean, na.rm=TRUE)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
entrain_items_groups

entrain_part_groups = entrain %>%
  dplyr::mutate(round1 = ifelse(partner %in% c("in"), "Same Community", "Different Community")) %>%
  dplyr::group_by(partner,ID,round1) %>%
  dplyr::summarise(mean = mean(aligncode)) %>%
  dplyr::group_by(round1) %>%
  dplyr::summarise(Entrain.m = mean(mean, na.rm=TRUE)*100,
                   Entrain.sd = sd(mean, na.rm=TRUE)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
entrain_part_groups

### Mixed effect model to regress entrainment against partner's community

entrain$partner <- as.factor(entrain$partner)
entrain$ID <- as.factor(entrain$ID)
entrain$aligncode <- as.factor(entrain$aligncode)

entrain <- entrain %>%
  dplyr::select(ID, target, aligncode, partner, condition) %>%
  dplyr::mutate(CenPartner = ifelse(partner %in% c("in"), -1, 1), 
                SecondPartner = ifelse(condition %in% c("SS", "LL"), "Same", "Different"), 
                CenSecond = ifelse(SecondPartner %in% c("Same"), -1, 1))
summary(entrain)

condition_entrain_singularfit <- glmer(aligncode ~ CenPartner*CenSecond + (1 + CenPartner*CenSecond|target) + (1|ID), 
                                       data = entrain, family=binomial, 
                                       na.action = na.omit, 
                                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(condition_entrain_singularfit) #singularfit 

condition_entrain_singularfit2 <- glmer(aligncode ~ CenPartner*CenSecond + (1 + CenPartner*CenSecond|target) + (1|ID), 
                                       data = entrain, family=binomial, 
                                       na.action = na.omit, 
                                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

condition_entrain_singularfit3 <- glmer(aligncode ~ CenPartner*CenSecond + (1 + CenPartner+CenSecond|target) + (1|ID), 
                           data = entrain, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
condition_entrain_singularfit3

condition_entrain_singularfit4 <- glmer(aligncode ~ CenPartner*CenSecond + (1 + CenPartner|target) + (1|ID), 
                           data = entrain, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
condition_entrain_singularfit4

condition_entrain_int <- glmer(aligncode ~ CenPartner + CenSecond + CenPartner:CenSecond + (1 |target) + (1|ID), 
                           data = entrain, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
#same baseline in all conditions (no effect of second partner)
summary(condition_entrain_int)

condition_entrain2 <- glmer(aligncode ~ CenPartner + (1|target) + (1|ID), 
                           data = entrain, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(condition_entrain2)
BIC(condition_entrain2) #3571

condition_entrain_null <- glmer(aligncode ~  1 + (1|target) + (1|ID), 
                            data = entrain, family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(condition_entrain_null) 
BIC(condition_entrain_null) #3563

BF_BIC = exp((BIC(condition_entrain2) - BIC(condition_entrain_null))/2)  # BICs to Bayes factor
BF_BIC

BF_BIC / (BF_BIC + 1)


#BF expresses the probability of the data given H1 relative to H0 (i.e., values larger than 1 are in favour of H1)

#We found strong evidence for a lexical entrainment effect,
#but no evidence for an effect of partner's community on participants' tendency to entrain. 


# 2. MAINTENANCE OF REFERENTIAL PRECEDENTS

spanish$ID <-as.factor(spanish$ID)
spanish$target <-as.factor(spanish$target)
spanish$reuse <-as.factor(spanish$reuse)
spanish$switch <-as.factor(spanish$switch)
spanish$first <-as.factor(spanish$first)
spanish$second <-as.factor(spanish$second)
spanish$R1 <-as.factor(spanish$R1)

reuse_ID = na.omit(spanish) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(entrained1 = sum(R1=="1" & R2=="1")*100/21) %>%
  dplyr::summarise(entrained1.m = mean(entrained1),
                   entrained1.sd = sd(entrained1))
reuse_ID

reuse_target = na.omit(spanish) %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(entrained1 = sum(R1=="1" & R2=="1")*100/161) %>%
  dplyr::summarise(entrained1.m = mean(entrained1),
                   entrained1.sd = sd(entrained1))
reuse_target

reuse_target = na.omit(spanish) %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(entrained1 = sum(R1=="1" & R2=="1")*100/161) 

entrainedbyitem <- merge(alignbyitem_total, reuse_target, by.x="target", by.y="target")

# maintenance effect
wilcox.test(as.numeric(entrainedbyitem$freq1), # vs norming task
            as.numeric(entrainedbyitem$entrained1), 
            paired=TRUE, 
            exact = FALSE) 
wilcox.test(as.numeric(entrainedbyitem$alignbyitem), # vs session 1
            as.numeric(entrainedbyitem$entrained1), 
            paired=TRUE, 
            exact = FALSE) 

entrainedbyitem_table <- merge(alignbyitem_spread, reuse_target, by.x = "target", by.y="target")
entrainedbyitemtable_long <- entrainedbyitem_table %>%
  tidyr::gather(Response, Frequency, freq1:entrained1)
entrainedbyitemtable <- entrainedbyitemtable_long

entrainedbyitemtable <- entrainedbyitemtable_long %>%
  dplyr::group_by(Response) %>%
  dplyr::summarise(Entrain.m = mean(Frequency, na.rm=TRUE),
                   Entrain.sd = sd(Frequency, na.rm=TRUE),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
entrainedbyitemtable$Response <- as.factor(entrainedbyitemtable$Response)

print(levels(entrainedbyitemtable$Response))
entrainedbyitemtable$Response <- factor(entrainedbyitemtable$Response, 
                                        levels(entrainedbyitemtable$Response)[c(2,3,4,1)])                                        

reuse_tab = na.omit(spanish) %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(R1==1) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_tab

reuse_items = na.omit(spanish) %>%
  dplyr::group_by(target) %>%
  dplyr::filter(R1==1) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_items

reuse_first = na.omit(spanish) %>%
  dplyr::filter(R1==1) %>%
  dplyr::group_by(ID, first) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::group_by(first) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_first

reuse_switch = na.omit(spanish) %>%
  dplyr::filter(R1==1) %>%
  dplyr::group_by(ID, switch) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::group_by(switch) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_switch

reuse_FirstSwitch = na.omit(spanish) %>%
  dplyr::filter(R1==1) %>%
  dplyr::group_by(ID, switch, first) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::group_by(first, switch) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_FirstSwitch

print(levels(reuse_FirstSwitch$switch))
reuse_FirstSwitch$switch <- factor(reuse_FirstSwitch$switch, 
                                        levels(reuse_FirstSwitch$switch)[c(1,2)])   

reuse_entrained_graph <- ggplot(reuse_FirstSwitch, aes(x = first, y = Reuse.m, fill= switch)) +
  geom_bar(stat="identity", position=position_dodge(), width = .8) + 
  geom_errorbar(aes(ymin=Reuse.m-Reuse.se, ymax=Reuse.m+Reuse.se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.8)) +
  geom_line(aes(group = switch), size=.8, position=position_dodge(.8)) +
  labs(x = "First Partner's Community", 
       y = "Maintenance of Entrained Terms in Session 2 (%)") +
  theme_bw() +
  theme(legend.position=c(0.5, 0.95),
        legend.direction = "horizontal",
        axis.line= element_line(),
        axis.title.y = element_text(color="black", size=16, face = "bold"), 
        axis.title.x = element_text(color="black", size=16, face = "bold"), 
        axis.text.x = element_text(color="black", size=16), 
        axis.text.y = element_text(color="black", size=16), 
        legend.title=element_text(color="black", size=16, face = "bold"), 
        legend.text=element_text(color="black", size=16),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(color="black", size=16)) +
  ylim(0,100) +
  scale_x_discrete(labels = c("in" = "Same-Community","out" = "Other-Community")) +
  scale_fill_manual(values=c("grey70", "grey30"), 
                    name  ="Second Partner's \nCommunity",
                    breaks=c("no", "yes"),
                    labels=c("Same", "Different"))
reuse_entrained_graph


# Effects of First Partner and Second Partner

data <- spanish %>%
  dplyr::select(ID, target, reuse, switch, first, second, R1) %>%
  dplyr::mutate(CenSwitch = ifelse(switch %in% c("no"), -1, 1), 
                CenFirst = ifelse(first %in% c("in"), -1, 1), 
                CenSecond = ifelse(second %in% c("in"), -1, 1),
                CenR1 = ifelse(R1 %in% c("0"), -1, 1))
summary(data)


#full model does not converge (random structure: 1+CenFirst*CenSwitch|target + 1+CenR1|ID)

#this is the most complex model that worked:

Interaction_singlefit1 <- glmer(reuse ~ CenFirst + CenSwitch + CenFirst:CenSwitch + (1+CenFirst+CenSwitch+CenFirst:CenSwitch|target) + (1|ID), 
                                data = subset(data, R1==1), family=binomial, 
                                na.action = na.omit, 
                                glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(Interaction_singlefit1)

Interaction_singlefit2 <- glmer(reuse ~ CenFirst + CenSwitch + CenFirst:CenSwitch + (1+CenFirst+CenFirst:CenSwitch|target) + (1|ID), 
                                data = subset(data, R1==1), family=binomial, 
                                na.action = na.omit, 
                                glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(Interaction_singlefit2)


Interaction <- glmer(reuse ~ CenFirst + CenSwitch + CenFirst:CenSwitch + (1|target) + (1|ID), 
                     data = subset(data, R1==1), family=binomial, 
                     na.action = na.omit, 
                     glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(Interaction)


# There's no effect of Switch (second partner), 
# but there is an effect of second partner's community depending on first partner's community 

# subset analysis
SwitchInsiders_sing <- glmer(reuse ~ CenSwitch + (1+CenSwitch|target) + (1|ID), 
                     data = subset(data, R1==1 & first=="in"), family=binomial, 
                     na.action = na.omit, 
                     glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(SwitchInsiders_sing)

SwitchInsiders <- glmer(reuse ~ CenSwitch + (1|target) + (1|ID), 
                             data = subset(data, R1==1 & first=="in"), family=binomial, 
                             na.action = na.omit, 
                             glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(SwitchInsiders)

SwitchOutsiders_sing <- glmer(reuse ~ CenSwitch + (1+CenSwitch|target) + (1|ID), 
                         data = subset(data, R1==1 & first=="out"), family=binomial, 
                         na.action = na.omit, 
                         glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(SwitchOutsiders_sing)

SwitchOutsiders <- glmer(reuse ~ CenSwitch + (1|target) + (1|ID), 
                        data = subset(data, R1==1 & first=="out"), family=binomial, 
                        na.action = na.omit, 
                        glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(SwitchOutsiders)
