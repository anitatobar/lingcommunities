library(dplyr)
library(ggplot2)
library(tidyr)

getwd()
setwd("/home/anita/Dropbox/R/EnglishCoordination/Mexican/")


######################################################
#         Ling Status, Entrainment Analysis          #
######################################################

entrain <- read.csv(file="LingStatusEntrain.csv", header = TRUE, sep=",", stringsAsFactors = TRUE)
entrain <- entrain %>%
  dplyr::select(-c(X)) %>%
  dplyr::mutate(FirstPartner= ifelse(condition %in% c("AM"), "LowerStatus", "HigherStatus"), 
                SecondPartner= "Ingroup") %>%
  dplyr::rename(aligncode = align)

conditions_entrain <- entrain %>% 
  dplyr::group_by(FirstPartner) %>% 
  dplyr::summarise(no_rows = length(FirstPartner) / 21)
conditions_entrain #high = 38 low = 36

alignbyitem <- entrain %>%
  dplyr::group_by(target, FirstPartner, freq1) %>%
  dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE))

alignbyitem_spread <- alignbyitem %>% 
  tidyr::spread(FirstPartner, alignbyitem)

alignbyitem_spread$LowerStatus <- alignbyitem_spread$LowerStatus*100/36
alignbyitem_spread$HigherStatus <- alignbyitem_spread$HigherStatus*100/38

alignbyitem_long <- alignbyitem_spread %>%
  tidyr::gather(Response, Frequency, freq1:LowerStatus)
psych::describe(alignbyitem_spread$freq1)

alignbycond <- alignbyitem_long %>%
  dplyr::group_by(Response) %>%
  dplyr::summarise(Entrain.m = mean(Frequency, na.rm=TRUE),
                   Entrain.sd = sd(Frequency, na.rm=TRUE),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
alignbycond

alignbyitem2 <- entrain %>%
  dplyr::group_by(ID, FirstPartner) %>%
  dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE)*100/n()) %>%
  dplyr::group_by(FirstPartner) %>%
  dplyr::summarise(mean = mean(alignbyitem), 
                   sd = sd(alignbyitem), 
                   N = n(),
                   se = sd/sqrt(n()))
alignbyitem2[3,] =list("Pretest", 4, 6.06, 20, 1.32)
alignbyitem2$mean <- as.numeric(alignbyitem2$mean)
alignbyitem2$se <- as.numeric(alignbyitem2$se)
alignbyitem2$FirstPartner <-as.factor(alignbyitem2$FirstPartner)
summary(alignbyitem2)

print(levels(alignbyitem2$FirstPartner))

plot1 <- ggplot(subset(alignbyitem2, FirstPartner != "Pretest"),
                aes(x = FirstPartner, y = as.numeric(as.character(mean))))+
  geom_bar(stat = "identity",position = "dodge", width = .8)+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),
                position=position_dodge(width=0.8),
                width=0.1)+
  geom_hline(aes(yintercept = subset(alignbyitem2, FirstPartner == "Pretest")$mean),linetype=8)+
  scale_linetype_manual(name = "", values = c(2))+
  theme_bw() +
  theme(
    legend.position=c(0.6, 0.95),
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
  scale_x_discrete( breaks = c("HigherStatus", "LowerStatus"), 
                    labels=c("Higher Status", "Lower Status"))+
  labs(y="Use of Disfavoured Terms in Session 1 (%)",
       x="First Partner's Community")

plot1

alignbycond <- alignbyitem_long %>%
  dplyr::group_by(Response) %>%
  dplyr::summarise(Entrain.m = mean(Frequency, na.rm=TRUE),
                   Entrain.sd = sd(Frequency, na.rm=TRUE),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
alignbycond


### Is this difference significant?

alignbyitem_total <- entrain %>%
  dplyr::group_by(target, freq1) %>%
  dplyr::summarise(alignbyitem = sum(as.numeric(as.character(aligncode)), na.rm = TRUE) *100 / 161)

wilcox.test(as.numeric(alignbyitem_total$freq1), 
            as.numeric(alignbyitem_total$alignbyitem), 
            paired=TRUE, 
            exact = FALSE) #Alignment Effect 


### Partner's effect?

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
  dplyr::mutate(round1 = ifelse(FirstPartner %in% c("LowerStatus"), "Lower-Status Communtiy", "Higher-Status Community")) %>%
  dplyr::group_by(FirstPartner,target,round1) %>%
  dplyr::summarise(mean = mean(aligncode)) %>%
  dplyr::group_by(round1) %>%
  dplyr::summarise(Entrain.m = mean(mean, na.rm=TRUE)*100,
                   Entrain.sd = sd(mean, na.rm=TRUE)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
entrain_items_groups

entrain_part_groups = entrain %>%
  dplyr::mutate(round1 = ifelse(FirstPartner %in% c("LowerStatus"), "Lower-Status Community", "Higher-Status Community")) %>%
  dplyr::group_by(FirstPartner,ID,round1) %>%
  dplyr::summarise(mean = mean(aligncode)) %>%
  dplyr::group_by(round1) %>%
  dplyr::summarise(Entrain.m = mean(mean, na.rm=TRUE)*100,
                   Entrain.sd = sd(mean, na.rm=TRUE)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
entrain_part_groups

### Mixed effect model to regress entrainment against partner's community

entrain$partner <- as.factor(entrain$FirstPartner)
entrain$ID <- as.factor(entrain$ID)
entrain$aligncode <- as.factor(entrain$aligncode)

entrain <- entrain %>%
  dplyr::select(ID, target, aligncode, partner) %>%
  dplyr::mutate(CenPartner = ifelse(partner %in% c("LowerStatus"), -1, 1))

library(lmerTest)
condition_entrain_singularfit <- glmer(aligncode ~ CenPartner + (1+CenPartner|target) + (1|ID), 
                                       data = entrain, family=binomial, 
                                       na.action = na.omit, 
                                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(condition_entrain_singularfit) #singularfit 

condition_entrain <- glmer(aligncode ~ CenPartner + (1|target) + (1|ID), 
                           data = entrain, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(condition_entrain)
BIC(condition_entrain) #1609.85

condition_entrain_null <- glmer(aligncode ~ 1 + (1|target) + (1|ID), 
                           data = entrain, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(condition_entrain_null)
BIC(condition_entrain_null) #1602.642

BF_BIC = exp((BIC(condition_entrain) - BIC(condition_entrain_null))/2)  # BICs to Bayes factor
BF_BIC

BF_BIC / (BF_BIC + 1)


###################################################
#           Ling Status Reuse Analysis            #
###################################################

LingStatusReuse <- read.csv(file="LingStatusReuse.csv", header = TRUE, sep=",", stringsAsFactors = TRUE)
LingStatusReuse <- LingStatusReuse %>%
  dplyr::select(-c(X)) %>%
  dplyr::rename(R1 = align) %>%
  dplyr::mutate(first = ifelse(condition %in% c("AM"), "LowerStatus", "HigherStatus"))
summary(LingStatusReuse)


LingStatusReuse$ID <-as.factor(LingStatusReuse$ID)
LingStatusReuse$target <-as.factor(LingStatusReuse$target)
LingStatusReuse$reuse <-as.factor(LingStatusReuse$reuse)
LingStatusReuse$first <-as.factor(LingStatusReuse$first)
LingStatusReuse$R1 <-as.factor(LingStatusReuse$R1)


reuse_tab = na.omit(LingStatusReuse) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_tab

reuse_items = na.omit(LingStatusReuse) %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_items

reuse_entrain = na.omit(LingStatusReuse) %>%
  dplyr::group_by(ID, R1) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::group_by(R1) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n())) %>%
  dplyr::mutate(Entrain = factor(ifelse(R1 %in% c("0"), "Non-Entrainment Trials", "Entrainment Trials")))
reuse_entrain


reuse_ID = na.omit(LingStatusReuse) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(entrained1 = sum(R1=="1" & reuse=="1")*100/20) %>%
  dplyr::summarise(entrained1.m = mean(entrained1),
                   entrained1.sd = sd(entrained1))
reuse_ID

reuse_target = na.omit(LingStatusReuse) %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(entrained1 = sum(R1=="1" & reuse=="1")*100/80) 
entrainedbyitem <- merge(alignbyitem_total, reuse_target, by.x="target", by.y="target")

wilcox.test(as.numeric(entrainedbyitem$freq1), 
            as.numeric(entrainedbyitem$entrained1), 
            paired=TRUE, 
            exact = FALSE) 

wilcox.test(as.numeric(entrainedbyitem$alignbyitem), 
            as.numeric(entrainedbyitem$entrained1), 
            paired=TRUE, 
            exact = FALSE) 

entrainedbyitem_table <- merge(alignbyitem_spread, reuse_target, by.x = "target", by.y="target")
entrainedbyitemtable_long <- entrainedbyitem_table %>%
  tidyr::gather(Response, Frequency, freq1:entrained1)

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

reuse_tab = na.omit(LingStatusReuse) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_tab

reuse_items = na.omit(LingStatusReuse) %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n()))
reuse_items

reuse_entrain = na.omit(LingStatusReuse) %>%
  dplyr::group_by(ID, R1) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::group_by(R1) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n())) %>%
  dplyr::mutate(Entrain = factor(ifelse(R1 %in% c("0"), "Non-Entrainment Trials", "Entrainment Trials")))
reuse_entrain

reuse_2int = na.omit(LingStatusReuse) %>%
  dplyr::group_by(ID, R1, first) %>%
  dplyr::summarise(reuse = mean(as.numeric(as.character(reuse)), na.rm=TRUE)) %>%
  dplyr::group_by(R1,first) %>%
  dplyr::summarise(Reuse.m = mean(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   Reuse.sd = sd(as.numeric(as.character(reuse)), na.rm=TRUE)*100,
                   N = n(),
                   Reuse.se = Reuse.sd/sqrt(n())) %>%
  dplyr::filter(R1=="1")
reuse_2int

reuse_entrained_graph <- ggplot(reuse_2int, aes(x = first, y = Reuse.m)) +
  geom_bar(stat="identity", position=position_dodge(), width = .8) + 
  geom_errorbar(aes(ymin=Reuse.m-Reuse.se, ymax=Reuse.m+Reuse.se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.8)) +
  labs(x = "First Partner's Community", 
       y = "Maintenance of Entrained Terms in Session 2 (%)") +
  theme_bw() +
  theme(legend.position=c(0.6, 0.95),
        legend.direction = "horizontal",
        axis.line= element_line(), 
        axis.title.y = element_text(color="black", size=20, face = "bold"), 
        axis.title.x = element_text(color="black", size=20, face = "bold"), 
        axis.text.x = element_text(color="black", size=20), 
        axis.text.y = element_text(color="black", size=20), 
        legend.title=element_text(color="black", size=20, face = "bold"), 
        legend.text=element_text(color="black", size=20),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(color="black", size=20)) +
  ylim(0,90) +
  scale_x_discrete(labels = c("HigherStatus" = "Higher Status","LowerStatus" = "Lower Status"))
reuse_entrained_graph


######

data <- LingStatusReuse %>%
  dplyr::select(ID, target, reuse, first, R1) %>%
  dplyr::mutate(CenFirst = ifelse(first %in% c("HigherStatus"), -1, 1))
summary(data)


#full model does not converge (random structure: 1+CenFirst*CenSwitch|target + 1+CenR1|ID)

#this is the most complex model that converged:

First_singlefit1 <- glmer(reuse ~ CenFirst+(1+CenFirst|target) + (1|ID), 
                                data = subset(data, R1=="1"), family=binomial, 
                                na.action = na.omit, 
                                glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(First_singlefit1) # but, 1.0 correlation for slope

First <- glmer(reuse ~ CenFirst+(1|target) + (1|ID), 
                          data = subset(data, R1=="1"), family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(First)

First_null <- glmer(reuse ~ 1+(1|target) + (1|ID), 
               data = subset(data, R1=="1"), family=binomial, 
               na.action = na.omit, 
               glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(First_null)

#they were as likely to reuse a Higher-Status term vs a Lower-Status term
BIC(First)
BIC(First_null) #869.1238

BF_BIC2 = exp((BIC(First) - BIC(First_null))/2)  # BICs to Bayes factor
BF_BIC2

BF_BIC2 / (BF_BIC2 + 1)
