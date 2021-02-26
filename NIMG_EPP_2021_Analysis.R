#############################################################################################
# This script contains analysis code that reproduces all the figures and analyses
# reported in the following manuscript: 
 
# Parés-Pujolràs, E., Travers, E., Ahmetoglu, Y., & Haggard, P. (2021).
# Evidence accumulation under uncertainty -
# a neural marker of emerging choice and urgency.
# NeuroImage, 232(May 2021). https://doi.org/10.1101/2020.06.30.179622

# Script author: Elisabeth Parés-Pujolràs
# https://github.com/Elisabeth-Pares/NIMG_Pares-Pujolras_etal_2021
#############################################################################################

#Initialise required packages
{
  library(psych)
  library(tidyr)
  library(dplyr)
  library(lme4)
  library(multcomp)
  library(openxlsx)
  library(ggplot2)
  library(cowplot)
  library(devEMF)
}
#Set up plotting parameters
{# Labels

  cond.palette <- c('black', 'blue', 'cyan')
  cond.labs <- c("proAction", "NH", "NL")
  names(cond.labs) <- c("-1","3", "4")
  
  ev.palette <- c('red', 'green')
  ev.labs <- c('-Ev', '+Ev')
  names(ev.labs) <- c("0", "1")
  
  cond.dec.palette <- c("grey","darkgrey", "blue", "blue4", "skyblue", "dodgerblue")
  
  act.labs <- c('No-Action', 'Action')
  names(act.labs) <- c("0", "1")
  
  gcond.labs <- c("Informative", "NH", "NL")
  names(gcond.labs) <- c(1,3,4)
  
  order.labs <- c("First", "Last")
  names(order.labs) <- c('first', 'last')
}
#Load wrapper functions
{
gg.bar.mean_se_point <- function(df,x,y,color = NULL,fill = NULL,
                                 palette = gg.default_palette(),
                                 xname = '', xlabs = c(), 
                                 yname = '', ybreaks = c(), ylabs = c(), ycoord = c(),
                                 legend = TRUE){
  
  p = ggplot(df, aes_string(x = x , y = y, color = color, fill = fill)) +
    stat_summary(geom = 'bar', fun = "mean", position = position_dodge(), alpha=1, width = 0.75, show.legend = legend) +
    geom_point(position=position_dodge(0.75), color = 'grey50', size=0.75, show.legend = legend) +  
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size = 9) + 
    scale_x_discrete(labels = xlabs)+ 
    scale_fill_manual(values = palette) +
    labs(title = '', x = xname, y = yname)  
  
  return(p)
}

#Bar graph with mean_se 
gg.bar.mean_se <- function(df,x,y,color = NULL,fill = NULL,
                           palette = gg.default_palette(),
                           xname = '', xlabs = c(), 
                           yname = '', ybreaks = c(), ylabs = c(), ycoord = c(),
                           legend = TRUE){
  
  p = ggplot(df, aes_string(x = x , y = y, color = color, fill = fill)) +
    stat_summary(geom = 'bar', fun = "mean", position = position_dodge(), alpha=1, width = 0.75, show.legend = legend) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size = 9) + 
    scale_x_discrete(labels = xlabs)+ 
    scale_fill_manual(values = palette) +
    theme(strip.background = element_rect(colour="white", fill="white")) +
    labs(title = '', x = xname, y = yname)  
  
  return(p)
}

gg.default_palette <- function(n = 3) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# ERP mean_sem
gg.erp.mean_sem <- function(df,x,y,color = NULL,fill = NULL,
                            palette = gg.default_palette(),
                            xname = '', xbreaks = c(), xlabs = c(), xcoord = c(),
                            yname = '', ybreaks = c(), ylabs = c(), ycoord = c(),
                            legend = TRUE){
  
  p = ggplot(df, aes_string(x = x , y = y, color = color, fill = fill)) +
    stat_summary(geom = "line", fun = mean, size = 0.5, show.legend = legend) +
    stat_summary(geom = "ribbon", fun.data = mean_se , alpha = 0.3, colour = NA, show.legend = legend) +
    scale_color_manual(values = palette, name = "")+
    scale_fill_manual(values = palette, name = "")+
    labs(x = xname, y = yname) + 
    scale_x_continuous(breaks = xbreaks, labels = xlabs) + 
    scale_y_reverse(breaks = ybreaks, labels = ylabs) + 
    theme_classic(base_size=9) +
    #coord_cartesian(x = xcoord, y = ycoord) + 
    theme(strip.background = element_rect(colour="white", fill="white"),
          panel.background = element_rect(fill = "white")) +
    geom_vline(aes_(xintercept= 0), color = 'black', size = 0.25)
  
  return(p)
}
}
#Define data folder and create output directories
{
dir <-dirname(rstudioapi::getSourceEditorContext()$path) #Get current directory
dir.data <- paste(dir,'/DATA', sep = '')
dir.stats <- paste(dir,'/STATS', sep = '')
dir.create(dir.stats)
dir.figures <- paste(dir,'/FIGURES', sep = '')
dir.create(dir.figures)
}

#############################################################################################
# 1.BEHAVIOURAL ANALYSIS & Figure 2
#############################################################################################
# Load data 
behData <- read.csv(paste(dir.data, "/NIMG_EPP_2021_behData.csv", sep = ''), header = TRUE)

# Grand-average data
GA <- behData %>% 
  group_by(ID, conditionName) %>% 
  summarize(pct.action = mean(action == 1)*100, # percentage of action trials
            time.action = mean(actionTime, na.rm = TRUE),             # average time of action
            ptime.action = mean(as.numeric(timeVAS/4), na.rm = TRUE)) # average perceived time of action 

# Test differences in PERCENTAGE of action trials between neutral conditions
spread_pctga <- GA %>% 
  dplyr::select(ID, conditionName, pct.action) %>% 
  spread(conditionName, pct.action)
t.test(spread_pctga$NH, spread_pctga$NL, paired = TRUE)

# Test differences in TIME of action between neutral conditions
spread_timega <- GA %>%
  dplyr::select(ID, conditionName, time.action) %>%
  spread(conditionName, time.action)
t.test(spread_timega$NH, spread_timega$NL, paired = TRUE)

# Descriptive statistics by condition
describeBy(GA, GA$conditionName, digits = 5)

# Remove error trials from dataset 
behData_noerror <- filter(behData, accuracy != -1)

# Grand-average 
GA <- behData_noerror %>% 
  filter(conditionName != 'antiAction') %>%
  group_by(ID, conditionName) %>% 
  summarize(time.action = mean(actionTime, na.rm = TRUE),             # average time of action
            ptime.action = mean(as.numeric(timeVAS/4), na.rm = TRUE)) # average perceived time of action 

GA <- GA %>%
  group_by(ID)%>%
  summarize(gatime.action = mean(time.action, na.rm = TRUE),             # average time of action
            gaptime.action = mean(ptime.action, na.rm = TRUE))
# Test bias in PERCEIVED time of action  
GA$bias <- GA$gatime.action - GA$gaptime.action
mean(GA$bias, na.exclude = TRUE); sd(GA$bias, na.rm = TRUE);
t.test(GA$bias)

# Test differences in CONFIDENCE between conditions
#Pool informative conditions
behData_noerror$pooledCondition <- behData_noerror$condition
behData_noerror[behData_noerror$pooledCondition == 1,]$pooledCondition <- 2
behData_noerror[behData_noerror$pooledCondition == -1,]$pooledCondition <- 2

GA_Conf <- behData_noerror %>%
  group_by(ID,pooledCondition) %>%
  summarize(meanConf = mean(confVAS)
  )

spreadconf <- GA_Conf %>%
  spread(pooledCondition,meanConf)

colnames(spreadconf) <- (c('id', 'inf', 'nh', 'nl'))
t.test(spreadconf$inf, spreadconf$nh, paired = TRUE)
t.test(spreadconf$inf, spreadconf$nl, paired = TRUE)
t.test(spreadconf$nh, spreadconf$nl, paired = TRUE)

# Test PRIMACY effect 
peffA1 <- glmer(action ~ 1 + conditionName*firstLetPosEv + 
                  (1|ID),
                data = behData[(behData$conditionName == 'NH' | behData$conditionName == 'NL'),],
                family = binomial(link = "logit"))

car::Anova(peffA1, type = c(2))

# Test RECENCY effect 
GA_recency <- behData_noerror %>%
  filter(action == 1 & conditionName != 'antiAction')%>%
  group_by(ID,conditionName)%>%
  summarise(garecency = mean(recency))
  
summary(aov(garecency ~ conditionName + Error(ID), data=GA_recency))

rec_spread <- GA_recency %>%
  spread(conditionName, garecency)
t.test(rec_spread$NL, rec_spread$NH, paired = TRUE)
t.test(rec_spread$NL, rec_spread$proAction, paired = TRUE)
t.test(rec_spread$NH, rec_spread$proAction, paired = TRUE)
describeBy(GA_recency, GA_recency$conditionName)

# Create & save Figure 2 
{#Get summary measures for panels a,b,d
  GA_fig2abd <- behData %>%
    filter(condition != 1 & action == 1)%>%
    mutate(condition = as.factor(condition))%>%
    group_by(ID,condition) %>%
    summarise(
      totalev = mean(totalev, na.rm = TRUE),
      balev = mean(balev, na.rm = TRUE),
      mean_rt = mean(meanrt, na.rm = TRUE),
      recency = first(recency))
  
  #Get summary measures for panel c 
  GA_fig2c <- behData %>%
    mutate(condition = as.factor(condition), 
           firstLetPosEv = as.factor(firstLetPosEv))%>%
    filter(condition != -1 & condition != 1) %>%
    group_by(ID,condition,firstLetPosEv) %>%
    summarise(meansw = mean(action))
  
  fig2a <- gg.bar.mean_se_point(GA_fig2abd, x = "condition", y = "mean_rt",
                       fill = "condition", palette = cond.palette,
                       xname = '', xlabs = cond.labs,
                       yname = 'RT (s)', legend = FALSE)
  
  fig2b <- gg.bar.mean_se_point(GA_fig2abd, x = "condition", y = "totalev",
                       fill = "condition", palette = cond.palette, 
                       xname = '', xlabs = cond.labs,
                       yname = 'Evidence items count', legend = FALSE)
  
  fig2c <- gg.bar.mean_se_point(GA_fig2c, x = "condition", y = "meansw",
                       fill = "firstLetPosEv", palette = ev.palette,
                       xname = '', xlabs = cond.labs,
                       yname = 'P(Action)', legend = FALSE)
  fig2c <- fig2c + scale_y_continuous(breaks = c(0,0.25,0.5), labels = c(0,0.25,0.5)) 
  
  fig2d <- gg.bar.mean_se_point(GA_fig2abd, x = "condition", y = "recency",
                       fill = "condition", palette = cond.palette,
                       xname = '', xlabs = cond.labs,
                       yname = 'Deviation score (a.u.)', legend = FALSE)
  #Concatenate all panels
  fig2 <- plot_grid(fig2a, fig2b, fig2c, fig2d,
                    ncol = 4)
  
  #Save figure in vectorial format
  emf(paste(dir.figures, '/Figure_2.emf',sep =''),
      bg = "transparent", width = 6, height = 2, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  fig2
  dev.off()
}

#############################################################################################
# 2.EEG ANALYSIS
#############################################################################################
# Load long dataset 
dir <- c('F:/PhD - London/Y2 - VTS/Paper/GitHub_Public/DATA')
shortData <- read.csv(paste(dir, "/NIMG_EPP_2021_shortData.csv", sep = ''))
shortData$evidenceType <- as.factor(shortData$evidenceType)
shortData$action <- as.factor(shortData$action)

#############################################################################################
# EEG p3 analysis - sequential p3 analysis, action-locked single-event analysis  (TABLE S1, Figure 3A-D)
#############################################################################################
# Select only Action trials, events before movement
preActionData <- filter(shortData, (action == 1 & movedYet == 0))
# Run stats (Table S1)
{
# Initialize workbook to export results 
wb <- createWorkbook(dir.stats) # workbook to store the results
addWorksheet(wb, sheetName = 'p3 Dyn') # worksheet format
addWorksheet(wb, sheetName = 'p3 Dyn posEv') # worksheet format
addWorksheet(wb, sheetName = 'p3 Dyn negEv') # worksheet format

m1 <- lmer(p300Amp ~ 1 +  timeToAction*gconditionName*evidenceType + 
              (1|sub/trialN) ,
            data = preActionData)
p3dyn <- car::Anova(m1, type = c(2))
pvals <- p3dyn$`Pr(>Chisq)`
p3dyn$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = p3dyn, sheet = 'p3 Dyn', rowNames = TRUE)

pos <- lmer(p300Amp ~ 1 +  timeToAction*gconditionName+ 
               (1|sub/trialN) ,
             data = preActionData[preActionData$evidenceType == 1,])

p3posdyn <- car::Anova(pos, type = c(2))
pvals <- p3posdyn$`Pr(>Chisq)`
p3posdyn$adj_pvals <- p.adjust(pvals, 'BH')
rm(pvals)
writeDataTable(wb, x = p3posdyn, sheet = 'p3 Dyn posEv', rowNames = TRUE)

neg <- lmer(p300Amp ~ 1 + timeToAction*gconditionName + 
               (1|sub/trialN) ,
             data = preActionData[preActionData$evidenceType == 0,])

p3negdyn <- car::Anova(neg, type = c(2))
pvals <- p3negdyn$`Pr(>Chisq)`
p3negdyn$adj_pvals <- p.adjust(pvals, 'BH')
rm(pvals)
writeDataTable(wb, x = p3negdyn, sheet = 'p3 Dyn negEv', rowNames = TRUE)
saveWorkbook(wb, paste(dir.stats, "/NIMG_Stats_TableS1.xlsx", sep = ''), overwrite = TRUE) # Actually writes book
}

# Figure 3A-D 
{ 
  GARTs <- shortData[shortData$action == 1,] %>% 
    group_by(sub, trialN, gcondition) %>% 
    summarize (RT = first(actionTime))
  
  GARTs <- GARTs %>% 
    group_by(sub,  gcondition) %>% 
    summarize (gaRT = mean(RT))
  
  GART <- GARTs%>% 
    group_by(gcondition) %>% 
    summarize (RT = mean(gaRT))
  
  
  {
    astimLocked <- ggplot(shortData[shortData$movedYet == 0,], aes(x = OTime, y=p300Amp, color=factor(gcondition))) +
      geom_smooth(se=TRUE, method='loess', span = 0.75,size = 0.35, show.legend = FALSE) +
      theme_classic(base_size=9) + 
      theme(strip.background = element_rect(colour="white", fill="white")) +
      coord_cartesian(x = c(0,25), y = c(0,6))+ 
      geom_vline(data=filter(shortData, action== 1 & gcondition == 1), aes_(xintercept= GART[GART$gcondition == 1,]$RT), color = 'black', size = 0.35)+
      geom_vline(data=filter(shortData, action== 1 & gcondition== 3), aes_(xintercept= GART[GART$gcondition == 3,]$RT), color = 'blue', size = 0.35)+
      geom_vline(data=filter(shortData, action== 1 & gcondition== 4), aes_(xintercept= GART[GART$gcondition == 4,]$RT), color = 'skyblue', size = 0.35) + 
      scale_color_manual(values = c("black", "blue", "skyblue"), labels = "", name = "") +
      labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter")+
      facet_grid(action~., labeller = labeller(action = act.labs))
    
    bstimLocked <- ggplot(shortData[shortData$movedYet == 0,], aes(x = OTime, y=p300Amp, color = evidenceType)) + #, color=factor(NT))) +
      stat_smooth(se=TRUE, method='loess', span = 0.75, size = 0.35, show.legend = FALSE) +
      coord_cartesian(x = c(0,25), y = c(-1,8))+ 
      theme_classic(base_size=9)+
      scale_y_continuous(name = '') +
      theme(strip.background = element_rect(colour="white", fill="white")) +
      geom_vline(data=filter(shortData, action== 1 & gcondition == 1), aes_(xintercept= GART[GART$gcondition == 1,]$RT), color = 'black', size = 0.35)+
      geom_vline(data=filter(shortData, action== 1 & gcondition== 3), aes_(xintercept= GART[GART$gcondition == 3,]$RT), color = 'blue', size = 0.35)+
      geom_vline(data=filter(shortData, action== 1 & gcondition== 4), aes_(xintercept= GART[GART$gcondition == 4,]$RT), color = 'skyblue', size = 0.35) + 
      scale_color_manual(values = c("red", "green"), labels = "", name = "") +
      labs(title = "", x = "Time (s)", y = "", fill = "Letter") +
      facet_grid(
        action~gcondition, labeller = labeller(action = act.labs, gcondition = gcond.labs))
    
    fig3_ab <- plot_grid(astimLocked, bstimLocked, 
                         ncol = 2 , rel_widths = c(1.3,2),
                         align = 'vh', axis = 'tblr',
                         labels = c('a.', 'b.'), label_size = 16)
    
    emf(paste(dir.figures, '/Figure_3_ab.emf', sep = ''),
        bg = "transparent", width = 5, height = 2.35, coordDPI = 600,
        emfPlus=TRUE,family = "Helvetica",pointsize = 9)
    fig3_ab
    dev.off()
    
    ## Fig3.c
    calocked <- ggplot(shortData[shortData$movedYet == 0,], aes(x = timeToAction, y=p300Amp, color=factor(gcondition))) +
      geom_smooth(se=TRUE, method='loess', span = 0.75,size = 0.35, show.legend = FALSE) +
      theme_classic(base_size=9) + 
      theme(strip.background = element_rect(colour="white", fill="white"),
            panel.background = element_rect(fill = "white")) +
      coord_cartesian(x = c(-15,0), y = c(0,6))+ 
      geom_vline(xintercept = c(0)) + 
      scale_color_manual(values = c("black", "blue", "skyblue"), labels = "", name = "") +
      labs(title = "", x = "Time to Action (s)", y = "p3 amplitude (uV)")
    
    # Fig3.d
    dalocked <- ggplot(shortData[shortData$movedYet == 0,], aes(x = timeToAction, y=p300Amp, color = evidenceType)) + #, color=factor(NT))) +
      stat_smooth(se=TRUE, method='loess', size = 0.35, show.legend = FALSE) +
      scale_x_continuous(breaks = c(-15,-10,-5,0), limits = c(-15,0)) +
      scale_y_continuous(name = '') +
      theme_classic(base_size=9)+
      theme(strip.background = element_rect(colour="white", fill="white"),
            panel.background = element_rect(fill = "white")) +
      geom_vline(xintercept = c(0)) + 
      scale_color_manual(values = c("red", "green"), labels = "", name = "") +
      labs(title = "", x = "Time to Action (s)", y = "p3 amplitude (uV)", fill = "Letter") +
      facet_grid(
        ~gcondition, labeller = labeller(gcondition = gcond.labs))
    
    fig3_cd <- plot_grid(calocked, dalocked,
                         ncol = 2, rel_widths = c(1.3,2),
                         align = 'vh', axis = 'tblr')
    
    emf(paste(dir.figures, '/Figure_3_cd.emf', sep = ''),
        bg = "transparent", width = 5, height =1.68, coordDPI = 600,
        emfPlus=TRUE,family = "Helvetica",pointsize =9)
    fig3_cd
    dev.off()
    }
}
#############################################################################################
# EEG p3 analysis - average p3 amplitude, trial-by-trial analysis (TABLE S2, Figure 3e)
#############################################################################################
#Get trial-by-trial P3 amplitude averages, sorted by condition and evidecnce type
GA <- shortData %>%
  filter(movedYet == 0)%>% #In action trials, take events only up to the time of action
  group_by(sub,trialN,gconditionName,action,evidenceType) %>%
  summarize(meanp3 = mean(p300Amp))

# Run stats (Table S2)
{# Initialize workbook to export results 
wb <- createWorkbook(dir.stats) # workbook to store the results
addWorksheet(wb, sheetName = 'p3 TrialAv all') # worksheet format
addWorksheet(wb, sheetName = 'p3 TrialAv Action') # worksheet format
addWorksheet(wb, sheetName = 'p3 TrialAv No-Action') # worksheet format

m1 <- lmer(meanp3 ~ 1 + evidenceType*gconditionName*action + 
             (1|sub/trialN),
           data = GA)

erpall <- car::Anova(m1, type = c(2))
pvals <- erpall$`Pr(>Chisq)`
erpall$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = erpall, sheet = 'p3 TrialAv all', rowNames = TRUE)

# Interaction follow-up
act <- lmer(meanp3 ~ 1 + evidenceType*gconditionName + 
                 (1|sub/trialN) ,
               data = GA[GA$action == 1,]) # take only action trials
erpact <- car::Anova(act, type = c(2))
pvals <- erpact$`Pr(>Chisq)`
erpact$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = erpact, sheet = 'p3 TrialAv Action', rowNames = TRUE)

noact <- lmer(meanp3 ~ 1 + evidenceType*gconditionName + 
               (1|sub/trialN) ,
             data = GA[GA$action == 0,])

erpnoact <- car::Anova(noact, type = c(2))
pvals <- erpnoact$`Pr(>Chisq)`
erpnoact$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = erpnoact, sheet = 'p3 TrialAv No-Action', rowNames = TRUE)


saveWorkbook(wb, paste(dir.stats, "/NIMG_Stats_TableS2.xlsx", sep = ''), overwrite = TRUE) # Actually writes book
}

# Create & save Fig 3e 
  #Load & preprocess erp data 
  {
  erpData <- read.csv(paste(dir.data, "/NIMG_EPP_2021_longData.csv", sep = ''), header = TRUE)
  
  # Add labels to erp data 
  names(erpData)[10:(384+10)] = stringr::str_c('t', seq(-.5, 1., 1/256))
  #Remove rows with NaN - rejected due to artefacts
  erp <- erpData[!is.na(erpData$sub),] 
  
  #Create single trial index
  erp <- erp %>% 
    mutate(trialN = group_indices(., block, trial))
  # Grand average long data
  longerp = erp %>%
    gather(key='time', value='voltage', `t-0.5`:`t1`) %>%
    arrange(sub, trialN,timeFromOnset)
  #Remove string to use as numeric variable
  longerp$time = longerp$time %>% stringr::str_remove('t') %>% as.numeric()
  
  #Average within subjects, by condition and evidence type
  ssLong<- longerp %>% 
    group_by(sub, condition, evidenceType, action,time) %>% 
    summarise(meanv = mean(voltage))
  
  #Add column with grouped condition coding
  ssLong$gcondition <- ssLong$condition
  ssLong[ssLong$gcondition == 2,]$gcondition <- 1
  
  }
  
  #Make figure
{
  fig3_e <- gg.erp.mean_sem(ssLong,x = "time",y = "meanv",color = "evidenceType",fill = "evidenceType",
                            palette = ev.palette, legend = FALSE,
                            xname = 'Time (s)', xbreaks = c(-0.5,0,0.5,1), xlabs = c('-0.5','0','0.5','1'),
                            yname = 'Pz amplitude (uV)', ybreaks = c(-2.5,0,2.5,5,7.5), ylabs = c('-2.5','0','2.5','5','7.5'))
  fig3_e <- fig3_e + facet_grid(gcondition~action, 
                                labeller = labeller(gcondition = gcond.labs, action = act.labs))
  

  emf(paste(dir.figures, '/Figure_3e_erp.emf', sep = ''),
      bg = "transparent", width = 2.35, height = 2.5, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica", pointsize = 9)
  fig3_e
  dev.off()
  
  #Arrange data for single-trial bar graphs
  trialAvData <-  shortData %>%
    filter(movedYet != 1)%>%
    group_by(sub, trialN, gcondition, evidenceType, action) %>% 
    summarize (meanp3 = mean(p300Amp))
  
  spread_trialAvData <- trialAvData %>%
    spread(evidenceType, meanp3)
  
  colnames(spread_trialAvData)[5:6] <- c('negEv', 'posEv')
  spread_trialAvData$diff <- spread_trialAvData$posEv - spread_trialAvData$negEv
  
  #Diff
  fig3_eright <- gg.bar.mean_se(spread_trialAvData, x = 'action', y = 'diff',
                                fill = interaction('action', 'gcondition'),
                                palette = cond.dec.palette, 
                                yname = 'p3 amplitude (uV)', legend = FALSE)
  fig3_eright <- fig3_eright + facet_grid(gcondition~., 
                                          labeller = labeller(gcondition = gcond.labs))
  
  emf(paste(dir.figures, '/Figure_3e_bar.emf', sep = ''),
      width = 1.5, height = 2.3, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  fig3_eright
  dev.off()
}
#############################################################################################
# EEG p3 analysis - first vs. last p3 amplitude, trial-by-trial analysis  (Figure 3f)
#############################################################################################
## Get FIRST vs. LAST evidence item in each trial trial
FLp3Y <- preActionData %>% 
  group_by(sub, trialN, gconditionName, evidenceType) %>% 
  summarize (first = first(p300Amp),
             last = last(p300Amp),
  )

longGA<- data.frame()
longGA <- FLp3Y %>%
  gather(time, "p3", 5:6)

# Run stats 
{
flmod <- lmer(p3 ~ 1 + evidenceType*gconditionName*time 
                + (1|sub/trialN), 
                data = longGA)

fl <- car::Anova(flmod, type = c(2))
pvals <- fl$`Pr(>Chisq)`
fl$adj_pvals <- p.adjust(pvals, 'BH') 


first <- lmer(p3 ~ 1 + evidenceType*gconditionName
              + (1|sub/trialN), 
              data = longGA[longGA$time == 'first',],
)
first <- car::Anova(first, type = c(2))
pvals <- first$`Pr(>Chisq)`
first$adj_pvals <- p.adjust(pvals, 'BH') 


last <- lmer(p3 ~ 1 + evidenceType*gconditionName
             + (1|sub/trialN), 
             data = longGA[longGA$time == 'last',],
)
last <- car::Anova(last, type = c(2))
pvals <- last$`Pr(>Chisq)`
last$adj_pvals <- p.adjust(pvals, 'BH') 
}

# Create & save Figure 3f - DONE, maybe create csv dataset
{ ## Get first per trial per condition 
  # FIRST
  firstoftrial <- erp %>%
    group_by(sub,block,trial, evidenceType) %>%
    summarise_all(.funs = c("first"))
  
  firstoftrial$idx <- 'first'
  #LAST
  lastoftrial <- erp %>%
    group_by(sub,block,trial, evidenceType) %>%
    summarize_all(.funs = c("last"))
  
  lastoftrial$idx <- 'last'
  
  fl <- rbind(firstoftrial,lastoftrial) # combine both
  
  # Put in long format 
  long.data = fl %>%
    gather(key='time', value='voltage', `t-0.5`:`t1`) %>%
    arrange(sub, trialN,timeFromOnset)
  long.data$time = long.data$time %>% stringr::str_remove('t') %>% as.numeric()
  
  #Average within subjects
  ssLong<- long.data %>% 
    group_by(sub, condition, evidenceType, action,time,idx) %>% 
    summarise(meanv = mean(voltage))
  
  ssLong$gcondition <- ssLong$condition
  ssLong[ssLong$gcondition == 2,]$gcondition <- 1
  
  ssLong <- filter(ssLong, (!is.na(idx)))
  
  #Plot
  fig3_f <- gg.erp.mean_sem(ssLong[ssLong$action == 1,],x = "time",y = "meanv",color = "evidenceType",fill = "evidenceType",
                            palette = ev.palette,
                            xname = 'Time (s)', xbreaks = c(-0.5,0,0.5,1), xlabs = c('-0.5','0','0.5','1'),
                            yname = 'Pz amplitude (uV)', ycoord= c(-5,12),ybreaks = c(-5,0,5,10), ylabs = c('-5','0','5','10'),
                            legend = FALSE)
  fig3_f <- fig3_f + facet_grid(gcondition~idx, 
                                labeller = labeller(gcondition = gcond.labs, idx = order.labs))
  
  emf(paste(dir.figures, "/Figure_3f.emf", sep = ''), fig3_f, 
      width = 2.35, height = 2.5, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  
  fig3_f
  dev.off()
}
#############################################################################################
# EEG p3 analysis - Decoding (TABLE S3, Figure 4)
#############################################################################################
# TRIAL AVERAGE 
#Preprocess
{
  ga <- shortData %>%
    group_by(sub, trialN, gcondition, evidenceType, action) %>%
    summarize(meanp3 = mean(p300Amp))
  
  decode <- ga %>%
    spread('evidenceType', 'meanp3')
  
  colnames(decode) <- c('sub', 'trialN', 'gcondition', 'action', 'negEv', 'posEv')
  
  decode <- na.omit(decode)
  
  #P3-based prediction: if -Ev>+Ev --> NoAction; if -Ev<+Ev --> Action
  decode$pred <- NA
  decode[decode$negEv > decode$posEv,]$pred <- 'NoAction'
  decode[decode$negEv < decode$posEv,]$pred <- 'Action'
  
  decode$gdiff <- NA
  decode$gdiff <- decode$posEv - decode$negEv
  
  #P3-based predictive accuracy
  decode$predacc <- NA
  decode[decode$pred == 'Action' & decode$action == 1,]$predacc <- 1
  decode[decode$pred == 'NoAction' & decode$action == 0,]$predacc <- 1
  decode[decode$pred == 'Action' & decode$action == 0,]$predacc <- 0
  decode[decode$pred == 'NoAction' & decode$action == 1,]$predacc <- 0
  
  decode$action <- as.integer(decode$action)-1
  decode$gcondition <- as.factor(decode$gcondition)
  
  #GA decoding performance
  gadec <- decode %>%
    group_by(sub)%>%
    summarise(clasacc = mean(predacc)*100)
  
  mean(gadec$clasacc)
  sd(gadec$clasacc)/sqrt(16)
  
  gadecode <- decode %>%
    group_by(sub, gcondition)%>%
    summarise(clasacc = mean(predacc)*100)
}

#STATS 
model <- glmer(action ~ 1 + factor(pred)*gcondition+ (1|sub), 
               data = decode, 
               na.action = na.omit,
               family = binomial(link = 'logit'), 
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
car::Anova(model, type = c(2))

#Are predictions better using continuous diff values? 
model <- glmer(action ~ 1 + gdiff*gcondition+ (1|sub), 
               data = decode, 
               na.action = na.omit,
               family = binomial(link = 'logit'), 
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
car::Anova(model, type = c(2))

#Time-wise decoding
#Preprocessing
{ #Add 2-back lagged values for each target letter
  temp <- shortData %>%
  subset(select = -c(3,12))%>%
  group_by(sub,trialN)%>%
  mutate(laggedP3 = lag(p300Amp), 
         lagged2P3 = lag(p300Amp, n=2),
         laggedT = lag(evidenceType), 
         lagged2T = lag(evidenceType,n = 2))

temp <- na.omit(temp)
temp$consdiff <- NA
temp[temp$laggedT != temp$evidenceType,]$consdiff <- temp[temp$laggedT != temp$evidenceType,]$p300Amp - temp[temp$laggedT != temp$evidenceType,]$laggedP3
temp[(temp$laggedT == temp$evidenceType) & (temp$lagged2T != temp$evidenceType),]$consdiff <- temp[(temp$laggedT == temp$evidenceType) & (temp$lagged2T != temp$evidenceType),]$p300Amp - temp[(temp$laggedT == temp$evidenceType) & (temp$lagged2T != temp$evidenceType),]$lagged2P3

decoding_time <- temp
decoding_time <- na.omit(temp)

decoding_time$pred <- NA
decoding_time[decoding_time$evidenceType == 0 & decoding_time$consdiff < 0,]$pred <- 'Action'
decoding_time[decoding_time$evidenceType == 1 & decoding_time$consdiff > 0,]$pred <- 'Action'
decoding_time[decoding_time$evidenceType == 0 & decoding_time$consdiff > 0,]$pred <- 'NoAction'
decoding_time[decoding_time$evidenceType == 1 & decoding_time$consdiff < 0,]$pred <- 'NoAction'

decoding_time$predacc <- NA
decoding_time[decoding_time$pred == 'Action' & decoding_time$action == 1,]$predacc <- 1
decoding_time[decoding_time$pred == 'NoAction' & decoding_time$action == 0,]$predacc <- 1
decoding_time[decoding_time$pred == 'Action' & decoding_time$action == 0,]$predacc <- 0
decoding_time[decoding_time$pred == 'NoAction' & decoding_time$action == 1,]$predacc <- 0

gadecoding <- decoding_time %>%
  group_by(sub, gcondition,OTime)%>%
  summarise(clasacc = mean(predacc)*100)

selection <- dplyr::select(behData,ID,trialN,actionTime)
colnames(selection)[1] <- c('sub')
allData <- merge(decoding_time, selection, by = c("sub","trialN"))
allData$timeToAction <- allData$OTime - allData$actionTime
}

# Run & save statistical results (Table S3)
{
wb <- createWorkbook(dir.stats) # workbook to store the results
addWorksheet(wb, sheetName = 'Classification accuracy') # worksheet format
addWorksheet(wb, sheetName = 'Class.acc preAction') # worksheet format
addWorksheet(wb, sheetName = 'Class.acc postAction') # worksheet format

fullmodel <- glmer(predacc ~ 1 + timeToAction*factor(movedYet)*factor(gcondition) + (1|sub), 
                   family = binomial(link = 'logit'), 
                   data = allData[allData$action == 1,])

fullmod <- car::Anova(fullmodel, type = c(2))
pvals <- fullmod$`Pr(>Chisq)`
fullmod$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = fullmod, sheet = 'Classification accuracy', rowNames = TRUE)


preaction <- glmer(predacc ~ 1 + timeToAction*factor(gcondition)+ (1|sub), 
                   family = binomial(link = 'logit'), 
                   data = allData[allData$action == 1 & allData$movedYet == 0,])

preact <- car::Anova(preaction, type = c(2))
pvals <- preact$`Pr(>Chisq)`
preact$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = preact, sheet = 'Class.acc preAction', rowNames = TRUE)

postaction <- glmer(predacc ~ 1 + timeToAction*factor(gcondition) + (1|sub), 
                    family = binomial(link = 'logit'), 
                    data = allData[allData$action == 1 & allData$movedYet == 1,])

postact <- car::Anova(postaction, type = c(2))
pvals <- postact$`Pr(>Chisq)`
postact$adj_pvals <- p.adjust(pvals, 'BH') 
writeDataTable(wb, x = postact, sheet = 'Class.acc postAction', rowNames = TRUE)

saveWorkbook(wb, paste(dir.stats, "/NIMG_Stats_TableS3.xlsx", sep = ''), overwrite = TRUE) # Actually writes book
}

# Create & save Figure 4 
{
  trialavdec <- ggplot(gadecode, aes(x = gcondition, y = clasacc, fill = gcondition)) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75, show.legend = FALSE)+
    geom_point(position=position_dodge2(0), color = 'grey50', size=1, show.legend = FALSE) +  
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), show.legend = FALSE)+
    scale_fill_manual(values = c('grey25', 'blue', 'cyan')) + 
    geom_hline(yintercept = 50)+
    coord_cartesian(ylim = c(20,80))+
    scale_y_continuous(name = '% Classification accuracy') + 
    scale_x_discrete(labels = c('Informative', 'NH', 'NL'), name = '') + 
    theme_classic(base_size = 8) + 
    theme(strip.background = element_rect(colour="white", fill="white"), 
          plot.title = element_text(hjust = 0.5, size = 8)) 
  
  timedecode <- ggplot(decoding_time, aes (x = OTime, y = predacc*100, color = as.factor(gcondition), fill = as.factor(gcondition)))+
    stat_smooth(se=TRUE, method='loess', size = 1, show.legend = FALSE) +
    geom_line(stat = 'smooth', alpha = 0.25,se=FALSE, method='loess', size = 0.5, show.legend = FALSE, aes(group = sub)) +
    coord_cartesian(x = c(0,25), y = c(20,80))+ 
    scale_fill_manual(values = c('black', 'blue', 'cyan')) +
    scale_color_manual(values = c('black', 'blue', 'cyan')) + 
    scale_y_continuous(name = 'Classification /n accuracy from /n trial onset (%)') + 
    geom_hline(yintercept = 50)+
    scale_x_continuous(name = 'Time (s)') + 
    theme_classic(base_size=8)+
    theme(strip.background = element_rect(colour="white", fill="white")) +
    facet_grid(~gcondition, labeller = labeller(gcondition = gcond.labs))
  
  magnitudep3 <- ggplot(decode, aes (x = gdiff, y = action, color = as.factor(gcondition), fill = as.factor(gcondition)))+
    geom_smooth(method="glm",  method.args=list(family="binomial"), show.legend = FALSE) +
    coord_cartesian(x = c(-30,+30), y = c(0,1))+ #, y = c(20,80))+ 
    scale_fill_manual(values = c('black', 'blue', 'cyan')) +
    scale_color_manual(values = c('black', 'blue', 'cyan')) + 
    #geom_hline(yintercept = 50)+
    geom_vline(xintercept = 0)+
    scale_x_continuous(name = '+Ev minus -Ev  /n P3 amplitude (uV)') + 
    scale_y_continuous(name = 'P(Action)') + 
    theme_classic(base_size=8)+
    theme(strip.background = element_rect(colour="white", fill="white"))
  
  #merge with beh data to add switch time 
  actiondec <- filter(decoding_time, action == 1)
  sshortData <- subset(shortData, select = c(1,2,3)) %>%
    group_by(sub,trialN)%>%
    summarise(actionTime = first(actionTime))
  allData <- merge(actiondec,sshortData, by = c('sub', 'trialN'))
  allData$timeToAction <- allData$OTime - allData$actionTime
  
  actiondecoding <- ggplot(allData, aes (x = timeToAction, y = predacc*100, color = as.factor(gcondition), fill = as.factor(gcondition)))+
    stat_smooth(se=TRUE, method='loess', size = 1, show.legend = FALSE) +
    geom_line(stat = 'smooth', alpha = 0.25,se=FALSE, method='loess', size = 0.5, show.legend = FALSE, aes(group = sub)) +
    coord_cartesian(x = c(-15,5), y = c(20,80))+ 
    scale_fill_manual(values = c('black', 'blue', 'cyan')) +
    scale_color_manual(values = c('black', 'blue', 'cyan')) + 
    scale_y_continuous(name = 'Classification /n accuracy locked to /n action time (%)') + 
    geom_hline(yintercept = 50)+
    geom_vline(xintercept = 0)+
    scale_x_continuous(name = 'Time to action (s)') + 
    theme_classic(base_size=8)+
    theme(strip.background = element_rect(colour="white", fill="white")) +
    facet_grid(~gcondition, labeller = labeller(gcondition = gcond.labs))
  
  decodingbottom <- plot_grid(magnitudep3, actiondecoding, 
                              ncol = 2, 
                              rel_widths = c(1,2), 
                              align = 'vh', axis = 'tblr')
  
  decodingtop <- plot_grid(trialavdec,timedecode, 
                           ncol = 2, 
                           rel_widths = c(1,2), 
                           align = 'vh', axis = 'tblr')
  
  
  alldecoding <- plot_grid(decodingtop, decodingbottom, 
                           nrow = 2, 
                           rel_heights = c(1,1),
                           align = 'v', axis = 'tb')
  
  emf(paste(dir.figures, "/Figure_4.emf", sep = ''),
      bg = "transparent", width = 6, height = 4, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  alldecoding
  dev.off()
}
#############################################################################################
# EEG RP analysis - RP (Figure 5)
#############################################################################################
# Create & save Figure 5
{ rpdata <- read.csv(paste(dir.data, '/NIMG_EPP_2021_RPData.csv', sep = ''))
  colnames(rpdata) <- c('sub', 'condition', 'time', 'CZamp')
  rpdata$condition <- as.factor(rpdata$condition)
 
  #Plot RP
  rp <- gg.erp.mean_sem(rpdata, x = "time", y = "CZamp", color = 'condition', fill = 'condition', 
                        palette = cond.palette, 
                        xbreaks = c(-2,-1,0,0.5), xlabs = c(-2,-1,0,0.5),
                        ybreaks = c(-10,-5,0,5,10), ylabs = c(-10,-5,0,5,10),ycoord = c(10,-10), 
                        legend= FALSE)
    
  rp = rp + annotate("rect", xmin=-2.5, xmax=-2, ymin=-Inf, ymax=+Inf, alpha= 0.2, fill='black', colour = NA)

  emf(paste(dir.figures, '/Figure_5.emf', sep = ''), rp,
      bg = "transparent", width = 6, height = 3, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 10)
  rp
  dev.off()
  }




