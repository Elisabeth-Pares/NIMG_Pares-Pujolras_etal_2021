#############################################################################################
# This script contains analysis code that reproduces the figures and analyses
# reported in the supplementary material of the following manuscript: 
#
# Parés-Pujolràs, E., Travers, E., Ahmetoglu, Y., & Haggard, P. (2021).
# Evidence accumulation under uncertainty -
# a neural marker of emerging choice and urgency.
# NeuroImage, 232(May 2021). https://doi.org/10.1016/j.neuroimage.2021.117863
#
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
  cond.labs <- c("antiAction", "proAction", "NH", "NL")
  names(cond.labs) <- c("1","2","3", "4")
  
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

#Load data 
behData <- read.csv(paste(dir.data, "/NIMG_EPP_2021_behData.csv", sep = ''), header = TRUE)
shortData <- read.csv(paste(dir.data, "/NIMG_EPP_2021_shortData.csv", sep = ''), header = TRUE)
erpData <- read.csv(paste(dir.data, "/NIMG_EPP_2021_longData.csv", sep = ''), header = TRUE)
# Add labels to erp data 
names(erpData)[10:(384+10)] = stringr::str_c('t', seq(-.5, 1., 1/256))

#Figure S2
{
  #SINGLE SUBJECTS
  shortData <- arrange(shortData, sub, trialN)
  
  GARTs <- shortData[shortData$action == 1,] %>% 
    group_by(sub, trialN, gcondition) %>% 
    summarize (RT = first(actionTime))
  
  GARTs <- GARTs %>% 
    group_by(sub,  gcondition) %>% 
    summarize (gaRT = mean(RT))
  
  j = 0
  a<- list()
  for (i in unique(shortData$sub)){
    subset <- filter(shortData, sub == i)
    j <- j+1
    print(i) 
    
    a[[j]] <- ggplot(subset, aes(x = OTime, y=p300Amp, color=factor(gcondition))) +
      geom_smooth(se=TRUE, method='loess', span = 0.75,size = 0.35, show.legend = FALSE) +
      geom_line(stat = 'smooth', alpha = 0.25,se=FALSE, method='loess', size = 0.5, show.legend = FALSE, aes(group = sub)) +
      theme_classic(base_size=9) + 
      theme(strip.background = element_rect(colour="white", fill="white"), 
            plot.title = element_text(size=9)) +
      coord_cartesian(x = c(0,25))+#, y = c(0,6))+ 
      geom_vline(data=filter(subset, action== 1 & gcondition== 1), aes_(xintercept= GARTs[GARTs$sub == i & GARTs$gcondition== 1,]$gaRT), color = 'black', size = 0.35)+
      geom_vline(data=filter(subset, action== 1 & gcondition== 3), aes_(xintercept= GARTs[GARTs$sub == i & GARTs$gcondition== 3,]$gaRT), color = 'blue', size = 0.35)+
      geom_vline(data=filter(subset, action== 1 & gcondition== 4), aes_(xintercept= GARTs[GARTs$sub == i & GARTs$gcondition== 4,]$gaRT), color = 'skyblue', size = 0.35) + 
      scale_color_manual(values = c("black", "blue", "skyblue"), labels = "", name = "") +
      labs(title = sprintf('ID = %s', j), x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter")+
      scale_y_continuous(guide = guide_axis(check.overlap = TRUE))+
      facet_grid(action~., labeller = labeller(action = act.labs))#, gcondition = cond.labs))
    #print(a)
  }
  library(gridExtra)
  require(devEMF)
  
  emf(paste(dir.figures, "/Figure_S2.emf", sep = ''),
      bg = "transparent", width = 5.5, height = 6, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  do.call(grid.arrange,a)
  dev.off()
}
#Figure S3 
{

  # Add labels to erp data 
  names(erpData)[10:(384+10)] = stringr::str_c('t', seq(-.5, 1., 1/256))
  #Remove rows with NaN - rejected due to artefacts
  erp <- erpData[!is.na(erpData$sub),] 
  
  #Create single trial index
  erp <- erp %>% 
    mutate(trialN = group_indices(., block, trial))
  
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
  #Plot First vs. Last for all single participants 
  j = 0
  a<- list()
  for (i in unique(ssLong$sub)){
    subset <- filter(ssLong, sub == i)
    j <- j+1
    print(i) 
    
    a[[j]] <- ggplot(subset[subset$action == 1,], aes(x = time, y = meanv, colour = factor(evidenceType))) + 
      geom_line(size = 0.35,show.legend = FALSE) +
      scale_colour_manual(values = c("red2", "green2"), name = "", label = '')+
      scale_fill_manual(values = c("red2", "green2"), name = "", label = '')+
      labs(x = "Time (s)", y = "Pz amplitude (uV)") + 
      scale_x_continuous(breaks = c(-0.5,0,0.5,1), labels = c("-0.5","0","0.5","1")) + 
      theme_classic(base_size=9) +
      scale_y_reverse(guide = guide_axis(check.overlap = TRUE))+  
      labs(title = sprintf('ID = %s', j))+
      theme(strip.background = element_rect(colour="white", fill="white"),
            panel.background = element_rect(fill = "white"),
            plot.title = element_text(size=9))+
      geom_vline(aes_(xintercept= 0), color = 'black', size = 0.25)+
      facet_grid(gcondition~idx, 
                 labeller = labeller(gcondition = gcond.labs, idx = order.labs))
  }
  
  library(gridExtra)
  emf(paste(dir.figures, '/Figure_S3.emf', sep = ''),  
      width = 5.5, height = 6, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  do.call(grid.arrange,a)
  dev.off()
  
  
}
#Figure S4 
{
  palette = c( "darksalmon", "brown2","red2", "darkred","greenyellow","green", "green4", "darkgreen")
  erpData$rownm <- seq.int(nrow(erpData))
  
    # Fig S4_a
    #Get last 
    lastonly <- erpData %>%
      group_by(sub, block,trial, evidenceType) %>%
      summarize_all(last)
    
    erpl1 <- erpData[!(erpData$rownm %in% lastonly$rownm),]  #Create dataset without last ones 
    lastonly$index <- 'l-1'
    
    #Get N-2
    n2back <- erpl1 %>%
      group_by(sub, block, trial, evidenceType) %>%
      summarize_all(last)
    
    erpl2 <- erpl1[!(erpl1$rownm %in% n2back$rownm),]  #Create dataset without n-2
    n2back$index <- 'l-2'
    
    #Get N-3
    n3back <- erpl2 %>%
      group_by(sub, block, trial, evidenceType) %>%
      summarize_all(last)
    
    erpl3 <- erpl2[!(erpl2$rownm %in% n3back$rownm),]  #Create dataset without n-2
    n3back$index <- 'l-3'
    
    #Get n-4
    n4back <- erpl3 %>%
      group_by(sub, block, trial, evidenceType) %>%
      summarize_all(last)
    
    n4back$index <- 'l-4'
    
    nback <- rbind(lastonly,n2back, n3back, n4back) # combine both
    
    # Put in long format 
    long.data = nback %>%
      gather(key='time', value='voltage', `t-0.5`:`t1`) %>%
      arrange(sub, block, trial,timeFromOnset)
    long.data$time = long.data$time %>% stringr::str_remove('t') %>% as.numeric()
    
    #Average within subjects
    ssLong<- long.data %>% 
      group_by(sub, condition, evidenceType, action,time,index) %>% 
      summarize (meanv = mean(voltage))
    
    gaLong<- ssLong %>% 
      group_by(condition, evidenceType, action,time,index) %>% 
      summarize (gav = mean(meanv),
      )
    
    # 3 last for +Ev and -Ev separately 
    plot.Order <- gaLong[gaLong$action == 1,]
    plot.Order <- na.omit(plot.Order)
    figs4_a <- ggplot(plot.Order, aes(x = time, y = gav, colour = interaction(index,evidenceType))) + 
        geom_line(size = 0.75,aes(group = index),show.legend = FALSE) +
        scale_colour_manual(values = palette, name = "", label = c("Last", "Last-1", "Last-3", "Last-4"))+
        labs(x = "Time (s)", y = "Pz amplitude (uV)") + 
        scale_x_continuous(breaks = c(-0.5,0,0.5,1), labels = c("-0.5","0","0.5","1")) + 
        scale_y_reverse() +
        coord_cartesian(y = c(10,-5)) + 
        theme_classic(base_size=9) +
        geom_vline(aes_(xintercept= 0), color = 'black', size = 1)+
        facet_grid(evidenceType~condition, 
                   labeller = labeller(condition = cond.labs, evidenceType = ev.labs))
  figs4_a
  # Fig S4_b

  orderData <- shortData %>% 
    filter(movedYet == 0, condition != 1)%>%
    group_by(sub,  trialN, evidenceType,condition, action) %>% 
    summarize (last = last(p300Amp),
               last2 = nth(p300Amp, -2L),
               last3 = nth(p300Amp, -3L),
               last4 = nth(p300Amp, -4L), 
    )
  long_orderData <- orderData %>%
    gather(index, "p3amp", 6:9)
  
  #PLOTS
  figs4_b <- ggplot(long_orderData[long_orderData$action == 1,], aes(x=rev(index), y = p3amp, fill = interaction(index,evidenceType))) +
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75, show.legend = FALSE) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    labs(title = "", x = "Condition", y = "p3 amplitude (uV)", fill = "Ev") + 
    theme_classic(base_size=9) +
    scale_x_discrete(labels = c('-3','-2','-1','n')) + 
    scale_fill_manual(values = palette, name = NULL, labels = c("-Ev", "+Ev")) +
    facet_grid(evidenceType~condition, labeller = labeller(condition = cond.labs, evidenceType = ev.labs))
  
  
  require(cowplot)
  figs4 <- plot_grid(figs4_a, figs4_b, 
                     ncol = 2)
  
  emf(paste(dir.figures, '/Figure_S4.emf', sep = ''),  
      width = 5.5, height = 3, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize = 9)
  figs4
  dev.off()
}
#Figure S5
{ 
  myActionData <- filter(shortData, (action == 1 & movedYet == 0))
  myBinnedData_sl_action <- myActionData %>% 
    mutate(bins = cut( OTime, breaks = c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25)))%>%
    group_by(sub,trialN,condition,evidenceType,OTime,bins)%>%
    summarise(p3 = mean(p300Amp))
  
  myBinnedData_sl_noaction <- shortData[shortData$action == 0,] %>%  
    mutate(bins = cut( OTime, breaks = c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25)))%>%
    group_by(sub,trialN,condition,evidenceType,OTime,bins)%>%
    summarise(p3 = mean(p300Amp))
  
  ## Bar graphs 
  #Stim locked data, stay  
  aleft_stim_noaction = ggplot(data = myBinnedData_sl_noaction, mapping = aes(x=(bins), y = p3, fill = factor(condition))) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size=9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
    coord_cartesian(y = c(-1,8))+ 
    scale_fill_manual(values = c("black", "blue", "skyblue"), labels = c("Informative", "NH", "NL"), name = "Condition") +
    labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter") + 
    facet_grid(condition~., labeller = labeller(condition = cond.labs))
  
  #Sorted by evidence type 
  aright_stim_noaction_ev = ggplot(data =myBinnedData_sl_noaction, mapping = aes(x=bins, y = p3, fill = factor(evidenceType))) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size=9) + 
    coord_cartesian(y = c(-3,8))+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
    scale_fill_manual(values = c("red", "green"), labels = c("-Ev", "+Ev"), name = "EvidenceType") +
    labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter") + 
    facet_grid(condition~evidenceType, labeller = labeller(condition = cond.labs, evidenceType = ev.labs))
  
  #Stim locked data 
  bleft_stim_action = ggplot(data = myBinnedData_sl_action, mapping = aes(x=(bins), y = p3, fill = factor(condition))) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size=9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
    coord_cartesian(y = c(-1,8))+ 
    scale_fill_manual(values = c("black", "blue", "skyblue"), labels = c("Informative", "NH", "NL"), name = "Condition") +
    labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter") + 
    facet_grid(condition~., labeller = labeller(condition = cond.labs))
  
  #Sorted by evidence type 
  bright_stim_action_ev = ggplot(data =myBinnedData_sl_action, mapping = aes(x=bins, y = p3, fill = factor(evidenceType))) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size=9) + 
    coord_cartesian(y = c(-3,8))+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
    scale_fill_manual(values = c("red", "green"), labels = c("-Ev", "+Ev"), name = "EvidenceType") +
    labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter") + 
    facet_grid(condition~evidenceType, labeller = labeller(condition = cond.labs, evidenceType = ev.labs))
  
  #Generate action-locked data
  myBinnedData_al <- myActionData %>% 
    mutate(bins = cut(timeToAction, breaks = c(-15,-12.5,-10,-7.5,-5,-2.5,0)))%>%
    group_by(sub,trialN,condition, evidenceType,bins)%>%
    summarise(p3 = mean(p300Amp, na.action = 'omit'))
  
  #Action locked data 
  myBinnedData_al <- na.exclude(myBinnedData_al)
  cleft_act <- ggplot(data = myBinnedData_al, aes(x=(bins), y = p3, fill = factor(condition))) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size=9) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
    coord_cartesian(y = c(-3,8))+ 
    scale_fill_manual(values = c("black", "blue", "skyblue"), labels = c("Informative", "NH", "NL"), name = "Condition") +
    labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter") + 
    facet_grid(condition~., labeller = labeller(condition = cond.labs))
  
  #Action locked data, separated by evidence type 
    cright_act <- ggplot(data = myBinnedData_al, aes(x=bins, y = p3, fill = factor(evidenceType))) + 
    geom_bar(stat="summary", fun.y = "mean", position = position_dodge(), alpha=1, width = 0.75) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha=1, width = 0.2, position = position_dodge(0.75), inherit.aes = TRUE)+
    theme_classic(base_size=9) + 
    coord_cartesian(y = c(-3,8))+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
    scale_fill_manual(values = c("red", "green"), labels = c("-Ev", "+Ev"), name = "EvidenceType") +
    labs(title = "", x = "Time (s)", y = "p3 amplitude (uV)", fill = "Letter") + 
    facet_grid(condition~evidenceType, labeller = labeller(condition = cond.labs, evidenceType = ev.labs))

    figs5_a <- plot_grid(aleft_stim_noaction, aright_stim_noaction_ev,
                         ncol = 2, rel_widths = c(1,1.5))
    
    figs5_b <- plot_grid(bleft_stim_action, bright_stim_action_ev,
                         ncol = 2, rel_widths = c(1,1.5))
    
    figs5_c <- plot_grid(cleft_act, cright_act,
                         ncol = 2, rel_widths = c(1,1.5))
    
    figs5 <- plot_grid(figs5_a, figs5_b, figs5_c, 
                       nrow = 3)
    
    emf(paste(dir.figures, '/Figure_S5.emf', sep = ''),  
        width = 5.5, height = 9, coordDPI = 600,
        emfPlus=TRUE,family = "Helvetica",pointsize = 9)
    figs5
    dev.off()
    
    }
#Figure S6
{
  palette = c( "darksalmon", "brown2","red2", "greenyellow","green", "green4")

  #Fig S6_b
  shortData<- shortData %>%
    group_by(sub,trialN,evidenceType)%>%
    mutate(sortidx = row_number())
  
  m <- lmer(p300Amp ~ 1 + gconditionName*evidenceType + (1|sub),
            data = shortData[shortData$sortidx == 3,])
  
  car::Anova(m, type = c(2))
  
  shortData<- shortData %>%
    group_by(sub,trialN)%>%
    mutate(evidx = row_number())
  
  m <- lmer(p300Amp ~ 1 + gconditionName*evidx + (1|sub/trialN),
            data = shortData[shortData$gconditionName != 'Informative' & shortData$evidx <= 8,])
  
  evidx <- car::Anova(m, type = c(2))
  pvals <- evidx$`Pr(>Chisq)`
  evidx$adj_pvals <- p.adjust(pvals, 'BH') 
  
  m <- lmer(p300Amp ~ 1 + evidx + (1|sub/trialN),
            data = shortData[shortData$gconditionName == 'NL' & shortData$evidx <= 8,])
  
  evidx <- car::Anova(m, type = c(2))
  pvals <- evidx$`Pr(>Chisq)`
  evidx$adj_pvals <- p.adjust(pvals, 'BH') 
  
  
  fig_s6b <- ggplot(shortData[shortData$evidx <=8 & shortData$gconditionName != 'Informative',], aes(x = evidx, y = p300Amp, colour = gconditionName))+ 
    geom_smooth(method = 'glm', se = TRUE, show.legend = FALSE) + 
    scale_colour_manual(values = c('blue', 'cyan')) +
    scale_y_continuous(name = 'P3 amplitude (uV)')+
    scale_x_continuous(name = 'Evidence #index')+
    theme_classic(base_size = 10)
  
  emf(paste(dir.figures, '/Figure_s6_b.emf', sep = ''),
      bg = "transparent", width = 3, height = 2, coordDPI = 600,
      emfPlus=TRUE,family = "Helvetica",pointsize =10)
  fig_s6b
  dev.off()
}

