# Now combine both data sets and use first data set as dependend variable

#### pre stuff ####
rm(list = ls())

library(ggplot2)
library(grid)
library(gridExtra)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(dict)
library(gtable)
library(lubridate)
library(xlsx)
# install.packages('xlsx')
# library(data.table)


this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# source('multiplot.R')

catList <- c('academic', #scikit
             'academic', #numpy
             'academic', #shogun
             'academic', #encog
             'academic', #Theano
             'academic', #R
             'corporate', #yooreka
             'academic', #orange
             'corporate', #nupic 
             'academic', #torch, #not sure, individuals working in different companies, probably unpaid volunteers
             'foundation', #spark
             'corporate', #dlib
             'corporate', #deeplearning4j, Skymind
             'academic', #elki
             'academic', #keras, individual working at google
             'corporate', #H20
             'corporate', #tensorflow
             'corporate', #openNN
             'corporate' #openAI
             )

d <- dict()
d[['davisking/dlib']] <- 'corporate'
d[['elki-project/elki']] <- 'academic'
d[['encog/encog-java-core']] <- 'academic'
d[['h2oai/h2o-3']] <- 'corporate'
d[['mlpack/mlpack']] <- 'academic'
d[['numpy/numpy']] <- 'academic'     
d[['numenta/nupic']] <- 'corporate'
d[['deeplearning4j/deeplearning4j']] <- 'corporate'
d[['openai/gym']] <- 'corporate'
d[['Artelnics/OpenNN']] <- 'corporate'
d[['biolab/orange3']] <- 'academic'
d[['wch/r-source']] <- 'academic'
d[['scikit-learn/scikit-learn']] <- 'academic'
d[['shogun-toolbox/shogun']] <- 'academic'
d[['tensorflow/tensorflow']] <- 'corporate'
d[['Theano/Theano']] <- 'academic'
d[['torch/torch7']] <- 'academic'
d[['fchollet/keras']] <- 'academic'
d[['marmanis/yooreeka']] <- 'corporate'
d[['mlpack/mlpack']] <- 'academic'
d[['BVLC/caffe']] <- 'academic'
d[['NervanaSystems/neon']] <- 'corporate'
d[['apache/hadoop']] <- 'foundation'
d[['apache/kafka-site']] <- 'foundation'
d[['apache/storm']] <- 'foundation'
d[['apache/spark']] <- 'foundation'
# d[['apache/hadoop']] <- 'academic'
# d[['apache/kafka-site']] <- 'academic'
# d[['apache/storm']] <- 'academic'
# d[['apache/spark']] <- 'academic'

catDict <-d
catDictValues <- unlist(catDict$values())
catDictKeys <- unlist(catDict$keys())
# 'mlpack/mlpack',
# 'waikato/moa',
             

#### Loading etc. ... ####
load("RObject_pull_comp_comments.gzip")
DF <- GH_data_df_pull_comp
unique_pull_events <- unique(DF$pull_id)


pullDF <- data.frame(pull_id = unique_pull_events)
repo <- c()
opener <- c()
closer <- c()
merger <- c()
closed <- c()
merged <- c()
additions <- c()
deletions <- c()
commits <- c()
comments <-c()
comments_manual <-c()
files_changed <- c()
t_open <- c()
t_close <- c()
internal_pull_ID <-c()

# unique_pull_events <- as.vector(unique_pull_events)
# unique_pull_events <- unique_pull_events[-which("3099291.0" == unique_pull_events)]#hat zwei mal close
# funktioniert nicht, goibts wohl oefter


for(id in unique_pull_events){
  i <- which(id == unique_pull_events)
  print(i)
  repo[i] <- NA
  opener[i] <- NA
  closer[i] <- NA
  merger[i] <- NA
  closed[i] <- NA
  merged[i] <- NA
  additions[i] <- NA 
  deletions[i] <- NA
  commits[i] <- NA
  comments[i] <- NA
  comments_manual[i] <- NA
  files_changed[i] <- NA 
  t_open[i] <- NA
  t_close[i] <- NA
  internal_pull_ID[i] <-NA
  
  for(closeFlag in DF[DF$pull_id == id,]$closed){
    if (closeFlag == "0.0"){
      opener[i] <- as.character(DF[DF$pull_id == id & DF$closed == "0.0",]$user_name)
      t_open[i] <- as.character(DF[DF$pull_id == id & DF$closed == "0.0",]$event_time)
      repo[i] <- as.character(DF[DF$pull_id == id & DF$closed == "0.0",]$repo_name)
      internal_pull_ID[i] <- as.numeric(DF[DF$pull_id == id & DF$closed == "0.0",]$pull_number)
    }else if(closeFlag == "1.0"){
      repo[i] <- as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$repo_name)
      closer[i] <- as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$user_name)
      merger[i] <- as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$merger)
      closed[i] <- 1
      merged[i] <- as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$merged)
      additions[i] <- as.numeric(as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$additions))
      deletions[i] <- as.numeric(as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$deletions))
      commits[i] <- as.numeric(as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$commits))
      comments[i] <- as.numeric(as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$comments))
      comments_manual[i] <- as.numeric(as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$comments_manual))
      files_changed[i] <- as.numeric(as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$changed_files))
      t_close[i] <-as.character(DF[DF$pull_id == id & DF$closed == "1.0",]$event_time)
      internal_pull_ID[i] <- as.numeric(DF[DF$pull_id == id & DF$closed == "1.0",]$pull_number)
    }
  }
}

#### after long loop ####
pullDF$repo <- repo
pullDF$closed <- closed
pullDF$merged <- merged
pullDF$opener <- opener
pullDF$closer <- closer
pullDF$merger <- merger
pullDF$add <- additions
pullDF$del <- deletions
pullDF$comm <- commits
pullDF$comments <- comments
pullDF$comments_manual <- comments_manual
pullDF$files <- files_changed
pullDF$t_open <- t_open
pullDF$t_close <- t_close

# save(pullDF,file="pullDF_R_old2_comments.Rda")

#### factorize ####

# load("pullDF_R_old2_comments.Rda")

pullDF$repo <- factor(pullDF$repo)
pullDF$opener <- factor(pullDF$opener)
pullDF$closer <- factor(pullDF$closer)
pullDF$merger <- factor(pullDF$merger)

save(pullDF,file="pullDF_R_comments.Rda")

#### make subsettable ####
load("pullDF_R_comments.Rda")
pullDF$pull_id <- as.numeric(pullDF$pull_id)
pullDF$repo <- as.character(pullDF$repo)
pullDF[is.na(pullDF$merged),]$merged <- "NA"
pullDF[pullDF$merged == "True",]$merged <- "1.0"
pullDF[pullDF$merged == "False",]$merged <- "0.0"
pullDF$merged <- as.numeric(pullDF$merged)
pullDF$t_open <- as.POSIXlt(pullDF$t_open)
pullDF$t_close <- as.POSIXlt(pullDF$t_close)
# save(pullDF,file="pullDF_comments_subset.Rda")

#### non Merge right users ####
# load("pullDF_comments_subset.Rda")
#example for caffe
DFusers <- subset(pullDF, closed == 1 & merged == 1)
# DFusers <- DFusers[!is.na(DFusers$closed),] # doppelt gemoppelt?
DFusers <- DFusers[!is.na(DFusers$opener),] # nur komplette Punkte
openers <- unique(DFusers$opener)
closers <- unique(DFusers$closer)

opened <- aggregate(DFusers$merged, by = list(DFusers$opener), sum)
opened <- opened[rev(order(opened$x)),]

DFusers$mergeRight <- NA
DFusers$num_merge <- NA
for(id in unique(DFusers$pull_id)){
  DFusers[DFusers$pull_id == id,]$mergeRight <- as.character(DFusers[DFusers$pull_id == id,]$opener) %in% closers #closers richtige Gruppe, weil oben auf nur gemergte pulls gesubsetted wurde
  # if(!is.na(DFusers[DFusers$pull_id == id,]$opener)){DFusers[DFusers$pull_id == id,]$num_merge <-  opened[opened$Group.1 == DFusers[DFusers$pull_id == id,]$opener,]$x}
  DFusers[DFusers$pull_id == id,]$num_merge <-  opened[opened$Group.1 == DFusers[DFusers$pull_id == id,]$opener,]$x
}

save(DFusers,file="DFusers.Rda")

bin = "week"
plotting = 0
correlations = 0

#### loop ####
# pdf(paste0('noMerge_correlation_',bin ,'.pdf'))
for (repo_id in c(1:length(unique(DFusers$repo))) ){
# for (repo_id in c(14) ){
  # for (repo_id in c(1:1) ){
  # DFwork <- subset(pullDF, repo == unique(pullDF$repo)[repo_id] & closed == 1)
  DFwork <- subset(DFusers, repo == unique(DFusers$repo)[repo_id] & closed == 1 & merged == 1 & mergeRight == FALSE)
  if(dim(DFwork)[1] == 0){
    print("no merges")
    next
  }

  DFtemp <- DFwork
  # bin = "month"
  # DFtemp$t_bin <- as.character(round(DFtemp$t_close, bin ))
  DFtemp$t_bin <- as.character(floor_date(as.POSIXlt(DFtemp$t_close), bin))
  
  # b <- aggregate(DFtemp$add, by = list(DFtemp$t_bin) , sum )
  b <- aggregate(DFtemp$comments_manual, by = list(DFtemp$t_bin) , sum )
  c <- aggregate(DFtemp$closed, by = list(DFtemp$t_bin) , sum)
  a <- as.POSIXlt(b$Group.1)
  
  min(a)
  max(a)
  timevec <- seq(min(a), max(a), by = bin) # grid
  # tvr <- as.POSIXct(round(timevec, bin))
  tvr <- floor_date(DFtemp$t_close, bin)
  
  N <- length(tvr)
  y1vec <- integer(N)
  y2vec <- integer(N)
  for (j in c(1:length(a))){
    y1vec[which(tvr == a[j])] <- b$x[j]
    y2vec[which(tvr == a[j])] <- c$x[j]
  }
  
  # DFnewOld <- data.frame(time = as.POSIXlt(b$Group.1), events = c$x, adds = b$x)
  DFnew <- data.frame(time = tvr, events = y2vec, adds = y1vec)
  if(correlations == 1){
    try({
      corrdata <- ccf(DFnew$events, DFnew$adds)
      print(unique(DFtemp$repo))
      print(corrdata[,1])
      plot(corrdata, sub =paste0(unique(DFtemp$repo)))
      # title(paste0(unique(DFtemp$repo)))
      # dev.copy(png,paste0('noMerge_correlation_', repo_id, '.png'))
      # dev.off()
      }
    )
  }
  
  if(plotting == 1){
      p3 <- ggplot(data=DFwork)+
        # geom_point(aes(x=t_close, y = add), alpha = 0.2)
        geom_point(aes(x=t_close, y = comments_manual), alpha = 0.2)
      
      ggplot(data=DFwork)+
        geom_point(aes(x=deltat, y = comments_manual), alpha = 0.1)
      
      
      p1 <- ggplot(data = DFnew)+
        geom_line(aes(x=time, y=events), alpha = 0.1)
      p2 <- ggplot(data = DFnew)+
        geom_line(aes(x=time, y=adds), alpha = 0.1)
      
      
      # plots <- c()
      # plots[[1]] <- p1
      # plots[[2]] <- p2
      # multiplot(plotlist = plots, cols = 1)
      
      # gl = lapply(list(p1,p2), ggplotGrob)     
      gl = lapply(list(p1,p3), ggplotGrob)     
      g = do.call(rbind, c(gl, size="first"))
      g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
      
      title <- textGrob(as.character(unique(DFusers$repo)[repo_id]),gp=gpar(fontsize=20))
      padding <- unit(5,"mm")
      table <- gtable_add_rows(
        g, 
        heights = grobHeight(title) + padding,
        pos = 0)
      table <- gtable_add_grob(
        table, 
        title, 
        1, 1, 1, ncol(table))
      
      grid.newpage()
      grid.draw(table)
      # ggsave(table, file=paste("comments_output_non_merge",bin, repo_id, ".pdf"))  
  }
  
}
# dev.off()

#### cummulative user number ####
cummUserDF <- data.frame(repo= NA, t=as.POSIXct(pullDF[1,]$t_open), uniqueUsers = 0)
for (repo_id in c(1:length(unique(DFusers$repo))) ){
  # for (repo_id in c(1:1) ){
  # DFwork <- subset(pullDF, repo == unique(pullDF$repo)[repo_id] & closed == 1)
  DFwork <- subset(DFusers, repo == unique(DFusers$repo)[repo_id] & closed == 1 & merged == 1)
  DFwork <- subset(DFusers, repo == unique(DFusers$repo)[repo_id] & closed == 1)
  DFwork <- subset(pullDF, repo == unique(pullDF$repo)[repo_id] & closed == 1)
  if(dim(DFwork)[1] == 0){
    print("no merges")
    next
  }
  
  openers_repo <- unique(DFwork$opener)
  closers_repo <- unique(DFwork$closer)
  
  DFwork$t_open <- floor_date(as.POSIXct(DFwork$t_open), bin)
  timepoints <- as.vector(unique(DFwork$t_open))
  timepoints <- unique(DFwork$t_open)
  for(i in 1:length(timepoints)){
    openers_temp <- unique(DFwork[DFwork$t_open <= timepoints[i], ]$opener)
    cs <- length(intersect(openers_repo, openers_temp))
    cummUserDF <- rbind(cummUserDF, data.frame(repo=unique(DFusers$repo)[repo_id] , t=timepoints[i], uniqueUsers = cs))
  }
}
cummUserDF <- cummUserDF[-1,]
ggplot(data = cummUserDF)+
  geom_line(aes(x=t, y=uniqueUsers, col = as.factor(repo)))

#### load big data set ####

load("RObject_comp.gzip")
df_temp <- data.frame(repo = GH_data_df_comp$repo_name, event_time = GH_data_df_comp$event_time, event_type = GH_data_df_comp$event_type, user_name = GH_data_df_comp$user_name)
df_temp$event_time <- as.POSIXlt(as.character(df_temp$event_time))
df_temp$ones <- 1

DFlong <- df_temp
save(DFlong,file="DFlong.Rda")

for (repo_id in c(1:length(unique(df_temp$repo))) ){
  DFwork <- subset(df_temp, repo == unique(df_temp$repo)[repo_id])
  DFwork$t_bin <- as.character(floor_date(as.POSIXlt(DFwork$event_time), bin))  
  
  forks <- aggregate(DFwork[DFwork$event_type == "ForkEvent",]$ones, by = list(DFwork[DFwork$event_type == "ForkEvent",]$t_bin) ,sum )
  # dls <- aggregate(DFwork[DFwork$event_type == "DownloadEvent",]$ones, by = list(DFwork[DFwork$event_type == "DownloadEvent",]$t_bin) ,sum )
  watch <- aggregate(DFwork[DFwork$event_type == "WatchEvent",]$ones, by = list(DFwork[DFwork$event_type == "WatchEvent",]$t_bin) ,sum )
  a <- as.POSIXlt(forks$Group.1)
  
  # following code to prevent empty bins
  timevec <- seq(min(a), max(a), by = bin) # grid
  # tvr <- as.POSIXct(round(timevec, bin))
  tvr <- floor_date(DFwork$event_time, bin)
  N <- length(tvr)
  forksvec <- integer(N)
  watchvec <- integer(N)
  # dlsvec <- integer(N)
  for (j in c(1:length(a))){
    forksvec[which(tvr == a[j])] <- forks$x[j]
    watchvec[which(tvr == a[j])] <- watch$x[j]
    # dlsvec[which(tvr == a[j])] <- dls$x[j]
  }
  
  
  
  # DFnewOld <- data.frame(time = as.POSIXlt(b$Group.1), events = c$x, adds = b$x)
  DFnew <- data.frame(time = tvr, forks = forksvec, watch = watchvec)#, dls = dlsvec)
  aggDF <- data.frame
  
}

