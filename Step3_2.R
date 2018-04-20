# Skript fuer fast alle Result-Grafiken

#### pre stuff ####
rm(list = ls())

saving <- 0

library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)
library(ggpubr)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(dict)
library(gtable)
library(lubridate)
library(xlsx)
library(reshape)
library(reshape2)
library(lme4)
library(MuMIn)
library(lfe)
library(lsr)
library(apaTables)
library(gtools)
library(nlme)
library(MASS)
library(car)
library(effsize)
library(ez)
library(BaylorEdPsych)
library(r2glmm)
library(plm)
library(lmtest)
# install.packages("lme4")
# install.packages('xlsx')
# install.packages('lsr')
# install.packages('lfe')
# install.packages('plm')
# install.packages('gtools')
# install.packages('apaTables')
# install.packages('GGally')
# install.packages("MuMIn")
# install.packages("r2glmm")


this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

d <- dict()
d[['davisking/dlib']] <- 'corporate'
d[['elki-project/elki']] <- 'academic'
d[['encog/encog-java-core']] <- 'academic'
d[['h2oai/h2o-3']] <- 'corporate'
d[['numpy/numpy']] <- 'academic'     
d[['numenta/nupic']] <- 'corporate'
d[['deeplearning4j/deeplearning4j']] <- 'corporate'
d[['openai/gym']] <- 'corporate'
d[['Artelnics/OpenNN']] <- 'corporate'
d[['biolab/orange3']] <- 'academic'
d[['scikit-learn/scikit-learn']] <- 'academic'
d[['shogun-toolbox/shogun']] <- 'academic'
d[['tensorflow/tensorflow']] <- 'corporate'
d[['Theano/Theano']] <- 'academic'
d[['torch/torch7']] <- 'academic'
d[['fchollet/keras']] <- 'corporate' # developed by google engineer, first on its own
d[['mlpack/mlpack']] <- 'academic'
d[['BVLC/caffe']] <- 'academic'
d[['NervanaSystems/neon']] <- 'corporate'
d[['apache/hadoop']] <- 'foundation'
d[['apache/kafka-site']] <- 'foundation'
d[['apache/storm']] <- 'foundation'
d[['apache/spark']] <- 'foundation'

catDictFull <-d
catDictValuesFull <- unlist(catDictFull$values())
catDictKeysFull <- unlist(catDictFull$keys())

catDict <-catDictFull 
catDictValues <- unlist(catDict$values())
catDictKeys <- unlist(catDict$keys())

#### useful functions ####
remove_outliers <- function(x, na.rm = TRUE, ...) {
  # according to inter quartile rule 
  # https://www.thoughtco.com/what-is-the-interquartile-range-rule-3126244
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

eta2_effect <- function(X){
  Y<-c()
  index <- 0
  for(x in X){
    index <- index + 1
    if(!is.na(x)){
      
      if(x>0.138){
        y <- 'large'
      } else if(x<0.138 && x>0.059){
        y <- 'medium'
      } else if(x<0.059 && x>0.01){
        y <- 'small'
      } else
        y <- 'zero'
      
    } else
      y <- 'na'
    Y[index] <- y
  }
  Y
}

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

ggpie <- function (dat, by, totals, legendFlag = TRUE) {
  dat2 <- data.frame(by = dat[by], totals = dat[totals])
  SUM <- sum(dat2[totals])
  rest <- 1- SUM
  dat2 <- rbind(dat2, c(factor("rest"), rest))
  print(dat2)
  ggplot(dat2, aes_string(x=factor(1), y=totals, fill=by)) +
    geom_bar(stat='identity', color='black', width = 1, position = position_fill(reverse = TRUE)) +
    coord_polar("y", start=0)+
    theme(axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(colour='black'),
          axis.title=element_blank()) +
    scale_y_continuous(breaks=cumsum(dat2[[totals]]) - dat2[[totals]]/2, labels=dat2[[by]])+
    blank_theme + if(!legendFlag){theme(legend.position="none")
    }else{theme()
        }
}

star <- function(pval) {
  if (pval <= 0.001) {
    return("***")
  }
  if (pval <= 0.01) {
    return(" **")
  }
  if (pval <= 0.05) {
    return(" *")
  }
  else {return(" ")
  }
}


#### load Data ####
load("DFusers.Rda")
DFusers$corporate <- NA

for (repoName in catDictKeysFull){ 
  DFusers[DFusers$repo == repoName,]$corporate <- (catDictFull[[repoName]] == "corporate")
}

load("DFlong.Rda")
# load("DFlong_completeTime.Rda")

DFlong$event_time <- as.POSIXlt(DFlong$event_time, format= "%Y-%m-%d %H:%M:%S")
DFlong2011 <- DFlong
DFlong <- DFlong[DFlong$event_time >= "2013-01-01 CET",] #damit es zu den pull Daten passt.

load("pullDF_R_comments.Rda")
pullDF$pull_id <- as.numeric(pullDF$pull_id)
pullDF$repo <- as.character(pullDF$repo)
pullDF[is.na(pullDF$merged),]$merged <- "NA"
pullDF[pullDF$merged == "True",]$merged <- "1.0"
pullDF[pullDF$merged == "False",]$merged <- "0.0"
pullDF$merged <- as.numeric(pullDF$merged)
pullDF$t_open <- as.POSIXlt(pullDF$t_open, format= "%Y-%m-%d %H:%M:%S")
pullDF$t_close <- as.POSIXct(pullDF$t_close, format= "%Y-%m-%d %H:%M:%S")
pullDF$deltaT <- pullDF$t_close - pullDF$t_open


#### combine data and binning ####
# Neuer Ansatz, zeitliches Binning, User beruecksichtigen
bin <- "bimonth"
bin <- '2 months'
bin <- 'quarter'
bin <- 'halfyear'
bin <- '6 months'
bin <- 'month'

time_bin_vec <- sort(na.omit(unique(as.character(floor_date(as.POSIXlt(DFusers$t_close), bin)))))
time_birth_vec <- list()
DFusers$t_close_bin <- as.character(floor_date(as.POSIXlt(DFusers$t_close), bin))
DFlong$t_event_bin <- as.character(floor_date(as.POSIXlt(DFlong$event_time), bin))
pullDF$t_event_bin <- as.character(floor_date(as.POSIXlt(pullDF$t_close), bin))

for (repoName in catDictKeysFull){ 
  time_birth_vec[[which(repoName == catDictKeysFull)]] <-min(subset(DFlong, repo == repoName)$event_time)
  print(min(subset(DFlong, repo == repoName)$event_time))
}

load("DFcomb_allProjects_adminsVers3.Rda")
DFcomb <- subset(DFcomb)


DFcomb$corporate <- NA
pullDF$corporate <- NA

for (repoName in catDictKeysFull){ 
  DFcomb[DFcomb$repo == repoName,]$corporate <- (catDictFull[[repoName]] == "corporate")
  pullDF[pullDF$repo == repoName,]$corporate <- (catDictFull[[repoName]] == "corporate")
}


DFcomb2 <- DFcomb[!is.na(DFcomb$aveAdd),]


DFcomb2$logAdd <- log(DFcomb2$aveAdd)
DFcomb2$numUsers <- as.numeric(DFcomb2$numUsers)
DFcomb2$numUsersTot <- as.numeric(DFcomb2$numUsersTot)
DFcomb2$numAdminsAll <- as.numeric(DFcomb2$numAdminsAll)
DFcomb2$numAdmins <- as.numeric(DFcomb2$numAdmins)
DFcomb2$numAdminsTot <- as.numeric(DFcomb2$numAdminsTot)
DFcomb2$numMergedPulls <- as.numeric(DFcomb2$numMergedPulls)
DFcomb2$stars <- as.numeric(DFcomb2$stars)
DFcomb2$forks <- as.numeric(DFcomb2$forks)
DFcomb2$releases <- as.numeric(DFcomb2$releases)
DFcomb2$speed <- as.numeric(DFcomb2$speed)
DFcomb2$age <- as.numeric(DFcomb2$age)
DFcomb2[is.na(DFcomb2$age),]$age <- 0

DFcomb2$time_bin <- as.POSIXct(DFcomb2$time_bin) 
DFcomb2$time_bin_new <- as.character(floor_date(as.POSIXlt(DFcomb2$time_bin), bin))



DFusers <- DFusers[DFusers$repo %in% catDictKeys, ]  
DFlong <- DFlong[DFlong$repo %in% catDictKeys, ] 
pullDF <- pullDF[pullDF$repo %in% catDictKeys, ]  
DFcomb2 <- DFcomb2[DFcomb2$repo %in% catDictKeys, ]  

DFusers$repo <- factor(DFusers$repo)
DFlong$repo <- factor(DFlong$repo)
DFlong2011$repo <- factor(DFlong2011$repo)
pullDF$repo <- factor(pullDF$repo)
DFcomb2$repo <- factor(DFcomb2$repo)




#### plotten ####
#### figure 4.1 ####

DFtemp <- data.frame(time_bin = unique(DFcomb2$time_bin_new))
DFtemp$forkNum <- aggregate(DFcomb2$forks, by = list(DFcomb2$time_bin_new), sum)$x

pullDFmerge <- subset(pullDF, merged == 1)
pullDFmerge$t_event_bin<-as.POSIXct(pullDFmerge$t_event_bin) 
pullDFmerge2 <- pullDFmerge[c('merger', "t_event_bin", "repo")]



tempData <- ddply(pullDFmerge2, .(t_event_bin), function(x){
  uniqueUsers <- length(na.omit(unique(x$merger)))
  mergeCount <- dim(x)[1]
  data.frame(uniqueUserNum = uniqueUsers, mergeCount = mergeCount)
})
#ddply function is very useful. syntax: ddply(df, .(<columnName>), function(x){<what should function do?>})
#it takes dataframe df, splits it along <columnName>, applys function to resulting dataframe, and puts it back together
# DFtemp$uniqueUserNum <- na.omit(tempData[tempData$t_event_bin <"2017-01-01" & tempData$t_event_bin >"2013-01-01",]$uniqueUserNum) #old version

DFtemp$uniqueUserNum <- na.omit(tempData[!is.na(tempData$t_event_bin),]$uniqueUserNum)
DFtemp$mergeCount <- na.omit(tempData[!is.na(tempData$t_event_bin),]$mergeCount)


tempData <- ddply(pullDFmerge2, .(t_event_bin), function(x){
  uniqueRepos <- length(na.omit(unique(x$repo)))
  data.frame(uniqueRepoNum = uniqueRepos)
})

DFtemp$uniqueRepoNum <- na.omit(tempData[!is.na(tempData$t_event_bin),]$uniqueRepoNum)


DFtemp$time_bin <- as.POSIXct(DFtemp$time_bin)  #new

allDF <- DFtemp
allDF$month <- as.numeric(row.names(allDF))

a1 <- ggplot(data = subset(allDF, time_bin <"2017-01-01"))+
  geom_line(aes(x= time_bin, y = (forkNum))) +
  ylab(paste0("Forks\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()

a2 <- ggplot(data = subset(allDF, time_bin <"2017-01-01"))+
  # geom_line(aes(x= time_bin, y = cumsum(forkNum)))
  geom_line(aes(x= time_bin, y = (uniqueUserNum))) +
  ylab(paste0("Unique active users\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()

a3 <- ggplot(data = subset(allDF, time_bin <"2017-01-01"))+
  # geom_line(aes(x= time_bin, y = cumsum(forkNum)))
  geom_line(aes(x= time_bin, y = (uniqueRepoNum))) +
  ylab("Unique active repositories\nper month") + xlab("Time [Month]")+
  theme_classic()

a4 <- ggplot(data = subset(allDF, time_bin <"2017-01-01"))+
  # geom_line(aes(x= time_bin, y = cumsum(forkNum)))
  geom_line(aes(x= time_bin, y = (mergeCount))) +
  ylab(paste0("Unique active repositories\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()

a4dat <- subset(allDF, time_bin %in% as.POSIXct(c("2013-01-01", "2016-07-01")))


#http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page
p1a <- ggarrange(a1, a2, a3,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
p1a
if(saving == 1){ggsave(paste0("4a_", bin,".pdf"), width = 22.5, height = 15, units = "cm")}

p1b <- ggarrange(a1, a2, a3, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)
# ggsave(paste0("4a_", bin,".pdf"), width = 27, height = 9, units = "cm")


#### statistical testing 4.2.1 ####

allDF$centerMonth <- allDF$month - mean(allDF$month)

summary(lm(data = subset(allDF, time_bin <"2017-01-01"), forkNum ~ centerMonth))
summary(lm(data = subset(allDF, time_bin <"2017-01-01"), uniqueUserNum  ~ centerMonth))
summary(lm(data = subset(allDF, time_bin <"2017-01-01"), uniqueRepoNum ~ centerMonth))



#### figure 4b ####

pullDFmerge2 <- pullDFmerge[c('merger', "t_event_bin", "repo", "corporate")] 


tempData <- ddply(pullDFmerge2, .(t_event_bin), function(x){
  uniqueUsersCorp     <- length(na.omit(unique(x[x$corporate == TRUE, ]$merger))) 
  uniqueUsersNonCorp  <- length(na.omit(unique(x[x$corporate != TRUE, ]$merger))) 
  data.frame(uniqueUserNumCorp = uniqueUsersCorp, uniqueUserNumNonCorp = uniqueUsersNonCorp)
})


tempData
tempDataLong <- melt(tempData, id.vars =c("t_event_bin"))
# melt function very useful. Syntax: melt(df, id.vars = c(<columnNames>))
# it takes dataframe df, uses <columnName> to horizontally align values and takes other columnNames as dummy variables
# see: http://seananderson.ca/2013/10/19/reshape.html
tempDataLong$variable <- tempDataLong$variable == "uniqueUserNumCorp"

corpDF <- tempDataLong
corpDF$corp <- corpDF$variable
corpDF$variable <- NULL
corpDF$numUsers <- corpDF$value
corpDF$value <- NULL

tempData <- ddply(pullDFmerge2, .(t_event_bin), function(x){
  uniqueReposCorp     <- length(na.omit(unique(x[x$corporate == TRUE, ]$repo)))
  uniqueReposNonCorp  <- length(na.omit(unique(x[x$corporate != TRUE, ]$repo)))

  data.frame(uniqueRepoNumCorp = uniqueReposCorp  , uniqueRepoNumNonCorp  = uniqueReposNonCorp)
})
tempData
tempDataLong <- melt(tempData, id.vars =c("t_event_bin"))
tempDataLong$variable <- tempDataLong$variable == "uniqueRepoNumCorp"

corpDF$numRepos <- tempDataLong$value


tempData <- ddply(pullDFmerge2, .(t_event_bin), function(x){
  mergeCountCorp     <- dim(x[x$corporate == TRUE, ])[1]
  mergeCountNonCorp  <- dim(x[x$corporate == FALSE, ])[1]
  
  data.frame(mergeCountCorp = mergeCountCorp, mergeCountNonCorp = mergeCountNonCorp)
})
tempData
tempDataLong <- melt(tempData, id.vars =c("t_event_bin"))
tempDataLong$variable <- tempDataLong$variable == "mergeCountCorp"

corpDF$mergeCount <- tempDataLong$value

corpDF <- na.omit(corpDF[corpDF$t_event_bin < '2017-01-01' & corpDF$t_event_bin >="2013-01-01",])

# https://www.rdocumentation.org/packages/GGally/versions/1.3.2/topics/gglegend
ggally_points(data = subset(corpDF, t_event_bin <"2017-01-01"), 
              ggplot2::aes(x= t_event_bin, y = numUsers/numRepos, col = corp))
points_legend <- gglegend(ggally_points)
points_legend(data = subset(corpDF, t_event_bin <"2017-01-01"), 
              ggplot2::aes(x= t_event_bin, y = numUsers/numRepos, col = corp))
same_points_legend <- gglegend("points")
identical(
  attr(attr(points_legend, "fn"), "original_fn"),
  attr(attr(same_points_legend, "fn"), "original_fn")
)
custom_legend <- wrap(gglegend("points"), size = 6)
b0 <- custom_legend(data = subset(corpDF, t_event_bin <"2017-01-01"), 
                    ggplot2::aes(x= t_event_bin, y = numUsers/numRepos, col = corp))

DFtemp <- aggregate(DFcomb2$forks, by = list(DFcomb2$time_bin, DFcomb2$corporate), sum)
DFtemp <- aggregate(DFcomb2$forks, by = list(DFcomb2$time_bin_new, DFcomb2$corporate), sum) #new
DFtemp$Group.1 <- as.POSIXct(DFtemp$Group.1)  #new
tempDFfilled <- melt(dcast(subset(DFtemp, Group.1 <"2017-01-01"), Group.1 ~ Group.2, value.var = c("x")), id.vars = c("Group.1"), na.rm = FALSE)
corpDF$forkNum <- tempDFfilled[order(tempDFfilled$variable, decreasing = TRUE),]$value

b1b <- ggplot(data = subset(corpDF, t_event_bin <"2017-01-01"))+
  geom_line(aes(x= t_event_bin, y = forkNum, col = corp)) +
  # geom_smooth(aes(x= t_event_bin, y = forkNum,group = corp, col = corp), method = "lm") +
  ylab(paste0("Forks\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ theme(legend.position="none")+
  scale_colour_discrete(name  ="Corporate")

b2b <- ggplot(data = subset(corpDF, t_event_bin <"2017-01-01"))+
  geom_line(aes(x= t_event_bin, y = numUsers, col = corp)) +
  # geom_smooth(aes(x= t_event_bin, y = numUsers,group = corp, col = corp), method = "lm") +
  ylab(paste0("Unique Users\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ theme(legend.position="none")+
  scale_colour_discrete(name  ="Corporate")
# scale_colour_discrete(name  ="Corporate",labels=c("non corporate","corporate"))


b3b <- ggplot(data = subset(corpDF, t_event_bin <"2017-01-01"))+
  geom_line(aes(x= t_event_bin, y = numRepos, col = corp)) +
  # geom_smooth(aes(x= t_event_bin, y = numRepos,group = corp, col = corp), method = "lm") +
  ylab(paste0("Unique Repos\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ theme(legend.position="none")+
  scale_colour_discrete(name  ="Corporate")+ 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
# scale_colour_discrete(name  ="Corporate",labels=c("non corporate","corporate"))



p2b <- ggarrange(b1b, b2b, b3b, b0,
                labels = c("A", "B", "C", ""),
                ncol = 2, nrow = 2)
p2b
if(saving == 1){ggsave(paste0("4b_", bin,".pdf"), width = 22.5, height = 15, units = "cm", useDingbats=FALSE)}

#### statistical testing 4.2.2 ####
monthCol <- ddply(corpDF, .(t_event_bin), function(x){
  which(unique(corpDF$t_event_bin) == x$t_event_bin)
})
corpDF$month <- monthCol$V1
centerMonthDF <- ddply(corpDF, .(corp), function(x){
  centerMonth <- x$month - mean(x$month)
  data.frame(centerMonth = centerMonth)
})
corpDF$centerMonth <- centerMonthDF$centerMonth


summary(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == TRUE), forkNum ~ centerMonth))
summary(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == FALSE), forkNum ~ centerMonth))
summary(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == TRUE), numUsers  ~ centerMonth))
summary(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == FALSE), numUsers  ~ centerMonth))
summary(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == TRUE), numRepos ~ centerMonth))
summary(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == FALSE), numRepos ~ centerMonth))


apa.reg.table(lm(data = subset(corpDF, t_event_bin <"2017-01-01" & corp == TRUE), forkNum ~ centerMonth))
#### figure 4d ####
DFlong2011$event_time <- as.POSIXlt(DFlong2011$event_time, format= "%Y-%m-%d %H:%M:%S")
DFlong2011$time_bin <- as.character(floor_date(as.POSIXlt(DFlong2011$event_time), bin))
DFlong2011fork <- subset(DFlong2011, event_type == "ForkEvent")

start <- 2013
if(start == 2013){
  DFlongfork <-DFlong2011fork[DFlong2011fork$event_time >= "2013-01-01 CET",]
}else{
  DFlongfork <-DFlong2011fork
}
  
repoDFlong <- ddply(DFlongfork, .(time_bin), function(x){
  timePointDF <- aggregate(x$ones, by=list(x$repo), sum)
  timePointDF$totalForksTP <- sum(timePointDF$x)
  data.frame(repo = timePointDF$Group.1, numForks = timePointDF$x, totalForksTP = timePointDF$totalForksTP)
})

repoDFlongWide <- dcast(repoDFlong, time_bin ~ repo, value.var = "numForks")
repoDFlongNA <- melt(repoDFlongWide, id.vars = c("time_bin"), na.rm = FALSE)
repoDFlongNA <- rename(repoDFlongNA, c("variable"="repo", "value"="numForks"))
repoDFlongWide <- dcast(repoDFlong, time_bin ~ repo, value.var = "totalForksTP")
repoDFlongNA$totalForksTP <- melt(repoDFlongWide, id.vars = c("time_bin"), na.rm = FALSE)$value
repoDFlongNA[is.na(repoDFlongNA$totalForksTP), ]$totalForksTP <- 100000000
repoDFlongNA[is.na(repoDFlongNA$numForks), ]$numForks <- 0
repoDFlongNA$relForks <- repoDFlongNA$numForks/repoDFlongNA$totalForksTP 

bigplayers <- unique(repoDFlongNA[repoDFlongNA$relForks > 0.1,]$repo)

repoDFlongNA[repoDFlongNA$repo %in% as.vector(bigplayers),]

p3a <- ggplot(data = subset(repoDFlongNA, repo %in% bigplayers & time_bin <"2017-01-01"))+
  geom_line(aes(x= as.POSIXct(time_bin), y = relForks, group = repo, col = repo)) +
  ylab(paste0("Relaitve Fork Number\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ 
  scale_colour_discrete(name  ="Repositories")

ggplot(data = subset(repoDFlongNA, repo %in% bigplayers & time_bin <"2017-01-01"))+
  geom_line(aes(x= as.POSIXct(time_bin), y =(numForks), group = repo, col = repo)) +
  ylab(paste0("Forks\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ 
  scale_colour_discrete(name  ="Repositories")+theme()


finalBin <- max(repoDFlongNA[repoDFlongNA$time_bin < "2017-01-01",]$time_bin)
offsets <- "                                         "
if(start == 2013){
  pp2 <- ggpie(subset(repoDFlongNA, repo %in% bigplayers & time_bin == "2013-01-01"), by='repo', totals='relForks', legendFlag = FALSE)
  pp3 <- ggpie(subset(repoDFlongNA, repo %in% bigplayers & time_bin == finalBin), by='repo', totals='relForks', legendFlag = FALSE)
  
  pp2 <- ggarrange(pp2, ggparagraph(text = paste0(offsets,"2013-01-01"), face = "italic", size = 11, color = "black"), 
            ncol = 1, nrow = 2,
            heights = c(1, 0.1))
  pp3 <- ggarrange(pp3, ggparagraph(text = paste0(offsets,finalBin), face = "italic", size = 11, color = "black"), 
                   ncol = 1, nrow = 2,
                   heights = c(1, 0.1))
  
  p3b <- ggarrange(pp2, pp3,
                   labels = c("B", "C"),
                   ncol = 2, nrow = 1)
}else{
  pp1 <- ggpie(subset(repoDFlongNA, repo %in% bigplayers & time_bin == "2011-07-01"), by='repo', totals='relForks', legendFlag = FALSE)
  pp2 <- ggpie(subset(repoDFlongNA, repo %in% bigplayers & time_bin == "2013-01-01"), by='repo', totals='relForks', legendFlag = FALSE)
  pp3 <- ggpie(subset(repoDFlongNA, repo %in% bigplayers & time_bin == finalBin), by='repo', totals='relForks', legendFlag = FALSE)
  
  pp1 <- ggarrange(pp1, ggparagraph(text = paste0(offsets,"2011-07-01"), face = "italic", size = 11, color = "black"), 
                   ncol = 1, nrow = 2,
                   heights = c(1, 0.1))
  pp2 <- ggarrange(pp2, ggparagraph(text = paste0(offsets,"2013-01-01"), face = "italic", size = 11, color = "black"), 
                   ncol = 1, nrow = 2,
                   heights = c(1, 0.1))
  pp3 <- ggarrange(pp3, ggparagraph(text = paste0(offsets,finalBin), face = "italic", size = 11, color = "black"), 
                   ncol = 1, nrow = 2,
                   heights = c(1, 0.1))
  
  
  p3b <- ggarrange(pp1, pp2, pp3,
                   labels = c("B", "C", "D"),
                   ncol = 3, nrow = 1)
}


subset(repoDFlongNA, time_bin == finalBin & repo %in% bigplayers)

p3 <- ggarrange(p3a, p3b,
               labels = c("A", ""),
               ncol = 1, nrow = 2)
p3
if(saving == 1){ggsave(paste0("4d_", bin,".pdf"), width = 22.5, height = 15, units = "cm")}


#### figure 4c ####
repoDFusers <- ddply(pullDFmerge2, .(t_event_bin), function(x){
  uniqueUsersRepo <- ddply(x, .(repo), function(x){
    uniqueUsers <- length(na.omit(unique(x$merger)))
    mergeCount  <- dim(x)[1] #
    data.frame(userNum = uniqueUsers, mergeCount = mergeCount)
    # data.frame(userNum = uniqueUsers)
  })
  data.frame(uniqueUserNumRepo = uniqueUsersRepo)
})

repoDFusers <- rename(repoDFusers, c("uniqueUserNumRepo.repo"="repo", "uniqueUserNumRepo.userNum"="numUsers", "uniqueUserNumRepo.mergeCount"="mergeCount"))
repoDFusersWide <- dcast(repoDFusers, t_event_bin ~ repo, value.var = "numUsers")
repoDFusersNA <- melt(repoDFusersWide, id.vars = c("t_event_bin"), na.rm = FALSE)
repoDFusersNA <- rename(repoDFusersNA, c("variable"="repo", "value"="numUsers"))
repoDFusersNA[is.na(repoDFusersNA$numUsers), ]$numUsers <- 0

repoDFusersWideb <- dcast(repoDFusers, t_event_bin ~ repo, value.var = "mergeCount")
repoDFusersNAb <- melt(repoDFusersWideb, id.vars = c("t_event_bin"), na.rm = FALSE)
repoDFusersNAb <- rename(repoDFusersNAb, c("variable"="repo", "value"="mergeCount"))
repoDFusersNA$mergeCount <- repoDFusersNAb$mergeCount
repoDFusersNA[is.na(repoDFusersNA$mergeCount), ]$mergeCount <- 0


d1 <- ggplot(data = subset(repoDFlongNA, time_bin <"2017-01-01"))+
  geom_line(aes(x= as.POSIXct(time_bin), y =(numForks), group = repo, col = repo)) +
  ylab(paste0("Forks\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ theme(legend.position="none")+
  scale_colour_discrete(name  ="Repositories")+theme()

d2 <- ggplot(data = subset(repoDFusersNA, t_event_bin <"2017-01-01"))+
  geom_line(aes(x= as.POSIXct(t_event_bin), y = numUsers, group = repo, col = repo)) +
  ylab(paste0("Unique Users\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ theme(legend.position="none")+
  scale_colour_discrete(name  ="Repositories")

d4 <- ggplot(data = subset(repoDFusersNA, t_event_bin <"2017-01-01"))+
  geom_line(aes(x= as.POSIXct(t_event_bin), y = mergeCount, group = repo, col = repo)) +
  ylab(paste0("Merge Count\nper ",bin)) + xlab(paste0("Time [",bin,"]"))+
  theme_classic()+ theme(legend.position="none")+
  scale_colour_discrete(name  ="Repositories")

# https://www.rdocumentation.org/packages/GGally/versions/1.3.2/topics/gglegend
ggally_points(data = subset(repoDFusersNA, t_event_bin <"2017-01-01"), ggplot2::aes(x= as.POSIXct(t_event_bin), y = numUsers, group = repo, col = repo))
points_legend <- gglegend(ggally_points)
points_legend(data = subset(repoDFusersNA, t_event_bin <"2017-01-01"), ggplot2::aes(x= as.POSIXct(t_event_bin), y = numUsers, group = repo, col = repo))
same_points_legend <- gglegend("points")
identical(
  attr(attr(points_legend, "fn"), "original_fn"),
  attr(attr(same_points_legend, "fn"), "original_fn")
)
custom_legend <- wrap(gglegend("points"), size = 6)
d3 <- custom_legend(data = subset(repoDFusersNA, t_event_bin <"2017-01-01"), ggplot2::aes(x= as.POSIXct(t_event_bin), y = numUsers, group = repo, col = repo))


ggplot(data = subset(repoDFusersNA, t_event_bin ==finalBin))+
  geom_density(aes(x=numUsers))
ggplot(data = subset(repoDFlongNA, time_bin ==finalBin))+
  geom_density(aes(x=numForks))


p4 <- ggarrange(d1, d2,d3,
                labels = c("A", "B", ""),
                ncol = 3, nrow = 1)
p4

p4b <- ggarrange(d1, d2,d4,d3,
                labels = c("A", "B", "C", "D"),
                ncol = 2, nrow = 2)
# p4b

p4
if(saving == 1){ggsave(paste0("4c_", bin,".pdf"), width = 15*2, height = 5*2, units = "cm", useDingbats=FALSE)}

#### Statistical Modelling ####
#### Data integration ####
# new binning
control_bin <- bin
control_bin <- 'month'

DFcombInt <- DFcomb2
DFcombInt$time_bin_new <- as.character(floor_date(as.POSIXlt(DFcombInt$time_bin), control_bin))

DFcombInt2 <- ddply(DFcombInt, .(time_bin_new), function(x){
  aggregate(x[-c(1,13,15,16,17)], by=list(x$repo), mean)
  aggregate(x[-c(1,14,16,17,18)], by=list(x$repo), mean)
})

DFcombInt2 <- rename(DFcombInt2, c("Group.1" = "repo", "time_bin_new" = "control_bin", 
                                   "numUsers" = "userCount","numUsersTot" = "userAllCount", "numAdmins" = "adminCount", 
                                   "numAdminsAll" = "adminAllCount", "numAdminsTot" = "adminTotCount",
                                   "numMergedPulls" = "mergeCount",
                                   "aveDeltaT" = "avgDeltaT", "speed" = "avgSpeed", "aveAdd" = "avgAdd", 
                                   "forks" = "forkCount", "stars" = "starCount", "releases" = "releaseCount"))
DFcombInt2 <- DFcombInt2[c(1,2,15,3,4,5,6,7, 8, 12,13,14,9,10,11)]
DFcombInt2$control_bin <- as.POSIXct(DFcombInt2$control_bin)

DFusersInt <- DFusers
DFusersInt$control_bin <- as.character(floor_date(as.POSIXlt(DFusersInt$t_close), control_bin))
DFusersInt$control_bin <- as.POSIXct(DFusersInt$control_bin)

DFusersInt$pull_id <- factor(DFusersInt$pull_id)
DFusersInt$ID <- 1:dim(DFusersInt)[1]

comments_final <- ddply(DFusersInt[-c(14,15,20,21)], .(ID), function(x){
  max(x$comments, x$comments_manual)
})

DFusersInt$comments <- comments_final$V1
DFusersInt2 <- DFusersInt[c(21,2,19,6,7,8,9,10,13,11,16,17)]

#### DFTest generation ####
DFTest <- merge(DFusersInt2, DFcombInt2, by=c("control_bin", "repo"))
DFTest$speed <- 1/as.numeric(DFTest$deltaT)
DFTest$avgData <-ddply(DFTest, .(control_bin), function(x){
  ddply(x, .(repo), function(y){
    len_combi <- dim(y)[1]
    flagVec <- c(1, integer(len_combi - 1))
    data.frame(shortDF = flagVec)
  })
})$shortDF == 1


DFTest$adminCountSq <- DFTest$adminCount^2
DFTest$ageSq <- DFTest$age^2
DFTest$concentration <- DFTest$adminTotCount/DFTest$userAllCount#### change!!!!
DFTest$concentrationSq <- DFTest$concentration^2
DFTest$mergeDensity <-DFTest$mergeCount/DFTest$adminTotCount
DFTest$mergeDensitySq <- DFTest$mergeDensity^2
DFTest$logSpeed <- log(DFTest$speed) #changed from log(x+1)
DFTest$logAvgSpeed <- log(DFTest$avgSpeed) #changed from log(x+1)
DFTest$logForkCount <- log(DFTest$forkCount) 
DFTest[is.infinite(DFTest$logForkCount), ]$logForkCount <- 0
DFTest$logStarCount <- log(DFTest$starCount) 
DFTest[is.infinite(DFTest$logStarCount), ]$logStarCount <- 0
DFTest$logAdd <- log(DFTest$add) 
DFTest[is.infinite(DFTest$logAdd), ]$logAdd <- 0
DFTest$logAdd <- log(DFTest$add) 
DFTest[is.infinite(DFTest$logAdd), ]$logAdd <- 0
DFTest$logComm<- log(DFTest$comm) 
DFTest[is.infinite(DFTest$logComm), ]$logComm <- 0
DFTest$logComments<- log(DFTest$comments) 
DFTest[is.infinite(DFTest$logComments), ]$logComments <- 0
DFTest$logReleaseCount<- log(DFTest$releaseCount) 
DFTest[is.infinite(DFTest$logReleaseCount), ]$logReleaseCount <- 0

dftemp <- ddply(subset(pullDF[c(2,3,4,17)], t_event_bin < "2017-01-01"), .(t_event_bin), function(x){
  ddply(x, .(repo), function(y){
    closes <- sum(subset(y, t_event_bin == y$t_event_bin & repo == y$repo & closed == 1)$closed)
    merges <- sum(subset(y, t_event_bin == y$t_event_bin & repo == y$repo & closed == 1)$merged)
    data.frame(control_bin = unique(y$t_event_bin), repo = unique(y$repo), mergeRatio = merges/closes)
  })
})
dftemp$t_event_bin <- NULL

DFTest <- merge(DFTest, dftemp, by=c("control_bin", "repo"))


dTest <- dict()
dTest[['davisking/dlib']] <- 'corporate'
dTest[['elki-project/elki']] <- 'academic'
dTest[['encog/encog-java-core']] <- 'academic'
dTest[['h2oai/h2o-3']] <- 'corporate'
dTest[['numpy/numpy']] <- 'academic'     
dTest[['numenta/nupic']] <- 'corporate'
dTest[['deeplearning4j/deeplearning4j']] <- 'corporate'
dTest[['openai/gym']] <- 'corporate'
dTest[['Artelnics/OpenNN']] <- 'corporate'
dTest[['biolab/orange3']] <- 'academic'
dTest[['scikit-learn/scikit-learn']] <- 'academic'
dTest[['shogun-toolbox/shogun']] <- 'academic'
dTest[['tensorflow/tensorflow']] <- 'corporate'
dTest[['Theano/Theano']] <- 'academic'
dTest[['torch/torch7']] <- 'academic'
dTest[['fchollet/keras']] <- 'corporate' # developed by google engineer, first on its own
dTest[['mlpack/mlpack']] <- 'academic'
dTest[['BVLC/caffe']] <- 'academic'
dTest[['NervanaSystems/neon']] <- 'corporate'

catDictTest <-dTest
catDictTestValues <- unlist(catDictTest$values())
catDictTestKeys <- unlist(catDictTest$keys())

DFTest <- subset(DFTest, repo %in% catDictTestKeys)
DFTest <- subset(DFTest, control_bin < "2017-01-01")

#standardized variables
#https://stats.stackexchange.com/questions/7112/when-and-how-to-use-standardized-explanatory-variables-in-linear-regression
DFTestScale <- DFTest
DFTestScale$logSpeed <- scale(DFTestScale$logSpeed)
DFTestScale$add <- scale(DFTestScale$add)
DFTestScale$comm <- scale(DFTestScale$comm)
DFTestScale$comments <- scale(DFTestScale$comments)
DFTestScale$concentration <- scale(DFTestScale$concentration)
DFTestScale$concentrationSq <- scale(DFTestScale$concentrationSq)
DFTestScale$userCount <- scale(DFTestScale$userCount)
DFTestScale$userAllCount <- scale(DFTestScale$userAllCount)
DFTestScale$adminCount <- scale(DFTestScale$adminCount)
DFTestScale$adminAllCount <- scale(DFTestScale$adminAllCount)
DFTestScale$adminTotCount <- scale(DFTestScale$adminTotCount)
DFTestScale$adminCountSq <- scale(DFTestScale$adminCountSq)
DFTestScale$mergeCount <- scale(DFTestScale$mergeCount)
DFTestScale$mergeDensity <- scale(DFTestScale$mergeDensity)
DFTestScale$mergeDensitySq <- scale(DFTestScale$mergeDensitySq)
DFTestScale$mergeRatio <- scale(DFTestScale$mergeRatio)
DFTestScale$forkCount<- scale(DFTestScale$forkCount)
DFTestScale$starCount<- scale(DFTestScale$starCount)
DFTestScale$avgSpeed <- scale(DFTestScale$avgSpeed) 
DFTestScale$logAvgSpeed <- scale(DFTestScale$logAvgSpeed)
DFTestScale$logAdd <- scale(DFTestScale$logAdd)
DFTestScale$logComm <- scale(DFTestScale$logComm)
DFTestScale$logComments <- scale(DFTestScale$logComments)
DFTestScale$logForkCount<- scale(DFTestScale$logForkCount)
DFTestScale$logStarCount<- scale(DFTestScale$logStarCount)

DFTestSub<- subset(DFTest, avgData == TRUE)
DFTestSum <- aggregate(DFTestSub[-c(1,2,3,4,5,12)], by = list(DFTestSub$repo), mean)
DFTestSum <- rename(DFTestSum, c("Group.1" = "repo", "Group.2" = control_bin))


#### actual Analysis ####
#### Organizational variables ####

dp1 <- ggplot(DFTestSub)+
  geom_boxplot(aes(x=corporate, y = adminTotCount, fill = corporate))+
  coord_flip()+
  stat_summary(aes(x=corporate, y = adminTotCount, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  xlab("Repository") + ylab("Active admins")+
  theme_classic()+
  theme(legend.position = "none")+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

dp2 <- ggplot(DFTestSub)+
  geom_boxplot(aes(x=corporate, y = concentration, fill = corporate))+
  coord_flip()+
  stat_summary(aes(x=corporate, y = concentration, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  xlab("Repository") + ylab("Admin concentration")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dp3 <- ggplot(DFTestSub)+
  geom_boxplot(aes(x=corporate, y = mergeDensity, fill = corporate))+
  coord_flip()+
  stat_summary(aes(x=corporate, y = mergeDensity, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  xlab("Repository") + ylab("Workload per admin")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dp4 <- ggplot(DFTestSub)+
  geom_boxplot(aes(x=corporate, y = mergeRatio, fill = corporate))+
  coord_flip()+
  stat_summary(aes(x=corporate, y = mergeRatio, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  xlab("Repository") + ylab("Merge ratio")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dp5 <- ggplot(DFTestSub)+
  geom_boxplot(aes(x=corporate, y = mergeRatio, fill = corporate))+
  coord_flip()+
  stat_summary(aes(x=corporate, y = adminTotCount, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  xlab("Repository") + ylab("Workload per admin")+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p1a <- ggarrange(dp1, dp2, dp3,dp4,
                 labels = c("A", "B", "C", "D"),
                 ncol = 4, nrow = 1)
p1aB <- ggarrange(dp1, dp2, dp3,dp5,
                 labels = c("A", "B", "C", "D"),
                 ncol = 4, nrow = 1)
p1a
repoNew1 <- with((subset(DFTest, avgData == TRUE)), reorder(repo, adminTotCount, median))
bp1 <- ggplot(subset(DFTest, avgData == TRUE), aes(repoNew1, adminTotCount, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(repoNew1, adminTotCount, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  # scale_y_continuous(breaks = c(5, 10, 15, 20), labels = c( '5', '10.0', '15', '20'))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  theme(legend.position = "none")+
  xlab("Repository") + ylab("Active admins")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  coord_flip()

repoNew2 <- with((subset(DFTest, avgData == TRUE)), reorder(repo, concentration, median))
bp2 <- ggplot(subset(DFTest, avgData == TRUE), aes(repoNew1, concentration, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(repoNew1, concentration, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "none")+
  xlab("Repository") + ylab("Admin concentration")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) +
  coord_flip()

repoNew3 <- with((subset(DFTest, avgData == TRUE)), reorder(repo, mergeDensity, median))
bp3 <- ggplot(subset(DFTest, avgData == TRUE), aes(repoNew1, mergeDensity, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(repoNew1, mergeDensity, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # scale_y_continuous(breaks = c(0, 10, 20, 40), labels = c( '0', '10.0', '20', '40'))+
  xlab("Repository") + ylab("Workload per admin")+
  theme(legend.position = "none")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) +
  coord_flip()

repoNew4 <- with((subset(DFTest, avgData == TRUE)), reorder(repo, mergeRatio, median))
bp4 <- ggplot(subset(DFTest, avgData == TRUE), aes(repoNew1, mergeRatio, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(repoNew1, mergeRatio, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("Repository") + ylab("Merge ratio")+
  theme(legend.position = "none")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  coord_flip()


p1b <- ggarrange(bp1, bp2, bp3,bp4,
                 labels = c("E", "F", "G", "H"),
                 ncol = 4, nrow = 1)

p1b

p1 <- ggarrange(p1a, p1b,
                labels = c("", ""),
                ncol = 1, nrow = 2,
                heights = c(0.8, 1.8))
p1
# ggsave(paste0("4_orga", bin,".pdf"), width = 22.5, height = 22.5, units = "cm", useDingbats=FALSE)
if(saving == 1){ggsave(paste0("4e_", bin,".pdf"), width = 22.5*2, height = 22.5, units = "cm")}

t.test(DFTestSub$adminTotCount ~ DFTestSub$corporate) #
t.test(DFTestSub$concentration ~ DFTestSub$corporate) #no sig difference
t.test(DFTestSub$mergeDensity ~ DFTestSub$corporate) #more individual commits -> better coding discipline?
t.test(DFTestSub$mergeRatio ~ DFTestSub$corporate) #more discussion in non-corporate

sd(subset(DFTestSub, corporate == TRUE)$adminTotCount)
sd(subset(DFTestSub, corporate == FALSE)$adminTotCount)
sd(subset(DFTestSub, corporate == TRUE)$concentration)
sd(subset(DFTestSub, corporate == FALSE)$concentration)
sd(subset(DFTestSub, corporate == TRUE)$mergeDensity)
sd(subset(DFTestSub, corporate == FALSE)$mergeDensity)
sd(subset(DFTestSub, corporate == TRUE)$mergeRatio)
sd(subset(DFTestSub, corporate == FALSE)$mergeRatio)

anova(lm(data = subset(DFTestSub), adminTotCount ~ corporate))
anova(lm(data = subset(DFTestSub, corporate == TRUE), adminTotCount ~ repo))
anova(lm(data = subset(DFTestSub, corporate == FALSE), adminTotCount ~ repo))

anova(lm(data = DFTestSub, concentration ~ corporate))
anova(lm(data = subset(DFTestSub, corporate == TRUE), concentration ~ repo))
anova(lm(data = subset(DFTestSub, corporate == FALSE), concentration ~ repo))

anova(lm(data = DFTestSub, mergeDensity ~ corporate))
anova(lm(data = subset(DFTestSub, corporate == TRUE), mergeDensity ~ repo))
anova(lm(data = subset(DFTestSub, corporate == FALSE), mergeDensity ~ repo))

anova(lm(data = DFTestSub, mergeRatio ~ corporate))
anova(lm(data = subset(DFTestSub, corporate == TRUE), mergeRatio ~ repo))
anova(lm(data = subset(DFTestSub, corporate == FALSE), mergeRatio ~ repo))

#### Process Variables ####

a4 <- ggplot(DFTest)+ 
  geom_density(aes(speed), linetype="blank", fill="lightblue")+ 
  scale_x_continuous(limits = c(0,0.0025))+ #scale originally to .5
  xlab("Speed") + ylab("Density")+
  theme_classic()

b4 <- ggplot(DFTest)+ 
  geom_density(aes(logSpeed), linetype="blank", fill="lightblue")+ 
  xlab("log(Speed)") + ylab("Density")+
  theme_classic()

c4 <- ggplot(DFTestScale)+ 
  geom_density(aes(logSpeed), linetype="blank", fill="lightblue")+ 
  xlab("scale(log(Speed))") + ylab("Density")+
  theme_classic()

ggarrange(a4, b4,c4, 
          labels = c("A", "B", "C"),
          nrow = 1, ncol = 3)
# ggsave(paste0("4f2_", bin,".pdf"), width = 22.5, height = 7.5, units = "cm")


DF_rem_out <- DFTest
DF_rem_out$speed <- remove_outliers(DF_rem_out$speed)
DF_rem_out$add <- remove_outliers(DF_rem_out$add)
DF_rem_out$comm <- remove_outliers(DF_rem_out$comm)
DF_rem_out$comments <- remove_outliers(DF_rem_out$comments)
DF_rem_out$logSpeed <- log(DF_rem_out$speed)
# DF_rem_out[is.infinite(DF_rem_out$logSpeed), ]$logSpeed <- 0
DF_rem_out$logAdd <- log(DF_rem_out$add)
DF_rem_out[is.infinite(DF_rem_out$logAdd), ]$logAdd <- 0
DF_rem_out$logComm <- log(DF_rem_out$comm)
DF_rem_out[is.infinite(DF_rem_out$logComm), ]$logComm <- 0
DF_rem_out$logComments <- log(DF_rem_out$comments)
DF_rem_out[is.infinite(DF_rem_out$logComments), ]$logComments <- 0

ggplot(DF_rem_out)+
  geom_density(aes(speed), linetype="blank", fill="lightblue")+ 
  xlab("scale(log(Speed))") + ylab("Density")+
  theme_classic()

ggplot(DF_rem_out, aes(x=corporate, y =speed))+
  stat_summary(fun.y="mean", geom="bar")+
  stat_summary(fun.data="mean_se", fun.args = list(mult=1), 
               geom="errorbar", color="red")

ggplot(DFTestScale, aes(x=corporate, y =logSpeed))+
  geom_boxplot()


t.test(DFTest$logSpeed ~ DFTest$corporate) 
t.test(DFTest$logAdd ~ DFTest$corporate) 
t.test(DFTest$logComm ~ DFTest$corporate) 
t.test(DFTest$logComments ~ DFTest$corporate) 

sd(DFTest[DFTest$corporate == TRUE,]$logSpeed)
sd(DFTest[DFTest$corporate == FALSE,]$logSpeed)
sd(DFTest[DFTest$corporate == TRUE,]$logAdd)
sd(DFTest[DFTest$corporate == FALSE,]$logAdd)
sd(DFTest[DFTest$corporate == TRUE,]$logComm)
sd(DFTest[DFTest$corporate == FALSE,]$logComm)
sd(DFTest[DFTest$corporate == TRUE,]$logComments)
sd(DFTest[DFTest$corporate == FALSE,]$logComments)


corTab <- cor(DF_rem_out[c(34,38,39,40, 30, 31, 32, 33)], use="complete.obs")
print(corTab)

d_ply(DF_rem_out, .(repo), function(x){
  corTab <- cor(x[c(34,38,39,40)], use="complete.obs")
  print(corTab)
})


corTab <- cor(DF_rem_out[c(6,8,10,26)], use="complete.obs")
print(corTab)

rcorr(as.matrix(DFTestScale[c(6,8,10,26)]), type="pearson") 

dp1 <- ggplot(DFTestScale)+
  geom_boxplot(aes(x=corporate, y = logSpeed, fill = corporate))+
  stat_summary(aes(x=corporate, y = logSpeed, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  coord_flip()+
  xlab("Repository") + ylab("scale(log(Speed))")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dp2 <- ggplot(DFTestScale)+
  geom_boxplot(aes(x=corporate, y = logAdd, fill = corporate))+
  stat_summary(aes(x=corporate, y = logAdd, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  coord_flip()+
  xlab("Repository") + ylab("scale(log(Add))")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dp3 <- ggplot(DFTestScale)+
  geom_boxplot(aes(x=corporate, y = logComm, fill = corporate))+
  stat_summary(aes(x=corporate, y = logComm, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  coord_flip()+
  xlab("Repository") + ylab("scale(log(Comm))")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dp4 <- ggplot(DFTestScale)+
  geom_boxplot(aes(x=corporate, y = logComments, fill = corporate))+
  stat_summary(aes(x=corporate, y = logComments, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  coord_flip()+
  xlab("Repository") + ylab("scale(log(Comments))")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p1a <- ggarrange(dp1, dp2, dp3,dp4,
                 labels = c("A", "B", "C", "D"),
                 ncol = 4, nrow = 1)

p1a

repoNew1 <- with(DFTestScale, reorder(repo, corporate, median))
bp1 <- ggplot(DFTestScale, aes(repoNew1, logSpeed, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(x=repoNew1, y = logSpeed, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  # scale_y_continuous(breaks = c(5, 10, 15, 20), labels = c( '5', '10.0', '15', '20'))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  theme(legend.position = "none")+
  xlab("Repository") + ylab("scale(log(Speed))")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  coord_flip()

bp2 <- ggplot(DFTestScale, aes(repoNew1, logAdd, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(x=repoNew1, y = logAdd, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "none")+
  xlab("Repository") + ylab("scale(log(Add))")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) +
  coord_flip()

bp3 <- ggplot(DFTestScale, aes(repoNew1, logComm, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(x=repoNew1, y = logComm, fill = corporate), 
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # scale_y_continuous(breaks = c(0, 10, 20, 40), labels = c( '0', '10.0', '20', '40'))+
  xlab("Repository") + ylab("scale(log(Comm))")+
  theme(legend.position = "none")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) +
  coord_flip()

bp4 <- ggplot(DFTestScale, aes(repoNew1, logComments, fill = corporate))+
  geom_boxplot()+theme_classic()+
  stat_summary(aes(x=repoNew1, y = logComments, fill = corporate),
               fun.y="mean", geom="point", shape = "x", col = "blue", size = 2)+
  theme(axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank()) +
  xlab("Repository") + ylab("scale(log(Comments))")+
  theme(legend.position = "none")+
  # theme(axis.text.x=element_text(angle=90,hjust=1)) +
  coord_flip()


p1b <- ggarrange(bp1, bp2, bp3,bp4,
                 labels = c("E", "F", "G", "H"),
                 ncol = 4, nrow = 1)

p1b

p1 <- ggarrange(p1a, p1b,
                labels = c("", ""),
                ncol = 1, nrow = 2,
                heights = c(0.8, 1.8))
p1

# ggsave(paste0("4_process", bin,".pdf"), width = 22.5, height = 22.5, units = "cm", useDingbats=FALSE)

anova(lm(data = DFTestScale, logSpeed ~ repo), lm(data = DFTestScale, logSpeed ~ corporate))


anova(lm(data = DFTestScale, logSpeed ~  repo + corporate  + logAdd + logComm + logComments + 
           concentration + concentrationSq +mergeDensity + mergeDensitySq + mergeRatio))

anova(lm(data = subset(DFTestScale), logSpeed ~ corporate))
anova(lm(data = subset(DFTestScale, corporate == TRUE), logSpeed ~ repo))
anova(lm(data = subset(DFTestScale, corporate == FALSE), logSpeed ~ repo))

anova(lm(data = DFTestScale, logAdd ~ corporate))
anova(lm(data = subset(DFTestScale, corporate == TRUE), logAdd ~ repo))
anova(lm(data = subset(DFTestScale, corporate == FALSE), logAdd ~ repo))

anova(lm(data = DFTestScale, logComm ~ corporate))
anova(lm(data = subset(DFTestScale, corporate == TRUE), logComm ~ repo))
anova(lm(data = subset(DFTestScale, corporate == FALSE), logComm ~ repo))

anova(lm(data = DFTestScale, logComments ~ corporate))
anova(lm(data = subset(DFTestScale, corporate == TRUE), logComments ~ repo))
anova(lm(data = subset(DFTestScale, corporate == FALSE), logComments ~ repo))

#### Regression: Part one DV speed ####

m1 <- felm(mergeRatio ~ concentration + mergeDensity  
           | repo, DFTestScale)


m1 <- felm(logSpeed ~ logAdd + logComm 
           | repo, DFTestScale)
m2 <- felm(logSpeed ~ logAdd + logComm  + logComments 
           | repo, DFTestScale)
m3 <- felm(logSpeed ~ logAdd + logComm  + logComments  + concentration 
           | repo, DFTestScale)
m4 <- felm(logSpeed ~ logAdd + logComm  + logComments  + concentration + concentrationSq  
           | repo, DFTestScale)
m5 <- felm(logSpeed ~ logAdd + logComm  + logComments  + concentration + concentrationSq + 
            mergeDensity | repo, DFTestScale)
m6 <- felm(logSpeed ~ logAdd + logComm  + logComments  + concentration + concentrationSq + 
            mergeDensity + mergeDensitySq | repo, DFTestScale)
m7 <- felm(logSpeed ~ logAdd + logComm  + logComments  + concentration + concentrationSq + 
            mergeDensity + mergeDensitySq + mergeRatio | repo, DFTestScale)
m8 <- felm(logSpeed ~  concentration + concentrationSq + 
             mergeDensity + mergeDensitySq + mergeRatio | repo, DFTestScale)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)


a <- summary(m1)
b <- summary(m2)
c <- summary(m3)
d <- summary(m4)
e <- summary(m5)
f <- summary(m6)
g <- summary(m7)


a$tTable[,c(1,2,5)]
b$tTable[,c(1,2,5)]
c$tTable[,c(1,2,5)]
d$tTable[,c(1,2,5)]


xx_val <- t(rbind(a$coefficients[,1],
                 b$coefficients[,1],
                 c$coefficients[,1],
                 d$coefficients[,1],
                 e$coefficients[,1],
                 f$coefficients[,1],
                 g$coefficients[,1]))

xx_err <- t(rbind(a$coefficients[,2],
                 b$coefficients[,2],
                 c$coefficients[,2],
                 d$coefficients[,2],
                 e$coefficients[,2],
                 f$coefficients[,2],
                 g$coefficients[,2]))

xx_pv <- t(rbind(a$coefficients[,4],
                 b$coefficients[,4],
                 c$coefficients[,4],
                 d$coefficients[,4],
                 e$coefficients[,4],
                 f$coefficients[,4],
                 g$coefficients[,4]))

Rsq <- t(rbind(a$adj.r.squared,
               b$adj.r.squared,
               c$adj.r.squared,
               d$adj.r.squared, 
               e$adj.r.squared,
               f$adj.r.squared,
               g$adj.r.squared))

matrix(as.vector(sapply((xx_pv),FUN = function(x) star(x))), nrow = 8, byrow = FALSE)



clip <- pipe("pbcopy", "w")  
write.table(Rsq,file = clip,sep="\t",row.names=FALSE,col.names=FALSE)
close(clip)
