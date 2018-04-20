# install.packages("igraph")
rm(list = ls())
library(igraph)
library(reshape2)
library(plyr)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#### new tries ####
load("DFusers.Rda")
vortexVec <- as.vector(unique(DFusers$repo))



testDF <- dcast(DFusers, opener ~ repo, value.var = "merged", fun.aggregate = sum)

X <- c()
edgeVec0 <- ddply(testDF, .(opener), function(x){
  y<- as.vector(x[1,])
  y[[1]]<- NULL
  y[as.list(y) == 0] <- NULL
  if(dim(y)[2] > 1){
    combinations <- c(combn(colnames(y), m=2))
  }else{
    combinations <- c("0")
  }
  data.frame(combis = combinations)
})

edgeVec <- edgeVec0
edgeVec <- edgeVec[edgeVec$combis != 0,]
edgeVec <- as.character(edgeVec$combis)
edgeVec

g3 <- graph(edgeVec, directed=F)
g3$weight <- 1
E(g3)
plot(g3)
edge_attr(g3)
E(g3)$weight <- 1

l <- layout_in_circle(g3)

g3s <- simplify(g3, remove.multiple = T, remove.loops = F, edge.attr.comb=c(weight="sum"))
E(g3s)$width <- E(g3s)$weight*0.8

# pdf("project_network.pdf")
#circle plot
plot(g3s, layout=l, vertex.color="orange", vertex.frame.color="white", vertex.label=V(g3s)$media, 
     vertex.label.color="black", vertex.label.cex=.7, vertex.label.dist=1.5,
     vertex.label.degree = -pi/2)
# dev.off()


# pdf("project_network2.pdf")
#cluster plot
plot(g3s, edge.arrow.size=.2, edge.curved=0, 
     vertex.color="orange", vertex.frame.color="#555555", 
     vertex.label=V(g3)$media, vertex.label.color="black", vertex.label.cex=.7)
# dev.off()

#http://www.kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf for more details

#### compare to Hars & Ou ####
#### function from internet ####
# https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

#### testing ####

m1 <- 8/41
sd1 <- sqrt(m1*(1-m1))
n1 <- 79

m2 <- length(unique(V(g3s))) / length(unique(DFusers$repo))
sd2 <- sqrt(m2*(1-m2))
n2 <- length(unique(DFusers$merger))

df <- n1 + n2 -2
t.test2(m1,m2,sd1,sd2,n1,n2,m0=0,equal.variance=FALSE)
m2


