
library(circlize)
library(dplyr)
library(tibble)
library(tidyr)
library(cmocean)


setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/data")
rt_c <- read.csv('rt_central.csv')
rt_ch <- rt_c[-4,c(2:3)]
rt_ch2 <- rt_c[-4,]

setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
png('rt_fei_rtlinks.png',height = 4, width = 6, units = 'in', res=300)
par(mar=c(4,7,1,1))
b <- barplot(t(rt_ch[nrow(rt_ch):1,]),
             beside=T, col=c('gray50','gray90'),horiz = T, las=1,
             names.arg = rep(NA,5),
             xlab='Number of links')
barplot(t(rt_ch2[nrow(rt_ch2):1,4:5]),
        space=c(1,2,2,2,2),
        add=T,horiz = T,
        col=c('white'),density=c(20,0),angle=90+45,
        names.arg = rep(NA,5),xaxt='n')
barplot(t(rt_ch2[nrow(rt_ch2):1,6:7]),
        space=c(2,2,2,2,2),
        add=T,horiz = T,
        col=c('gray20'),density=c(20,0),
        names.arg = rep(NA,5),xaxt='n')
legend('topright',c('Water Quality','Red tide'),fill=c('gray90','gray50'),bty='n')
axis(2,b[2,], rev(c('Destin','Panama City','St. Petersburg','Madeira Beach','Pine Island')),las=2,tick=F)
axis(2,b[1,], paste('n = ',rt_c$n[-4]),las=2,tick=F)
dev.off()


setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/data")
dat <- read.csv('NewTermMerged.csv',row.names = 1)


chordDiagram(as.matrix(dat), col = cols, annotationTrack = "grid", 
             link.lwd = lwd_mat, link.border = border_mat,
             directional = 1, direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow", diffHeight = -mm_h(.5),
             preAllocateTracks = list(track.height =.4))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = T, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important


ind <- c(57,union(which(dat[,57]>0),which(dat[57,]>0)))
dat <- dat[ind,ind]
dat <- dat[-8,-8]

dat[11,] <- dat[11,] + dat[6,]
dat[,11] <- dat[,11] + dat[,6]
dat <- dat[-6,-6]

cols <- matrix('gray50',nrow(dat),ncol(dat))
ind_rt <- which(names(dat)=='Red.Tide')
cols[ind_rt,c(which(dat[ind_rt,]>0))] <- 2
cols[c(which(dat[,ind_rt]>0)),ind_rt] <- 3

lwd_mat <- matrix(1,nrow(dat),ncol(dat))
ind_rt <- which(names(dat)=='Red.Tide')
lwd_mat[ind_rt,c(which(dat[ind_rt,]>0))] <- 1
lwd_mat[c(which(dat[,ind_rt]>0)),ind_rt] <- 1
row.names(lwd_mat) <- row.names(dat)
colnames(lwd_mat) <- colnames(dat)

border_mat <- matrix(NA,nrow(dat),ncol(dat))
ind_rt <- which(names(dat)=='Red.Tide')
border_mat[ind_rt,c(which(dat[ind_rt,]>0))] <- 1
border_mat[c(which(dat[,ind_rt]>0)),ind_rt] <- 1
row.names(border_mat) <- row.names(dat)
colnames(border_mat) <- colnames(dat)

ord <- c("Red.Tide","Hurricanes","Offshore.Winds","Currents","Loop.Current","Freshwater.Input","Sewage.Runoff",
         "Water.Quality","Thermocline","Water.Temperature","Habitat","Inshore.Target.Fish","Prey.Fish",
         "Fish.Distribution","Fish.Mortality","Snappers.Groupers","Lionfish","Sharks",
         "Commercial.Effort","Recreational.Effort","Tourism","Fishery.Management")
grid.col <- c('red',rep(4,9),rep(3,8),rep('purple',4))
row.names(dat)
colnames(dat)
identical(rownames(dat), colnames(dat))
ord

sort(union(rownames(dat), colnames(dat)))
sort(ord)

setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
png('rt_fei_rtchord.png',height = 8, width = 8, units = 'in', res=300)
par(mar=c(4,4,4,4),xpd=T)
circos.clear()
circos.par(gap.after = 2, start.degree = 90)
chordDiagram(as.matrix(dat), col = cols, annotationTrack = "grid", 
             link.lwd = lwd_mat, link.border = border_mat,
             directional = 1, direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow", diffHeight = -mm_h(.5),
             preAllocateTracks = list(track.height =.4),
             order = ord, grid.col = grid.col)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = T, adj = c(0, 0.5))
  }, bg.border = NA) # here set bg.border to NA is important
dev.off()
  
  



destin <- read.csv('destin.csv',row.names = 1)
madbch <- read.csv('madbch.csv',row.names = 1)
panamacity <- read.csv('panamacity.csv',row.names = 1)
stpete1 <- read.csv('stpete1.csv',row.names = 1)
stpete2 <- read.csv('stpete2.csv',row.names = 1)
pi <- read.csv('pi.csv',row.names = 1)

#REPLACE NA WITH ZEROS
destin[is.na(destin)] <- 0
madbch[is.na(madbch)] <- 0
panamacity[is.na(panamacity)] <- 0
stpete1[is.na(stpete1)] <- 0
stpete2[is.na(stpete2)] <- 0
pi[is.na(pi)] <- 0

#REPLACE -1 WITH 1
destin[(destin)<0] <- 1
madbch[(madbch)<0] <- 1
panamacity[(panamacity)<0] <- 1
stpete1[(stpete1)<0] <- 0
stpete2[(stpete2)<0] <- 0
pi[(pi)<0] <- 0

#MAKE ROW AND COLUMN NAMES THE SAME
row.names(destin) <- colnames(destin)
row.names(madbch) <- colnames(madbch)
row.names(panamacity) <- colnames(panamacity)
row.names(stpete1) <- colnames(stpete1)
row.names(stpete2) <- colnames(stpete2)
row.names(pi) <- colnames(pi)

g <- graph.adjacency(as.matrix(destin), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(madbch), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.sp1), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.sp1), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.pi), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.pc), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)


#GET A LIST OF ALL OF THE VARIABLE NAMES
matixList <- list(destin, madbch, panamacity,stpete1, stpete2, pi)
variablesOld <- sort(unique(unlist(lapply(matixList, colnames))))

#GET A LIST OF ALL THE VARIABLE NAMES AND WHERE THEY CAME FROM
variablesDetailed <- data.frame(variablesOld)
variablesDetailed$destin <- as.numeric(variablesOld %in% names(destin))
variablesDetailed$madbch <- as.numeric(variablesOld %in% names(madbch))
variablesDetailed$panamacity <- as.numeric(variablesOld %in% names(panamacity))
variablesDetailed$stpete1 <- as.numeric(variablesOld %in% names(stpete1))
variablesDetailed$stpete2 <- as.numeric(variablesOld %in% names(stpete2))
variablesDetailed$pi <- as.numeric(variablesOld %in% names(pi))

write.csv(variablesDetailed, "1variableSources.csv", row.names = FALSE)

#READ IN LOOK UP TABLE
newNames <- read.csv("2variablerename.csv", stringsAsFactors = F)
names(newNames)
newNames$test <- 1

#CHECK THAT ALL OLD NAMES ARE IN THE LOOK UP TABLE
summary(variablesOld %in% newNames$variablesAll)

#CREATE FUNCTION TO RENAME VARIABLES
reName <- function(model, var) {
  #PREP MERGE FILE TO HAVE A SINGLE MERGIN VARIABLE
  useNames <- newNames[, colnames(newNames) %in% c("variablesAll", var)] 
  names(useNames)[names(useNames) == var] <- "varUse"
  #RENAME ROWS 
  mrow <- model
  mrow$rowName <- row.names(mrow)
  # names(mrow)[7] <- 'variablesAll'
  # merge(mrow,useNames,by='variablesAll',all.x=T)
  # merge(useNames,mrow,by='variablesAll',all=T)
  mrow <- left_join(mrow, useNames, by = c("rowName" = "variablesAll"))
  #SUM VALUES FOR IDENTICAL NAMES
  mrowSums <- mrow %>%
    select(-rowName) %>%
    group_by(varUse) %>%
    summarise_all(list(sum)) %>%
    data.frame
  row.names(mrowSums) <- mrowSums$varUse
  mrowDone <- select(mrowSums, -varUse)

  #RENAME COLUMNS
  mcol <- as.data.frame(t(mrowDone))
  mcol$colName <- row.names(mcol)
  mcol <- left_join(mcol, useNames, by = c("colName" = "variablesAll"))
  #SUM VALUES FOR IDENTICAL NAMES
  mcolSums <- mcol %>%
    select(-colName) %>%
    group_by(varUse) %>%
    summarise_all(list(sum)) %>%
    data.frame
  row.names(mcolSums) <- mcolSums$varUse
  mcolDone <- select(mcolSums, -varUse)
  mDone <- as.data.frame(t(mcolDone))
}

#FUNCTION TO CREATE MATRICES THAT HAVE THE VARIABLES ACROSS ALL MODELS
matchMatrix <- function(model, variablesAll) {
  #IDENTIFY WHICH VARIABLES ARE MISSING FROM EACH MODEL
  missing <- variablesAll[!variablesAll %in% names(model)]
  #CREATE EMPTY MATRIX FOR VARIABLES MISSING AS ROWS
  emptyRows <- matrix(ncol = length(names(model)), nrow = length(missing))
  #NAME THE ROWS AND COLUMNS AND ZERO VALUES FOR CONNECTIONS
  rownames(emptyRows) <- missing
  colnames(emptyRows) <- names(model)
  emptyRows[is.na(emptyRows)] <- 0
  newRows <- rbind(model, emptyRows)
  #CREATE EMPTY MATRIX FOR VARIABLES MISSING AS ROWS
  emptyCols <- matrix(ncol = length(missing), nrow = length(row.names(newRows)))
  #NAME THE ROWS AND COLUMNS AND ZERO VALUES FOR CONNECTIONS
  rownames(emptyCols) <- row.names(newRows) 
  colnames(emptyCols) <- missing
  emptyCols[is.na(emptyCols)] <- 0
  #BIND EMPTY MATRICES TO THE ORIGINAL MODEL
  final   <- cbind(newRows, emptyCols)
  final   <- final[order(colnames(final)),order(colnames(final))]
  #EXPORT THE FINAL MODEL
  final
}

#RUN FUNCTIONS WITH NEW.TERM
destin2 <- reName(destin, "New.Term")
madbch2 <- reName(madbch, "New.Term")
panamacity2 <- reName(panamacity, "New.Term")
stpete1.2 <- reName(stpete1, "New.Term")
stpete2.2 <- reName(stpete2, "New.Term")
pi2 <- reName(pi, "New.Term")


ntList <- list(destin2, madbch2, panamacity2, stpete1.2, stpete2.2, pi2)
ntVariables <- sort(unique(unlist(lapply(ntList, colnames))))

nt.destin<- matchMatrix(destin2, ntVariables)
nt.madbch <- matchMatrix(madbch2, ntVariables)
nt.pc <- matchMatrix(panamacity2, ntVariables)
nt.sp1 <- matchMatrix(stpete1.2, ntVariables)
nt.sp2 <- matchMatrix(stpete2.2, ntVariables)
nt.pi <- matchMatrix(pi2, ntVariables)


nt.merged <- nt.destin + nt.madbch
nt.merged <- nt.merged   + nt.pc
nt.merged <- nt.merged   + nt.sp1
nt.merged <- nt.merged   + nt.sp2
nt.merged <- nt.merged   + nt.pi

g <- graph.adjacency(as.matrix(nt.merged), mode="directed", weighted=TRUE)
sort(degree(g))
sort(betweenness(g))
sort(closeness(g,mode='out'))
sort(evcent(g)$vector)
sort(page_rank(g, directed = TRUE)$vector)
sort(alpha_centrality(g))
sort(power_centrality(g,exponent=-2))
sort(harmonic_centrality(g,mode='out'))
sort(authority_score(g)$vector)
sort(hub_score(g)$vector)

g <- graph.adjacency(as.matrix(nt.destin), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.madbch), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.sp1), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.sp1), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.pi), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)
g <- graph.adjacency(as.matrix(nt.pc), mode="directed", weighted=TRUE)
sort(degree(g))
sort(alpha_centrality(g))
sort(evcent(g)$vector)


# https://jokergoo.github.io/circlize_book/book/
  
data_long <- nt.merged %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

circos.clear()
circos.par(start.degree = 90)
chordDiagram(data_long, order=c("Hurricanes","Sahara.Dust","Onshore.winds","Currents","Loop.Current","Nearshore.Temperature","Water.Temperature","Freshwater.Input","Thermocline","Sewage.Runoff","Water.Quality","Red.Tide",
                                "Habitat","Prey.Fish","Fish.Distribution","Fish.Migration","Fish.Mortality","Predators","Fishery.Managment",
                                "Fishing.Effort"))

chordDiagram(data_long, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(data_long))))),
             order=c("Hurricanes","Sahara.Dust","Onshore.winds","Currents","Loop.Current","Nearshore.Temperature","Water.Temperature","Freshwater.Input","Thermocline","Sewage.Runoff","Water.Quality","Red.Tide",
                     "Habitat","Prey.Fish","Fish.Distribution","Fish.Migration","Fish.Mortality","Predators","Fishery.Managment",
                     "Fishing.Effort"))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important



#### other


library(circlize)
library(tibble)
library(tidyr)

setwd('~/Downloads/rt_cmaps/m1StartingMatrices')
fils <- list.files()

mats <- lapply(fils,read.csv,row.names=1)

for(i in 1:5){
  mats[[i]][is.na(mats[[i]])] <- 0
  row.names(mats[[i]]) <- colnames(mats[[i]])
  assign(paste0('mod',i),mats[[i]])
  }

#GET A LIST OF ALL OF THE VARIABLE NAMES
mats
variablesOld <- sort(unique(unlist(lapply(mats, colnames))))

#GET A LIST OF ALL THE VARIABLE NAMES AND WHERE THEY CAME FROM
variablesDetailed <- data.frame(variablesOld)
for(i in 1:5){
  variablesDetailed[,i+1] <- as.numeric(variablesOld %in% names(mats[[i]]))
}
variablesDetailed$sum <-apply(variablesDetailed[,2:6],1,sum)

write.csv(variablesDetailed, "1variableSources2.csv", row.names = FALSE)

#READ IN LOOK UP TABLE
setwd('~/Downloads/rt_cmaps')
newNames <- read.csv("3VariableRenamingMergefile2.csv", stringsAsFactors = F)
names(newNames)

#CHECK THAT ALL OLD NAMES ARE IN THE LOOK UP TABLE
summary(variablesOld %in% newNames$variablesAll)

mod1.2 <- reName(mod1, "Umbrella.Term")
mod2.2 <- reName(mod2, "Umbrella.Term")
mod3.2 <- reName(mod3, "Umbrella.Term")
mod4.2 <- reName(mod4, "Umbrella.Term")
mod5.2 <- reName(mod5, "Umbrella.Term")

ntList <- list(mod1.2, mod2.2, mod3.2, mod4.2, mod5.2)
ntVariables <- sort(unique(unlist(lapply(ntList, colnames))))

nt.mod1 <- matchMatrix(mod1.2, ntVariables)
nt.mod2 <- matchMatrix(mod2.2, ntVariables)
nt.mod3 <- matchMatrix(mod3.2, ntVariables)
nt.mod4 <- matchMatrix(mod4.2, ntVariables)
nt.mod5 <- matchMatrix(mod5.2, ntVariables)

nt.merged <- nt.mod1 + nt.mod2
nt.merged <- nt.merged   + nt.mod3
nt.merged <- nt.merged   + nt.mod4
nt.merged <- nt.merged   + nt.mod5

nt.mat <- as.matrix(nt.merged)
diag(nt.mat) <- 0

library(igraph)
g2 <- graph_from_adjacency_matrix(nt.mat, weighted = T)
g1 <- graph.adjacency(nt.mat, mode="directed", weighted=TRUE)
co <- layout_with_fr(g1)
plot(simplify(g1))
plot(simplify(g2))
plot(g1, edge.arrow.size=.5, edge.arrow.width=.5, edge.width=E(g1)$weight, margin=0, vertex.size= 5, edge.arrow.size=1)
plot(g2, edge.arrow.size=.5, edge.arrow.width=.5, edge.width=E(g2)$weight, layout=layout_in_circle)
summary(g2)

sort(degree(g1))
sort(betweenness(g1))
sort(closeness(g1))
sort(evcent(g1)$vector)
sort(page_rank(g1, directed = TRUE)$vector)
sort(alpha_centrality(g1))
sort(power_centrality(g1,exponent=-2))

nt.mat2 <- nt.mat[-c(1,2,31),-c(1,2,31)]
g3 <- graph.adjacency(nt.mat2, mode="directed", weighted=TRUE)
plot(g3,
     edge.arrow.size=.5, edge.arrow.width=.5, 
     edge.width=E(g3)$weight,vertex.size= 10, edge.arrow.size=1,
     layout=layout_with_fr)

sort(degree(g3))
sort(betweenness(g3))
sort(closeness(g3,mode='out'))
sort(evcent(g3)$vector)
sort(page_rank(g3, directed = TRUE)$vector)
sort(alpha_centrality(g3))
sort(power_centrality(g3,exponent=-2))
sort(harmonic_centrality(g3,mode='out'))
sort(authority_score(g3)$vector)
sort(hub_score(g3)$vector)

cen <- rank(alpha_centrality(g3))
# cen <- rank(closeness(g3))
cols <- cmocean('amp')(length(cen))

V(g3)$color <- cols[cen]
plot(g3)


i=2
for(i in 1:5){
  nt.mat <- as.matrix(mats[[i]])
  nt.mat[is.na(nt.mat)] <- 0
  nt.mat[(nt.mat)<0] <- 1
  g1 <- graph.adjacency(nt.mat, mode="directed", weighted=TRUE)
  ind <- grep('Red.Tide',rownames(nt.mat),ignore.case = T)
  print(fils[i])
  print(c((degree(g1,mode='in'))[ind],
  (degree(g1,mode='out'))[ind]))
  # print(c((degree(g1,mode='total'))[ind],
  # (betweenness(g1))[ind],
  # (closeness(g1,mode='total'))[ind],
  # (authority_score(g1)$vector)[ind],
  # (hub_score(g1)$vector)[ind]))
}
