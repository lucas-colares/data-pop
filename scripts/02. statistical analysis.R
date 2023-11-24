# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## Functional diversity in human song ###########
# # # # # # # # # # By: Lucas Colares # # # # # # # # # #
# # # # # # # # # # # # 07-08-2023 # # # # # # # ## # # # 

source("scripts/00. setup.R")

#################### Statistical Analysis ########################
read.csv("datasets/Streams.csv",row.names = 1,encoding = "latin1")->Streams
read.csv("datasets/Traits.csv",row.names = 1,encoding = "latin1")->Traits
read.csv("datasets/Genres.csv",row.names = 1,encoding = "latin1")->Genres
write.csv(data.frame(table(Genres$Genres)),"datasets/final genius tags.csv")
read.csv("results/functional diversity.csv",encoding = "latin1")->FinalIndex
read.table("datasets/careerTime.txt", h=T,row.names = 1,encoding = "latin1")->Time
FinalIndex$Artists->Time$col1
colnames(Time)=c("Artist", "Time")
log10(FinalIndex$Richness)->FinalIndex$Richness

#### Plot functional space ####
#### For the top and bottom 5 artists
paste(Traits$X.Artist.,"Alb",Traits$X.AlbumNo.,Traits$X.Song., sep = "")->rownames(Traits)
paste(Genres$Artist,"Alb",Genres$AlbumNo,Genres$Song, sep = "")->Genres$ID

ALBS<-tapply(rep(1, length(Genres$Genres)), list(Genres$ID, Genres$Genres) 
             , min,default = 0)

merge(Traits[,6:ncol(Traits)], ALBS[,colSums(ALBS)>nrow(ALBS)*5/100],by=0)->FinalTraits

decostand(FinalTraits[,c(2,6:14)], "standardize")->stand
cbind(stand,FinalTraits[,c(3:5,15:ncol(FinalTraits))])->TraitsStand

colnames(TraitsStand)=c("Duration","Acousticness", "Danceability", "Energy", "Instrumentalness","Liveliness", "Loudness", "Speechiness", "Valence", "Tempo", "Key", "Mode", "Time_Signature",gsub("_", " ", colnames(TraitsStand)[14:length(colnames(TraitsStand))]))

# Do PCA
respca1 <- prcomp(TraitsStand,scale. = T)
write.csv(as.data.frame(summary(respca1)[[6]]),"results/pca variance.csv")
# Extract PC axes for plotting
data.frame(respca1$x[,1:4], Artist=gsub("_"," ",sub("Alb.*", "", FinalTraits$Row.names)))->PCA1
PCAvalues1 <- data.frame(Artists = PCA1$Artist, respca1$x)
TPDs_Pop1 <- TPDs(species = PCA1$Artist, PCA1[,1:2],alpha = 0.99)
PCAloadings1 <- data.frame(Variables = rownames(respca1$rotation), respca1$rotation)
write.csv(PCAloadings1,"results/loadings pca.csv",row.names = F)
SelLoad1<-PCAloadings1[abs(PCAloadings1[,2])>0.3|abs(PCAloadings1[,3])>0.3,]

TPD_grid1 <- TPDs_Pop1$data$evaluation_grid
TPD_probs1 <- TPDs_Pop1$TPDs[1:length(unique(TPDs_Pop1$data$species))]
mat1 <- cbind(TPD_grid1, TPD_probs1)

for(i in names(mat1)[3:length(names(mat1))]){
  mat1[mat1[,i] == 0,i] <- NA
}

TPDEst1<-{}
for(i in 3:ncol(mat1)){
  data.frame(na.omit(mat1[,c(1:2,i)]),Artist=names(mat1)[i])->TPDEst1[[length(TPDEst1)+1]]
  colnames(TPDEst1[[length(TPDEst1)]])=c("PC1","PC2","prob","Artist")
}
data.frame(do.call("rbind",TPDEst1))->TPDEst1

rbind(data.frame(TPDEst1,Index="Richness"),data.frame(TPDEst1,Index="Evenness"),data.frame(TPDEst1,Index="Divergence"))->TPDAll
TPDAll$Pos<-TPDAll$Artist
for(x in unique(TPDAll$Index)){
  rev(FinalIndex$Artists[order(FinalIndex[,x])])->ORD
  d<-0
  for(y in ORD){
    d=d+1
    TPDAll[TPDAll$Index==x,]$Pos[grep(paste0("^",y,"$"),TPDAll[TPDAll$Index==x,]$Pos)]<-d
  }
}

levels(reorder(factor(FinalIndex$Artists),FinalIndex$Richness))[c(1:5,96:100)]->rich_selArt
levels(reorder(factor(FinalIndex$Artists),FinalIndex$Evenness))[c(1:5,96:100)]->even_selArt
levels(reorder(factor(FinalIndex$Artists),FinalIndex$Divergence))[c(1:5,96:100)]->div_selArt

rbind(rbind(data.frame(TPDEst1[grep(paste0(paste0("^",rich_selArt,"$"),collapse = "|"),TPDEst1$Artist),],Index="Richness")),
      rbind(data.frame(TPDEst1[grep(paste0(paste0("^",even_selArt,"$"),collapse = "|"),TPDEst1$Artist),],Index="Evenness")),
      rbind(data.frame(TPDEst1[grep(paste0(paste0("^",div_selArt,"$"),collapse = "|"),TPDEst1$Artist),],Index="Divergence")))->TPDEst

# Plot
rbind(data.frame(SelLoad1[,1:4],Index="Richness"),data.frame(SelLoad1[,1:4],Index="Evenness"),data.frame(SelLoad1[,1:4],Index="Divergence"))->SelLoad1

TPDEst$Pos<-NA
for(x in unique(TPDEst$Index)){
  if(x=="Richness"){
    rank<-rev(rich_selArt)
  }
  if(x=="Evenness"){
    rank<-rev(even_selArt)
  }
  if(x=="Divergence"){
    rank<-rev(div_selArt)
  }
  for(y in rank){
    TPDEst[TPDEst$Index==x,][TPDEst[TPDEst$Index==x,]$Artist==y,]$Pos<-grep(y,rank) 
  }
}

factor(TPDEst$Index,levels=c("Richness","Evenness","Divergence"))->TPDEst$Index

TPDAll[grep(paste0("^",c(1:5,96:100),"$",collapse = "|"),TPDAll$Pos),]->TPDAll_Menos

p<-ggplot() + 
  geom_contour_filled(data = TPDAll_Menos,aes(x = PC1,y = PC2,z = prob,color=Artist,alpha=..level..))+
  facet_grid(Pos~Index);p

contours <- layer_data(p)

AA<-{}
for(y in unique(TPDAll_Menos$Index)){
  TPDAll_Menos[TPDAll_Menos$Index==y,]->SelIndex
  for(x in unique(SelIndex$Artist)){
    SelIndex[SelIndex$Artist==x,]->SelArt
    p2<-ggplot() + 
      geom_contour_filled(data = SelArt,aes(x = PC1,y = PC2,z = prob,alpha=..level..));p2
    contours<-layer_data(p2)
    contours[contours$piece==1,]->contours2
    
    AA[[length(AA)+1]]<-data.frame(x=mean(contours2$x),y=mean(contours2$y),Index=y,Pos=unique(SelArt$Pos),Artist=x)
  }
}
do.call("rbind",AA)->AA

rbind(data.frame(SelLoad1,Pos="1st"),data.frame(SelLoad1,Pos="2nd"),data.frame(SelLoad1,Pos="3rd"),data.frame(SelLoad1,Pos="4th"),data.frame(SelLoad1,Pos="5th"),data.frame(SelLoad1,Pos="96th"),data.frame(SelLoad1,Pos="97th"),data.frame(SelLoad1,Pos="98th"),data.frame(SelLoad1,Pos="99th"),data.frame(SelLoad1,Pos="100th"))->SelLoad2

gsub("^Danceability$","Dance.",SelLoad2$Variables)->SelLoad2$Variables
gsub("^Speechiness$","Speech.",SelLoad2$Variables)->SelLoad2$Variables
gsub("^Loudness$","Loud.",SelLoad2$Variables)->SelLoad2$Variables
gsub("^Acousticness$","Acoustic.",SelLoad2$Variables)->SelLoad2$Variables

gsub("^1$","1st",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^2$","2nd",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^3$","3rd",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^4$","4th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^5$","5th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^96$","96th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^97$","97th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^98$","98th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^99$","99th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos
gsub("^100$","100th",TPDAll_Menos$Pos)->TPDAll_Menos$Pos

gsub("^1$","1st",AA$Pos)->AA$Pos
gsub("^2$","2nd",AA$Pos)->AA$Pos
gsub("^3$","3rd",AA$Pos)->AA$Pos
gsub("^4$","4th",AA$Pos)->AA$Pos
gsub("^5$","5th",AA$Pos)->AA$Pos
gsub("^96$","96th",AA$Pos)->AA$Pos
gsub("^97$","97th",AA$Pos)->AA$Pos
gsub("^98$","98th",AA$Pos)->AA$Pos
gsub("^99$","99th",AA$Pos)->AA$Pos
gsub("^100$","100th",AA$Pos)->AA$Pos

tiff("raw_figures/Figure 2.tiff", units="in", width=5, height=10, res=900)
{ggplot() + 
    geom_contour_filled(data = TPDAll_Menos,aes(x = PC1,y = PC2,z = prob,fill=..level..,color=..level..,alpha=..level..),linewidth=0.25)+
    geom_point(data=AA,aes(x=x,y=y),shape=22,size=1.5,color="white",fill="black",alpha=0.75)+
    geom_vline(xintercept=0,linetype="dashed",alpha=0.4)+
    geom_hline(yintercept=0,linetype="dashed",alpha=0.4)+
    geom_segment(data = SelLoad2, aes(x = 0, y = 0, xend = (PC1*8), yend = (PC2*8)), arrow = arrow(length = unit(1/8, "picas")),color = "black",alpha=0.5) +
    geom_text_repel(data=SelLoad2,aes(x = (SelLoad2$PC1*8), y = (SelLoad2$PC2*8)), label = SelLoad2$Variables, color="black", fontface="italic", size=2,alpha=0.8)+
    geom_text(data=AA,mapping = aes(label=Artist),x=max(TPDAll_Menos$PC1)+3.25,y=min(TPDAll_Menos$PC2)+0.6,fontface="bold.italic",family="sans",size=2.2,hjust=1)+
    facet_grid(factor(Pos,levels=c("1st","2nd","3rd","4th","5th","96th","97th","98th","99th","100th"))~factor(Index,levels=c("Richness","Evenness","Divergence")))+
    scale_alpha_manual(values = seq(0.3,1,0.1))+
    scale_fill_brewer(palette = "Spectral",direction = -1)+
    scale_color_brewer(palette = "Spectral",direction = -1)+
    scale_x_continuous(limits=c(min(TPDAll_Menos$PC1),(max(TPDAll_Menos$PC2)+3)))+
    theme_bw()+
    guides(alpha="none",color="none", fill=guide_legend(paste0("Artists")))+
    theme(strip.text = element_text(size=12,family="sans",face = "bold"),strip.background = element_rect(fill="white"),legend.text.align = 0,legend.title.align = 0,legend.justification = "top",legend.key.size = unit(0.5,"cm"),legend.position ="none", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = ggtext::element_markdown(family = "sans",size=12,face="bold"), legend.text = ggtext::element_markdown(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))
}
dev.off()

####### For one dimension at a time #####
#### For the first axis of the PCA
Means<-{}
for(x in unique(TPDAll$Artist)){
  Means[[length(Means)+1]]<-data.frame(Mean=median(TPDAll[TPDAll$Artist==x,1],na.rm = T),Artist=x)
}
do.call("rbind",Means)->Means

rbind(data.frame(Means,Index="Richness"),data.frame(Means,Index="Evenness"),data.frame(Means,Index="Divergence"))->Means2
Means2$Pos<-NA
for(x in unique(Means2$Index)){
  Means2[Means2$Index==x,]$Pos<-match(Means2[Means2$Index==x,]$Artist,FinalIndex[order(FinalIndex[,x],decreasing = T),]$Artists)
}
paste0(Means2$Pos,"th")->Means2$Pos

OrderFacet<-paste0(1:100,"th")
paste0(TPDAll$Pos,"th")->TPDAll$Pos

for(x in seq(1,100,10)[-2]){
  gsub(paste0("^",x,"th","$"),paste0(x,"st"),TPDAll$Pos)->TPDAll$Pos
  gsub(paste0("^",x,"th","$"),paste0(x,"st"),OrderFacet)->OrderFacet
  gsub(paste0("^",x,"th","$"),paste0(x,"st"),Means2$Pos)->Means2$Pos
}
for(x in seq(2,100,10)[-2]){
  gsub(paste0("^",x,"th","$"),paste0(x,"nd"),TPDAll$Pos)->TPDAll$Pos
  gsub(paste0("^",x,"th","$"),paste0(x,"nd"),OrderFacet)->OrderFacet
  gsub(paste0("^",x,"th","$"),paste0(x,"nd"),Means2$Pos)->Means2$Pos
}
for(x in seq(3,100,10)[-2]){
  gsub(paste0("^",x,"th","$"),paste0(x,"rd"),TPDAll$Pos)->TPDAll$Pos
  gsub(paste0("^",x,"th","$"),paste0(x,"rd"),OrderFacet)->OrderFacet
  gsub(paste0("^",x,"th","$"),paste0(x,"rd"),Means2$Pos)->Means2$Pos
}

BB<-aggregate(TPDAll$prob,list(TPDAll$Pos,TPDAll$Artist,TPDAll$Index),max)
colnames(BB)=c("Pos","Artist","Index","probMax")

TPDAll$MeanTrait<-NA
for(x in unique(TPDAll$Artist)){
  TPDAll[TPDAll$Artist==x,]$MeanTrait<-mean(TPDAll[TPDAll$Artist==x,]$PC1,na.rm=T)
}

tiff("raw_figures/Figure 1.tiff", units="in", width=8/1.5, height=9/1.2, res=900)
ggplot() +
  ggpolypath::geom_polypath(data = TPDAll,mapping=aes(x=PC1,y = prob,fill=MeanTrait,color=MeanTrait),size = 0.2)+
  geom_vline(data=Means2,aes(xintercept=Mean),linetype="solid",alpha=0.8,size=0.15)+
  scale_y_continuous(breaks = NULL) +
  labs(x = "Functional trait (1st axis of a PCA)", y = "Ranked artists") +
  facet_grid(factor(Pos,levels=OrderFacet) ~ factor(Index,levels=c("Richness","Evenness","Divergence")), switch = "y",scales="free") +
  scale_fill_gradient(low = "#D6604D",high = "#4393C3")+
  scale_color_gradient(low = "#D6604D",high = "#4393C3")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(),strip.background.y = element_blank(),strip.background.x = element_rect(color="white",fill="white"),strip.text.x = element_text(size=12,family="sans",face = "bold"), strip.text.y.left = element_text(size=5,family="sans",angle = 0, hjust = 1),panel.spacing.y = unit(-0.1, "cm"),panel.spacing.x = unit(0.5, "cm"),legend.text.align = 0,legend.title.align = 0,legend.justification = "top",legend.key.size = unit(0.5,"cm"),legend.position ="none", axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12,angle=90), axis.title = element_text(face="bold",family = "sans", size = 14))
dev.off()

tiff("raw_figures/Figure 1_Alt.tiff", units="in", width=8/1.5, height=9/1.2, res=900)
ggplot() +
  ggpolypath::geom_polypath(data = TPDAll,mapping=aes(x=PC1,y = prob,fill=MeanTrait,color=MeanTrait),size = 0.2)+
  geom_text(data=BB,aes(label=Artist),y=max(TPDAll$prob)-0.002,x=-Inf,size=1.4,hjust=0,fontface="italic",)+
  geom_vline(data=Means2,aes(xintercept=Mean),linetype="solid",alpha=0.8,size=0.15)+
  scale_y_continuous(breaks = NULL) +
  labs(x = "Functional trait (1st axis of a PCA)", y = "Ranked artists") +
  facet_grid(factor(Pos,levels=OrderFacet) ~ factor(Index,levels=c("Richness","Evenness","Divergence")), switch = "y")+#,scales="free") +
  scale_fill_gradient(low = "#D6604D",high = "#4393C3")+
  scale_color_gradient(low = "#D6604D",high = "#4393C3")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(),strip.background.y = element_blank(),strip.background.x = element_rect(color="white",fill="white"),strip.text.x = element_text(size=12,family="sans",face = "bold"), strip.text.y.left = element_text(size=5,family="sans",angle = 0, hjust = 1),panel.spacing.y = unit(-0.1, "cm"),panel.spacing.x = unit(0.5, "cm"),legend.text.align = 0,legend.title.align = 0,legend.justification = "top",legend.key.size = unit(0.5,"cm"),legend.position ="none", axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12,angle=90), axis.title = element_text(face="bold",family = "sans", size = 14))
dev.off()

#### NStreams ~ Funtional diveristy (Artists) ####
Streams[(Streams$Platform=="Spotify"),]->StreamsNoDz
aggregate(StreamsNoDz$Streams, list(StreamsNoDz$X), sum, na.rm=T)->STR
FinalIndex$Streams<-STR$x

StreamsTest=data.frame(FinalIndex,Time=Time$Time)
decostand(StreamsTest[,c(1:3,6)],method = "standardize")->StreamsTest[,c(1:3,6)]

lm(log10(Streams)~Richness+Evenness+Divergence+Time+Richness:Time+Evenness:Time+Divergence:Time,data=StreamsTest)->LM1
step(LM1,direction = "both")->stepLM1
write.csv(stepLM1$anova,file = "results/stepwise streams ~ artists.csv")
write.csv(summary(stepLM1)[[4]],file = "results/model coefs streams ~ artists.csv")
summary(stepLM1)

data.frame(reshape2::melt(FinalIndex[,1:4]),Streams=FinalIndex$Streams,Time=Time$Time)->LMPlot

tiff("raw_figures/Figure 3.tiff", units="in", width=11, height=3, res=900)
ggplot(LMPlot,aes(x=value, y=log10(Streams)))+
  geom_point(size=2,aes(color=Time))+
  geom_smooth(data=LMPlot[LMPlot$variable=="Richness",],method="lm",formula = y~poly(x,1),color="black")+
  ggh4x::facet_grid2(. ~ factor(variable,levels=c("Richness","Evenness","Divergence")), scales = "free_x")+
  theme_bw()+
  scale_color_gradient(low="#D6604D",high="#4393C3",name="Career\nlaunch")+
  geom_text_repel(label=LMPlot$Artists, color="black",alpha=0.7, fontface ="italic",size=2,min.segment.length = 0.1,max.overlaps = 6)+
  scale_y_continuous(name="Number of streams",labels = format(c(10^9,10^9.5,10^10,10^10.5),scientific = T,digits = 2),sec.axis = dup_axis(name="Estimated profit",labels=rev(c("$138M","$44M","$14M","$4M"))))+
  scale_x_continuous(name="Functional diversity")+
  theme(panel.spacing.x = unit(0.3, "cm"),strip.text = element_text(size=12,family="sans",face = "bold"),strip.background = element_rect(fill="white"), legend.text.align = 0,legend.title.align=0,legend.position = "right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(face="bold", family = "sans", colour = "black", size=12), legend.text = element_text(family ="sans", size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))
dev.off()

tiff("raw_figures/Figure S3.tiff", units="in", width=5.5, height=3.75, res=900)
ggplot(mapping = aes(x=Time$Time, y=log10(FinalIndex$Streams)))+
  geom_point(size=2)+
  geom_smooth(method="lm",formula = y~poly(x,1),color="black")+
  theme_bw()+
  geom_text_repel(label=FinalIndex$Artists, color="black",alpha=0.7, fontface ="italic",size=2,min.segment.length = 0.1,max.overlaps = 6)+
  scale_y_continuous(name="Number of streams",labels = format(c(9^10,9.5^10,10^10,10.5^10),scientific = T,digits = 2))+
  scale_x_continuous(name="Career launch")+
  theme(plot.margin = margin(0,0.5,0,0, "cm"),axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))
dev.off()

##### Streams ~ Traits #####
### For artists
Streams[Streams$Platform=="Spotify",]->StreamsNoDz
aggregate(StreamsNoDz$Streams, list(StreamsNoDz$X), sum, na.rm=T)->STR

aggregate(FinalTraits[,2:ncol(FinalTraits)],list(gsub("*Alb..*","",FinalTraits$Row.names)),median)->ArtTrait
ArtTrait_LMdata=ArtTrait

data.frame(scale(ArtTrait_LMdata[,c(2,6:14)]))->ArtTrait_LMdata[,c(2,6:14)]

write.csv(cor(ArtTrait_LMdata[,2:ncol(ArtTrait_LMdata)]),"results/correlation matrix - artist traits.csv")
data.frame(ArtTrait_LMdata,Streams=STR$x,Time=Time$Time)->ArtTrait_LMdata

lm(log10(Streams)~X.duration_ms.+X.key.+X.mode.+X.danceability.+X.instrumentalness.+X.liveliness.+X.energy.+X.speechiness.+X.valence.+X.tempo.+Country+Pop+Pop.Rock+R.B+Rock+Trap,data=ArtTrait_LMdata)->ArtLM
step(ArtLM)->ArtLM_step
summary(ArtLM_step)
write.csv(ArtLM_step$anova,file = "results/stepwise streams ~ traits of artists.csv")
write.csv(summary(ArtLM_step)[[4]],file = "results/model coefs streams ~ traits of artists.csv")

s=summary(ArtLM_step)

confint(ArtLM_step)->CI

## For albums
Streams[Streams$Platform=="Spotify",]->StreamsNoDz
aggregate(StreamsNoDz$Streams, list(paste0(StreamsNoDz$X,"Alb",StreamsNoDz$AlbumNo)), sum, na.rm=T)->STR2

aggregate(FinalTraits[,2:ncol(FinalTraits)],list(gsub("*Track..*","",FinalTraits$Row.names)),median)->AlbTrait
AlbTrait_LMdata=AlbTrait

data.frame(scale(AlbTrait_LMdata[,c(2,6:14)]))->AlbTrait_LMdata[,c(2,6:14)]

write.csv(cor(AlbTrait_LMdata[,2:ncol(AlbTrait_LMdata)]),"results/correlation matrix - albums traits.csv")

STR2[match(AlbTrait$Group.1,STR2$Group.1),]->STR2
data.frame(AlbTrait_LMdata,Streams=STR2$x)->AlbTrait_LMdata

lm(log10(Streams)~X.duration_ms.+X.key.+X.mode.+X.danceability.+X.instrumentalness.+X.liveliness.+X.energy.+X.speechiness.+X.valence.+X.tempo.+Country+Pop+Pop.Rock+R.B+Rock+Trap,data=AlbTrait_LMdata)->AlbLM
step(AlbLM,direction = "both")->AlbLM_step
summary(AlbLM_step)
write.csv(AlbLM_step$anova,file = "results/stepwise streams ~ traits of albums.csv")
write.csv(summary(AlbLM_step)[[4]],file = "results/model coefs streams ~ traits of albums.csv")

s2=summary(AlbLM_step)

confint(AlbLM_step)->CI2

### For Tracks
Streams[Streams$Platform=="Spotify",]->StreamsNoDz
aggregate(StreamsNoDz$Streams, list(paste0(StreamsNoDz$X,"Alb",StreamsNoDz$AlbumNo,StreamsNoDz$Song)), sum, na.rm=T)->STR3

aggregate(FinalTraits[,2:ncol(FinalTraits)],list(FinalTraits$Row.names),median)->TrackTrait
TrackTrait_LMdata=TrackTrait

data.frame(scale(TrackTrait_LMdata[,c(2,6:14)]))->TrackTrait_LMdata[,c(2,6:14)]

write.csv(cor(TrackTrait_LMdata[,2:ncol(TrackTrait_LMdata)]),"results/correlation matrix - track traits.csv")

STR3[match(TrackTrait$Group.1,STR3$Group.1),]->STR3
data.frame(TrackTrait_LMdata,Streams=STR3$x)->TrackTrait_LMdata

lm(log10(Streams+1)~X.duration_ms.+X.key.+X.mode.+X.time_signature.+X.danceability.+X.energy.+X.instrumentalness.+X.liveliness.+X.speechiness.+X.valence.+X.tempo.+Country+Pop+Pop.Rock+R.B+Rock+Trap,data=TrackTrait_LMdata)->TraitLM3

step(TraitLM3,direction = "both")->TraitLM3
write.csv(TraitLM3$anova,file = "results/stepwise streams ~ traits of songs.csv")
write.csv(summary(TraitLM3)[[4]],file = "results/model coefs streams ~ traits of songs.csv")

s3=summary(TraitLM3)
s3
confint(TraitLM3)->CI3

ArtCoef<-data.frame(Variable=names(s$coefficients[-1,1]),Estimate=s$coefficients[-1,1],CI[-1,],Level="Artist")
AlbCoef<-data.frame(Variable=names(s2$coefficients[-1,1]),Estimate=s2$coefficients[-1,1],CI2[-1,],Level="Album")
TrackCoef<-data.frame(Variable=names(s3$coefficients[-1,1]),Estimate=s3$coefficients[-1,1],CI3[-1,],Level="Track")

rbind(ArtCoef,AlbCoef,TrackCoef)->Coefz
gsub("*.*\\$","",Coefz$Variable)->Coefz$Variable
gsub("X.","",Coefz$Variable)->Coefz$Variable
gsub("\\.","",Coefz$Variable)->Coefz$Variable
gsub("&","",Coefz$Variable)->Coefz$Variable
gsub("-","",Coefz$Variable)->Coefz$Variable
gsub("`","",Coefz$Variable)->Coefz$Variable

LabelOrder<-rev(c(unique(Coefz$Variable)[order(unique(Coefz$Variable))][-c(1,8:11,15)],unique(Coefz$Variable)[order(unique(Coefz$Variable))][c(1,8:11,15)]))
LabelOrder<-capitalize(LabelOrder)
gsub("_ms","",LabelOrder)->LabelOrder
gsub("_"," ",LabelOrder)->LabelOrder
gsub("PopRock","Pop-Rock",LabelOrder)->LabelOrder
gsub("RB","R&B",LabelOrder)->LabelOrder

Coefz$Variable<-capitalize(Coefz$Variable)
gsub("_ms","",Coefz$Variable)->Coefz$Variable
gsub("_"," ",Coefz$Variable)->Coefz$Variable
gsub("PopRock","Pop-Rock",Coefz$Variable)->Coefz$Variable
gsub("RB","R&B",Coefz$Variable)->Coefz$Variable
gsub("Tempo","Beats per minute",Coefz$Variable)->Coefz$Variable
gsub("Tempo","Beats per minute",LabelOrder)->LabelOrder

tiff("raw_figures/Figure 4.tiff", units="in", width=8.5, height=3.75, res=900)
ggplot(Coefz, 
       aes(x = factor(Variable,levels=LabelOrder), y = Estimate)) +
  facet_grid(.~factor(Level,levels=c("Artist","Album","Track")),scales = "free_x")+
  geom_hline(yintercept = 0, 
             colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = factor(Variable,levels=LabelOrder),color=Estimate, ymin = X2.5..,ymax = X97.5..),lwd = 0.6) +
  geom_point(aes(x = factor(Variable,levels=LabelOrder), y = Estimate,color=Estimate),size=2) + 
  coord_flip()+
  scale_color_gradientn(colours = c("#4393C3", "#D6604D"))+
  theme_bw()+
  #scale_y_continuous(breaks = c(-0.5,0,0.5,1,1.5))+
  #scale_x_discrete(labels=rev(c("Acousticness","Danceability","Duration","Instrumentalness","Loudness","Liveliness","Speechiness","Tempo","Valence","Country","Pop","Pop/Rock","Rap","R&B","Trap")))+
  labs(x="Traits",y="Standardized effect size")+
  theme(legend.position = "none",strip.text.x = element_text(size=12,family="sans",face = "bold"), panel.spacing.x = unit(0.7,"cm"),strip.background = element_rect(fill="white"),axis.text = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))
dev.off()

##### Distribution of traits ----
TrackTrait[,c(2,6:14)]->ContinuousTraits
ContinuousTraits$X.duration_ms.<-(ContinuousTraits$X.duration_ms./60000)
colnames(ContinuousTraits)=c("Duration (min)","Acousticness","Danceability","Energy","Instrumentalness","Liveliness","Loudness (dB)","Speechiness","Valence","Beats per minute")
melt(ContinuousTraits)->ContinuousTraits
colnames(ContinuousTraits)=c("Trait","Value")

tiff("raw_figures/Figure S4a.tiff", units="in", width=9.4, height=9, res=900)
ggplot(ContinuousTraits, aes(y=Value,x=1)) + 
  geom_violin(trim=FALSE)+
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="#D6604D")+
  facet_wrap(.~Trait,scales = "free",nrow=2,ncol=5)+
  scale_x_continuous(name = "All artists",labels = NULL)+
  scale_y_continuous(name="Trait values")+
  theme_bw()+
  theme(legend.position = "none",strip.text.x = element_text(size=10,family="sans",face = "bold"), panel.spacing.x = unit(0.7,"cm"),strip.background = element_rect(fill="white"),axis.text = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))
dev.off()

TrackTrait[,c(3:5,15:21)]->CategoricalTraits
### Keys
data.frame(table(CategoricalTraits$X.key.)/sum(table(CategoricalTraits$X.key.)))->KeyTable
colnames(KeyTable)=c("Key","Freq")
KeyTable$Key=c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")

KeysPlot<-{ggplot(KeyTable, aes(fill=factor(Key,levels=KeyTable$Key), y=Freq, x=1)) + 
  geom_bar(position="fill", stat="identity")+
  theme_bw() + 
  scale_y_continuous(name="Frequency")+
  scale_x_continuous(name = "All artists",labels = NULL)+
  scale_fill_manual(name="Keys",values = c(brewer.pal(11,"Spectral"),brewer.pal(11,"Spectral")[1]))+
  theme(legend.text.align = 0,legend.title.align=0,legend.position = "right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(face="bold", family = "sans", colour = "black", size=12), legend.text = element_text(family ="sans", size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))};KeysPlot

### Mode
data.frame(table(CategoricalTraits$X.mode.)/sum(table(CategoricalTraits$X.mode.)))->ModeTable
colnames(ModeTable)=c("Mode","Freq")
ModeTable$Mode=c("Minor","Major")

ModePlot<-{ggplot(ModeTable, aes(fill=Mode, y=Freq, x=1)) + 
  geom_bar(position="fill", stat="identity")+
  theme_bw() + 
  scale_y_continuous(name=NULL,labels = NULL)+
  scale_x_continuous(name = "All artists",labels = NULL)+
  scale_fill_manual(name="Mode",values = brewer.pal(11,"Spectral")[c(2,10)])+
  theme(legend.text.align = 0,legend.title.align=0,legend.position = "right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(face="bold", family = "sans", colour = "black", size=12), legend.text = element_text(family ="sans", size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))};ModePlot

### Mode
data.frame(table(CategoricalTraits$X.time_signature.)/sum(table(CategoricalTraits$X.time_signature.)))->TimeTable
colnames(TimeTable)=c("Time","Freq")
TimeTable$Time=c("0/4","1/4","3/4","4/4","5/4")

TimePlot<-{ggplot(TimeTable, aes(fill=Time, y=Freq, x=1)) + 
    geom_bar(position="fill", stat="identity")+
    theme_bw() + 
    scale_y_continuous(name=NULL,labels = NULL)+
    scale_x_continuous(name = "All artists",labels = NULL)+
    scale_fill_manual(name="Time\nsignature",values = brewer.pal(5,"Spectral"))+
    theme(legend.text.align = 0,legend.title.align=0,legend.position = "right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(face="bold", family = "sans", colour = "black", size=12), legend.text = element_text(family ="sans", size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))};TimePlot

### Musical genres
melt(CategoricalTraits[,4:ncol(CategoricalTraits)])->MusicalGenres
MusicalGenres[!MusicalGenres$value==0,]->MusicalGenres
data.frame(table(MusicalGenres$variable)/sum(table(MusicalGenres$variable)))->GenresTable
colnames(GenresTable)=c("Genre","Freq")

GenresPlot<-{ggplot(GenresTable, aes(fill=Genre, y=Freq, x=1)) + 
    geom_bar(position="fill", stat="identity")+
    theme_bw() + 
    scale_y_continuous(name=NULL,labels = NULL)+
    scale_x_continuous(name = "All artists",labels = NULL)+
    scale_fill_manual(name="Musical\ngenres",values = brewer.pal(7,"Spectral"))+
    theme(legend.text.align = 0,legend.title.align=0,legend.position = "right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(face="bold", family = "sans", colour = "black", size=12), legend.text = element_text(family ="sans", size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"))};GenresPlot

### Aggregate plots
tiff("raw_figures/Figure S4b.tiff", units="in", width=10, height=5, res=900)
ggarrange(KeysPlot,ModePlot,TimePlot,GenresPlot,ncol = 4,nrow = 1)
dev.off()

######## End of script #########
