# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## Functional diversity in human song ###########
# # # # # # # # # # By: Lucas Colares # # # # # # # # # #
# # # # # # # # # # # # 07-08-2023 # # # # # # # ## # # # 

source("scripts/00. setup.R")

#### Calculate Functional diversity ----

## Load datasets
read.csv("datasets/Streams.csv",row.names = 1,encoding = "latin1")->Streams
read.csv("datasets/Traits.csv",row.names = 1,encoding = "latin1")->Traits
read.csv("datasets/Genres.csv",row.names = 1,encoding = "latin1")->Genres

Streams[!Streams$Platform=="Deezer",]->Streams # Remove deezer
paste(Traits$X.Artist.,"Alb",Traits$X.AlbumNo.,Traits$X.Song., sep = "")->rownames(Traits)
paste(Genres$Artist,"Alb",Genres$AlbumNo,Genres$Song, sep = "")->Genres$ID

## Genres table
ALBS<-tapply(rep(1, length(Genres$Genres)), list(Genres$ID, Genres$Genres) 
             , min,default = 0)

## Final Traits table
merge(Traits[,6:ncol(Traits)], ALBS[,colSums(ALBS)>nrow(ALBS)*5/100],by=0)->FinalTraits

## Scale continuous variables
decostand(FinalTraits[,c(2,6:14)], "standardize")->stand
cbind(stand,FinalTraits[,c(3:5,15:ncol(FinalTraits))])->TraitsStand
colnames(TraitsStand)=c("Duration","Acousticness", "Danceability", "Energy", "Instrumentalness","Liveliness", "Loudness", "Speechiness", "Valence", "Tempo", "Key", "Mode","Time_Signature",gsub("_", " ", colnames(TraitsStand)[14:length(colnames(TraitsStand))]))

## Use PCA axis to represent traits 
prcomp(TraitsStand, scale. = T)-> respca; summary(respca)
data.frame(respca$x[,1:2], Artist=gsub("_"," ",gsub("Alb.*", "", FinalTraits$Row.names)))->PCA

sub("Track.*", "", FinalTraits$Row.names)->PCA$Track
FinalTraits$Row.names->PCA$Song
paste0(Streams$X,"Alb",Streams$AlbumNo)->Streams$ID

TPDs_iris <- TPDs(species = PCA$Artist, PCA[,1:2],alpha = 0.99)

tapply(Streams$Streams,list(Streams$Platform, Streams$X), sum, na.rm=T)->abundances_comm_iris

TPDc_iris <- TPDc(TPDs = TPDs_iris, sampUnit = abundances_comm_iris)

REND(TPDc_iris, TPDs_iris)->RAO

RAO$species$FRichness->FinalRich
RAO$species$FEvenness->FinalEven
RAO$species$FDivergence->FinalDiv

## Save index
data.frame(Richness=FinalRich,Evenness=FinalEven,Divergence=FinalDiv, Artists=unique(PCA$Artist))->FinalIndex
write.csv(FinalIndex, "results/functional diversity.csv",row.names = F,fileEncoding = "latin1")
