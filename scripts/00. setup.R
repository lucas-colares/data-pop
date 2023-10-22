# # # # # # # # # # # # # #
########## Setup ##########
# # # By:  L. Colares # # #
# # # #  07-08-2023 # # # #
# # # # # # # # # # # # # #

##### Packages ----
if(require(reshape2)==FALSE){
  install.packages("reshape2")
  library(reshape2)
}

if(require(R.utils)==FALSE){
  install.packages("R.utils")
  library(R.utils)
}

if(require(vegan)==FALSE){
  install.packages("vegan")
  library(vegan)
}

if(require(ggplot2)==FALSE){
  install.packages("ggplot2")
  library(ggplot2)
}

if(require(TPD)==FALSE){
  install.packages("TPD")
  library(TPD)
}

if(require(ggrepel)==FALSE){
  install.packages("ggrepel")
  library(ggrepel)
}

if(require(ggh4x)==FALSE){
  install.packages("ggh4x")
  library(ggh4x)
}

if(require(ggpolypath)==FALSE){
  install.packages("ggpolypath")
  library(ggpolypath)
}

if(require(ggpubr)==FALSE){
  install.packages("ggpubr")
  library(ggpubr)
}

if(require(ggtext)==FALSE){
  install.packages("ggtext")
  library(ggtext)
}

if(require(RColorBrewer)==FALSE){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}