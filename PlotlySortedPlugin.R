#########################################################################################################################
#########################################################################################################################
###### PROJECT:        GWI Diet
###### NAME:           CSTsSpecificityIndex.R
###### AUTHOR:         Vitalii Stebliankin, PhD Student
###### AFFILIATION:    Florida International University
###### 
###### DESCRIPTION:    This file generates the allingtraphs with diversity and stacked bar plots
#########################################################################################################################
#########################################################################################################################

#rm(list=ls())

#projectPath = "/Users/stebliankin/Desktop/GWI_2022/experiment_runs/03-25-2022-bwt2_ABR/"
dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")
source("RIO.R")
library(gplots)
library("markovchain"); packageVersion("markovchain")
library("igraph"); packageVersion("igraph")
# library(caret)
library(vegan)

set.seed(1)
library(mixOmics)
library(plotly)
library(plyr)
require(reshape2)
require(ggplot2)
require(data.table)
library(rgl)
library(stringr)
hash <- function(name) {return(sum(utf8ToInt(name))/nchar(name))}

input <- function(inputfile) {
   parameters <<- readParameters(inputfile);
     
   Sys.setenv("plotly_username" = parameters["plotlyusername", 2])
   Sys.setenv("plotly_api_key" = parameters["plotlyapikey", 2])
ABR_X <<- as.data.frame(read.csv2(paste(prefix(), parameters["csvfile", 2], sep="/"), sep=','))
ABR <<- ABR_X
rownames(ABR) <<- ABR[,1]
ABR<<-ABR[,-1]
ABR<<-as.matrix(ABR)

melt_df <<- melt(data = ABR)
   
   colnames(melt_df) <<- readSequential(paste(prefix(), parameters["columns", 2], sep="/"))

}

run <- function() {}

output <- function(outputfile) {
melt_df$RPKM = as.numeric(melt_df$RPKM)
#melt_df = melt_df[order(melt_df$Id),]

# Distinct colors:
library(RColorBrewer)
n <- 33
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


s3 <- plot_ly(melt_df,  x = melt_df~Id,y = melt_df~RPKM, type = 'bar', color =  melt_df~Gene, showlegend = T, colors = col_vector) %>%
  layout(yaxis = list(title = 'Output', showgrid=F),xaxis = list(showticklabels=T, showgrid=F), barmode = 'stack', 
         annotations=list(text = "",xref = "paper",yref = "paper",
                          yanchor = "bottom",xanchor = "center",align = "center",
                          x = 0.3,y = 1,showarrow = F))
s3
plotly_IMAGE(s3, width = 1000, height = 500, format = "png", scale = 3,out_file = outputfile)
}
