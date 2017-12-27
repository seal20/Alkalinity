#!/usr/bin/Rscript

#This R script takes all the files (report generated from a titrino, titouch, metrohm titration device), calculate total alkalinity using the gran plot method and stor the results in a csv file.

#setup some initial value
acid_conc<-0.1
acid_density<-1

#list all files in the current directory
files<-list.files(pattern=".txt")
alkalinity<-data.frame(Filename=character(),Sample=character(),Sample_weight=numeric(),Tot_Alkalinity=numeric(),stringsAsFactors=FALSE)

for (i in 1:length(files)){

#read the data file
alk_temp<-read.csv(files[i],sep="\t",row.names = NULL,header=FALSE,fill=TRUE,col.names=c(1:11),colClasses="character")

#prepare the line numbers to and get the value
spl_info<-as.numeric(which(alk_temp == "$S Sample data V1"))+1
tit_start<-as.numeric(which(alk_temp == "$S Mode 1"))+1
tit_end<-as.numeric(which(alk_temp == "$S DETERM V1"))-3

#get file name, sample name and weight from files
alkalinity[i,2]<-paste(as.character(alk_temp[spl_info,1]),"_",as.character(alk_temp[spl_info,2]))
alkalinity[i,3]<-as.numeric(alk_temp[spl_info,3])
alkalinity[i,1]<-files[i]

#get titration data between pH 4 and 2.9
alk_tit<-data.frame(alk_temp[tit_start[1]:tit_end,2:3])
colnames(alk_tit)<-c("vol","pH")
alk_tit[,1]<-as.numeric(alk_tit[,1])
alk_tit[,2]<-as.numeric(alk_tit[,2])
alk_tit<-alk_tit[alk_tit[["pH"]] < 4 & alk_tit[["pH"]] > 2.9,]

#calculate the gran valu
alk_gran<-as.data.frame(alkalinity[i,3]+alk_tit[["vol"]])/alkalinity[i,3]*10^(4-alk_tit[["pH"]])
colnames(alk_gran)<-"G"

#merge the data frames
alk_tit<-data.frame(alk_gran,alk_tit)


##Get an approximate TA from the gran function
#calculate the gran function
gran_plot<-NULL
try(gran_plot<-lm(alk_tit[["G"]]~alk_tit[["vol"]]),silent=TRUE)

#calculate the alkalintity
if (!is.null(gran_plot)) {
    slope<- as.numeric(coef(gran_plot)[2])
    intercept<-as.numeric(coef(gran_plot)[1])
    alkalinity[i,4]<-(((-intercept/slope)*acid_conc)/alkalinity[i,3])*10^6
  } else {
    alkalinity[i,4]<- "Error"
    }
}

##Get a more accurate value from non linear regression taken from seacarb
##problem we need the salinity! we could input it in Identification 2



##Write all results
write.csv(alkalinity,file=paste(format(Sys.time(), "%Y%m%d_%H%M"),".csv"))

 

