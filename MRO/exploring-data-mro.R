library(ggplot2)

load("MRO/data/abs2011.rda")
load("MRO/data/aec2013.rda")

#view data information
rxGetInfo(abs2011,getVarInfo=TRUE)

#transform column type
abs2011<-rxFactors(abs2011,
                   factorInfo=list(
                     Electorate=list(sortLevels=TRUE),
                     State=list(sortLevels=TRUE) 
                                   ),
                   overwrite=TRUE)

rxGetInfo(abs2011,getVarInfo=TRUE)$varInfo[c("Electorate","State")]

#Unemployment
rxHistogram(~Unemployed,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(0,12))
rxHistogram(~Unemployed|State,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(0,12))

tmp<-rxCube(Unemployed~State,abs2011,means=TRUE)
result<-rxResultsDF(tmp)
result
ggplot(result,aes(x=State,y=Unemployed,fill=State))+
  geom_bar(stat="identity")+
  labs(x = "State",
     y = "% Unemployment") + 
  theme_minimal() + 
  theme(legend.position = "none") 

#Income
rxHistogram(~MedianIncome,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(250,1100))
rxHistogram(~MedianIncome|State,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(250,1100))

tmp<-rxCube(MedianIncome~State,abs2011,means=TRUE)
result<-rxResultsDF(tmp)
result
ggplot(result,aes(x=State,y=MedianIncome,fill=State))+
  geom_bar(stat="identity")+
  labs(x = "State",
       y = "MedianIncome") + 
  theme_minimal() + 
  theme(legend.position = "none") 

#Education
rxHistogram(~Bachelor,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(0,30))
rxHistogram(~Bachelor|State,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(0,30))

tmp<-rxCube(Bachelor~State,abs2011,means=TRUE)
result<-rxResultsDF(tmp)
result
ggplot(result,aes(x=State,y=Bachelor,fill=State))+
  geom_bar(stat="identity")+
  labs(x = "State",
       y = "% electorate with a Bachelor degree") + 
  theme_minimal() + 
  theme(legend.position = "none") 

rxLinePlot(MedianIncome~Bachelor,abs2011,type="p")

tmp<-rxCube(Postgraduate~State,abs2011,means=TRUE)
result<-rxResultsDF(tmp)
result
ggplot(result,aes(x=State,y=Postgraduate,fill=State))+
  geom_bar(stat="identity")+
  labs(x = "State",
       y = "% electorate with a Postgraduate degree") + 
  theme_minimal() + 
  theme(legend.position = "none") 

rxLinePlot(MedianIncome~Postgraduate,abs2011,type="p")

rxLinePlot(Postgraduate+Bachelor~MedianIncome,abs2011,type="p")

#Internet
rxHistogram(~Internet,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(85,100))
rxHistogram(~Internet|State,abs2011,numBreaks=50,histType="Percent",xAxisMinMax=c(85,100))

## 2013 Federal Election Data

#view data information of 2013 election data
rxGetInfo(aec2013,getVarInfo=TRUE)

#transform column type
aec2013<-rxFactors(aec2013,
                   factorInfo=list(
                     Electorate=list(sortLevels=TRUE),
                     PollingPlace=list(sortLevels=TRUE),
                     Elected=list(sortLevels=TRUE),
                     HistoricElected=list(sortLevels=TRUE),
                     PartyAb=list(sortLevels=TRUE),
                     PartyNm=list(sortLevels=TRUE),
                     State=list(sortLevels=TRUE),
                     DivisionNm.y=list(sortLevels=TRUE)
                   ),
                   overwrite=TRUE)

rxGetInfo(aec2013,getVarInfo=TRUE)$varInfo[c("PollingPlace","Elected","PartyNm")]

#Total votes by Party  
tmp<-rxCube(OrdinaryVotes~PartyNm,aec2013,means=FALSE)
total_votes_for_parties<-rxResultsDF(tmp)
library(ggplot2)
library(scales)
ggplot(total_votes_for_parties, aes(reorder(PartyNm, OrdinaryVotes), OrdinaryVotes )) +
  geom_point() + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_bw() +
  ylab("Total ordinary votes") +
  xlab("Party") +
  theme(text = element_text(size=10))

#Total votes by Electorate and Party
tmp<-rxCrossTabs(OrdinaryVotes~Electorate:PartyNm,aec2013)
result <- rxResultsDF(tmp,output="sums")
result[1:6,1:3]
colnames(result)[-1]<-substring(colnames(result[,-1]),9)
cellnote1<-matrix(as.character(data.matrix(round(result[,-1]))),
                  byrow=FALSE,nrow=dim(result)[1])
library(gplots)
par(oma=c(0.1,0.1,0.1,0.1))
heatmap.2(data.matrix(result[,-1]), 
          labRow=result[,1], col=cm.colors(255),
          trace='none',dendrogram ="none",
          #cellnote=cellnote1,notecex=0.4,notecol="black",
          na.color=par("bg"))






























