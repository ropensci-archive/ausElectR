

ACT_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-ACT.csv",
                     header=TRUE,skip = 1)


ACT_vote$DivisionID <- as.factor(ACT_vote$DivisionID)

NSW_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-NSW.csv",
                     header=TRUE,skip = 1)


NSW_vote$DivisionID <- as.factor(NSW_vote$DivisionID)

NT_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-NT.csv",
                     header=TRUE,skip = 1)


NT_vote$DivisionID <- as.factor(NT_vote$DivisionID)

QLD_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-QLD.csv",
                    header=TRUE,skip = 1)


QLD_vote$DivisionID <- as.factor(QLD_vote$DivisionID)

SA_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-SA.csv",
                     header=TRUE,skip = 1)


SA_vote$DivisionID <- as.factor(SA_vote$DivisionID)

TAS_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-TAS.csv",
                    header=TRUE,skip = 1)


TAS_vote$DivisionID <- as.factor(TAS_vote$DivisionID)

VIC_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-VIC.csv",
                     header=TRUE,skip = 1)


VIC_vote$DivisionID <- as.factor(VIC_vote$DivisionID)

WA_vote <- read.csv("AECdata/HouseStateFirstPrefsByPollingPlace/HouseStateFirstPrefsByPollingPlaceDownload-17496-WA.csv",
                     header=TRUE,skip = 1)


WA_vote$DivisionID <- as.factor(WA_vote$DivisionID)

AUS_vote <- rbind(ACT_vote,NSW_vote,NT_vote,QLD_vote,SA_vote,TAS_vote,VIC_vote,WA_vote)


AUS_vote_summary <- tapply(AUS_vote$OrdinaryVotes, list(AUS_vote$DivisionID, AUS_vote$PartyNm), sum)

AUS_vote_summary <- as.data.frame(AUS_vote_summary)
plot(AUS_vote_summary$Liberal,AUS_vote_summary[,3],xlab="Liberal",ylab="Labor",xlim=c(0,50000),ylim=c(0,50000))