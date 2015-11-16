setwd("H:/plink-1.07-dos/")
library(openxlsx)

fam <- read.table("US.fam")
case <- subset(fam, fam[,6]==2)
control <- subset(fam, fam[,6]==1)
key <- read.xlsx("NARAC_ISAC_SampleID+Nyckel_HK.xlsx", sheet=2)
key_ctrl <- read.xlsx("NARAC_ISAC_SampleID+Nyckel_HK.xlsx", sheet=1)
ab <- read.xlsx("2014-10-22_NARAC_Results_AU_HK.xlsx", sheet=2)
test <- ab[,1] == ab[,2] #TRUE:3023; FALSE:153
test <- ab[,1] %in% key[,6] #"Sample" TRUE:2850; FALSE:326 
test <- ab[,2] %in% key[,6] #"Sample CleanID" TRUE:2988; FALSE:188
test <- substr(key[,3],1,1) #start with 0:1185; 1:109; F:985; K:709
                            #out of 1185 start with "0", all second characters are non-zero
#NARAC_key"Specimen ID": 1)1185 start with "0"; 2)a lot end with blank" "
tmp <- sub(" ", "", key[,3]) #1555 match
for (i in 1:length(tmp)) ifelse(substr(tmp[i],1,1)=="0", tmp[i] <- substr(tmp[i],2,nchar(tmp[i])), tmp[i] <- tmp[i])
# 2010 match
key_new <- cbind(key, tmp)
#after eleminating "0" in the beginning of column "Subject ID", 606 match found (before:151)
#almost all (605 out of 606) also found in 2010 Specimen matched cases
tmp <- sub(" ", "", key[,2])
for (i in 1:length(tmp)) ifelse(substr(tmp[i],1,1)=="0", tmp[i] <- substr(tmp[i],2,nchar(tmp[i])), tmp[i] <- tmp[i])
key_new <- cbind(key_new, tmp)
names(key_new)[13] <- "Specimen_new"
names(key_new)[14] <- "Subject_new"
write.table(key_new, "NARAC_key_newID.txt", quote=F, row.names=F, col.names=T, sep="\t")
#after excluding samples, 1908 cases remain
match <- key_new[,c(6,13)]
names(match)[1] <- names(ab)[2]
names(match)[2] <- "iid"
names(case)[2] <- "iid"
case_match <- merge(match, case, by="iid")
case_match <- case_match[,1:2]
ab_match <- merge(case_match, ab, by="Sample.CleanID")
ab_match <- ab_match[,c(2,1,7:73)]
ab_match <- merge(case_match, ab, by="Sample.CleanID")
new <- merge(case, ab_match, by="iid")
new <- new[,c(2,1,7:74)]
names(new)[1] <- "fid"
write.table(new, "narac_acpa_matchID.txt", quote=F, row.names=F, col.names=T, sep="\t")

#binary phenotypes based on cutoffs
ab <- read.table("NARAC_key_newID.txt", header=T) #1908 cases after exclusion
cutoff <- read.xlsx("2014-10-22_NARAC_Results_AU_HK.xlsx", sheet=5, startRow=13, colNames=T)
#1)NYC cutoff with some outliers; 2)NYC+EIRA cutoff
cutoff <- c("narac+eira","98th","cutoff",cutoff[2,2:68])
names(cutoff) <- names(ab)
ab_new <- rbind(cutoff, ab)
ab_new <- ab_new[,4:70]
