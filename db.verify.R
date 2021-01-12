# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.verify<-function(registry, #the previously saved registry
                    verified.IDs=F, #IDs of the records to remove
                    filename='debugging' #the base filename
){
  print('Running db.verify.R ...')
  print('This function expects a registry as produced by db.registry() and a 4-column table with the verified records and these column names:')
  print('  ID -- Verified -- Verified.by -- Date.verified  (header is compulsory)')
  print('If no table of verified records is provided, than a table for records verification is generated')
  
  # Checks before anything can be done
  if(verified.IDs[[1]][1]==F){
    registry$Verified.by<-''
    registry$Date.verified<-''
    write.table(registry,file=paste(filename,'verify.csv',sep='.'),row.names=F,sep=';')
    stop()
  } else {
    if(ncol(verified.IDs)!=4){
      print('ERROR: Table that specifies the verification of the records does not have 4 columns; check input!')
      stop()
    }
  }
  
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  df<-df[order(df$ID),]
  verified<-verified.IDs
  verified<-verified[order(verified$ID),]
  
  # Check if today files under the same filename have already been removed If so, load that file and append
  files<-list.files(getwd())
  fullfilename<-paste(filename,'verification.log.csv',sep='.')
  if(fullfilename %in% files){
    print('Records were already verified earlier today. The newly verified records are appended to the same log.')
    verification<-read.table(fullfilename,header=T,sep=';')    
    test<-is.na(as.Date(verification[1,2]))
    if(test){
      verification[,2]<-as.Date(verification[,2], format='%d-%m-%Y')
    } else {
      verification[,2]<-as.Date(verification[,2])
    }
  } else {
    verification<-data.frame()
  }
  
  # Find and store the original records
  old<-df$ID %in% verified$ID
  print(paste(sum(old),'records had their verification value changed. The verification was logged in',fullfilename))
  done<-merge(df,verified,by='ID')
  verification<-rbind(verification,done)
  verification<-unique(verification)
  exclude<-verification$Verified.x==verification$Verified.y
  verification<-verification[!exclude,]
  write.table(verification,file=fullfilename,row.names=F,sep=';')
  
  # Replace the contents of the Verified-column in the registry by the new values
  df$Verified[old]<-verified$Verified
  
  return(df)
}
