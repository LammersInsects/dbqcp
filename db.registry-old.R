# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.registry<-function(existing.data.registry=F, #the previously saved registry
                      new.records, #the records to be added
                      filename='debugging' #the base filename
){
  print('Running db.registry.R ...')
  print('This function expects a dataframe with max 5 columns:')
  print('  Date recorded -- Subject -- Field -- Value [-- Source]   (header is compulsory)')
  #Neither is a category column
  
  # Checks before anything can be done
  new.records<-new.records[,1:5]
  #TODO use db.input.qc() to do all QC checks >under construction
  
  #Who is recording data?
  user<-readline(prompt='Who has recorded the data? Name: ')
  print(paste('New data is recorded by',user))
  #   if(user %in% users){
  if(user %in% local(users,env=hidden.env)){
    new.records$Recorded.by<-user
    new.records$Verified<-0
  } else {
    print('Given user name is not validated')
    stop()
  }      
  
  #is this a new data registry?
  if(existing.data.registry[[1]][[1]]==F){
    print('No existing data registry is provided, constructing a new one from the new records')
    header<-colnames(new.records)
    id<-0
  } else {
    print(paste('Existing registry is provided. A backup of it is saved as',paste(today,filename,'registry.backup',sep='.')))
    write.table(existing.data.registry,file=paste(today,filename,'registry.backup',sep='.'),row.names=F,sep=';')
    header<-colnames(existing.data.registry)
    id<-max(existing.data.registry$ID)
  }
  
  # Remove NAs
  nosubject<-emptyvalues(new.records[,2])
  nofield<-emptyvalues(new.records[,3])
  novalue<-emptyvalues(new.records[,4])
  remove<-nosubject | nofield | novalue
  if(sum(remove)>0){
    print(paste(sum(remove),'records with missing Subject, Field or Value found. These are removed from the registry.'))
    print('NOTE: If this is unexpected; verify input and re-run db.registry without reloading the existing data registry!')
    # print(new.records[remove,])
    new.records<-new.records[!remove,]
  }
  
  # Allocate IDs to new registry
  new.records$ID<-(id+1):(id+nrow(new.records))
  new.records<-new.records[,c(ncol(new.records),1:(ncol(new.records)-1))]
  
  #colnames should be equal
  if(existing.data.registry[[1]][[1]]!=F){
    if(all(colnames(existing.data.registry)==colnames(new.records))){
      #       print('CHECKPOINT OK: Names of columns of both existing and new registry are equal')
    } else {
      print('ERROR: Names of columns of existing and new registry do not match')
      print('Existing registry:')
      print(colnames(existing.data.registry))
      print('New records:')
      print(colnames(new.records))
      stop()
    }
  }
  
  # Convert standard Excel date format to R-friendly
  if(existing.data.registry[[1]][[1]]!=F){
    test1<-is.na(as.Date(existing.data.registry[1,2],format='%d-%m-%Y'))
    if(test1){
      existing.data.registry[,2]<-as.Date(existing.data.registry[,2],format='%Y-%m-%d')
    } else {
      existing.data.registry[,2]<-as.Date(existing.data.registry[,2],format='%d-%m-%Y')
    }
  } 
  
  test2<-is.na(as.Date(new.records[1,2],format='%d-%m-%Y'))
  if(test2){
    test3<-is.na(as.Date(new.records[1,2],format='%d/%m/%Y'))
    if(test3){
      test4<-is.na(as.Date(new.records[1,2],format='%d.%m.%Y'))
      if(test4){
        print('Date format was not converted. This generally means that the loaded registry was a pre-existing one saved by this script')
        new.records[,2]<-as.Date(new.records[,2],format='%Y-%m-%d')
      } else {
        new.records[,2]<-as.Date(new.records[,2],format='%d.%m.%Y')
      }
    } else {
      new.records[,2]<-as.Date(new.records[,2],format='%d/%m/%Y')
    }
  } else{
    new.records[,2]<-as.Date(new.records[,2],format='%d-%m-%Y')
  }
  
  # If no date provided, use today
  nodate<-emptyvalues(new.records[,2])
  if(sum(nodate)>0){
    print(paste(sum(nodate),"records have no date. Today's date is imputed in these records:"))
    print(new.records[nodate,])
    new.records[nodate,2]<-as.Date(today, format='%Y%m%d')
  }
  
  # Check whether all records have a source
  nosource<-emptyvalues(new.records[,6])
  if(sum(nosource)>0){
    print(paste('WARNING!',sum(nosource),'records have no source. Records:'))
    print(new.records[nosource,])
    print(paste('Source can be added by reading it as a new record with a newer date. They have been saved as ',today,filename,'registry.nosource.csv',sep='.'))
    write.table(new.records[nosource,],file=paste(today,filename,'registry.nosource.csv',sep='.'),sep=';',row.names=F)
  }
  
  # Construct registry
  if(existing.data.registry[[1]][[1]]==F){
    existing.data.registry<-data.frame()
  }
  df<-rbind(existing.data.registry,new.records)
  df.length.raw<-nrow(df)
  df<-df[rownames(unique(df[,2:5])),]
  df.length.clean<-nrow(df)
  if(df.length.raw>df.length.clean){
    print(paste(df.length.raw-df.length.clean,'records were already present in the registry'))
    #TODO would be good to print the ones already present
  } else {
    print('All records are new to the registry')
  }
  
  # Remove any trailing whitespaces in each column
  for(i in 3:6){
    #     trailing.space<-lapply(df[,i],function(x){substr(x,nchar(x),nchar(x)+1)})==' '
    #     trailing.space<-ifelse(is.na(trailing.space),F,trailing.space)
    #     df[trailing.space,i]<-sapply(df[trailing.space,i],function(x){substr(x,1,nchar(x)-1)})
    df[,i]<-trailingspace(df[,i])
  }
  
  # Export registry
  write.table(df, file=paste(filename,'.registry.csv',sep=''), sep=';',row.names=F)
  print(paste('Registry has been saved as',paste(filename,'.registry.csv',sep='')))
  
  # Return registry
  print('Constructed registry is returned')
  return(df)
}