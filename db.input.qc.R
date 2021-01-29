# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.input.qc<-function(existing.data.registry=F,
                      new.records, #the records to be added
                      filename='debugging', #the base filename
                      in.pipeline=T
){
  print('Running db.input.qc.R ...')
  print('This function expects a dataframe with max 5 columns:')
  print('  Date recorded -- Subject -- Field -- Value [-- Source]   (header is compulsory)')
  #Neither is a category column
  
  # Checks before anything can be done
  if(ncol(new.records)>5){
    new.records<-new.records[,1:5]
    print('Input dataframe has more than 5 columns. Only the first 5 are used, the rest is ignored.')
  }
  
  #is this a new data registry?
  if(existing.data.registry[[1]][[1]]==F){
    print('No existing data registry is provided, constructing a new one from the new records')
    header<-colnames(new.records)
    id<-0
  } else {
    if(in.pipeline){
      print(paste('Existing registry is provided. A backup of it is saved as',paste(today,filename,'registry.backup',sep='.')))
      write.table(existing.data.registry,file=paste(today,filename,'registry.backup',sep='.'),row.names=F,sep=';')
    } else {
      print('Existing registry is provided.')
    }
    header<-colnames(existing.data.registry)
    # id<-max(existing.data.registry$ID)
  }
  
  # Remove NAs
  nosubject<-emptyvalues(new.records[,2])
  novalue<-emptyvalues(new.records[,4])
  remove<-nosubject | novalue
  if(sum(remove)>0){
    print(paste(sum(remove),'records with missing Subject or Value found. These are removed from the registry.'))
    print('NOTE: If this is unexpected; verify input and re-run db.registry without reloading the existing data registry!')
    # print(new.records[remove,])
    new.records<-new.records[!remove,]
  }
  
  #colnames should be equal
  if(existing.data.registry[[1]][[1]]!=F){
    if(all(colnames(existing.data.registry)==colnames(new.records))){
      print('CHECKPOINT OK: Names of columns of both existing and new registry are equal')
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
        print('It could also mean that the date format is unusual or irregular!')
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
    if(in.pipeline){
      print(paste('Source can be always added by reading it as a new record with a newer date. They have been saved as ',
                  today,filename,'registry.nosource.csv',sep='.'))
      write.table(new.records[nosource,],file=paste(today,filename,'registry.nosource.csv',sep='.'),sep=';',row.names=F)
    } else {
      print('Source can always be added by reading it as a new record with a newer date.')
    }
    
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
    #TODO Use duplicates() to find them
  } else {
    print('All records are new to the registry')
  }
  
  # Remove any trailing whitespaces in each column
  for(i in 3:6){
    df[,i]<-trailingspace(df[,i])
  }
  
  # Export registry
  write.table(df, file=paste(filename,'.registry.csv',sep=''), sep=';',row.names=F)
  print(paste('Registry has been saved as',paste(filename,'.registry.csv',sep='')))
  
  # Return registry
  print('Constructed registry is returned')
  return(df)
}