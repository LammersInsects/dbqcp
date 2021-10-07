# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.input.qc<-function(existing.data.registry=F,
                      new.records, #the records to be added
                      filename='debugging', #the base filename
                      in.pipeline=T,
                      quiet=F, #absolutely no information is printed
                      print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.input.qc.R ...\n'))
    if(print.help){
      cat(note('This function expects a dataframe with max 5 columns:\n'))
      cat(note('  Date recorded -- Subject -- Field -- Value [-- Source]   (header is compulsory)\n'))
    }
  }
  #Neither is a category column
  
  # Checks before anything can be done
  if(ncol(new.records)>5){
    new.records<-new.records[,1:5]
    cat(warn('Input dataframe has more than 5 columns. Only the first 5 are used, the rest is ignored.\n'))
  }
  
  #is this a new data registry?
  if(existing.data.registry[[1]][[1]]==F){
    if(!quiet){
      cat(note('No existing data registry is provided, constructing a new one from the new records\n'))
    }
    header<-colnames(new.records)
    id<-0
  } else {
    if(in.pipeline){
      if(!quiet){
        cat(note('Existing registry is provided. A backup of it is saved as',paste(today,filename,'registry.backup',sep='.'),'\n'))
      }
      write.table(existing.data.registry,file=paste(today,filename,'registry.backup',sep='.'),row.names=F,sep=';')
    } else {
      if(!quiet){
        cat(note('Existing registry is provided.\n'))
      }
    }
    header<-colnames(existing.data.registry)
    # id<-max(existing.data.registry$ID)
  }
  
  # Remove NAs
  nosubject<-emptyvalues(new.records[,2])
  novalue<-emptyvalues(new.records[,4])
  remove<-nosubject | novalue
  if(sum(remove)>0){
    cat(note(sum(remove),'records with missing Subject or Value found. These are removed from the registry.\n'))
    cat(warn('NOTE: If this is unexpected; verify input and re-run db.registry without reloading the existing data registry!\n'))
    # print(new.records[remove,])
    new.records<-new.records[!remove,]
  }
  
  #colnames should be equal
  if(existing.data.registry[[1]][[1]]!=F){
    if(all(colnames(existing.data.registry)==colnames(new.records))){
      if(!quiet){
        cat(note('CHECKPOINT OK: Names of columns of both existing and new registry are equal'))
      }
    } else {
      cat(error('ERROR: Names of columns of existing and new registry do not match\n'))
      cat(note('Existing registry:\n'))
      print(colnames(existing.data.registry))
      cat(note('New records:\n'))
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
        cat(warn('Date format was not converted. This generally means that the loaded registry was a pre-existing one saved by this script\n'))
        cat(warn('It could also mean that the date format is unusual or irregular!\n'))
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
    cat(warn(sum(nodate),"records have no date. Today's date is imputed in these records:\n"))
    print(new.records[nodate,])
    new.records[nodate,2]<-as.Date(today, format='%Y%m%d')
  }
  
  # Check whether all records have a source
  nosource<-emptyvalues(new.records[,6])
  if(sum(nosource)>0){
    cat(warn('WARNING!',sum(nosource),'records have no source. Records:\n'))
    print(new.records[nosource,])
    if(in.pipeline){
      if(!quiet){
        cat(note('Source can be always added by reading it as a new record with a newer date. They have been saved as ',
                 paste(today,filename,'registry.nosource.csv',sep='.'),'\n'))
      }
      write.table(new.records[nosource,],file=paste(today,filename,'registry.nosource.csv',sep='.'),sep=';',row.names=F)
    } else {
      if(!quiet){
        cat(note('Source can always be added by reading it as a new record with a newer date.\n'))
      }
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
    cat(warn(df.length.raw-df.length.clean,'records were already present in the registry\n'))
    #TODO would be good to print the ones already present
    #TODO Use duplicates() to find them
  } else {
    if(!quiet){
      cat(note('All records are new to the registry'))
    }
  }
  
  # Remove any trailing whitespaces in each column
  for(i in 3:6){
    df[,i]<-trailingspace(df[,i])
  }
  
  # Export registry
  write.table(df, file=paste(filename,'.registry.csv',sep=''), sep=';',row.names=F)
  if(!quiet){
    cat(note('Registry has been saved as',paste(filename,'.registry.csv',sep=''),'\n'))
  }
  
  # Return registry
  if(!quiet){
    cat(note('Constructed registry is returned\n'))
  }
  return(df)
}