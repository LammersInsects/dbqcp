# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.registry<-function(existing.data.registry=F, #the previously saved registry
                      new.records=F, #the records to be added
                      too.old=300, #a number of days against which new records' dates are checked for age
                      expected.missing=0, #use this if the default new input always has a fixed number of incomplete records
                      filename='debugging', #the base filename
                      user=NA, #optional: if not entered here, the script will prompt you for a manual input
                      quiet=F, #absolutely no information is printed
                      print.help=F, #no help message is printed, overridden by quiet flag
                      write.output=T, #flag whether output should be written to working directory
                      save.backup=T
){
  if(!quiet){
    cat(note('Running db.registry.R ...\n'))
    if(print.help){
      cat(note('The existing registry must be produced by db.registry()\n'))
      cat(note('For new records, this function expects a dataframe with max 5 columns:\n'))
      cat(note('  Date recorded -- Subject -- Field -- Value [-- Source]   (header is compulsory)\n'))
    }
  }
  
  #Who is recording data?
  if(is.na(user)){
    user<-readline(prompt='Who has recorded the data? Name: ')
  }
  if(!quiet){
    print(paste('New data is recorded by',user))
  }
  #   if(user %in% users){
  if(user %in% local(users,env=hidden.env)){
    if(new.records[[1]][[1]]==F){
      #
    } else {
      
    }
  } else {
    stop('Given user name is not validated')
  }
  
  # Checks before anything can be done
  #is any input provided?
  if(existing.data.registry[[1]][[1]]==F & new.records[[1]][[1]]==F){
    stop('ERROR: No input is provided.')
  } else {
    
  }
  
  if(!is.numeric(too.old)){
    stop('Variable too.old must be a number')
  }
  
  #is an existing registry provided?
  if(existing.data.registry[[1]][[1]]==F){
    if(!quiet){
      cat(note('No existing data registry is provided, constructing a new one from the new records\n'))
    }
    no.existing<-T
    header<-colnames(new.records)
    id<-0
  } else {
    no.existing<-F
    # Convert standard Excel date format to R-friendly
    if(existing.data.registry[[1]][[1]]!=F){
      test1<-is.na(as.Date(existing.data.registry[1,2],format='%d-%m-%Y'))
      if(test1){
        existing.data.registry[,2]<-as.Date(existing.data.registry[,2],format='%Y-%m-%d') #this is what it should be
      } else {
        existing.data.registry[,2]<-as.Date(existing.data.registry[,2],format='%d-%m-%Y')
      }
    }
    #test whether it's a registry, store a backup if it's not made yet today
    if(db.is.registry(registry = existing.data.registry, quiet = T)){
      if(!quiet){
        cat(note('Existing registry is provided.\n'))
      }
      if(save.backup){
        if(!file.exists(paste(getwd(),'/',today,'.',filename,'.registry.backup',sep=''))){
          if(!quiet){
            write.table(existing.data.registry,file=paste(today,filename,'registry.backup',sep='.'),row.names=F,sep=';')
            cat(note('A backup of it is saved as',paste(today,filename,'registry.backup',sep='.'),'\n'))
          }
        } else {
          cat(warn('A backup is not saved as one was made already earlier today.\n'))
        }
      } else {
        cat(warn('A backup is not saved as this action was overridden by user.\n'))
      }
      #store some essential information
      header<-colnames(existing.data.registry)
      id<-max(existing.data.registry$ID)
    } else {
      cat(error('The provided existing registry does not comply with the registry format\n'))
      db.is.registry(registry = existing.data.registry, quiet = F)
      stop()
    }
  }
  
  #are new records provided?
  if(new.records[[1]][[1]]==F){
    if(!quiet){
      cat(note('No new records are provided, loading the existing registry\n'))
    }
    no.new<-T
  } else { #If so, continue processing the new records
    cat(note('New records are provided\n'))
    no.new<-F
    new.records<-new.records[,1:5]
    #TODO use db.input.qc() to do all QC checks >under construction
    
    # Remove NAs
    nosubject<-emptyvalues(new.records[,2])
    nofield<-emptyvalues(new.records[,3])
    novalue<-emptyvalues(new.records[,4])
    to.remove<-nosubject | nofield | novalue
    if(sum(to.remove)>expected.missing){
      cat(warn(sum(to.remove)-expected.missing,
               'records with missing Subject, Field or Value found. These will be removed from the registry.\n'))
      cat(warn('NOTE: If this is unexpected; verify input and re-run db.registry without reloading the existing data registry!\n'))
      # print(new.records[to.remove,])
    }
    if(sum(to.remove>0)){
      new.records<-new.records[!to.remove,]
      cat(warn(sum(to.remove),' records were removed from the input (expected.missing was set to ',expected.missing,').\n',sep=''))
      if(nrow(new.records)==0){
        cat(warn('No new records are remaining! Aborting... \n'))
        stop()
      }
    }
    
    #Create to extra columns
    new.records$Recorded.by<-user
    new.records$Verified<-0
    
    # Allocate IDs to new registry
    new.records$ID<-(id+1):(id+nrow(new.records))
    new.records<-new.records[,c(ncol(new.records),1:(ncol(new.records)-1))]
    
    # Convert date format
    test2<-is.na(as.Date(new.records[1,2],format='%d-%m-%Y'))
    if(test2){
      test3<-is.na(as.Date(new.records[1,2],format='%d/%m/%Y'))
      if(test3){
        test4<-is.na(as.Date(new.records[1,2],format='%d.%m.%Y'))
        if(test4){
          cat(warn('Date format was not converted. This generally means that the loaded registry was a pre-existing one saved by this script\n'))
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
    
    # If new records contain records from more than 300 days ago, give a warning; they are most likely typos
    # Similarly, a warning should be printed if records with dates in the future are entered
    if(no.new | no.existing){ #this should not get triggered if only an existing registry is loaded!
      
    } else {
      veryold<-new.records[,2] <= as.Date(today, format='%Y%m%d')-too.old
      if(sum(veryold)>0){
        cat(warn('WARNING!',sum(veryold),'records have dates over',too.old,"days old. Do these records' dates contain typos?\n"))
        print(new.records[veryold,])
      }
      
      future<-new.records[,2] > as.Date(today, format='%Y%m%d')
      if(sum(future)>0){
        cat(warn('WARNING!',sum(future),"records have dates that are in the future! Do these records' dates contain typos?\n"))
        print(new.records[future,])
      }
    }
    
    # Check whether all records have a source
    nosource<-emptyvalues(new.records[,6])
    if(sum(nosource)>0){
      cat(warn('WARNING!',sum(nosource),'records have no source. Records:\n'))
      print(new.records[nosource,])
      cat(warn('Source can be added by reading it as a new record with a later date. They have been saved as',
               paste(today,filename,'registry.nosource.csv\n',sep='.')))
      write.table(new.records[nosource,],file=paste(today,filename,'registry.nosource.csv',sep='.'),sep=';',row.names=F)
    }
    
    # Check whether any are adding new subjects or fields
    if(no.existing){
      #then we cannot check it
    } else {
      n.new.subj<-sum(!unique(trailingspace(new.records[,3])) %in% unique(trailingspace(existing.data.registry[,3])))
      n.new.field<-sum(!unique(trailingspace(new.records[,4])) %in% unique(trailingspace(existing.data.registry[,4])))
      if(n.new.subj>0){
        if(n.new.field>0){
          if(!quiet){
            cat(note('The new records add ',n.new.subj,' new subjects and ',n.new.field,' new fields to the database\n',sep=''))
          }
        } else {
          if(!quiet){
            cat(note('The new records add ',n.new.subj,' new subjects to the database\n',sep=''))
          }
        }
      } else {
        if(n.new.field>0){
          if(!quiet){
            cat(note('The new records add ',n.new.field,' new fields to the database\n',sep=''))
          }
        } else {
          if(!quiet){
            cat(note('The new records add no new subjects and no new fields to the database\n',sep=''))
          }
        }
      }
    }
    
  }
  
  #colnames should be equal
  if(existing.data.registry[[1]][[1]]!=F & new.records[[1]][[1]]!=F){
    if(all(colnames(existing.data.registry)==colnames(new.records))){
      #       print('CHECKPOINT OK: Names of columns of both existing and new registry are equal')
    } else {
      cat(error('ERROR: Names of columns of existing and new registry do not match\n'))
      cat(warn('Existing registry:\n'))
      print(colnames(existing.data.registry))
      cat(warn('New records:\n'))
      print(colnames(new.records))
      stop()
    }
  }
  
  # Construct registry
  if(existing.data.registry[[1]][[1]]==F){
    existing.data.registry<-data.frame()
  } else if(new.records[[1]][[1]]==F){
    new.records<-data.frame()
  }
  df<-rbind(existing.data.registry,new.records)
  df.length.raw<-nrow(df)
  df<-df[rownames(unique(df[,2:5])),]
  df.length.clean<-nrow(df)
  if(df.length.raw>df.length.clean){
    cat(warn(df.length.raw-df.length.clean,'records were already present in the registry\n'))
    #TODO would be good to print the ones already present
    #TODO and distinguish between ones already present and duplicates in the new.records
  } else {
    if(nrow(new.records)>0){
      if(!quiet){
        cat(note('All',nrow(new.records),'records are new to the registry\n'))
      }
      #TODO it's a bit funny that it says that when the registry already existed
    } else {
      if(!quiet){
        cat(note('All',nrow(existing.data.registry),'records come from the existing registry\n'))
      }
    }
  }
  
  # Remove any trailing whitespaces in each column
  for(i in 3:6){
    df[,i]<-trailingspace(df[,i])
  }
  
  if(!db.is.registry(df)){
    db.is.registry(df, quiet = F)
    stop('An unexpected ERROR occurred. The registry has to be checked and recreated!')
  }
  
  # Export registry
  if(write.output){
    write.table(df, file=paste(filename,'.registry.csv',sep=''), sep=';',row.names=F)
    if(!quiet){
      cat(note('Registry has been saved as',paste(filename,'.registry.csv',sep=''),'\n'))
    }
  }
  
  # Return registry
  if(!quiet){
    cat(note('Constructed registry is returned\n'))
  }
  return(df)
}
