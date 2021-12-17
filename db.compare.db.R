# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Define function
db.compare.db<-function(existing.db,
                        new.db,
                        date=Sys.Date(),
                        source='db.compare.db',
                        filename='debugging', #TODO currently not used
                        quiet=F, #absolutely no information is printed
                        print.help=F, #no help message is printed, overridden by quiet flag
                        write.output=F #flag whether output should be written to working directory
){
  if(!quiet){
    cat(note('Running db.compare.db.R ...\n'))
    if(print.help){
      cat(note('This function expects two registries as produced by db.registry()\n'))
    }
  }
  
  # Check date format
  # read.date.format(date)
  #else #take today's date for these new records
  
  # Change empty values into NAs
  new.db[sapply(new.db,emptyvalues)]<-NA
  
  # Remove any trailing whitespaces in each column
  for(i in 1:ncol(new.db)){
    new.db[,i]<-trailingspace(new.db[,i])
  }
  
  # sort both databases by subject
  sort.col<-colnames(existing.db)[1] #this definitely works for databases from db, but is not an all-over solution for other dfs
  existing.db<-existing.db[order(existing.db[,sort.col]),]
  new.db<-new.db[order(new.db[,sort.col]),]
  
  # order the columns alphabetically
  #store the current column order
  e.cols<-colnames(existing.db)
  n.cols<-colnames(new.db)
  #then change it
  existing.db<-existing.db[,sort(colnames(existing.db))]
  new.db<-new.db[,sort(colnames(new.db))]
  
  # compare databases
  new.subjects<-!new.db[,sort.col] %in% existing.db[,sort.col] #test whether any new lines have been added
  new.columns<-!colnames(new.db) %in% colnames(existing.db)#test whether any new columns have been added
  new.db.s<-new.db[!new.subjects,!new.columns] #if so, both of these have to be excluded in the next tests
  new.values<-existing.db!=new.db.s #test whether any cell contents have changed from one value to another
  changed.na<-is.na(existing.db)!=is.na(new.db.s) #test whether any values changed from or to NA
  changed.values<-new.values | changed.na #combine both test results
  changed.values[emptyvalues(changed.values)]<-FALSE #These were NA and are still NA
  edited.cols<-colSums(changed.values) #how many were changed per column
  
  #generate an empty output data frame
  new.records<-data.frame()
  
  if(any(new.subjects)){
    if(!quiet){
      cat(note(sum(new.subjects, na.rm = T), 'new subjects were added to the database. Generating new records from these...\n'))
      print(new.db[new.subjects,n.cols])
    }
    #if so, get those lines and generate new records from those
    process<-remove.empty.columns.and.rows(new.db[new.subjects,])
    subject<-process[,sort.col]
    for(col in colnames(process)[colnames(process)!=sort.col]){
      new.records<-rbind(new.records,
                         data.frame(date=date,
                                    subject=subject,
                                    field=col,
                                    value=process[,col],
                                    source=source))
    }
  }
  
  if(any(new.columns)){
    if(!quiet){
      cat(note(sum(new.columns, na.rm = T), 'new fields were added to the database. Generating new records from these...\n'))
      print(colnames(new.db)[new.columns])
    }
    #if so, get those lines and generate new records from those
    for(col in colnames(new.db)[new.columns]){
      new.records<-rbind(new.records,data.frame(date=date,
                                                subject=new.db[!emptyvalues(new.db[,col]),sort.col],
                                                field=colnames(new.db)[new.columns],
                                                value=new.db[!emptyvalues(new.db[,col]),new.columns],
                                                source=source))
    }
    #the cells that happen to be both in new rows and new columns get added twice, so remove duplicates
    new.records<-unique(new.records)
  }
  
  if(any(changed.values)){
    if(!quiet){
      cat(note(sum(changed.values, na.rm = T), 'cells have different contents than the stored database. Generating new records from these...\n'))
      cat(note('Number of new values per column:\n'))
      print(edited.cols)
    }
    # n.cols[!new.columns]
    for(col in colnames(new.db.s)[edited.cols>0]){ #these are the fields for which we need to make new records
      new.records<-rbind(new.records,
                         data.frame(date=date, #take today's date for these new records
                                    subject=new.db.s[changed.values[,col],sort.col],
                                    field=col,
                                    value=new.db.s[changed.values[,col],col],
                                    source=source)) #not sure whether this is useful
    }
    
  }
  
  if(!any(new.subjects) & !any(new.columns) & !any(changed.values)){
    if(!quiet){
      cat(note('No new lines, columns, or changed cell values found. It seems that both databases are identical...\n'))
      cat(note('Returning an empty data frame\n'))
      
    }
    
  } else {
    
    # sort new records by subject
    new.records<-new.records[order(new.records[,2]),]
    
    #restore column names to existing db names
    file<-paste(filename,'.registry.csv', sep='')
    if(file %in% list.files(getwd())){
      if(!quiet){
        cat(note('Found the registry in the working directory. Copying column names from those...\n'))
      }
      column.names<-colnames(read.csv(file, header = T, sep = ';'))
      colnames(new.records)<-column.names[2:6]
    } else {
      if(!quiet){
        cat(warn('Associated registry is not found in the working directory. Keeping column names as defaults.\n'))
      }
    }
    
    #restore row names
    row.names(new.records)<-1:nrow(new.records)
  }
  
  if(write.output){
    write.table(new.records,file = paste(today,filename,'compare-db.csv',sep='.'),sep=';',row.names = F)
    if(!quiet){
      cat(note(' and has been exported as ',today,'.',filename,'.compare-db.csv\n',sep=''))
    }
  }
  
  # return new records
  return(new.records)
  
}
