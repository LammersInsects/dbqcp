# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Define function
db.compare.db<-function(existing.db,
                        new.db,
                        commit.date=Sys.Date(), #date in format YYYY-mm-dd
                        commit.source='db.compare.db',
                        file.base.name='debugging',
                        quiet=F, #absolutely no information is printed
                        print.help=F, #no help message is printed, overridden by quiet flag
                        write.output=F #flag whether output should be written to working directory
){
  
  # Enforce data formats
  commit.date<-as.Date(commit.date[[1]])
  #TODO if this fails use today's date for these new records
  commit.source<-as.character(commit.source[[1]])
  file.base.name<-as.character(file.base.name[[1]])
  if(!is.logical(c(quiet,print.help,write.output))){
    cat(error('ERROR: All the variables quiet & print.help & write.output must be of class logical !'))
    stop()
  }
  if(!is.data.frame(existing.db) | !is.data.frame(new.db)){
    cat(error('ERROR: Both the existing database and the new database must be in data.frame format !'))
    stop()
  }
  
  if(!quiet){
    cat(note('Running db.compare.db.R ...\n'))
    if(print.help){
      cat(note('This function expects two databases as produced by db.build()\n'))
    }
  }
  
  # Change empty values into NAs
  new.db[sapply(new.db,emptyvalues)]<-NA
  
  # Remove any trailing whitespaces in each column
  for(i in 1:ncol(new.db)){
    new.db[,i]<-trimws(new.db[,i])
  }
  # And also from the column names (normally not an issue, but sometimes new columns have a trailing space)
  colnames(new.db)<-trimws(colnames(new.db))
  
  # Address the possibility that data may be stored in inappropriate data types
  new.db<-type.convert(new.db, as.is=T)
  existing.db<-type.convert(existing.db, as.is=T)
  
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
                         data.frame(date=commit.date,
                                    subject=subject[!emptyvalues(process[,col])],
                                    field=col,
                                    value=process[,col][!emptyvalues(process[,col])],
                                    source=commit.source))
    }
  }
  
  if(any(new.columns)){
    if(!quiet){
      cat(note(sum(new.columns, na.rm = T), 'new fields were added to the database. Generating new records from these...\n'))
      print(colnames(new.db)[new.columns])
    }
    #if so, get those lines and generate new records from those
    for(col in colnames(new.db)[new.columns]){
      if(!all(emptyvalues(new.db[,col]))){
        new.records<-rbind(new.records,data.frame(date=commit.date,
                                                  subject=new.db[!emptyvalues(new.db[,col]),sort.col],
                                                  field=col,
                                                  value=new.db[!emptyvalues(new.db[,col]),col],
                                                  source=commit.source))
      } else {
        if(!quiet){
          cat(warn('Column <',col,'> is completely empty !\n'))
        }
      }
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
                         data.frame(date=commit.date, #take today's date for these new records
                                    subject=new.db.s[changed.values[,col],sort.col],
                                    field=col,
                                    value=new.db.s[changed.values[,col],col],
                                    source=commit.source)) #not sure whether this is useful
    }
    
  }
  
  if((!any(new.subjects) & !any(new.columns) & !any(changed.values)) | nrow(new.records)==0){
    if(!quiet){
      cat(note('No new lines, columns, or changed cell values found. It seems that both databases are identical...\n'))
      cat(note('Returning an empty data frame\n'))
    }
    
  } else {
    
    #sort new records by subject
    new.records<-new.records[order(new.records[,2]),]
    
    #exclude records that have empty values
    # they get introduced when cells in new.db were emptied of all contents
    # which is not a valid method of removing data from a registry-based database
    # instead, the user should make a removal action with db.create.action()
    if(nrow(new.records)==1){
      if(sum(emptyvalues(new.records[,2:4]))>0){
        new.records<-new.records[-1,]
      }
    } else {
      new.records<-new.records[!rowSums(sapply(new.records[,2:4], emptyvalues))>0,]
    }
    
    #restore column names to existing db names
    file<-paste(file.base.name,'.registry.csv', sep='')
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
    if(nrow(new.records)>0){
      row.names(new.records)<-1:nrow(new.records)
    }
  }
  
  if(write.output){
    write.table(new.records,file = paste(format(Sys.Date(),'%Y%m%d'),file.base.name,'compare-db.csv',sep='.'),sep=';',row.names = F)
    if(!quiet){
      cat(note(' and has been exported as ',format(Sys.Date(),'%Y%m%d'),'.',file.base.name,'.compare-db.csv\n',sep=''))
    }
  }
  
  # return new records
  return(new.records)
  
}
