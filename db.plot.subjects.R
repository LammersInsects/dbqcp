# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
db.plot.subjects<-function(registry, #a registry as produced by the db package
                           subjects, #subjects of the registry
                           y.field=NA, #a numeric (or integer) field of the registry
                           text.fields=c(), #one or more fields of the registry
                           additional.plot.head=NA, #a field of the registry, or any other string
                           extend.date.to=NA, #NA, Sys.Date(), or any other date variable
                           quiet=F, #absolutely no information is printed
                           print.help=F #no help message is printed, overridden by quiet flag
){
  #first check all the input
  if(!quiet){
    cat(note('Running db.plot.subjects() with a registry as input. Checking input ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() and a number of other important values.\n'))
    }
  }
  
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  #check subject list
  test<-subjects %in% registry[,3]
  if(all(test)){
    #all subjects in the registry
  } else {
    cat(warn('WARNING: Not all given subjects are part of the registry! Omitting these subjects:\n'))
    print(subjects[!test])
    subjects<-subjects[test]
  }
  
  #check the field that will be on the y.axis
  test<-y.field %in% registry[,4]
  if(test){ #the field is in the registry
    if(is.integer(y.field) | is.numeric(y.field)){ #the y.field is numeric
      
    } else {
      cat(error('ERROR: The given y.field is not numeric or integer!\n'))
      stop()
    }
  } else {
    cat(error('ERROR: The given y.field name is not part of the registry!\n'))
    stop()
  }
  
  #check the text fields
  test<-text.fields %in% registry[,4]
  if(all(test)){
    #all text fields are in the registry
  } else {
    cat(warn('WARNING: Not all given text.fields are part of the registry! Omitting these text.fields:\n'))
    print(text.fields[!test])
    subjects<-text.fields[test]
  }
  
  #check variable extend.date.to
  if(is.na(extend.date.to)){
    #default
  } else {
    if(class(extend.date.to)=='Date'){
      #the only allowed alternative
    } else {
      cat(warn('WARNING: The variable extend.date.to is not a date variable! Use Sys.Date() or as.Date(). Ignoring variable...\n'))
      extend.date.to<-NA
    }
  }
  
  #after all checks have passed, compress the registry
  compressed<-db.compress(registry = registry, quiet = quiet)
  
  #find the index of the y.field
  #TODO how??
  y.col
  
  #TODO and the text.fields??
  
  #the plot all the subjects
  for (i in 1:length(subjects)){
    if(!quiet){
      cat(note('Making a plot for ',subjects[i],'.\n',sep=''))
    }
    
    #extract the relevant data
    res<-as.data.frame(compressed[c((y.col-2):(y.col+2)),i], col.names = c('ID','Date',y.field,'Source','Recorded.by'))
    print(res)
    
    #check extend.date.to variable
    if(is.na(extend.date.to)){
      #this variable has not been set or has been ignored
    } else {
      if(res[nrow(res),2]<extend.date.to){ #in case the last record is before the set date
        if(!quiet){
          cat(note('Extending the x.axis up to ',extend.date.to,'.\n'))
        }
        res[nrow(res)+1,]<-data.frame(999999,extend.date.to,res[nrow(res),3:5]) #add one for visualization purposes
      } else {
        if(!quiet){
          cat(note('The last record is on or beyond ',extend.date.to,'.\n'))
        }
      }
    }
    
    #generate plot header
    if(is.na(additonal.plot.head)){ #default
      if(!quiet){
        cat(note('No additional.plot.head is provided.\n'))
      }
      header<-subjects[i]
    } else {
      
      if(additional.plot.head %in% registry[,4]){
        if(!quiet){
          cat(note('An additional.plot.head is provided as a field of the registry.\n'))
        }
        #TODO
      } else {
        if(!quiet){
          cat(note('An additional.plot.head is provided as a fixed string.\n'))
        }
        header<-paste(subjects, '|', additional.plot.head) 
      }
    }
    
    #produce the basic plot
    plot(res[,2:3], type='b', main=header, ylim=c(0,max(as.numeric(res[,y.field]))))
    
    #add text.fields
    records<-db.findrecords(registry = registry, to.find = i, exact = T, level = 3, print.result = F)
    records<-records[records[,4] %in% text.fields,]
    for (j in 1:length(unique(records[,2]))){
      records.s<-records[records[,2]==unique(records[,2])[j],]
      if(nrow(records.s)>1){ #if multiple records from the same day
        mtext(paste(records.s[,5], collapse = '; '), #combine them in one string
              side = 1, line = -2, at = records.s[,2], adj=0, las=3, cex = 0.6, col = "grey40")
      } else {
        mtext(records.s[,5], #if not, print the value
              side = 1, line = -2, at = records.s[,2], adj=0, las=3, cex = 0.6, col = "grey40")
      }
    }
  }
  
  return() #TODO anything?
}

# Explore and plot data

