# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.findrecords<-function(registry, #the previously saved registry
                         to.find=F,
                         exact=F,
                         level='all', # all, subject, field or value
                         invert=F, #get everything except the to.find, invert=TRUE gives only the records that conform to to.find 
                         print.result=F,
                         quiet=F,
                         filename='debugging' #the base filename
){
  if(!quiet){
    cat(note('Running db.findrecords.R ...\n'))
    cat(note('This function expects a registry as produced by db.registry() and a text string to find\n'))
    cat(note('It also works on other datasets when level is given as column index\n'))
  }
  
  # Checks before anything can be done
  # if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  # } else {
    # db.is.registry(registry = registry, quiet=F)
    # stop()
  # }
  
  if(length(to.find)!=1){
    cat(error('ERROR! Only one search term at a time please\n'))
    stop()
  } else {
    find<-to.find
  }
  
  options(warn = -1)
  
  if(!emptyvalues(as.integer(level))){
    lev<-level
  } else if(level=='all'){
    lev<-3:5
  } else if(level=='subject'){
    lev<-3
  } else if(level=='field'){
    lev<-4
  } else if(level=='value'){
    lev<-5
  } else {
    stop('ERROR! Level must be all, subject, field or value, no other text input is accepted. Or try using column indices.')
  }
  
  options(warn = 0)
  
  output<-data.frame()
  for(i in lev){
    if(exact){
      if(invert){
        result<-df[df[,i]!=to.find,]
      } else {
        result<-df[df[,i]==to.find,]
      }
    } else {
      result<-df[grep(to.find,df[,i],ignore.case=T,invert = invert),]
    }
    
    if(!quiet){
      cat(note(nrow(result),'records found in column',colnames(df)[i],'\n'))
    }
    if(nrow(result)>0){
      if(print.result){
        print(result)
      }
    }
    
    output<-rbind(output,result)
  }
  
  if(!quiet){
    cat(note('All matching records are returned as one registry\n'))
  }
  
  return(output)
}