# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

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
    print('Running db.findrecords.R ...')
    print('This function expects a registry as produced by db.registry() and a text string to find')
    print('It also works on other datasets when level is given as column index')
  }
  
  # Checks before anything can be done
  # if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  # } else {
    # db.is.registry(registry = registry, quiet=F)
    # stop()
  # }
  
  if(length(to.find)!=1){
    print('ERROR! Only one search term at a time please')
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
    print('ERROR! Level must be all, subject, field or value, no other text input is accepted. Or try using column indices.')
    stop()
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
    if(!quiet){print(paste(nrow(result),'records found in column',colnames(df)[i]))}
    if(print.result){print(result)}
    output<-rbind(output,result)
  }
  
  if(!quiet){print('All matching records are returned as one registry')}
  return(output)
}