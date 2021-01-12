# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.mask<-function(registry, #the previously saved registry
                  values.to.mask=F,
                  level='subject', # subject or field
                  invert=F, #get everything except the values.to.mask, invert=TRUE gives only the subjects that conform to values.to.mask 
                  filename='debugging' #the base filename
){
  print('Running db.mask.R ...')
  print('This function expects a registry as produced by db.registry() and a set of values to mask')
    
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  if(level=='subject'){
    lev<-3
  } else {
    if(level=='field'){
      lev<-4
    } else {
      print('Level must either be subject or field, no other text input is accepted')
      stop()
    }
  }
  
  val<-values.to.mask
    if(val[1]!=F){
    remove<-df[df[,5] %in% val,lev]
    keep<-!df[,lev] %in% remove
    if(invert){
      keep<-!keep
      print('Only keeping subjects that would have been removed because invert=TRUE')
    }
    omit<-df[!keep,]
    df<-df[keep,]
    print(paste(nrow(omit),' records were removed based on the values given. They are saved as ',filename,'.masked.csv',sep=''))
    write.table(omit,file=paste(filename,'.masked.csv',sep=''),row.names=F,sep=';')
    print('Masked register is returned')
  } else {
    print('No values to remove were provided')
    stop()
  }  
  
  return(df)
}