# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.mask<-function(registry, #the previously saved registry
                  values.to.mask=F,
                  level='subject', # subject, field, or both
                  invert=F, #get everything except the values.to.mask, invert=TRUE gives only the subjects that conform to values.to.mask
                  quiet=F, #absolutely no information is printed
                  print.help=F, #no help message is printed, overridden by quiet flag
                  write.output=F, #flag whether output should be written to working directory
                  filename='debugging' #the base filename
){
  if(!quiet){
    cat(note('Running db.mask.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() and a set of values to mask\n'))
    }
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  if(level=='subject'){
    lev<-3
  } else if(level=='field'){
    lev<-4
  } else if(level=='both'){
    lev<-c(3,4)
  } else {
    stop('Level must either be subject, field, or both, no other text input is accepted')
  }
  
  val<-values.to.mask
  if(val[1]!=F){
    remove<-df[df[,5] %in% val,c(1,lev)]
    if(length(lev)>1){
      keep<-!rownames(df[,c(1,lev)]) %in% rownames(remove)
    } else {
      keep<-!df[,lev] %in% remove[,2]
    }
    if(invert){
      keep<-!keep
      cat(warn('Only keeping subjects that would have been removed because invert=TRUE\n'))
    }
    omit<-df[!keep,]
    df.s<-df[keep,]
    if(!quiet){
      cat(note(nrow(omit),'records were removed based on the values given.\n'))
    }
    if(write.output){
      write.table(omit,file=paste(filename,'.masked.csv',sep=''),row.names=F,sep=';')
      if(!quiet){
        cat(note('They are saved as',paste(filename,'.masked.csv',sep=''),'\n'))
      }
    }
  } else {
    stop('No values to remove were provided')
  }  
  
  if(!quiet){
    cat(note('Masked register is returned\n'))
  }
  return(df.s)
}