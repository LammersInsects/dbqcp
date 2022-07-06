# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

db.has.actions<-function(registry, #the previously saved registry
                         type='all', #remove, translate, or all actions
                         quiet=F, #absolutely no information is printed
                         print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.has.actions.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() \n'))
    }
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  actions<-df[grep('db~',df[,3], fixed = T),]
  
  if(type=='remove' | type=='db~remove'){
    todo<-'db~remove'
  } else if(type=='translate' | type=='db~translate'){
    todo<-'db~translate'
  } else if(type=='all'){
    todo<-c('db~remove','db~translate')
  } else {
    cat(error('Currently, only the actions <remove> and <translate> are supported\n'))
    stop()
  }
  
  #return assessment
  if(nrow(actions)>0){
    if(!quiet){
      cat(note('Found',paste(do.call(paste,as.data.frame(table(actions[,3]))[,c(2,1)]), collapse = ' + '),'actions\n'))
    }
    return(T)
  } else {
    if(!quiet){
      cat(warn('No actions were found\n'))
    }
    return(F)
  }
}
