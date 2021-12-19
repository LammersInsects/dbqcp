# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

db.process.actions<-function(registry, #the previously saved registry
                             type, #remove, translate, or all actions
                             quiet=F, #absolutely no information is printed
                             print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.process.actions.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() \n'))
      cat(note('It is usually better to process the removals first, before the translations\n'))
    }
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  actions<-df[grep('db~',df[,3]),]
  
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
  
  #subset actions for those that are of the type requested to be processed
  actions.todo<-actions[actions$Categorie %in% todo,]
  #process in antechronological order
  actions.todo<-actions.todo[rev(rownames(actions.todo)),]
  
  #do the processing one by one
  rm.count<-0
  tr.count<-0
  translated<-0
  for(i in actions.todo$ID){
    do<-df[df$ID==i,]
    if(do[,3]=='db~remove'){
      #remove target record
      df<-df[df[,1]!=as.numeric(do[,4]),]
      rm.count<-rm.count+1
    } else if(do[,3]=='db~translate'){
      #translate for the subject, field, value, and source column all values according to the action
      for(column in 3:6){
        translated<-translated+sum(df[,column]==record[,4],na.rm = T)
        df[,column]<-gsub(do[,4],do[,5],df[,column],fixed = T)
      }
      tr.count<-tr.count+1
    } else {
      cat(error('ERROR: This line of code should should never be triggered! The following record causes a problem:\n'))
      print(do)
    }
    #remove the record of the action
    df<-df[df[,1]!=i,]
  }
  
  #report success
  if(!quiet){
    if(rm.count>0 & tr.count>0){
      if((rm.count+tr.count)==nrow(actions.todo)){
        cat(note(rm.count,'records were successfully removed and',translated,'cells had their contents translated by',
                 tr.count,'translation actions\n'))
      } else {
        cat(warn(nrow(actions.todo)-(rm.count+tr.count),'out of',nrow(actions.todo),
                 'actions were not executed. Most likely these actions are removed by more recent actions\n'))
      }
      cat(note('Translated registry without the removed records is returned\n'))
    } else if(rm.count>0){
      if(rm.count==nrow(actions.todo)){
        cat(note(rm.count,'records were successfully removed\n'))
      } else {
        cat(warn(nrow(actions.todo)-rm.count,'out of',nrow(actions.todo),
                 'actions were not executed. Most likely these actions are removed by more recent actions\n'))
      }
      cat(note('Registry without the removed records is returned\n'))
    } else if(tr.count>0){
      if(tr.count==nrow(actions.todo)){
        cat(note(translated,'cells had their contents translated by',tr.count,'translation actions\n'))
      } else {
        cat(warn(nrow(actions.todo)-tr.count,'out of',nrow(actions.todo),
                 'actions were not executed. Most likely these actions are removed by more recent actions\n'))
      }
      cat(note('Translated registry  is returned\n'))
    } else {
      if(nrow(actions.todo)>0){
        cat(warn('No actions were executed, while actions were requested. Something went wrong?\n'))
      } else {
        cat(warn('No actions were executed because none of type',type,'is present in the registry\n'))
      }
      cat(note('Unchanged registry is returned\n'))
    }
  }
  
  #return the new registry
  return(df)
}
