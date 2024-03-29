# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.translate<-function(registry, #the previously saved registry
                       translate, #the previously saved database
                       quiet=F, #absolutely no information is printed
                       print.help=F, #no help message is printed, overridden by quiet flag
                       file.base.name='debugging'
){
  if(!quiet){
    cat(note('Running db.translate.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() and a 3-column dataframe with dates, original and replacement values\n'))
      cat(warn('Functionality of this function will be taken over by implementation of db.create.action and db.process.action'))
    }
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  tr<-translate
  
  # Limit to only the newest values
  table<-as.data.frame(table(tr[,2]))
  tr$keep1<-tr[,2] %in% table[table$Freq==1,1]
  tr$keep2<-tr[,3] %in% tr[,2]
  tr$keep<-tr$keep1&!tr$keep2
  
  keep.tr<-tr[tr$keep==T,1:3]
  process.tr<-tr[tr$keep==F,1:3]
  
  #   process.tr<-process.tr[order(process.tr[,2]),]
  omit<-data.frame()
  for (i in unique(process.tr[,2])){
    todo<-subset(tr,tr[,2]==i)
    if(nrow(todo)>1){
      todo<-todo[order(as.Date(todo[,1],format='%d-%m-%Y'),decreasing=T),]
    } else {
      if(todo[,3] %in% tr[,2]){
        todo<-rbind(todo,tr[tr[,2]==todo[,3],])
        todo<-todo[order(as.Date(todo[,1],format='%d-%m-%Y'),decreasing=T),]
      }
    }
    keep<-todo[1,1:3]
    keep.tr<-rbind(keep.tr,keep)
    omit<-rbind(omit,todo[2:nrow(todo),])
  }
  keep.tr<-unique(keep.tr)
  omit<-unique(omit)
  
  if(!quiet){
    cat(note(nrow(omit),'records of replacements were omitted because newer records replace those\n'))
  }
  
  # Find values to be translated
  for(i in 4:6){
    todo<-df[,i] %in% keep.tr[,2]
    if(sum(todo>0)){
      if(!quiet){
        cat(note(sum(todo),'values were replaced in column',colnames(df)[i],'\n'))
      }
      replacement<-merge(df[todo,c(1,i)],keep.tr[,2:3],by.x=colnames(df)[i],by.y=colnames(keep.tr)[2],all.x=T)[,c(2,1,3)]
      replacement<-replacement[order(replacement[,1]),]
      if(all(replacement[,2]==df[todo,i])){
        if(!quiet){
          print(replacement)
        }
        df[todo,i]<-replacement[,3]
      } else {
        stop('Something went wrong')
      }
    }
  }
  
  return(df)
}
