# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

#To create a removal

#a function like this:
#db.create.action(action = 'remove', record.ID = 5504, reason = 'invoerfout', user='ML', filename='pot')

#should output:
#  "ID";"Datum";"Pot";"Plant";"Waarde";"Bron";"Recorded.by"
#?;Sys.Date();action.name;record.ID;reason.ok;"db.create.action";user


db.create.action<-function(registry, #the previously saved registry
                           action, #remove or translate
                           record.ID, #IDs of the record(s) to remove
                           reason, #the reason to remove record(s)
                           original, #the term to be translated
                           translation, #what the original should be changed into
                           user, #the user who provides the input
                           quiet=F, #absolutely no information is printed
                           print.help=F, #no help message is printed, overridden by quiet flag
                           write.output=F, #flag whether output should be written to working directory
                           filename='debugging' #the base filename
){
  if(!quiet){
    cat(note('Running db.create.action.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() and a set of values to remove\n'))
    }
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  #check user
  #TODO
  
  if(action=='remove'){
    if(missing(record.ID) | missing(reason)){
      cat(error('Please provide a record.ID and a reason when using < action = remove >\n'))
      stop()
    }
    
    action.name<-'db~remove'
    
    #check provided record ID
    #TODO add support for multiple records to remove for the same reason
    if(!is.numeric(record.ID)){
      cat(error('Please provide the record ID as a number\n'))
      stop()
    } else {
      if(!record.ID%%1==0){
        cat(error('Please provide the record idea as a whole number\n'))
        stop()
      }
    }
    
    #check reason provided
    if(!emptyvalues(as.character(reason))){
      reason.ok<-reason
    } else {
      cat(error('Please provide a reason why the action should be added\n'))
      stop()
    }
    
    #create new record
    record<-c('?',format(Sys.Date(),'%d.%m.%Y'),action.name,record.ID,reason.ok,"db.create.action",user,0)
    #TODO remove last zero from record once column Verified is purged from the package
    
  } else if(action=='translate'){
    if(missing(original) | missing(translation)){
      cat(error('Please provide the original text and the desired translation when using < action = translate >\n'))
      stop()
    }
    
    action.name<-'db~translate'
    
    #TODO here write script to create a translate action
    
  } else {
    cat(error('Currently, only the actions <remove> and <translate> are supported\n'))
    stop()
  }
  
  #reformat record
  record<-as.data.frame(record)
  if(ncol(record)==1){
    record<-t(record)
    record<-data.frame(record)
  }
  colnames(record)<-colnames(registry)
  rownames(record)<-NULL
  
  #write this to a file with pending new records 
  
  #these should be imported at the next run of db.registry with the same filename
  
  return(record)
  
}
