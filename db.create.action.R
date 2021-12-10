# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

#To create a removal

#a function like this:
#db.create.action(action = 'remove', record.ID = 5504, reason = 'invoerfout', user='ML', filename='pot')

#should output:
#  "ID";"Datum";"Pot";"Plant";"Waarde";"Bron";"Recorded.by"
#?;Sys.Date();action.name;record.ID;reason.ok;"db.create.action";user


db.create.action<-function(registry, #the previously saved registry
                    remove.IDs, #IDs of the records to remove
                    invert=F, #include everything except the remove.IDs, invert=TRUE gives only the records that conform to remove.IDs
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
  
  if(action=='remove'){
    action.name<-'db~remove'
  } else if(action=='translate'){
    action.name<-'db.translate'
  } else {
    cat('\n')
    stop()
  }
  
  #check provided record ID
  #TODO: db.translate would not work with record IDs!
  if(!is.numeric(record.ID)){
    cat(error('Please provide the record ID as a number\n'))
    stop()
  } else {
    #TODO check here whether it is a non-decimal number
  }
  
  #check reason provided
  #TODO: db.translate would not need a reason!
  if(!emptyvalues(as.character(reason))){
    reason.ok<-reason
  } else {
    cat(error('Please provide a reason why the action should be added\n'))
    stop()
  }
  
  #check user
  #TODO
  
  #create new record
  record<-c('?',format(Sys.Date(),'%d.%m.%Y'),action.name,record.ID,reason.ok,"db.create.action",user)
  
  #write this to a file with pending new records 
  
  #these should be imported at the next run of db.registry with the same filename
  
  
}
