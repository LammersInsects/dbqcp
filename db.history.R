# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.history<-function(registry, #the previously saved registry
                     do='recording.activities', #recording.activities, time.first.last, or a combination of those using c()
                     filename='debugging' #the base filename
){
  print('Running db.history.R ...')
  print('This function expects a registry as produced by db.registry()')
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  if('recording.activities' %in% do){
    # Plot activity of data recording
    time.df<-max(df[,2])-min(df[,2])
    print(paste(time.df,'days between oldest and newest records'))
    breaks<-julian(max(df[,2]),origin=min(df[,2]))[1]
    if(breaks==0){breaks<-1}
    hist(df[,2],breaks=breaks,freq=T,
         main='Barplot of recording activity',
         xlab='',
         xaxt='n',
         yaxs='r',
         xaxs='r')
    axis.Date(1, at = seq(min(df[,2]), max(df[,2]), length.out=20),
              format= "%m-%Y", las = 2) 
    axis(2)
    print('A barplot of recording activities is drawn')
  }
  
  if('time.first.last' %in% do){
    # Plot a histogram of the duration between the first and last records of a subject
    subjects<-as.data.frame(table(df[,3]),stringsAsFactors = F)
    for (i in 1:nrow(subjects)){
      records<-df[df[,3]==subjects$Var1[i],]
      duration<-julian(max(records[,2]),origin=min(records[,2]))[1]
      if(duration==0){duration<-1}
      subjects$Duration[i]<-duration
    }
    x.axis<-pretty(subjects$Duration)
    hist(subjects$Duration, breaks=max(subjects$Duration), xlim = x.axis[c(1,length(x.axis))],
         main='Histogram of time\nbetween first and last record per subject',xlab='Duration [days]')
    print('A histogram of the time between first and last record per subject is drawn')
  }
  
}
