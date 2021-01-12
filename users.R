# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga

temp.wd<-getwd()
setwd(paste(wd.base, 'Common_scripts/database_package', sep=''))

#Load users that have clearance
users<-read.csv('users.csv',header=T,sep=';')
users<-users[,1]

# Probably the same could be achieved, but with users hidden, using methods described here:
# https://rpubs.com/chrisbrunsdon/local2
hidden.env <- new.env()
local(users<-users, env=hidden.env)
rm(users)
# users
# local(users,env=hidden.env)
# And then db.registry() needs to evaluate users in the hidden.env

setwd(temp.wd)