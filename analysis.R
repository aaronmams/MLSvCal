
require(data.table)
require(ggplot2)
require(dplyr)

########################################################
########################################################
#parameters

#the backbone of this analysis is really simple,
# it is based on what you make in year 1 and what you
# make in year 5 according to 4 different paths


#starting non-MLS salary for UC Berkely grads
s.CAL <- 60400

#starting non-MLS salary for the no college option
s.NC <- 0.62*60400

# salary for the professional soccer development contract
s.DEV <- 26000

# salary for the college option (to account for living expenses being paid for)
s.COL <- 14000

# starting salary for the MLS contract (based on the current median MLS salary)
s.MLS <- 91000

g.CAL <- 1.08
g.MLS <- 1.1
g.NC <- 1.08

#set up the earnings caps 
cap <- c(150000,1000000,150000,1000000)

df <- data.frame(path=c(1:4),year1=c(s.DEV,s.DEV,s.COL,s.COL),year5=c(s.NC,s.MLS,s.CAL,s.MLS),
                 growth=c(g.NC,g.MLS,g.CAL,g.MLS), cap=cap,
                 desc=c("developmental contract + no MLS","developmental contract + MLS","college + no MLS","college + MLS"))

#time horizon
T <- 14

#beta - the discount rate
beta <- 0.97

#probability of MLS success in the college scenario
p.CAL <- .5

#probability premium for the non college option
p.prem <- 0
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

#the following analysis is based on 4 paths:

#1.  developmental contract/no MLS
#2.  developmental contract/MLS
#3.  college/no MLS
#4.  college/MLS


#--------------------------------------------------------------

npv <- function(path){
  E <- data.frame(rbindlist(lapply(c(1:T),function(t){
    if(t<5){
      v <- df$year1[df$path==path] * beta^(t-1)
    }else{
      v <- min(df$cap[df$path==path],(df$year5[df$path==path] * (df$growth[df$path==path]^(t-5)))) * beta^(t-1)
    }
    return(data.frame(v=v))
  })))
  return(E)
}

#------------------------------------------------------------------

#-------------------------------------------------------------------
#so now we'll call the function for each path
#
#	For starters we assume the college-no college
#	paths are equally likely to lead to an MLS
#	contract in the 5th year.  This means the expected
# 	net present value of the no-college path is 
#		0.5*npv[no college/no MLS] + 0.5*npv[no college/no MLS]
#	
#	and the net present value of the college path is:
#		0.5*npv[college/no MLS] * 0.5*npv[college/MLS]
#

earnings <- data.frame(npv(path=1),npv(path=2),npv(path=3),npv(path=4))
names(earnings) <- c("path1","path2", "path3", "path4")

#Initial Run
EV <- data.frame(path=c("no college","college"),
                 EV=c( ((1-(p.CAL+p.prem))*sum(earnings$path1))+((p.CAL+p.prem)*sum(earnings$path2)), 
                       ((1-p.CAL)*sum(earnings$path3))+(p.CAL*sum(earnings$path4)) ))

#now let's plot the probability premiums. for this we'll write another function that accepts
# the probability premium as an input

EV.sim <- function(p.add){
  return(data.frame(path=c("no college","college"),
                    val=c( ((1-(p.CAL+p.add))*sum(earnings$path1))+((p.CAL+p.add)*sum(earnings$path2)),
                           ((1-p.CAL)*sum(earnings$path3))+ ((p.CAL)*sum(earnings$path4))),
                    prem=c(rep(p.add,2)))
  )
}

df.prem <- data.frame(rbindlist(lapply(seq(from=0,to=0.2,by=0.01),EV.sim)))
ggplot(df.prem,aes(x=prem,y=val/100000,color=path))+geom_line()+geom_point() + 
    ylab("NPV ($100,000)") + xlab("probability premium") + theme_bw()

png(file="U:/personal/mls_plot1.jpg",width=400,height=400)
ggplot(df.prem,aes(x=prem,y=val/100000,color=path))+geom_line()+geom_point() + 
  ylab("NPV ($100,000)") + xlab("probability premium") + theme_bw()
dev.off()

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################


#---------------------------------------------------------------------
#Life time analysis

#the following assumptions are made for the lifetime analysis:

#	1.  conditional on making the MLS the player will have a 10 year career
#	2.  after 10 years the non college educated player's salary reverts back to 
#		the year 11 salary for a non-college educated worker and the college educated
#   salary reverts back to the year 11 salary for a Berkeley educated worker
#	3.  the rate of earnings growth is 1.5 times higher for Berkeley educated workers than for
#		non college educated worker
#	4.  the period of analysis is 25 years
#      

# make a few changes to the base data frame
df$growth <- c(1.052,1.1,1.08,1.1)

df$year15 <- 0
df$year15[df$path==1] <- df$year5[df$path==1]*(df$growth[df$path==1])^10

#we assume that once the 10 year MLS career is over the player's year 16
# starting salary is as if he were a high school educated worker with 10 years
# work experience

df$year15[df$path==2] <- df$year5[df$path==1]*(df$growth[df$path==1])^10
#df$year16[df$path==2] <- 80000

#the year 15 salary for a Cal grad is just the continuation of his prior 
# earnings growth
df$year15[df$path==3] <- df$year5[df$path==3]*(df$growth[df$path==3])^10

#the year 15 salary for a Cal grad who player 10 years in the MLS is
# equal to the year 11 salary for a Cal grad with 10 years experience
df$year15[df$path==4] <- df$year5[df$path==3]*(df$growth[df$path==3])^10

#amend the npv calculation sligthly to introduce the proper starting
# salary at the termination of the 10 year MLS contract.  Also, imposing the cap
# is a little funky for the lifetime analysis so we'll break this down into parts
val <- function(path){
  E <- data.frame(rbindlist(lapply(c(1:T),function(t){
    if(t<5){
      v <- df$year1[df$path==path]
    }else if(t>=5 & t<15){
      v <- (df$year5[df$path==path] * (df$growth[df$path==path]^(t-5))) 
    }else{
      v <- min(df$cap[df$path==path],(df$year15[df$path==path]*(df$growth[df$path==path]^(t-15))))
    }
    return(data.frame(v=v))
  })))
  return(E)
}
#---------------------------------------------------------------------


#initial lifetime run
T<- 25
earnings <- data.frame(val(path=1),val(path=2),val(path=3),val(path=4))
names(earnings) <- c("path1","path2","path3","path4")

#this part is clunky but I gots other work to do tonigth so I'm not investing in elegance...
# feel free to clone this shit in GitHub and push some changes!

#now impose the cap by hand for paths 3 and 4 and discount to PV terms
earnings$path2[which(earnings$path2>df$cap[1] & as.numeric(rownames(earnings))>15)] <- df$cap[1]
earnings$path4[which(earnings$path4>df$cap[3] & as.numeric(rownames(earnings))>15)] <- df$cap[3]

#now discount to present value
earnings <- t(sapply(1:nrow(earnings),function(i)beta^(i-1)*earnings[i,]))
earnings <- data.frame(path1=unlist(earnings)[1:25],
                       path2=unlist(earnings)[26:50],
                       path3=unlist(earnings)[51:75],
                       path4=unlist(earnings)[76:100])


EV <- data.frame(path=c("no college","college"),
                 EV=c( ((1-(p.CAL+p.prem))*sum(earnings$path1))+((p.CAL+p.prem)*sum(earnings$path2)), 
                       ((1-p.CAL)*sum(earnings$path3))+(p.CAL*sum(earnings$path4))) )


#simulate some probability premiums
EV.sim <- function(p.add){
  return(data.frame(path=c("no college","college"),
                    val=c( ((1-(p.CAL+p.add))*sum(earnings$path1))+((p.CAL+p.add)*sum(earnings$path2)),
                           ((1-p.CAL)*sum(earnings$path3))+ ((p.CAL)*sum(earnings$path4))),
                    prem=c(rep(p.add,2)))
  )
}

df.prem <- data.frame(rbindlist(lapply(seq(from=0,to=0.5,by=0.02),EV.sim)))
png(file="U:/personal/mlsplot2.png",width=400,height=400)
ggplot(df.prem,aes(x=prem,y=val/100000,color=path))+geom_line()+geom_point() + 
  ylab("NPV ($100,000)") + xlab("probability premium") + theme_bw()
dev.off()






