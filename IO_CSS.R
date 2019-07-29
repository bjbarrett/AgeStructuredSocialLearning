library(beepr)
########LOOPING GRAPH#######
###initial pop conditions
###initial pop conditions
timesteps <- 400 #length of simulation

##parameters
U <- c(0.01,0.05,0.1,0.2,0.4) #prob environment changes states
U <- 0.2
mu_s_vec <- rep(0,4)   #mortality rate of social learners
mu_s<- mu_s_vec[1]   #mortality rate of social learners

#mu_i_prop <- c(1.1,1.5,2,3)
mu_i <- c( 0.05, 0.10, 0.20)

s <- c(1,0.75,0.5) #prob individual learner succesfully acquires adaptive behavior
ba <- c(1.05,1.25,1.5,2) ##avg broodsize of adult with adaptive behavior. ba > bn
bn <- 1   ##avg broodsize of adult with non-adaptive behavior
p_as <- 0 #prob an adapted adult produces stressed offspring
p_ns <- 1 #prob an nonadapted adult produces stressed offspring

#mu_s <- 0   #mortality rate of social learners
#mu_n <- 1 # excess mortality factor of non-adapted individuals (to be added at some point with overlapping generations)


#create bins to store recursion values for each type
#juvenile recursions
n_IOJH <- rep(0,timesteps + 1)
n_IOJS <- rep(0,timesteps + 1)

#adult recursions
n_IOAa <- rep(0,timesteps + 1)
n_IOAn <- rep(0,timesteps + 1)


#storage for terms of interest
n_Aa <- rep(0,timesteps + 1)
n_An <- rep(0,timesteps + 1)
N <- rep(0,timesteps + 1)
u <- rep(0,timesteps + 1)
Q <- rep(0,timesteps + 1)


n_IOAa[1] <- 0.25
n_IOAn[1] <- 0.25
n_An[1] <- n_IOAn[1] 
n_Aa[1] <- n_IOAa[1]
Q[1] <- 1
n_IOJH[1] <- 0.25
n_IOJS[1] <- 0.25





# pdf("IO_p_as=0_p_ns=1.pdf",width=18,height=9)
# 
# par(mfrow=c(3,5))
# par(mar=c(2,2,2.25,0.1))
for (j in 1:length(mu_i)){
    for (k in 1:length(s)){
        for (l in 1:length(ba)){
            for (i in 1:length(U)){
                
                
u[1] <- 0
N[1] <- n_IOAa[1] + n_IOAn[1] + n_IOJH[1] + n_IOJS[1] 


#recursions
for (t in 1:timesteps)
{
    # juvenile recruitment
    n_IOJH[t+1] <- n_IOAa[t]*ba[l]*(1-p_as) + n_IOAn[t]*bn*(1-p_ns)
    n_IOJS[t+1] <- n_IOAa[t]*ba[l]*p_as + n_IOAn[t]*bn*p_ns

    #adult recruitment
    n_IOAa[t+1] <- n_IOJS[t]*s[k]*(1-mu_i[j]) + (1-u[t])*n_IOJH[t]*(Q[t])*(1-mu_s)
    n_IOAn[t+1] <- n_IOJS[t]*(1-s[k])*(1-mu_i[j]) + n_IOJH[t]*((1-u[t])*(1-Q[t]) + u[t])*(1-mu_s)
   

    N[t+1] <- n_IOAa[t+1] + n_IOAn[t+1] + n_IOJH[t+1] + n_IOJS[t+1]
    u[t+1] <- rbinom(n=1,prob=U[i],size=1) #sample environment changing

    Q[t+1] <- n_IOAa[t+1]/(n_IOAa[t+1] + n_IOAn[t+1]) #freq of adaptive behavior among adults

    #normalize so math does not explode
    n_IOAa[t+1] <- n_IOAa[t+1]/N[t+1]
    n_IOAn[t+1] <- n_IOAn[t+1]/N[t+1]
    n_IOJH[t+1] <- n_IOJH[t+1]/N[t+1]
    n_IOJS[t+1] <- n_IOJS[t+1]/N[t+1]
    

    #N[t+1] <-  N[t+1]/  N[t+1]

}

# compute proportions of each strategy for summary plot
p_IO <- ( n_IOJH + n_IOJS + n_IOAa + n_IOAn ) 

q_IO <-  n_IOAa / ( n_IOAa + n_IOAn )

qAdults <- n_Aa/(n_Aa + n_An)


plot( Q, ylim=c(0,1) , type="l" , col="darkorchid3",  main = paste("u= ", U[i], "mu_i= ", mu_i[j], "s= ", s[k], "ba= ", ba[l] , "bn= ", bn, "p_as= ", p_as, "p_ns= ", p_ns) , cex.main=0.8)
# lines( 1:length(n_IOJH) , n_IOJH , col="blue" , lty=1)
# lines( 1:length(n_IOJS) , n_IOJS , col="blue" , lty=2)
# lines( 1:length(n_IOAa) , n_IOAa , col="violet" , lty=1)
# lines( 1:length(n_IOAn) , n_IOAn , col="violet" , lty=2)
abline(h=mean(Q) , col="black" , lty=3)
# legend("topleft" , c("IO", "II", "OO" , "OV" ) , lty=1 , lwd=c(1,1,1,1,1) , col=c("darkorchid3", "blue", "red" , "orange" ) , pch=c(15,15,15,15,15) ,bty='n', cex=0.7)
# legend("topleft" , c("IO") , lty=1 , lwd=c(1) , col=c("darkorchid3") , pch=c(15) ,bty='n', cex=0.7)
legend("topleft" , c("IO", "II", "OO" , "OV" ) , lty=c(1,2,1,2) , lwd=c(1,1,1,1) , col=c("blue", "blue", "violet" , "violet" ) , pch=c(15,15,15,15,15) ,bty='n', cex=0.7)


            }
        }
    }
}

# dev.off()
beep(3)

# 
# ########LOOPING GRAPH FREQUENCIESSSS#######
# ###initial pop conditions
# timesteps <- 400 #length of simulation
# 
# ##parameters
# U <- c(0.01,0.05,0.1,0.2,0.4) #prob environment changes states
# U <- 0.2
# mu_s_vec <- rep(0,4)   #mortality rate of social learners
# mu_s<- mu_s_vec[1]   #mortality rate of social learners
# 
# #mu_i_prop <- c(1.1,1.5,2,3)
# mu_i <- c( 0.05, 0.10, 0.20)
# mu_i <- c( 0.1)
# 
# s <- c(1,0.75,0.5) #prob individual learner succesfully acquires adaptive behavior
# s<- 0.8
# ba <- c(1.05,1.25,1.5,2) ##avg broodsize of adult with adaptive behavior. ba > bn
# ba <- 1.5
# bn <- 1   ##avg broodsize of adult with non-adaptive behavior
# p_as <- 0 #prob an adapted adult produces stressed offspring
# p_ns <- 1 #prob an nonadapted adult produces stressed offspring
# 
# #mu_s <- 0   #mortality rate of social learners
# #mu_n <- 1 # excess mortality factor of non-adapted individuals (to be added at some point with overlapping generations)
# 
# 
# #create bins to store recursion values for each type
# #juvenile recursions
# n_IOJH <- rep(0,timesteps + 1)
# n_IOJS <- rep(0,timesteps + 1)
# 
# #adult recursions
# n_IOAa <- rep(0,timesteps + 1)
# n_IOAn <- rep(0,timesteps + 1)
# 
# 
# #storage for terms of interest
# n_Aa <- rep(0,timesteps + 1)
# n_An <- rep(0,timesteps + 1)
# N <- rep(0,timesteps + 1)
# u <- rep(0,timesteps + 1)
# Q <- rep(0,timesteps + 1)
# 
# 
# 
# n_IOAa[1] <- 0.25
# n_IOAn[1] <- 0.25
# n_An[1] <- n_IOAn[1] 
# n_Aa[1] <- n_IOAa[1]
# Q[1] <- 1
# n_IOJH[1] <- 0.25
# n_IOJS[1] <- 0.25
# 
# 
# # 
# # pdf("IO_p_as=0_p_ns=1_freq.pdf",width=18,height=9)
# # 
# # par(mfrow=c(3,5))
# # par(mar=c(2,2,2.25,0.1))
# for (j in 1:length(mu_i)){
#     for (k in 1:length(s)){
#         for (l in 1:length(ba)){
#             for (i in 1:length(U)){
#                 
#                 
# u[1] <- 0
# N[1] <- n_IOAa[1] + n_IOAn[1] + n_IOJH[1] + n_IOJS[1] 
# 
# 
# #recursions
# for (t in 1:timesteps)
# {
#     # juvenile recruitment
#     N[t+1] <- n_IOAa[t]*ba[l]*(1-p_as) + n_IOAn[t]*bn*(1-p_ns) + n_IOAa[t]*ba[l]*p_as + n_IOAn[t]*bn*p_ns + n_IOJS[t]*s[k]*(1-mu_i[j]) + (1-u[t])*n_IOJH[t]*(Q[t])*(1-mu_s) + n_IOJS[t]*(1-s[k])*(1-mu_i[j]) + n_IOJH[t]*((1-u[t])*(1-Q[t]) + u[t])*(1-mu_s)
#     n_IOJH[t+1] <- n_IOAa[t]*ba[l]*(1-p_as) + n_IOAn[t]*bn*(1-p_ns)
# 
#     #adult recruitment
#     n_IOAa[t+1] <- n_IOJS[t]*s[k]*(1-mu_i[j]) + (1-u[t])*n_IOJH[t]*(Q[t])*(1-mu_s)
#     n_IOAn[t+1] <- n_IOJS[t]*(1-s[k])*(1-mu_i[j]) + n_IOJH[t]*((1-u[t])*(1-Q[t]) + u[t])*(1-mu_s)
#     
#     n_IOAa[t+1] <- n_IOAa[t+1]/N[t+1]
#     n_IOAn[t+1] <- n_IOAn[t+1]/N[t+1]
#     n_IOJH[t+1] <- n_IOJH[t+1]/N[t+1]
#     n_IOJS[t+1] <- 1 -  n_IOAa[t+1] - n_IOAn[t+1] - n_IOJH[t+1]
# 
# 
#     u[t+1] <- rbinom(n=1,prob=U[i],size=1) #sample environment changing
# 
#     Q[t+1] <- n_IOAa[t+1]/(n_IOAa[t+1] + n_IOAn[t+1]) #freq of adaptive behavior among adults
# 
# 
# }
# 
# # compute proportions of each strategy for summary plot
# p_IO <- ( n_IOJH + n_IOJS + n_IOAa + n_IOAn ) 
# 
# q_IO <-  n_IOAa / ( n_IOAa + n_IOAn )
# 
# qAdults <- n_Aa/(n_Aa + n_An)
# 
# 
# plot( Q, ylim=c(0,1) , type="l" , col="darkorchid3",  main = paste("u= ", U[i], "mu_i= ", mu_i[j], "s= ", s[k], "ba= ", ba[l] , "bn= ", bn, "p_as= ", p_as, "p_ns= ", p_ns) , cex.main=0.8)
# # lines( 1:length(n_IOJH) , n_IOJH , col="blue" , lty=1)
# # lines( 1:length(n_IOJS) , n_IOJS , col="blue" , lty=2)
# # lines( 1:length(n_IOAa) , n_IOAa , col="violet" , lty=1)
# # lines( 1:length(n_IOAn) , n_IOAn , col="violet" , lty=2)
# abline(h=mean(Q) , col="black" , lty=3)
# # legend("topleft" , c("IO", "II", "OO" , "OV" ) , lty=1 , lwd=c(1,1,1,1,1) , col=c("darkorchid3", "blue", "red" , "orange" ) , pch=c(15,15,15,15,15) ,bty='n', cex=0.7)
# # legend("topleft" , c("IO") , lty=1 , lwd=c(1) , col=c("darkorchid3") , pch=c(15) ,bty='n', cex=0.7)
# legend("topleft" , c("IO", "II", "OO" , "OV" ) , lty=c(1,2,1,2) , lwd=c(1,1,1,1) , col=c("blue", "blue", "violet" , "violet" ) , pch=c(15,15,15,15,15) ,bty='n', cex=0.7)
# 
# 
#             }
#         }
#     }ā
# }
# 
# # dev.off()
# beep(3)
