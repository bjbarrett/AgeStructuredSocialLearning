library(beepr)
########LOOPING GRAPH#######
###initial pop conditions
timesteps <- 4000 #length of simulation

##parameters
U <- c(0.01,0.05,0.1,0.2,0.3,0.4) #prob environment changes states
mu_i <- c(0.01,0.05,0.1,0.2,0.4) #mortality rate of individual learners
s <- c(1,0.8,0.6,0.4) #prob individual learner succesfully acquires adaptive behavior
ba <- c(1.1,1.5,2,3,4) ##avg broodsize of adult with adaptive behavior. ba > bn
bn <- 1   ##avg broodsize of adult with non-adaptive behavior
p_as <- 0  #prob an adapted adult produces stressed offspring
p_ns <- 1  #prob an nonadapted adult produces stressed offspring
mu_s <- 0   #mortality rate of social learners

#mu_n <- 1 # excess mortality factor of non-adapted individuals (to be added at some point with overlapping generations)

#create bins to store recursion values for each type
#juveniles by genotype
n_VVJH <- rep(0,timesteps + 1)
n_VVJS <- rep(0,timesteps + 1)
n_IIJH <- rep(0,timesteps + 1)
n_IIJS <- rep(0,timesteps + 1)
n_OOJH <- rep(0,timesteps + 1)
n_OOJS <- rep(0,timesteps + 1)

#adults by genotype
n_VVAa <- rep(0,timesteps + 1)
n_VVAn <- rep(0,timesteps + 1)
n_IIAa <- rep(0,timesteps + 1)
n_IIAn <- rep(0,timesteps + 1)
n_OOAa <- rep(0,timesteps + 1)
n_OOAn <- rep(0,timesteps + 1)

#other bines
n_Aa <- rep(0,timesteps + 1)        #num adapted adults
n_An <- rep(0,timesteps + 1)        #num non-adapted adukts
N <- rep(0,timesteps + 1)       #total population size
u <- rep(0,timesteps + 1)       #prob environ changes
fa <- rep(0,timesteps + 1)      #weird fun-cundity metric for adapted vertical learners
fn <- rep(0,timesteps + 1)      #weird fun-cundity metric for nonadapted vertical learners


    #initial simulation values
    N0 <- 180
    n_VVAa[1] <- 2
    n_VVAn[1] <- 0
    n_IIAa[1] <- 100
    n_IIAn[1] <- 0
    n_OOAa[1] <- 100
    n_OOAn[1] <- 0
    n_An[1] <- n_VVAn[1] + n_IIAn[1] + n_OOAn[1]
    n_Aa[1] <- n_VVAa[1] + n_IIAa[1] + n_OOAa[1]

    n_VVJH[1] <- 2
    n_VVJS[1] <- 2
    n_IIJH[1] <- 100
    n_IIJS[1] <- 100
    n_OOJH[1] <- 100
    n_OOJS[1] <- 100

pdf("II_OO_VVinvader_.pdf",width=18,height=9) 

par(mfrow=c(3,6))
par(mar=c(2,2,2.25,0.1))
    for (j in 1:length(mu_i)){
        for (k in 1:length(s)){
            for (l in 1:length(ba)){
                for (i in 1:length(U)){

            u[1] <- 0
            N[1] <- n_VVAa[1] + n_VVAn[1] + n_IIAa[1] + n_IIAn[1] + n_OOAa[1] + n_OOAn[1] + n_VVJH[1] + n_VVJS[1] + n_IIJH[1] + n_IIJS[1] + n_OOJH[1] + n_OOJS[1] 
            fa[1]<- (n_VVAa[1]*ba[l])/(n_VVAa[1]*ba[l] + n_VVAn[1]*bn)
            fn[1]<- (n_VVAn[1]*bn)/(n_VVAa[1]*ba[l] + n_VVAn[1]*bn)
            # fa[1]<- (n_VVAa[1])/(n_VVAa[1] + n_VVAn[1])
            # fn[1]<- (n_VVAn[1])/(n_VVAa[1] + n_VVAn[1])
            #recursions
                for (t in 1:timesteps)
                {
                    # juvenile recruitment
                    n_VVJH[t+1] <- (n_VVAa[t])*ba[l]*(1-p_as) + (n_VVAn[t])*bn*(1-p_ns)
                    n_VVJS[t+1] <- (n_VVAa[t])*ba[l]*p_as + (n_VVAn[t])*bn*p_ns
                    n_IIJH[t+1] <- (n_IIAa[t])*ba[l]*(1-p_as) + (n_IIAn[t])*bn*(1-p_ns)
                    n_IIJS[t+1] <- (n_IIAa[t])*ba[l]*p_as + (n_IIAn[t])*bn*p_ns
                    n_OOJH[t+1] <- (n_OOAa[t])*ba[l]*(1-p_as) + (n_OOAn[t])*bn*(1-p_ns)
                    n_OOJS[t+1] <- (n_OOAa[t])*ba[l]*p_as + (n_OOAn[t])*bn*p_ns
                    
                    #adult recruitment
                    n_VVAa[t+1] <- (n_VVJS[t] + n_VVJH[t])*(1-u[t])*fa[t]*(1-mu_s)
                    n_VVAn[t+1] <- (n_VVJS[t] + n_VVJH[t])*((1-u[t])*fn[t] + u[t])*(1-mu_s)
                    n_IIAa[t+1] <- (n_IIJS[t] + n_IIJH[t])*s[k]*(1-mu_i[j])
                    n_IIAn[t+1] <- (n_IIJS[t] + n_IIJH[t])*(1-s[k])*(1-mu_i[j])
                    n_OOAa[t+1] <- (n_OOJS[t] + n_OOJH[t])*(n_Aa[t]/(n_Aa[t] + n_An[t]))*(1-u[t])*(1-mu_s)
                    n_OOAn[t+1] <- (n_OOJS[t] + n_OOJH[t])*((1-u[t])*(n_An[t]/(n_Aa[t] + n_An[t])) + u[t])*(1-mu_s)
                    
                    
                    n_Aa[t+1] <- n_VVAa[t+1] + n_IIAa[t+1] + n_OOAa[t+1]
                    n_An[t+1] <- n_VVAn[t+1] + n_IIAn[t+1] + n_OOAn[t+1]
                    N[t+1] <- n_VVAa[t+1] + n_VVAn[t+1] + n_IIAa[t+1] + n_IIAn[t+1] + n_OOAa[t+1] + n_OOAn[t+1] + n_VVJH[t+1] + n_VVJS[t+1] + n_IIJH[t+1] + n_IIJS[t+1] + n_OOJH[t+1] + n_OOJS[t+1]
                    u[t+1] <- rbinom(n=1,prob=U[i],size=1) #sample environment changing
                    fa[t+1]<- (n_VVAa[t+1])/(n_VVAa[t+1] + n_VVAn[t+1])
                    fn[t+1]<- (n_VVAn[t+1])/(n_VVAa[t+1] + n_VVAn[t+1])

                    #normalize so math does not explode
                    n_VVAa[t+1] <- n_VVAa[t+1]/N[t+1]
                    n_VVAn[t+1] <- n_VVAn[t+1]/N[t+1] 
                    n_OOAa[t+1] <- n_OOAa[t+1]/N[t+1]
                    n_OOAn[t+1] <- n_OOAn[t+1]/N[t+1] 
                    n_IIAa[t+1] <- n_IIAa[t+1]/N[t+1]
                    n_IIAn[t+1] <- n_IIAn[t+1]/N[t+1]
                    n_VVJH[t+1] <- n_VVJH[t+1]/N[t+1]
                    n_VVJS[t+1] <- n_VVJS[t+1]/N[t+1] 
                    n_OOJH[t+1] <- n_OOJH[t+1]/N[t+1]
                    n_OOJS[t+1] <- n_OOJS[t+1]/N[t+1] 
                    n_IIJH[t+1] <- n_IIJH[t+1]/N[t+1]
                    n_IIJS[t+1] <- n_IIJS[t+1]/N[t+1]  
                    n_Aa[t+1] <- n_Aa[t+1]/N[t+1]
                    n_An[t+1] <- n_An[t+1]/N[t+1]
                    N[t+1] <-  N[t+1]/  N[t+1]

                }

            # compute proportions of each strategy for summary plot
            p_VV <- ( n_VVJH + n_VVJS + n_VVAa + n_VVAn ) / N
            p_II <- ( n_IIJH + n_IIJS + n_IIAa + n_IIAn ) / N
            p_OO <- ( n_OOJH + n_OOJS + n_OOAa + n_OOAn ) / N


            p_VVA <- ( n_VVAa + n_VVAn ) / (n_VVAa + n_VVAn + n_IIAa + n_IIAn + n_OOAa + n_OOAn )
            p_IIA <- ( n_IIAa + n_IIAn ) / (n_VVAa + n_VVAn + n_IIAa + n_IIAn + n_OOAa + n_OOAn )
            p_OOA <- ( n_OOAa + n_OOAn ) / (n_VVAa + n_VVAn + n_IIAa + n_IIAn + n_OOAa + n_OOAn )

            q_VV <-  n_VVAa / ( n_VVAa + n_VVAn )
            q_II <-  n_IIAa / ( n_IIAa + n_IIAn )
            q_OO <-  n_OOAa / ( n_OOAa + n_OOAn )

            # q_VV <-  n_VVAa / ( n_VVAa + n_VVAn + n_IIAa + n_IIAn + n_OOAa + n_OOAn)
            # q_II <-  n_IIAa / ( n_VVAa + n_VVAn + n_IIAa + n_IIAn + n_OOAa + n_OOAn)
            # q_OO <-  n_OOAa / ( n_VVAa + n_VVAn + n_IIAa + n_IIAn + n_OOAa + n_OOAn)


            plot( p_VV , ylim=c(0,1) , type="l" , col="violet" ,  main = paste("u= ", U[i], "mu_i= ", mu_i[j], "s= ", s[k], "ba= ", ba[l]) , cex.main=0.8)
            lines( 1:length(p_II) , p_II , col="red" )
            lines( 1:length(p_OO) , p_OO , col="black")
            lines( 1:length(p_VV) , p_VV , col="violet" )
            legend("topleft" , c("VV", "II", "OO") , lty=1 , lwd=c(1,1,1) , col=c("violet", "red", "black" ) , pch=c(15,15,15) ,bty='n' , cex=.7)
            }
        }
    }
}

dev.off()
beep(3)