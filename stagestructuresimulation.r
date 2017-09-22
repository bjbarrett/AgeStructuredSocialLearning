###initial pop conditions
timesteps <- 500 #length of simulation

##parameters
bn <- 1    ##avg broodsize of adult with non-adaptive behavior
ba <- 2*bn ##avg broodsize of adult with adaptive behavior. ba > bn
p_as <- 0.2  #prob an adapted adult produces stressed offspring
p_ns <- 0.8  #prob an nonadapted adult produces stressed offspring
s <- 0.8	#prob individual learner succesfully acquires adaptive behavior
U <- 0.1  #prob environment changes states
mu_s <- 0.1	#mortality rate of social learners
mu_i <- 0.3 #mortaility rate of individual learners

#initial frequencies of adults
nS_Aa0 <- 100
nS_An0 <- 0
nI_Aa0 <- 100
nI_An0 <- 0
n_Aa0 <- nS_Aa0 + nI_Aa0 #number of adults with adaptive behavior at t=0
n_An0 <- nS_An0 + nI_An0 #number of adults with adaptive behavior at t=0
N0 <- n_Aa0 + n_An0 #population size

nS_0 <- 0.5*N0 #number social learners at t=0
nI_0 <- 0.5*N0 #number social learners at t=0

nS_jh <- rep(0,timesteps + 1)
nI_jh <- rep(0,timesteps + 1)
nS_js <- rep(0,timesteps + 1)
nI_js <- rep(0,timesteps + 1)
nS_An <- rep(0,timesteps + 1)
nS_Aa <- rep(0,timesteps + 1)
nI_An <- rep(0,timesteps + 1)
nI_Aa <- rep(0,timesteps + 1)
n_Aa <- n_An <- n_Adults <- n_Juv <- N <- u <- rep(0,timesteps + 1)

nS_An[1] <- 0
nS_Aa[1] <- N0/2
nI_An[1] <- n_An[1] <- 0
nI_Aa[1] <- n_Aa[1] <-  N0/2
nS_jh[1] <- (N0/2)*ba*(1-p_as) + 0*bn*(1-p_ns)
nI_jh[1] <- (N0/2)*ba*(1-p_as) + 0*bn*(1-p_ns)
nS_js[1] <- (N0/2)*ba*p_as + 0*bn*p_as
nI_js[1] <- (N0/2)*ba*p_as + 0*bn*p_as
u[1] <- 0
N[1] <- nS_An[1] + nS_Aa[1] + nI_An[1] + nI_Aa[1] + nS_jh[1] + nI_jh[1] + nS_js[1] + nI_js[1]

plot( 0,fAa0 , xlim=c(0,timesteps) , ylim=c(0,1) , pch=15 , col="white" )

for (t in 1:timesteps){

		nS_js[t+1] <- (nS_Aa[t])*ba*p_as + (nS_An[t])*bn*p_ns #becomes zero
		nS_jh[t+1] <- (nS_Aa[t])*ba*(1-p_as) + (nS_An[t])*bn*(1-p_ns) #all healthy parents, equal brood sizes
		nI_js[t+1] <- (nI_Aa[t])*ba*p_as + (nI_An[t])*bn*p_ns #becomes zero
		nI_jh[t+1] <- (nI_Aa[t])*ba*(1-p_as) + (nI_An[t])*bn*(1-p_ns) #all healthy parents, equal brood sizes

		#recursions for individual learners 
		nI_Aa[t+1] <- ( nI_js[t] + nI_jh[t])*s*(1-mu_i)
		nI_An[t+1] <- ( nI_js[t] + nI_jh[t])*(1-s)*(1-mu_i)
		#recursions for social learners 
		nS_Aa[t+1] <-  ((nS_js[t] + nS_jh[t])*( (n_Aa[t]/(n_Aa[t] + n_An[t]))*(1-u[t]) + u[t]*0))*(1-mu_s)
		nS_An[t+1] <-  ((nS_js[t] + nS_jh[t])*( (n_An[t]/(n_Aa[t] + n_An[t]))*(1-u[t]) + u[t]*1))*(1-mu_s)
		#number of adaptive adults in this generation (for social learners later)
		n_Aa[t+1] <- nS_Aa[t+1] + nI_Aa[t+1]
		n_An[t+1] <- nS_An[t+1] + nI_An[t+1]
		N[t+1] <-  nS_An[t+1] + nI_An[t+1] + nS_Aa[t+1] + nI_Aa[t+1] + nS_jh[t+1] + nI_jh[t+1] + nS_js[t+1] + nI_js[t+1]
		u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing
}

plot((n_An/N)~ c(1:(timesteps +1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency  of adapted adults in population" ,  xlab="time/tiempo/zeit")
lines( (1:(timesteps +1)) , (n_Aa/N)  , col="slateblue", lty=1, lwd=1)
abline(h=mean(n_Aa/N) , lty=2 , lw=2)
#lines( (1:(timesteps +1)) , (n_An/N)  , col="orange", lty=1, lwd=2)
plot( (nS_An + nS_Aa)/N~ c(1:(timesteps +1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency  of adult social learners" ,  xlab="time/tiempo/zeit")
lines( (1:(timesteps +1)) , (nS_An + nS_Aa)/N  , col="orange", lty=1, lwd=1)
abline(h= mean((nS_An + nS_Aa)/N) , lty=2 , lw=2)

