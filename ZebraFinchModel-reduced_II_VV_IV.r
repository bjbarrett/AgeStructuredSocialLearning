###initial pop conditions
timesteps <- 200 #length of simulation

##parameters
bn <- 1    ##avg broodsize of adult with non-adaptive behavior
ba <- 2*bn ##avg broodsize of adult with adaptive behavior. ba > bn
p_as <- 0.2  #prob an adapted adult produces stressed offspring
p_ns <- 0.8  #prob an nonadapted adult produces stressed offspring
s <- 0.5	#prob individual learner succesfully acquires adaptive behavior
U <- 0.1  #prob environment changes states
mu_s <- 0.1	#mortality rate of social learners
mu_i <- 0.11 #mortality rate of individual learners
mu_n <- 1 # excess mortality factor of non-adapted individuals

#create bins to store recursion values for each type
n_IVJH <- rep(0,timesteps + 1)
n_IVJS <- rep(0,timesteps + 1)
n_VVJH <- rep(0,timesteps + 1)
n_VVJS <- rep(0,timesteps + 1)
n_IIJH <- rep(0,timesteps + 1)
n_IIJS <- rep(0,timesteps + 1)

n_IIAa <- rep(0,timesteps + 1)
n_IIAn <- rep(0,timesteps + 1)
n_VVAa <- rep(0,timesteps + 1)
n_VVAn <- rep(0,timesteps + 1)
n_IVAa <- rep(0,timesteps + 1)
n_IVAn <- rep(0,timesteps + 1)


n_Aa <- rep(0,timesteps + 1)
n_An <- rep(0,timesteps + 1)
N <- rep(0,timesteps + 1)
u <- rep(0,timesteps + 1)
fa <- rep(0,timesteps + 1)
fn <- rep(0,timesteps + 1)

N0 <- 200
n_VVAa[1] <- N0/6
n_VVAn[1] <- N0/6
n_IIAa[1] <- N0/6
n_IIAn[1] <- N0/6
n_IVAa[1] <- N0/6
n_IVAn[1] <- N0/6
n_An[1] <- n_VVAn[1] + n_IIAn[1] + n_IVAn[1]
n_Aa[1] <- n_VVAa[1] + n_IIAa[1] + n_IVAa[1]

n_VVJH[1] <- 10
n_VVJS[1] <- 10
n_IIJH[1] <- 10
n_IIJS[1] <- 10
n_IVJH[1] <- 10
n_IVJS[1] <- 10

u[1] <- 0
N[1] <- n_VVAa[1] + n_VVAn[1] + n_IIAa[1] + n_IIAn[1] + n_IVAa[1] + n_IVAn[1] + n_VVJH[1] + n_VVJS[1] + n_IIJH[1] + n_IIJS[1] + n_IVJH[1] + n_IVJS[1]
fa[1]<- (n_Aa[1]*ba)/(n_Aa[1]*ba + n_An[1]*bn)
fn[1]<- (n_An[1]*bn)/(n_Aa[1]*ba + n_An[1]*bn)

#recursions
for (t in 1:timesteps)
{
    # juvenile recruitment
    n_VVJH[t+1] <- (n_VVAa[t])*ba*(1-p_as) + (n_VVAn[t])*bn*(1-p_ns)
    n_VVJS[t+1] <- (n_VVAa[t])*ba*p_as + (n_VVAn[t])*bn*p_ns
    n_IIJH[t+1] <- (n_IIAa[t])*ba*(1-p_as) + (n_IIAn[t])*bn*(1-p_ns)
    n_IIJS[t+1] <- (n_IIAa[t])*ba*p_as + (n_IIAn[t])*bn*p_ns
    n_IVJH[t+1] <- (n_IVAa[t])*ba*(1-p_as) + (n_IVAn[t])*bn*(1-p_ns)
    n_IVJS[t+1] <- (n_IVAa[t])*ba*p_as + (n_IVAn[t])*bn*p_ns
    
    #adult recruitment
    n_VVAa[t+1] <- (n_VVJS[t] + n_VVJH[t])*(1-u[t])*fa[t]*(1-mu_s)
    n_VVAn[t+1] <- (n_VVJS[t] + n_VVJH[t])*((1-u[t])*fn[t] + u[t])*(1-mu_s)
    n_IIAa[t+1] <- (n_IIJS[t] + n_IIJH[t])*s*(1-mu_i)
    n_IIAn[t+1] <- (n_IIJS[t] + n_IIJH[t])*(1-s)*(1-mu_i)
    n_IVAa[t+1] <- n_IVJS[t]*s*(1-mu_i) + (1-u[t])*n_IVJH[t]*fa[t]*(1-mu_s)
    n_IVAn[t+1] <- n_IVJS[t]*(1-s)*(1-mu_i) + n_IVJH[t]*((1-u[t])*fn[t] + u[t])*(1-mu_s)
    
    
    n_Aa[t+1] <- n_VVAa[t+1] + n_IIAa[t+1] + n_IVAa[t+1]
    n_An[t+1] <- n_VVAn[t+1] + n_IIAn[t+1] + n_IVAn[t+1]
    N[t+1] <- n_VVAa[t+1] + n_VVAn[t+1] + n_IIAa[t+1] + n_IIAn[t+1] + n_IVAa[t+1] + n_IVAn[t+1]+ n_VVJH[t+1] + n_VVJS[t+1] + n_IIJH[t+1] + n_IIJS[t+1] + n_IVJH[t+1] + n_IVJS[t+1]
    u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing
    fa[t+1]<- (n_Aa[t+1]*ba)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
    fn[t+1]<- (n_An[t+1]*bn)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
}

# compute proportions of each strategy for summary plot
p_VV <- ( n_VVJH + n_VVJS + n_VVAa + n_VVAn ) / N
p_II <- ( n_IIJH + n_IIJS + n_IIAa + n_IIAn ) / N
p_IV <- (n_IVJH + n_IVJS + n_IVAa + n_IVAn) / N
q_VV <-  n_VVAa / ( n_VVAa + n_VVAn )
q_II <-  n_IIAa / ( n_IIAa + n_IIAn )
q_IV <- n_IVAa / (n_IVAa + n_IVAn)


plot( p_VV , ylim=c(0,1) , type="l" )
lines( 1:length(p_II) , p_II , col="red" )
lines( 1:length(q_VV) , q_VV , lty=2 )
lines( 1:length(q_II) , q_II , lty=2 , col="red" )

# age structure
p_JVV <- ( n_VVJS + n_VVJH ) / ( n_VVJH + n_VVJS + n_VVAa + n_VVAn )
p_AVV <- ( n_VVAa + n_VVAn ) / ( n_VVJH + n_VVJS + n_VVAa + n_VVAn )
plot( p_JVV , ylim=c(0,1) , type="l" )

p_JIV <- (n_IVJS + n_IVJH) / (n_IVJH + n_IVJS + n_IVAa + n_IVAn)
p_AIV <- (n_IVAa + n_IVAn) / (n_IVJH + n_IVJS + n_IVAa + n_IVAn)
plot( p_JIV , ylim=c(0,1) , type="l" )

p_JII <- ( n_IIJS + n_IIJH ) / ( n_IIJH + n_IIJS + n_IIAa + n_IIAn )
p_AII <- ( n_IIAa + n_IIAn ) / ( n_IIJH + n_IIJS + n_IIAa + n_IIAn )
plot( p_JII , ylim=c(0,1) , type="l" )
