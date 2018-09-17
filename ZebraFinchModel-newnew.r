###initial pop conditions
timesteps <- 500 #length of simulation

##parameters
bn <- 1    ##avg broodsize of adult with non-adaptive behavior
ba <- 2*bn ##avg broodsize of adult with adaptive behavior. ba > bn
p_as <- 0.1  #prob an adapted adult produces stressed offspring
p_ns <- 0.9  #prob an nonadapted adult produces stressed offspring
s <- 0.3	#prob individual learner succesfully acquires adaptive behavior
U <- 0.1  #prob environment changes states
mu_s <- 0.2	#mortality rate of social learners
mu_i <- 0.3 #mortaility rate of individual learners


#initial frequencies of adults
n_IVAa0 <- 10
n_IVAn0 <- 10
n_IOAa0 <- 10
n_IOAn0 <- 10
n_IIAa0 <- 10
n_IIAn0 <- 10
n_VVAa0 <- 10
n_VVAn0 <- 10
n_VOAa0 <- 10
n_VOAn0 <- 10
n_VIAa0 <- 10
n_VIAn0 <- 10
n_OVAa0 <- 10
n_OVAn0 <- 10
n_OOAa0 <- 10
n_OOAn0 <- 10
n_OIAa0 <- 10
n_OIAn0 <- 10

n_Aa0 <- n_IVAa0 + n_IOAa0 + n_IIAa0 + n_VVAa0 + n_VOAa0 + n_VIAa0 + n_OVAa0 + n_OOAa0 + n_OIAa0 #number of adults with adaptive behavior at t=0
n_An0 <- n_IVAn0 + n_IOAn0 + n_IIAn0 + n_VVAn0 + n_VOAn0 + n_VIAn0 + n_OVAn0 + n_OOAn0 + n_OIAn0 #number of adults with adaptive behavior at t=0
N0 <- n_Aa0 + n_An0 #population size at t=0

fa0<- (n_Aa0*ba)/(n_Aa0*ba + n_An0*bn)
fn0<- (n_An0*bn)/(n_Aa0*ba + n_An0*bn)

#create bins to store recursion values for each type
n_IVJH <- rep(0,timesteps + 1)
n_IVJS <- rep(0,timesteps + 1)
n_IOJH <- rep(0,timesteps + 1)
n_IOJS <- rep(0,timesteps + 1)
n_IIJH <- rep(0,timesteps + 1)
n_IIJS <- rep(0,timesteps + 1)
n_VVJH <- rep(0,timesteps + 1)
n_VVJS <- rep(0,timesteps + 1)
n_VOJH <- rep(0,timesteps + 1)
n_VOJS <- rep(0,timesteps + 1)
n_VIJH <- rep(0,timesteps + 1)
n_VIJS <- rep(0,timesteps + 1)
n_OVJH <- rep(0,timesteps + 1)
n_OVJS <- rep(0,timesteps + 1)
n_OOJH <- rep(0,timesteps + 1)
n_OOJS <- rep(0,timesteps + 1)
n_OIJH <- rep(0,timesteps + 1)
n_OIJS <- rep(0,timesteps + 1)
n_IVAa <- rep(0,timesteps + 1)
n_IVAn <- rep(0,timesteps + 1)
n_IOAa <- rep(0,timesteps + 1)
n_IOAn <- rep(0,timesteps + 1)
n_IIAa <- rep(0,timesteps + 1)
n_IIAn <- rep(0,timesteps + 1)
n_VVAa <- rep(0,timesteps + 1)
n_VVAn <- rep(0,timesteps + 1)
n_VOAa <- rep(0,timesteps + 1)
n_VOAn <- rep(0,timesteps + 1)
n_VIAa <- rep(0,timesteps + 1)
n_VIAn <- rep(0,timesteps + 1)
n_OVAa <- rep(0,timesteps + 1)
n_OVAn <- rep(0,timesteps + 1)
n_OOAa <- rep(0,timesteps + 1)
n_OOAn <- rep(0,timesteps + 1)
n_OIAa <- rep(0,timesteps + 1)
n_OIAn <- rep(0,timesteps + 1)
n_Aa <- rep(0,timesteps + 1)
n_An <- rep(0,timesteps + 1)
N <- rep(0,timesteps + 1)
u <- rep(0,timesteps + 1)
fa <- rep(0,timesteps + 1)
fn <- rep(0,timesteps + 1)

n_IVAa[1] <- N0/18
n_IVAn[1] <- N0/18
n_IOAa[1] <- N0/18
n_IOAn[1] <- N0/18
n_IIAa[1] <- N0/18
n_IIAn[1] <- N0/18
n_VVAa[1] <- N0/18
n_VVAn[1] <- N0/18
n_VOAa[1] <- N0/18
n_VOAn[1] <- N0/18
n_VIAa[1] <- N0/18
n_VIAn[1] <- N0/18
n_OVAa[1] <- N0/18
n_OVAn[1] <- N0/18
n_OOAa[1] <- N0/18
n_OOAn[1] <- N0/18
n_OIAa[1] <- N0/18
n_OIAn[1] <- N0/18
n_An[1] <- N0/2
n_Aa[1] <- N0/2
n_IVJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_IVJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_IOJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_IOJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_IIJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_IIJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_VVJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_VVJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_VOJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_VOJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_VIJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_VIJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_OVJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_OVJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_OOJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_OOJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
n_OIJH[1] <- (N0/18)*ba*(1-p_as) + (N0/18)*bn*(1-p_ns)
n_OIJS[1] <- (N0/18)*ba*p_as + (N0/18)*bn*p_ns
N[1] <- n_IVAa[1] + n_IVAn[1] + n_IOAa[1] + n_IOAn[1] + n_IIAa[1] + n_IIAn[1] + n_VVAa[1] + n_VVAn[1] + n_VOAa[1] + n_VOAn[1] + n_VIAa[1] + n_VIAn[1] + n_OVAa[1] + n_OVAn[1] + n_OOAa[1] + n_OOAn[1] + n_OIAa[1] + n_OIAn[1] + n_IVJH[1] + n_IVJS[1] + n_IOJH[1] + n_IOJS[1] + n_IIJH[1] + n_IIJS[1] + n_VVJH[1] + n_VVJS[1] + n_VOJH[1] + n_VOJS[1] + n_VIJH[1] + n_VIJS[1] + n_OVJH[1] + n_OVJS[1] + n_OOJH[1] + n_OOJS[1] + n_OIJH[1] + n_OIJS[1]
u[1] <- 0
fa[1]<- (n_Aa[1]*ba)/(n_Aa[1]*ba + n_An[1]*bn)
fn[1]<- (n_An[1]*bn)/(n_Aa[1]*ba + n_An[1]*bn)

#recursions
for (t in 1:timesteps)
{
    
    # juvenile recruitment
    n_IVJH[t+1] <- (n_IVAa[t])*ba*(1-p_as) + (n_IVAn[t])*bn*(1-p_ns)
    n_IVJS[t+1] <- (n_IVAa[t])*ba*p_as + (n_IVAn[t])*bn*p_ns
    n_IOJH[t+1] <- (n_IOAa[t])*ba*(1-p_as) + (n_IOAn[t])*bn*(1-p_ns)
    n_IOJS[t+1] <- (n_IOAa[t])*ba*p_as + (n_IOAn[t])*bn*p_ns
    n_IIJH[t+1] <- (n_IIAa[t])*ba*(1-p_as) + (n_IIAn[t])*bn*(1-p_ns)
    n_IIJS[t+1] <- (n_IIAa[t])*ba*p_as + (n_IIAn[t])*bn*p_ns
    n_VVJH[t+1] <- (n_VVAa[t])*ba*(1-p_as) + (n_VVAn[t])*bn*(1-p_ns)
    n_VVJS[t+1] <- (n_VVAa[t])*ba*p_as + (n_VVAn[t])*bn*p_ns
    n_VOJH[t+1] <- (n_VOAa[t])*ba*(1-p_as) + (n_VOAn[t])*bn*(1-p_ns)
    n_VOJS[t+1] <- (n_VOAa[t])*ba*p_as + (n_VOAn[t])*bn*p_ns
    n_VIJH[t+1] <- (n_VIAa[t])*ba*(1-p_as) + (n_VIAn[t])*bn*(1-p_ns)
    n_VIJS[t+1] <- (n_VIAa[t])*ba*p_as + (n_VIAn[t])*bn*p_ns
    n_OVJH[t+1] <- (n_OVAa[t])*ba*(1-p_as) + (n_OVAn[t])*bn*(1-p_ns)
    n_OVJS[t+1] <- (n_OVAa[t])*ba*p_as + (n_OVAn[t])*bn*p_ns
    n_OOJH[t+1] <- (n_OOAa[t])*ba*(1-p_as) + (n_OOAn[t])*bn*(1-p_ns)
    n_OOJS[t+1] <- (n_OOAa[t])*ba*p_as + (n_OOAn[t])*bn*p_ns
    n_OIJH[t+1] <- (n_OIAa[t])*ba*(1-p_as) + (n_OIAn[t])*bn*(1-p_ns)
    n_OIJS[t+1] <- (n_OIAa[t])*ba*p_as + (n_OIAn[t])*bn*p_ns
    
    #adult recruitment
    n_IVAa[t+1] <- n_IVJS[t]*s*(1-mu_i) + (1-u[t])*n_IVJH[t]*fa[t]*(1-mu_s)
    n_IVAn[t+1] <- n_IVJS[t]*(1-s)*(1-mu_i) + n_IVJH[t]*((1-u[t])*fn[t]+u[t])*(1-mu_s)
    n_IOAa[t+1] <- n_IOJS[t]*s*(1-mu_i) + ((1-u[t])*n_IOJH[t]*(n_Aa[t]/N[t]))*(1-mu_s)
    n_IOAn[t+1] <- n_IOJS[t]*(1-s)*(1-mu_i) + n_IOJH[t]*((1-u[t])*(n_An[t]/N[t])+u[t])*(1-mu_s)
    n_IIAa[t+1] <- (n_IIJS[t] + n_IIJH[t])*s*(1-mu_i)
    n_IIAn[t+1] <- (n_IIJS[t] + n_IIJH[t])*(1-s)*(1-mu_i)
    n_VVAa[t+1] <- (n_VVJS[t] + n_VVJH[t])*(1-u[t])*fa[t]*(1-mu_s)
    n_VVAn[t+1] <- (n_VVJS[t] + n_VVJH[t])*((1-u[t])*fn[t] + u[t])*(1-mu_s)
    n_VOAa[t+1] <- (n_VOJS[t]*fa[t] + n_VOJH[t]*(n_An[t]/N[t]))*(1-u[t])*(1-mu_s)
    n_VOAn[t+1] <- (n_VOJS[t]*((1-u[t])*fn[t] + u[t]) + n_VOJH[t]*((1-u[t])*(n_An[t]/N[t]) + u[t]))*(1-mu_s)
    n_VIAa[t+1] <- n_VIJS[t]*fa[t]*(1-u[t])*(1-mu_s) + n_VIJH[t]*s*(1-mu_i)
    n_VIAn[t+1] <- n_VIJS[t]*((1-u[t])*fn[t] + u[t])*(1-mu_s) + n_VIJH[t]*(1-s)*(1-mu_i)
    n_OVAa[t+1] <- (n_OVJS[t]*(n_Aa[t]/N[t]) + n_OVJH[t]*fa[t])*(1-u[t])*(1-mu_s)
    n_OVAn[t+1] <- (n_OVJH[t]*((1-u[t])*(n_An[t]/N[t]) + u[t]) + n_OVJH[t]*((1-u[t])*fn[t] + u[t]))*(1-mu_s)
    n_OOAa[t+1] <- (n_OOJS[t] + n_OOJH[t])*(n_An[t]/N[t])*(1-u[t])*(1-mu_s)
    n_OOAn[t+1] <- (n_OOJS[t] + n_OOJH[t])*((1-u[t])*(n_An[t]/N[t]) + u[t])*(1-mu_s)
    n_OIAa[t+1] <- (n_OIJS[t]*(1-u[t])*(n_Aa[t]/N[t]))*(1-mu_s) + n_OIJH[t]*s*(1-mu_i)
    n_OIAn[t+1] <- n_OIJS[t]*((1-u[t])*(n_An[t]/N[t]) + u[t])*(1-mu_s) + n_OIJH[t]*(1-s)*(1-mu_i)
    
    n_Aa[t+1] <- n_VVAa[t+1] + n_OVAa[t+1]
    n_An[t+1] <- n_VVAn[t+1] + n_OVAn[t+1]
    N[t+1] <- n_IVAa[t+1] + n_IVAn[t+1] + n_IOAa[t+1] + n_IOAn[t+1] + n_IIAa[t+1] + n_IIAn[t+1] + n_VVAa[t+1] + n_VVAn[t+1] + n_VOAa[t+1] + n_VOAn[t+1] + n_VIAa[t+1] + n_VIAn[t+1] + n_OVAa[t+1] + n_OVAn[t+1] + n_OOAa[t+1] + n_OOAn[t+1] + n_OIAa[t+1] + n_OIAn[t+1] + n_IVJH[t+1] + n_IVJS[t+1] + n_IOJH[t+1] + n_IOJS[t+1] + n_IIJH[t+1] + n_IIJS[t+1] + n_VVJH[t+1] + n_VVJS[t+1] + n_VOJH[t+1] + n_VOJS[t+1] + n_VIJH[t+1] + n_VIJS[t+1] + n_OVJH[t+1] + n_OVJS[t+1] + n_OOJH[t+1] + n_OOJS[t+1] + n_OIJH[t+1] + n_OIJS[t+1]
    u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing
    fa[t+1]<- (n_Aa[t+1]*ba)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
    fn[t+1]<- (n_An[t+1]*bn)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
}
