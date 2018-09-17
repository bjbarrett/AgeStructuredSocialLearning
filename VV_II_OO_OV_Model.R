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
mu_n <- 1 # excess mortality factor of non-adapted individuals (to be added at some point with overlapping generations)

#create bins to store recursion values for each type

n_VVJH <- rep(0,timesteps + 1)
n_VVJS <- rep(0,timesteps + 1)
n_IIJH <- rep(0,timesteps + 1)
n_IIJS <- rep(0,timesteps + 1)
n_OOJH <- rep(0,timesteps + 1)
n_OOJS <- rep(0,timesteps + 1)
n_OVJH <- rep(0,timesteps + 1)
n_OVJS <- rep(0,timesteps + 1)


n_VVAa <- rep(0,timesteps + 1)
n_VVAn <- rep(0,timesteps + 1)
n_IIAa <- rep(0,timesteps + 1)
n_IIAn <- rep(0,timesteps + 1)
n_OOAa <- rep(0,timesteps + 1)
n_OOAn <- rep(0,timesteps + 1)
n_OVAa <- rep(0,timesteps + 1)
n_OVAn <- rep(0,timesteps + 1)

n_Aa <- rep(0,timesteps + 1)
n_An <- rep(0,timesteps + 1)
N <- rep(0,timesteps + 1)
u <- rep(0,timesteps + 1)
fa <- rep(0,timesteps + 1)
fn <- rep(0,timesteps + 1)

N0 <- 240
n_VVAa[1] <- 20
n_VVAn[1] <- 20
n_IIAa[1] <- 20
n_IIAn[1] <- 20
n_OOAa[1] <- 20
n_OOAn[1] <- 20
n_OVAa[1] <- 20
n_OVAn[1] <- 20
n_An[1] <- n_VVAn[1] + n_IIAn[1] + n_OOAn[1] + n_OVAn[1]
n_Aa[1] <- n_VVAa[1] + n_IIAa[1] + n_OOAa[1] + n_OVAa[1]

n_VVJH[1] <- 10
n_VVJS[1] <- 10
n_IIJH[1] <- 10
n_IIJS[1] <- 10
n_OOJH[1] <- 10
n_OOJS[1] <- 10
n_OVJH[1] <- 10
n_OVJH[1] <- 10

u[1] <- 0
N[1] <- n_VVAa[1] + n_VVAn[1] + n_IIAa[1] + n_IIAn[1] + n_OOAa[1] + n_OOAn[1] + n_OVAa[1] + n_OVAn[1] + n_VVJH[1] + n_VVJS[1] + n_IIJH[1] + n_IIJS[1] + n_OOJH[1] + n_OOJS[1] + n_OVJH[1] + n_OVJS[1]
fa[1]<- (n_Aa[1]*ba)/(n_Aa[1]*ba + n_An[1]*bn)
fn[1]<- (n_An[1]*bn)/(n_Aa[1]*ba + n_An[1]*bn)

#recursions
for (t in 1:timesteps)
{
    # juvenile recruitment
    n_VVJH[t+1] <- (n_VVAa[t])*ba*(1 - p_as) + (n_VVAn[t])*bn*(1 - p_ns)
    n_VVJS[t+1] <- (n_VVAa[t])*ba*p_as + (n_VVAn[t])*bn*p_ns
    n_IIJH[t+1] <- (n_IIAa[t])*ba*(1 - p_as) + (n_IIAn[t])*bn*(1 - p_ns)
    n_IIJS[t+1] <- (n_IIAa[t])*ba*p_as + (n_IIAn[t])*bn*p_ns
    n_OOJH[t+1] <- (n_OOAa[t])*ba*(1 - p_as) + (n_OOAn[t])*bn*(1 - p_ns)
    n_OOJS[t+1] <- (n_OOAa[t])*ba*p_as + (n_OOAn[t])*bn*p_ns
    n_OVJH[t+1] <- n_OVAa[t]*ba*(1 - p_as) + n_OVAn[t]*bn*(1 - p_ns)
    n_OVJS[t+1] <- n_OVAa[t]*ba*p_as + n_OVAn[t]*bn*p_ns

    #adult recruitment
    n_VVAa[t+1] <- (n_VVJS[t] + n_VVJH[t])*(1-u[t])*fa[t]*(1-mu_s)
    n_VVAn[t+1] <- (n_VVJS[t] + n_VVJH[t])*((1-u[t])*fn[t] + u[t])*(1-mu_s)
    n_IIAa[t+1] <- (n_IIJS[t] + n_IIJH[t])*s*(1-mu_i)
    n_IIAn[t+1] <- (n_IIJS[t] + n_IIJH[t])*(1-s)*(1-mu_i)
    n_OOAa[t+1] <- (n_OOJS[t] + n_OOJH[t])*(n_Aa[t]/N[t])*(1-u[t])*(1-mu_s)
    n_OOAn[t+1] <- (n_OOJS[t] + n_OOJH[t])*((1-u[t])*(n_An[t]/N[t]) + u[t])*(1-mu_s)
    n_OVAa[t+1] <- (n_OVJS[t]*(n_Aa[t]/(n_Aa[t] + n_An[t])) + n_OVJH[t]*fa[t])*(1-u[t])*(1-mu_s)
    n_OVAn[t+1] <- (n_OVJS[t]*((1-u[t])*(n_An[t]/(n_Aa[t] + n_An[t])) + u[t]) + n_OVJH[t]*((1-u[t])*fn[t]+ u[t]))*(1-mu_s)


    n_Aa[t+1] <- n_VVAa[t+1] + n_IIAa[t+1] + n_OOAa[t+1] + n_OVAa[t+1]
    n_An[t+1] <- n_VVAn[t+1] + n_IIAn[t+1] + n_OOAn[t+1] + n_OVAn[t+1]
    N[t+1] <- n_VVAa[t+1] + n_VVAn[t+1] + n_IIAa[t+1] + n_IIAn[t+1] + n_OOAa[t+1] + n_OOAn[t+1] + n_OVAa[t+1] + n_OVAn[t+1] + n_VVJH[t+1] + n_VVJS[t+1] + n_IIJH[t+1] + n_IIJS[t+1] + n_OOJH[t+1] + n_OOJS[t+1] + n_OVJH[t+1] + n_OVJS[t+1]
    u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing
    fa[t+1]<- (n_Aa[t+1]*ba)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
    fn[t+1]<- (n_An[t+1]*bn)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
}

# compute proportions of each strategy for summary plot
p_VV <- ( n_VVJH + n_VVJS + n_VVAa + n_VVAn ) / N
p_II <- ( n_IIJH + n_IIJS + n_IIAa + n_IIAn ) / N
p_OO <- ( n_OOJH + n_OOJS + n_OOAa + n_OOAn ) / N
p_OV <- ( n_OVJH + n_OVJS + n_OVAa + n_OVAn ) / N

q_VV <-  n_VVAa / ( n_VVAa + n_VVAn )
q_II <-  n_IIAa / ( n_IIAa + n_IIAn )
q_OO <-  n_OOAa / ( n_OOAa + n_OOAn )
q_OV <-  n_OVAa / ( n_OVAa + n_OVAn )



plot( p_VV , ylim=c(0,1) , type="l" , col="violet")
lines( 1:length(p_II) , p_II , col="red" )
lines( 1:length(p_OO) , p_OO , col="black")
lines( 1:length(p_OV) , p_OV , col="green")

lines( 1:length(q_VV) , q_VV , lty=2 , col="violet")
lines( 1:length(q_II) , q_II , lty=2 , col="red" )
lines( 1:length(q_OO) , q_OO , lty=2 , col="black")
lines( 1:length(q_OV) , q_OV , lty=2 , col="green")


legend("topleft" , c("VV", "II", "OO" , "OV" ) , lty=1 , lwd=c(1,1,1,1,1) , col=c("violet", "red", "black" , "green" ) , pch=c(15,15,15,15,15) ,bty='n')


# age structure
p_JVV <- ( n_VVJS + n_VVJH ) / ( n_VVJH + n_VVJS + n_VVAa + n_VVAn )
p_AVV <- ( n_VVAa + n_VVAn ) / ( n_VVJH + n_VVJS + n_VVAa + n_VVAn )
plot( p_JVV , ylim=c(0,1) , type="l" )

p_JII <- ( n_IIJS + n_IIJH ) / ( n_IIJH + n_IIJS + n_IIAa + n_IIAn )
p_AII <- ( n_IIAa + n_IIAn ) / ( n_IIJH + n_IIJS + n_IIAa + n_IIAn )
plot( p_JII , ylim=c(0,1) , type="l" )

p_JOO <- ( n_OOJS + n_OOJH ) / ( n_OOJH + n_OOJS + n_OOAa + n_OOAn )
p_AOO <- ( n_OOAa + n_OOAn ) / ( n_OOJH + n_OOJS + n_OOAa + n_OOAn )
plot( p_JOO , ylim=c(0,1) , type="l" )

p_JOV <- ( n_OVJS + n_OVJH ) / ( n_OVJH + n_OVJS + n_OVAa + n_OVAn )
p_AOV <- ( n_OVAa + n_OVAn ) / ( n_OVJH + n_OVJS + n_OVAa + n_OVAn )
plot( p_JOV , ylim=c(0,1) , type="l" )
