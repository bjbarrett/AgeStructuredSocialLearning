###initial pop conditions
timesteps <- 500 #length of simulation

##parameters
bn <- 1    ##avg broodsize of adult with non-adaptive behavior
ba <- 2*bn ##avg broodsize of adult with adaptive behavior. ba > bn
p_as <- 0.2  #prob an adapted adult produces stressed offspring
p_ns <- 0.8  #prob an nonadapted adult produces stressed offspring
s <- 0.5	#prob individual learner succesfully acquires adaptive behavior
U <- 0.11  #prob environment changes states
mu_s <- 0.1	#mortality rate of social learners
mu_i <- 0.11 #mortality rate of individual learners
mu_n <- 1 # excess mortality factor of non-adapted individuals (to be added at some point?)

#create bins to store recursion values for each type
n_IOJH <- rep(0,timesteps + 1)
n_IOJS <- rep(0,timesteps + 1)
n_OOJH <- rep(0,timesteps + 1)
n_OOJS <- rep(0,timesteps + 1)
n_IIJH <- rep(0,timesteps + 1)
n_IIJS <- rep(0,timesteps + 1)

n_IIAa <- rep(0,timesteps + 1)
n_IIAn <- rep(0,timesteps + 1)
n_OOAa <- rep(0,timesteps + 1)
n_OOAn <- rep(0,timesteps + 1)
n_IOAa <- rep(0,timesteps + 1)
n_IOAn <- rep(0,timesteps + 1)


n_Aa <- rep(0,timesteps + 1)
n_An <- rep(0,timesteps + 1)
N <- rep(0,timesteps + 1)
u <- rep(0,timesteps + 1)
fa <- rep(0,timesteps + 1)
fn <- rep(0,timesteps + 1)

N0 <- 180
n_OOAa[1] <- 20
n_OOAn[1] <- 20
n_IIAa[1] <- 20
n_IIAn[1] <- 20
n_IOAa[1] <- 20
n_IOAn[1] <- 20
n_An[1] <- n_OOAn[1] + n_IIAn[1] + n_IOAn[1]
n_Aa[1] <- n_OOAa[1] + n_IIAa[1] + n_IOAa[1]

n_OOJH[1] <- 10
n_OOJS[1] <- 10
n_IIJH[1] <- 10
n_IIJS[1] <- 10
n_IOJH[1] <- 10
n_IOJS[1] <- 10

u[1] <- 0
N[1] <- n_OOAa[1] + n_OOAn[1] + n_IIAa[1] + n_IIAn[1] + n_IOAa[1] + n_IOAn[1] + n_OOJH[1] + n_OOJS[1] + n_IIJH[1] + n_IIJS[1] + n_IOJH[1] + n_IOJS[1]
fa[1]<- (n_Aa[1]*ba)/(n_Aa[1]*ba + n_An[1]*bn)
fn[1]<- (n_An[1]*bn)/(n_Aa[1]*ba + n_An[1]*bn)

#recursions
for (t in 1:timesteps)
{
    # juvenile recruitment
    n_OOJH[t+1] <- (n_OOAa[t])*ba*(1-p_as) + (n_OOAn[t])*bn*(1-p_ns)
    n_OOJS[t+1] <- (n_OOAa[t])*ba*p_as + (n_OOAn[t])*bn*p_ns
    n_IIJH[t+1] <- (n_IIAa[t])*ba*(1-p_as) + (n_IIAn[t])*bn*(1-p_ns)
    n_IIJS[t+1] <- (n_IIAa[t])*ba*p_as + (n_IIAn[t])*bn*p_ns
    n_IOJH[t+1] <- (n_IOAa[t])*ba*(1-p_as) + (n_IOAn[t])*bn*(1-p_ns)
    n_IOJS[t+1] <- (n_IOAa[t])*ba*p_as + (n_IOAn[t])*bn*p_ns

    #adult recruitment
    n_OOAa[t+1] <- (n_OOJS[t] + n_OOJH[t])*(n_Aa[t]/(n_Aa[t] + n_An[t]))*(1-u[t])*(1-mu_s)
    n_OOAn[t+1] <- (n_OOJS[t] + n_OOJH[t])*((1-u[t])*(n_An[t]/(n_Aa[t] + n_An[t])) + u[t])*(1-mu_s)
    n_IIAa[t+1] <- (n_IIJS[t] + n_IIJH[t])*s*(1-mu_i)
    n_IIAn[t+1] <- (n_IIJS[t] + n_IIJH[t])*(1-s)*(1-mu_i)
    n_IOAa[t+1] <- n_IOJS[t]*s*(1-mu_i) + ((1-u[t])*n_IOJH[t]*(n_Aa[t]/N[t]))*(1-mu_s)
    n_IOAn[t+1] <- n_IOJS[t]*(1-s)*(1-mu_i) + n_IOJH[t]*((1-u[t])*(n_An[t]/N[t]) + u[t])*(1-mu_s)


    n_Aa[t+1] <- n_OOAa[t+1] + n_IIAa[t+1] + n_IOAa[t+1]
    n_An[t+1] <- n_OOAn[t+1] + n_IIAn[t+1] + n_IOAn[t+1]
    N[t+1] <- n_OOAa[t+1] + n_OOAn[t+1] + n_IIAa[t+1] + n_IIAn[t+1] + n_IOAa[t+1] + n_IOAn[t+1]+ n_OOJH[t+1] + n_OOJS[t+1] + n_IIJH[t+1] + n_IIJS[t+1] + n_IOJH[t+1] + n_IOJS[t+1]
    u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing
    fa[t+1]<- (n_Aa[t+1]*ba)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
    fn[t+1]<- (n_An[t+1]*bn)/(n_Aa[t+1]*ba + n_An[t+1]*bn)
}

# compute proportions of each strategy for summary plot
p_OO <- ( n_OOJH + n_OOJS + n_OOAa + n_OOAn ) / N
p_II <- ( n_IIJH + n_IIJS + n_IIAa + n_IIAn ) / N
p_IO <- (n_IOJH + n_IOJS + n_IOAa + n_IOAn) / N
q_OO <-  n_OOAa / ( n_OOAa + n_OOAn )
q_II <-  n_IIAa / ( n_IIAa + n_IIAn )
q_IO <- n_IOAa / (n_IOAa + n_IOAn)


plot( p_OO , ylim=c(0,1) , type="l" , xlab="time" , ylab="frequency in population")
lines( 1:length(p_II) , p_II , col="red" )
lines( 1:length(p_IO) , p_IO , col= "blue")
lines( 1:length(q_OO) , q_OO , lty=2 )
lines( 1:length(q_II) , q_II , lty=2 , col="red" )
lines( 1:length(q_IO) , q_IO , lty=2 , col="blue" )

legend("topleft" , c("OO", "II", "IO" , "population" , "adults") , lty=c(0,0,0,1,2) , lwd=c(0,0,0,1,1) , col=c("black", "red", "blue" , "black" , "black") , pch=c(15,15,15,NA,NA) ,bty='n')


# age structure
p_JOO <- ( n_OOJS + n_OOJH ) / ( n_OOJH + n_OOJS + n_OOAa + n_OOAn )
p_AOO <- ( n_OOAa + n_OOAn ) / ( n_OOJH + n_OOJS + n_OOAa + n_OOAn )
plot( p_JOO , ylim=c(0,1) , type="l" )

p_JIO <- (n_IOJS + n_IOJH) / (n_IOJH + n_IOJS + n_IOAa + n_IOAn)
p_AIO <- (n_IOAa + n_IOAn) / (n_IOJH + n_IOJS + n_IOAa + n_IOAn)
plot( p_JIO , ylim=c(0,1) , type="l" )

p_JII <- ( n_IIJS + n_IIJH ) / ( n_IIJH + n_IIJS + n_IIAa + n_IIAn )
p_AII <- ( n_IIAa + n_IIAn ) / ( n_IIJH + n_IIJS + n_IIAa + n_IIAn )
plot( p_JII , ylim=c(0,1) , type="l" )
