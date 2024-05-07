library(reshape2)
require(scales)


rm(list=ls())

limit_zero<-function(x){
  
  if(x<0){
    return(0)
  }else{
    return(x)
  }
}

H=8
#maximum change in price that can be seen from 1 iteration to the next
price_multi_limit=2
#exponential smoothing parameter
price_anchoring=0.3

T_a=0
T_b=0


S_a=10^7
S_b=10^7

price_A=0.1
price_B=0.1

price_bottom=0.05

iterations=12*5
controller_supply=1.01
controller_price=2
controller_rewards=1.5
second_price_measure=3

minimum_supply=10^6

multiplier_rewards_increase=0.1
multiplier_rewards_decrease=0.05
multiplier_demand_increase=0.2
multiplier_demand_decrease=0.1
demand_burn=0.1

demand_satisfaction_mean=0.1
demand_satisfaction_std=0.1

D_a=S_a
D_b=S_b

R_a=S_a*abs(rnorm(1,0.1))
R_b=S_b*abs(rnorm(1,0.1))

prices_A=numeric(iterations)
prices_B=numeric(iterations)

rewards_A=numeric(iterations)
rewards_B=numeric(iterations)

demands_A=numeric(iterations)
demands_B=numeric(iterations)

for (i in seq(iterations)){
  print(i)
  
  T_a=(price_A*abs(rnorm(1,1,0.05)))*D_a
  T_b=(price_B*abs(rnorm(1,1,0.05)))*D_b
  

  
  if (i>1){

    price_A=(1-price_anchoring)*H*T_a/S_a+price_anchoring*prices_A[i-1]
    price_B=(1-price_anchoring)*H*T_b/S_b+price_anchoring*prices_B[i-1]
    
    if (price_A>prices_A[i-1]*price_multi_limit){
      price_A=prices_A[i-1]*price_multi_limit
    }
    if (price_B>prices_B[i-1]*price_multi_limit){
      price_B=prices_B[i-1]*price_multi_limit
    }
    
  }
  
  if (price_A<price_bottom){
    price_A=price_bottom
  }
  
  if(price_B<price_bottom){
    price_B=price_bottom
  }
  
  S_a=S_a+R_a-D_a*demand_burn
  S_b=S_b+R_b-D_b*demand_burn
  
  if (price_A>price_B*controller_price | S_a<S_b*controller_supply){
    print('A')
    R_a=S_a*multiplier_rewards_increase
    R_b=S_b*multiplier_rewards_decrease
    
    D_a=S_a*multiplier_demand_decrease*(1-abs(rnorm(1,mean=demand_satisfaction_mean,demand_satisfaction_std)))
    D_b=S_b*multiplier_demand_increase*(1-abs(rnorm(1,mean=demand_satisfaction_mean,demand_satisfaction_std)))
    
  }else if(price_B>price_A*controller_price | S_b<S_a*controller_supply){
    R_a=S_a*multiplier_rewards_decrease
    R_b=S_b*multiplier_rewards_increase
    
    D_a=S_a*multiplier_demand_increase*(1-(abs(rnorm(1,mean=demand_satisfaction_mean,demand_satisfaction_std))))
    D_b=S_b*multiplier_demand_decrease*(1-(abs(rnorm(1,mean=demand_satisfaction_mean,demand_satisfaction_std))))
  }
  
  if(R_a>R_b*controller_rewards){
    R_a=R_b*controller_rewards
  }else if(R_b>R_a*controller_rewards){
    R_b=R_a*controller_rewards
  }
  

  
  if(price_A>price_B*second_price_measure){
    D_b=D_b+(D_b+D_a)/4
    D_a=D_a-(D_b+D_a)/4
  }else if(price_B>price_A*second_price_measure){
    D_a=D_a+(D_b+D_a)/4
    D_b=D_b-(D_b+D_a)/4
  }
  
  D_a=D_a*(1+1/i)
  D_b=D_b*(1+1/i)
  
  D_a=limit_zero(D_a)
  D_b=limit_zero(D_b)
  R_a=limit_zero(R_a)
  R_b=limit_zero(R_b)
  S_a=limit_zero(S_a)
  S_b=limit_zero(S_b)
  
  
  
  prices_A[i]=price_A
  prices_B[i]=price_B
  
  rewards_A[i]=R_a
  rewards_B[i]=R_b
  
  demands_A[i]=D_a
  demands_B[i]=D_b
  
}

library(ggplot2)

df=data.frame(prices_A=prices_A,prices_B=prices_B,iteration=seq(length(prices_A)))
df2 <- melt(df, id="iteration")  # convert to long format


ggplot(data=df2,
       aes(x=iteration, y=value, colour=variable)) +
  geom_line()+scale_y_continuous(labels=comma)+ylab('price $')


rewards_df=data.frame(rewards_A=rewards_A,rewards_B=rewards_B,iteration=seq(length(prices_A)))
rewards_melt=melt(rewards_df, id="iteration")

ggplot(data=rewards_melt,aes(x=iteration, y=value, colour=variable)) +geom_line()+scale_y_continuous(labels=comma)+ylab('rewards')


demands_df=data.frame(demands_A=demands_A,demands_B=demands_B,iteration=seq(length(prices_A)))
demands_melt=melt(demands_df, id="iteration")

ggplot(data=demands_melt,aes(x=iteration, y=value, colour=variable)) +geom_line()+scale_y_continuous(labels=comma)+ylab('demands')



total_df=data.frame(prices_A=prices_A,prices_B=prices_B,rewards_A=rewards_A,rewards_B=rewards_B,demands_A=demands_A,demands_B=demands_B,iteration=seq(length(prices_A)))
