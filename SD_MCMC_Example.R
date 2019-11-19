#Simple MCMC
#data = rnorm(1,mean=100,sd=2)
data = c(142)
prior_params = c(list(c(114,0.01)),list(0.01))
#Step number 1 is setting up the prior distribution, done with the parameters
#Step 2 is setting up the function that calculates the posterior probability, given the parameter, data and the prior probability
samplePrior <-function()
{
  c(rnorm(1,prior_params[[1]][1],prior_params[[1]][2]),rexp(1,prior_params[[2]][1]))
}

prior <-function(param)
{
  #print(param)
  dnorm(param[1],prior_params[[1]][1],prior_params[[1]][2])*dexp(param[2],prior_params[[2]][1])
}

likelihood <- function(data,param)
{
  if(param[2]>0)
    #I know the cdf of one number. I will test the likelihood as the probability of sampling from the 
    return(prod(dnorm(data,param[1],param[2])))
    #return (dnorm(pnorm(data,param[1],param[2]),0.83,sd=.001))
  else
    return(0)
}

posterior <- function(data,param)
{
  #1 data point
  return(likelihood(data,param)*prior(param))
}


#MCMC, function to move from the current param one step

MCMC <-function(prior)
{
  for(i in 1:length(prior)) prior[i] = prior[i] + rnorm(1,0,1)
  prior
}

propsition <- function(data,current,new){
  post = posterior(data,current)
  new_post = posterior(data,new)
  ratio = min(1,new_post/post)
  flag = TRUE
  if(ratio < 1) flag = ratio>runif(1,0,1)
  #if(flag)print(paste("Current ",current,"New",new,"Posterior",posterior,"New Posterior",new_posterior,"Ratio is ",ratio))
  flag
}

burnin <- function(data,N=5e04)
{
  param = samplePrior()
  prior_prop = prior(param)
  first_posterior = posterior(data,param)
  for(i in 1:N)
  {
    nparam = samplePrior()
    nposterior = posterior(data,nparam)
    if(nposterior>first_posterior) {
      first_posterior = nposterior
      param=nparam
    }    
  }
  param
}

MH <-function(data,current,N=1e05)
{
  posterior_param = matrix(ncol=length(current),nrow=N)
  posterior_param[1,] = current
  for(i in 2:N)
  {
    new = MCMC(posterior_param[i-1,])
    flag = propsition(data,posterior_param[i-1,],new)
    if(flag){
      posterior_param[i,] = new
    }else{
      posterior_param[i,] = posterior_param[i-1,]
    }
  }
  posterior_param
}

#Step 3 function to sample from the prior distribution and get its probability
#at the start we need to make bigger jumps, until we see that we have a good starting point
param = burnin(data)
posterior_param = MH(data,param,1e05)
par(mfrow=c(1,2))
plot(density(posterior_param[,1]))
plot(density(posterior_param[,2]))
