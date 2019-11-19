#B = rnorm(60,mean=0.01,sd=0.01)
#Now Y follows a gamma distribution, because its parameter follows a gamma distribution
#So in essence, Y follows a gamma distribution of the Beta but scaled with the minutes
#
#P(B|D) = P(D|B)P(B)
#P(D|B) this is the likelihood of the data given the parameter
prob <- function(x,shape,scale){(1/(gamma(shape)*scale^shape))*x^(shape-1)*exp(-x/scale)}
theta = seq(0.01,3,length=100)
m = 30/60
v = 60/60
scale = v/m
shape = m/scale
beta = rgamma(1000,shape = shape,scale = scale)
minutes = rpois(1000,lambda = 30)
Y = minutes * beta
scv = vector()
shv = vector()
nomv = vector()
min = 5/60
max = 60/60
theta = seq(min,max,length=1000)
for(i in 1:1000)
{
  nomv[i] = nom = sum(dnorm(Y,mean = minutes*theta[i],sd=1,log=T))+log(1/(max-min))
}
theta[which(nomv == max(nomv))]

#OK, let us write the MH solution
#Simple MCMC
#data = rnorm(1,mean=100,sd=2)
#Min/max Boiler
min = 5
max = 90
mean = min*1.25
#Start from a prior with variance equal to max - mean to guarantee that we cover the maximum
scale = sqrt(max-mean)
shape = mean/scale
beta = c(rgamma(1000,shape = shape,scale = scale),rgamma(10,shape = shape*100,scale = scale*1.5))
plot(density(beta))
prior_params = c(scale,shape)
#Step number 1 is setting up the prior distribution, done with the parameters
#Step 2 is setting up the function that calculates the posterior probability, given the parameter, data and the prior probability
samplePrior <-function()
{
  rgamma(1,prior_params[1],prior_params[2])
}

logprior <-function(param)
{
  #print(param)
  dgamma(param,prior_params[1],prior_params[2],log = T)
}

loglikelihood <- function(data,param)
{
  (sum(dnorm(data,mean=param,sd=1,log = T)))
}

posterior <- function(data,param)
{
  #1 data point
  return(loglikelihood(data,param)+logprior(param))
}

#MCMC, function to move from the current param one step
MCMC <-function(prior)
{
  for(i in 1:length(prior)) prior[i] = prior[i] + rnorm(1,0,.5)
  prior
}

propsition <- function(data,current,new){
  post = posterior(data,current)
  new_post = posterior(data,new)
  ratio = min(1,exp(new_post-post))
  flag = TRUE
  if(ratio < 1) flag = ratio>runif(1,0,1)
  #if(flag)print(paste("Current ",current,"New",new,"Posterior",posterior,"New Posterior",new_posterior,"Ratio is ",ratio))
  flag
}

burnin <- function(data,N=1e04)
{
  param = samplePrior()
  #prior_prop = exp(logprior(param))
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
param = burnin(beta[beta<max])
posterior_param = MH(beta[beta<max],param,1e04)
pdf(file="aaa.pdf")
plot(density(posterior_param))
dev.off()
