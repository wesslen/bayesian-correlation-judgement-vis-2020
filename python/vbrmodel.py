import numpy as np
import pymc3 as pm
import theano.tensor as T
from scipy.stats import beta
from scipy.stats import pearsonr
from numpy.linalg import inv, det

#
# Simulated data
#
def generate_dataset(n=10, mn=np.array([0, 0]), sigma=np.array([1, 1]), rho=0):
  """
  n: sample size
  sigma: vector of standard deviation for each variable
  rho: population correlation
  """

  ss_x   = float(sigma[0]**2)
  ss_y   = float(sigma[1]**2)
  cov_xy = float(rho * sigma[0] * sigma[1])

  xy = np.random.multivariate_normal(mn, [[ss_x, cov_xy], [cov_xy, ss_y]], n)

  # force centering
  xy = xy - np.mean(xy, axis=0)

  return xy


def dataset_sampler(n=10, mn=np.array([0, 0]), sigma=np.array([1, 1]), rho=0):
    """
    Sample datasets until the sample correlation is close enough to desired value.
    """
    max_err = .001
    max_iter = 1000
    for i in range(max_iter):
        dat = generate_dataset(n=n, mn=mn, sigma=sigma, rho=rho)
        corr = pearsonr(dat[:,0], dat[:,1])[0]
        if np.abs(corr - rho) < max_err:
            return dat
    return None


#
# Bayesian model with Normal prior over correlation
#
def precision(sigma, rho):
    C = T.alloc(rho, 2, 2)
    C = T.fill_diagonal(C, 1.)
    S = T.diag(sigma)
    return T.nlinalg.matrix_inverse(S.dot(C).dot(S))


def create_model(data, normal_prior=np.array([0, .1])):

    with pm.Model() as model:

        mu = np.array([0., 0.]) # assume centered data
        sig = np.array([1., 1.]) # assume z-scored data

        # prior as a truncated normal
        BoundedNormal = pm.Bound(pm.Normal, lower=-1, upper=1)
        rho = BoundedNormal('r', mu=normal_prior[0], sd=normal_prior[1])

        prec = pm.Deterministic('prec', precision(sig, rho))

        # multivariate normal
        mult_n = pm.MvNormal('mult_n', mu=mu, tau=prec, observed=data)

    return model


def estimate(data, normal_prior=np.array([0, .1])):
  model = create_model(data, normal_prior=normal_prior)
  with model:
    trace = pm.sample(10000, tune=1000, step=pm.Metropolis())
  return trace


#
# Baseline Model (uniform prior over correlation)
#
def create_model_uniform(data):

    with pm.Model() as model:

        mu = np.array([0., 0.]) # assume centered data
        sig = np.array([1., 1.]) # assume z-scored data

        # uniform prior over correlation
        rho = pm.Uniform('r', lower=-1, upper=1)

        prec = pm.Deterministic('prec', precision(sig, rho))

        # multivariate normal
        mult_n = pm.MvNormal('mult_n', mu=mu, tau=prec, observed=data)

    return model


def estimate_uniform(data):
  model = create_model_uniform(data)
  with model:
    trace = pm.sample(10000, tune=1000, step=pm.Metropolis())
  return trace


#
# Bayesian model with weighting
#

def create_model_weighted(data, normal_prior=np.array([0, .1])):

  with pm.Model() as model:

      mu = np.array([0., 0.]) # assume centered data
      sig = np.array([1., 1.]) # assume z-scored data

      w = pm.Uniform('w', lower=.01, upper=20, testval=1)

      # prior as a truncated normal
      BoundedNormal = pm.Bound(pm.Normal, lower=-1, upper=1)
      rho = BoundedNormal('r', mu=normal_prior[0], sd = w * normal_prior[1])

      prec = pm.Deterministic('precision', precision(sig, rho))

      # multivariate normal
      mult_n = pm.MvNormal('mult_n', mu=mu, tau=prec, observed=data)

  return model


def estimate_weighted(data, normal_prior=np.array([0, .1])):
    model = create_model_weighted(data, normal_prior=normal_prior)
    with model:
      trace = pm.sample(10000, tune=1000, step=pm.Metropolis())
    return trace
