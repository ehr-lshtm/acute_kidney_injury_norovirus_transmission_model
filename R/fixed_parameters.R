# define parameters
# initial conditions
init <- c(
  3857263, 8103718, 42460865, 12374961,
  100, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0
)

init_matrix <- matrix(
  init,
  nrow = 4, ncol = 7
)

# fixed parameters

b <- (11.4 / 1000) / 365    # birth
# d <- (11.4 / 1000) / 365    # death
d <- c(0,0,0,0)
epsilon <- 1                # rate becoming infectious
psi <- 1 / 2                # rate symptomatic loss
gamma <- 1 / 10             # rate of recovery / 1/duration of asymptomatic infection
n_age_groups <- 4

# ageing
# prepare aging matrix
ages <- c(4, 14, 64, 80)
da <- diff(c(0, ages))
length(ages)
aging <- diag(-1 / da)
aging[row(aging) - col(aging) == 1] <- 1 / utils::head(da, -1)
aging <- aging /365

# contact matrix

age_groups <- c(0, 5, 15, 65)
polymod <- socialmixr::polymod
UK_structure <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  age.limits = c(age_groups),
  symmetric = TRUE
)

# Symmetrical contact matrix
uk_contact_rate_matrix <- as.matrix(UK_structure$matrix)
demography <- UK_structure$demography$population

uk_contact_rate_matrix <- t(t(uk_contact_rate_matrix) / demography)

# fitting parameters

# delta <- 1 / (2.0 * 365)    # waning immunity based on 5.1 years of immunity
# sigma <- 0.75               # proportion symptomatic
# w1 <- 9.42E-02
# w2 <- 1
# rho <- 0.05                 # relative infectiousness of asymptomatic vs symptomatic
# q1 <- 0.18
# q2 <- 0.036
# q <- c(q1, q2, q2, q2)      # probability of transmission with seasonal forcing on contacts for four pop

# surveillance parameters

# surveillance_report <- 1/287.6   ## community to surveillance ratio IID 2
surveillance_report <- 0.05   ## over 65 guess

hosp_rate <- 109000/19000000              ## CDC hosp rate
hosp_rate_primary <- 109000/19000000              ## CDC hosp rate
hosp_rate_secondary <- (109000*1.58)/19000000     ## CDC hosp rate x more common secondary diagnosis
hosp_rate_primary_0_4 <- 1/160             ## CDC hosp rate for under 5 years of age
hosp_rate_secondary_0_4 <- 1.58/160
hosp_rate_primary_65_plus <- 48.7/10000             ## 10.1177/20499361221136760
hosp_rate_secondary_65_plus <- (48.7*1.58)/10000
gp_rate <- 12.7/287.6     ## GP presentation ratio IID2  

# parameters list

# par = list(
#   b = b,
#   d = d,
#   delta = delta,
#   epsilon = epsilon,
#   sigma = sigma,
#   psi = psi,
#   gamma = gamma,
#   w1 = w1,
#   w2 = w2,
#   nstates = 5,
#   npops = 4           # change to 1 pop, for four age groups 4 npops
# )

# UK 2019 pop structure

# init <- c(3857263,8103718,42460865,12374961,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# init <- c(3860000,8100000,29190000,29620000,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # Simmons pop structure
# init <- c(3451560,9754443,37374240,9629769,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # POLYMOD pop structure
# init <- c(3453670, 7385454, 39774569, 9673058,60,60,60,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # noromod initial conditions

# init <- c(1.843476e+06, 6.179420e+06, 3.561739e+07, 1.134015e+07, 5.543698e+02, 5.282591e+02,
#           1.876738e+03, 2.835417e+02, 7.922342e+02, 7.550319e+02, 2.682455e+03, 4.052758e+02,
#           1.180888e+04, 7.077697e+03, 2.287535e+04, 3.165726e+03, 2.000732e+06, 1.915937e+06,
#           6.816038e+06, 1.030954e+06, 7.826982e+06, 2.170317e+06, 4.723889e+06, 3.376726e+05,
#           7.440857e+06, 7.408256e+06, 2.642395e+07, 4.006681e+06)

# aging

# ages <- c(0, 5, 15, 65)
# da <- diff(c(0,ages))
# length(ages)
# aging <- diag(-1/da)
# aging[row(aging)-col(aging)==1] <- 1/head(da,-1)
# filled.contour(aging)
# aging <- aging/(365)