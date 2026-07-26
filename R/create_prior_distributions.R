create_prior_df <- function(n_points = 1000) {
  # Create sequences for each parameter
  priors <- list(
    # Uniform priors for surveillance reports
    surveillance_report_1 = list(x = seq(0, 0.06, length.out = n_points), 
                                 density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_1_summer = list(x = seq(0, 0.06, length.out = n_points), 
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_1_winter = list(x = seq(0, 0.06, length.out = n_points), 
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_2 = list(x = seq(0, 0.06, length.out = n_points),
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_2_summer = list(x = seq(0, 0.06, length.out = n_points),
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_2_winter = list(x = seq(0, 0.06, length.out = n_points),
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    surveillance_report_3 = list(x = seq(0, 0.06, length.out = n_points),
                                 density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_3_summer = list(x = seq(0, 0.06, length.out = n_points),
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    # surveillance_report_3_winter = list(x = seq(0, 0.06, length.out = n_points),
    #                              density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    surveillance_report_4_summer = list(x = seq(0, 0.06, length.out = n_points),
                                 density = dunif(seq(0, 0.1, length.out = n_points), 0, 0.1)),
    surveillance_report_4_winter = list(x = seq(0, 0.06, length.out = n_points),
                                 density = dunif(seq(0, 0.1, length.out = n_points), 0, 0.1)),
    # Normal prior for sigma
    sigma = list(x = seq(0.5, 1, length.out = n_points),
                 density = dnorm(seq(0.5, 1, length.out = n_points), 0.75, 0.075)),
    # Uniform priors for seasonal parameters
    season_amp = list(x = seq(0, 10/100, length.out = n_points),
                      density = dunif(seq(0, 10/100, length.out = n_points), 0, 10/100)),
    season_offset = list(x = seq(0, 50/100, length.out = n_points),
                         density = dunif(seq(0, 50/100, length.out = n_points), 0, 50/100)),
    D_immun = list(x = seq(0.5, 18, length.out = n_points),
                   density = dunif(seq(0.5, 18, length.out = n_points), 0.5, 18)),
    # Log-uniform prior for AKI hospitalisation
    # aki_hospitalisation_4_summer = list(
    #   x = seq(0.0000001, 0.75, length.out = n_points),
    #   density = dunif(seq(0.0000001, 0.75, length.out = n_points), 0.0000001, 0.75)
    # ),
    aki_hospitalisation_4_winter = list(
      x = seq(0.0000001, 0.75, length.out = n_points),
      density = dunif(seq(0.0000001, 0.75, length.out = n_points), 0.0000001, 0.75)
    ),
    # Log-normal priors for transmission probabilities
    probT_under5 = list(
      x = seq(0.0000001, 0.5, length.out = n_points),
      density = dnorm(seq(0.0000001, 0.5, length.out = n_points), mean = 0.21, sd = 0.115)
    ),
    probT_over5 = list(
      x = seq(0.0000001, 0.114, length.out = n_points),
      density = dnorm(seq(0.0000001, 0.114, length.out = n_points), mean = 0.05, sd = 0.032)
    ),
    # Log-uniform priors for gastro parameters
    # gastro_hospitalisation_4_summer = list(
    #   x = seq(0.0000001, 0.5, length.out = n_points),
    #   density = dunif(seq(0.0000001, 0.5, length.out = n_points), 0.0000001, 0.5)
    # ),
    gastro_hospitalisation_4_winter = list(
      x = seq(0.0000001, 0.5, length.out = n_points),
      density = dunif(seq(0.0000001, 0.5, length.out = n_points), 0.0000001, 0.5)
    ),
    gastro_gp_attend_1 = list(
      x = seq(0.0000001, 0.75, length.out = n_points),
      density = dunif(seq(0.0000001, 0.75, length.out = n_points), 0.0000001, 0.75)
    )
    # gastro_gp_attend_2 = list(
    #   x = seq(0.0000001, 0.75, length.out = n_points),
    #   density = dunif(seq(0.0000001, 0.75, length.out = n_points), 0.0000001, 0.75)
    # )
  )
  
  # Convert to data frame
  prior_df <- data.frame(
    theta = rep(names(priors), each = n_points),
    x = unlist(lapply(priors, function(p) p$x)),
    density = unlist(lapply(priors, function(p) p$density))
  )
  
  return(prior_df)
}


### truncated normal for  sigma

create_prior_df <- function(n_points = 1000) {
  
  # Truncated normal for sigma: N(0.75, 0.075) hard-stopped at 0.9
  sigma_x   <- seq(0.5, 0.9, length.out = n_points)
  sigma_raw <- dnorm(sigma_x, mean = 0.75, sd = 0.075)
  sigma_den <- sigma_raw / (sum(sigma_raw) * (0.4 / (n_points - 1)))
  
  priors <- list(
    surveillance_report_1 = list(x = seq(0, 0.06, length.out = n_points),
                                 density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    surveillance_report_3 = list(x = seq(0, 0.06, length.out = n_points),
                                 density = dunif(seq(0, 0.06, length.out = n_points), 0, 0.06)),
    surveillance_report_4_summer = list(x = seq(0, 0.06, length.out = n_points),
                                        density = dunif(seq(0, 0.1, length.out = n_points), 0, 0.1)),
    surveillance_report_4_winter = list(x = seq(0, 0.06, length.out = n_points),
                                        density = dunif(seq(0, 0.1, length.out = n_points), 0, 0.1)),
    sigma = list(x = sigma_x, density = sigma_den),
    season_amp = list(x = seq(0, 10/100, length.out = n_points),
                      density = dunif(seq(0, 10/100, length.out = n_points), 0, 10/100)),
    season_offset = list(x = seq(0, 50/100, length.out = n_points),
                         density = dunif(seq(0, 50/100, length.out = n_points), 0, 50/100)),
    D_immun = list(x = seq(0.5, 18, length.out = n_points),
                   density = dunif(seq(0.5, 18, length.out = n_points), 0.5, 18)),
    aki_hospitalisation_4_winter = list(
      x = seq(0.0000001, 0.75, length.out = n_points),
      density = dunif(seq(0.0000001, 0.75, length.out = n_points), 0.0000001, 0.75)
    ),
    probT_under5 = list(
      x = seq(0.0000001, 0.5, length.out = n_points),
      density = dnorm(seq(0.0000001, 0.5, length.out = n_points), mean = 0.21, sd = 0.115)
    ),
    probT_over5 = list(
      x = seq(0.0000001, 0.114, length.out = n_points),
      density = dnorm(seq(0.0000001, 0.114, length.out = n_points), mean = 0.05, sd = 0.032)
    ),
    gastro_hospitalisation_4_winter = list(
      x = seq(0.0000001, 0.5, length.out = n_points),
      density = dunif(seq(0.0000001, 0.5, length.out = n_points), 0.0000001, 0.5)
    ),
    gastro_gp_attend_1 = list(
      x = seq(0.0000001, 0.75, length.out = n_points),
      density = dunif(seq(0.0000001, 0.75, length.out = n_points), 0.0000001, 0.75)
    )
  )
  
  prior_df <- data.frame(
    theta   = rep(names(priors), each = n_points),
    x       = unlist(lapply(priors, function(p) p$x)),
    density = unlist(lapply(priors, function(p) p$density))
  )
  
  return(prior_df)
}


# Create the prior data frame
prior_df <- create_prior_df()

#plotPosteriorDensity(trace = traceBurnThin_df, prior = prior_df)
