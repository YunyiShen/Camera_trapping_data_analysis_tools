Days_reduce_occu = function(n_det, # data, number of detected days
                            n_cameradays, # number of camera days to detect n_dets
                            decay_from = 0.99, # prior probability for occupancy
                            decay_to = 0.05, # probability of occupancy required
                            alpha = 0.5,
                            beta = 0.5 , # the prior distribution of detection probability, used beta
                            n_sample = 10000, # number of posterior sampels wanted
                            CI = 0.95 # CI for first detect
                            ){
  posterior_p = rbeta(n_sample,alpha+n_det,beta+(n_cameradays-n_det))
  
  posterior_days_to_decay = sapply(posterior_p,function(p,decay_from,decay_to){
    ceiling( min(uniroot(function(x,p,decay_from,decay_to){
      (decay_from*(1-p)^x)/((decay_from*(1-p)^x)+1-decay_from)-decay_to # solve the equation to get time needed
    },c( 1, 1e5),p,decay_from,decay_to)$root))
  },decay_from,decay_to) # solve for t for every posterior sample of p
  
  posterior_day_to_first_det = sapply(posterior_p,function(p,CI){
    qgeom(CI,p) # find CIth quantile of geometric distribution
  },CI)
  
  return(data.frame(posterior_days_to_decay=posterior_days_to_decay,
                    posterior_day_to_first_det = posterior_day_to_first_det ))
  
}

www = Days_reduce_occu(5,40)
hist(www$posterior_days_to_decay)
hist(www$posterior_day_to_first_det)