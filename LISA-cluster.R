#local moran's I

quadrant_323 <- vector(mode='numeric',length=nrow(london_exclude_city))

#centers the variable of interest around its mean
m.new_cases_rate <- london_exclude_city$new_cases_per_10k_population-mean(london_exclude_city$new_cases_per_10k_population)

#centers the local moran's around the mean
m.local_323 <- local_moran_323[,1]-mean(local_moran_323[,1])

#significance threshold
signif <- 0.05

#builds a data quadrant
quadrant_323[m.new_cases_rate>0 & m.local_323>0] <- 4
quadrant_323[m.new_cases_rate<0 & m.local_323<0] <- 1
quadrant_323[m.new_cases_rate<0 & m.local_323>0] <- 2
quadrant_323[m.new_cases_rate>0 & m.local_323<0] <- 3
quadrant_323[local_moran_323[,5]>signif] <- 0

#plotting in r
brks <- c(0,1,2,3,4)
colors <- c('white','blue',rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),'red')

plot(london_exclude_city,border='lightgray',main='LISA cluster',col=colors[findInterval(quadrant_323,brks,all.inside = FALSE)],max.plot=1)

legend('bottomright',legend=c('insignificant',
                             'low-low','low-high',
                             'high-low','high-high'),fill=colors,bty='n')


#for 5.23-6.22 data
quadrant_523 <- vector(mode='numeric',length=nrow(london_exclude_city_523))

m.new_cases_rate_523 <- london_exclude_city_523$new_cases_per_10k_523_622-mean(london_exclude_city_523$new_cases_per_10k_523_622)

m.local_523 <- local_moran_523[,1]-mean(local_moran_523[,1])

quadrant_523[m.new_cases_rate_523>0 & m.local_523>0] <- 4
quadrant_523[m.new_cases_rate_523<0 & m.local_523<0] <- 1
quadrant_523[m.new_cases_rate_523<0 & m.local_523>0] <- 2
quadrant_523[m.new_cases_rate_523>0 & m.local_523<0] <- 3
quadrant_523[local_moran_523[,5]>signif] <- 0

plot(london_exclude_city_523,border='lightgray',col=colors[findInterval(quadrant_523,brks,all.inside = FALSE)],max.plot=1,main='LISA cluster')
legend('bottomright',legend=c('insignificant',
                              'low-low','low-high',
                              'high-low','high-high'),fill=colors,bty='n')

