formula <- NULL
index <- 1
#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="rw2", constr = TRUE, hyper = list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)), scale.model = T) + 
		f(time.iid, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(region, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(regiontime, model = "iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "RW2-PC-U"
index <- index + 1
#------------------------------------------------------------#
formula[[index]] <- update(formula[[index-1]], ~ . - 
		f(region, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(region, model = "bym2", graph = mat, constr = TRUE,scale.model = TRUE, hyper = list(phi = list(prior = 'pc', param = c(0.5 , 2/3) , initial = -3), prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "RW2-PC-S"
index <- index + 1

#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="rw1", constr = TRUE, hyper = list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)), scale.model = T) + 
		f(time.iid, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(region, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(regiontime, model = "iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "RW1-PC-U"
index <- index + 1
#------------------------------------------------------------#
formula[[index]] <- update(formula[[index-1]], ~ . - 
		f(region, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(region, model = "bym2", graph = mat, constr = TRUE,scale.model = TRUE, hyper = list(phi = list(prior = 'pc', param = c(0.5 , 2/3) , initial = -3), prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "RW1-PC-S"
index <- index + 1

#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="ar1",  hyper = list(
			theta1 = list(prior="pc.prec", param=c(0.2/0.31, 0.01)),
			theta2 = list(prior="pc.cor1", param=c(0.9, 0.9)))) + 
		f(time.iid, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(region, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(regiontime, model = "iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "AR1-PC-U"
index <- index + 1

#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="ar1",  hyper = list(
			theta1 = list(prior="pc.prec", param=c(0.2/0.31, 0.01)),
			theta2 = list(prior="pc.cor1", param=c(0.9, 0.9)))) + 
		f(time.iid, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(regiontime, model = "iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "AR1-PC-random"
index <- index + 1
#------------------------------------------------------------#
formula[[index]] <- update(formula[[index-1]], ~ . - 
		f(region, model="iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + 
		f(region, model = "bym2", graph = mat, constr = TRUE,scale.model = TRUE, hyper = list(phi = list(prior = 'pc', param = c(0.5 , 2/3) , initial = -3), prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5))))
names(formula)[index] <- "AR1-PC-S"
index <- index + 1

#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="rw2", constr = TRUE, scale.model = T) + 
		f(time.iid, model="iid") + 
		f(region, model="iid") + 
		f(regiontime, model = "iid")
names(formula)[index] <- "RW2-default-U"
index <- index + 1
#------------------------------------------------------------#
formula[[index]] <- update(formula[[index-1]], ~ . - f(region, model="iid") + f(region, model = "bym2", graph = mat, constr = TRUE,scale.model = TRUE))
names(formula)[index] <- "RW2-default-S"
index <- index + 1

#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="rw1", constr = TRUE, scale.model = T) + 
		f(time.iid, model="iid") + 
		f(region, model="iid") + 
		f(regiontime, model = "iid")
names(formula)[index] <- "RW1-default-U"
index <- index + 1
#------------------------------------------------------------#
formula[[index]] <- update(formula[[index-1]], ~ . - f(region, model="iid")  + f(region, model = "bym2", graph = mat, constr = TRUE,scale.model = TRUE))
names(formula)[index] <- "RW1-default-S"
index <- index + 1

#------------------------------------------------------------#
formula[[index]] = counts ~ -1 + Intercept + 
		f(time, model="ar1") + 
		f(time.iid, model="iid") + 
		f(region, model="iid") + 
		f(regiontime, model = "iid")
names(formula)[index] <- "AR1-default-U"
index <- index + 1
#------------------------------------------------------------#
formula[[index]] <- update(formula[[index-1]], ~ . - f(region, model="iid")  + f(region, model = "bym2", graph = mat, constr = TRUE,scale.model = TRUE))
names(formula)[index] <- "AR1-default-S"
index <- index + 1




#------------------------------------------------------------#
# population as offset, without Queensgate
if(formula.group == 1){
	 # do nothing
}
#------------------------------------------------------------#
# population as offset, with Queensgate	
if(formula.group == 2){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
				~ . + Queensgate)
	}
}	
#------------------------------------------------------------#
# raw population	
if(formula.group == 3){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
				~ . + population_1e3)
	}
}
#------------------------------------------------------------#
# raw population	
if(formula.group == 4){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
				~ . + population_1e3 + Queensgate)
	}
}
#------------------------------------------------------------#
# log population
if(formula.group == 5){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
				~ . + log_population)
	}
}
#------------------------------------------------------------#
# log population with Queensgate
if(formula.group == 6){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
				~ . + log_population + Queensgate)
	}
}

#------------------------------------------------------------#
# Independent interaction formulas
if(fixed.group == 1){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
		~ . + pc_age25 + pc_male + pc_male25 +  
		pc_white + pc_black + pc_asian +  
		# pc_ind_alaska + pc_hawaiian + 
		pc_two_races + pc_hispanic + household_income +
		 per_cap_income +  pc_poverty + pc_bachelor +  pc_park +
		 pc_bus_quarter + fire + pharm + hospital + fqhc + 
		 otp + bup + fastfood + 
		 pc_singlefamily + pc_residential_other + pc_office + pc_commercial + pc_urban_mixed + pc_downtown + pc_manufacturing + pc_riverfront + pc_development + 
		 temperature + precipitation + crime_rate)
	}
}
if(fixed.group == 2){
	for(index in 1:length(formula)){
		formula[[index]] <- update(formula[[index]], 
		~ . + pc_male + pc_age18_24 + pc_age25_34 + pc_age35_49 + pc_age50_64 + pc_age65up +
		pc_white + pc_black + pc_asian +  
		# pc_ind_alaska + pc_hawaiian + 
		pc_two_races + pc_hispanic + household_income +
		 per_cap_income +  pc_poverty + home_value + home_value_change + pc_bachelor + pc_park +
		 pc_bus_quarter + fire + pharm + hospital + fqhc + 
		 otp + bup + fastfood + 
		 pc_singlefamily + pc_residential_other + pc_office + pc_commercial + pc_urban_mixed + pc_downtown + pc_manufacturing + pc_riverfront + pc_development + 
		 temperature + precipitation + crime_rate)
	}
}


formula.int <- formula
is.pc <- grep("PC", names(formula))
for(index in is.pc){
	formula.int[[index]] <- update(formula.int[[index]], 
			~ . -f(regiontime, model = "iid", hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31 , 0.01) ,initial = 5)))) + f(region.int, model = "besag", graph = mat, scale.model = TRUE, hyper = list(theta=list(prec = list(prior = "pc.prec", param = c(0.2/0.31, 0.01) ,initial = 5))), group = time.int, control.group = list(model = "ar1", hyper = list(
			rho = list(prior="pc.cor1", param=c(0.9, 0.9))))) 
			)
}
for(index in (1:length(formula))[-is.pc]){
	formula.int[[index]] <- update(formula.int[[index]], 
			~ . -f(regiontime, model = "iid") + f(region.int, model = "besag", graph = mat, scale.model = TRUE, group = time.int, control.group = list(model = "ar1")) 
			)
}
names(formula.int) <- paste0(names(formula.int), "-I")
formula <- c(formula, formula.int)
#------------------------------------------------------------#
# Baseline formulas
formula.base <- update(formula[[1]], 
	~ . -f(time, model = "rw2", constr = TRUE, hyper = list(prec = list(prior = "pc.prec", 
    param = c(0.2/0.31, 0.01), initial = 5)), scale.model = T) - 
    f(time.iid, model = "iid", hyper = list(theta = list(prec = list(prior = "pc.prec", 
        param = c(0.2/0.31, 0.01), initial = 5)))) - 
    f(region, 
    model = "iid", hyper = list(theta = list(prec = list(prior = "pc.prec", 
        param = c(0.2/0.31, 0.01), initial = 5)))) - 
    f(regiontime, 
    model = "iid", hyper = list(theta = list(prec = list(prior = "pc.prec", 
        param = c(0.2/0.31, 0.01), initial = 5)))))
formula.base.random <- update(formula.base, 
	~ . + f(regiontime, model = "iid"))
formula.base.strandom <- update(formula.base, 
	~ . - f(regiontime, model = "iid") + f(time, model = "iid") + f(region, model = "iid"))
formula <- c(formula, formula.base, formula.base.random, formula.base.strandom)
names(formula)[length(formula) - 2] <- "fixed"
names(formula)[length(formula) - 1] <- "random"
names(formula)[length(formula)] <- "st-random"


