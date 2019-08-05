
## This dofile creates functions that are used later

####### 
# Function to grab a mode
#######

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(c("a","b","a"))

#######
# Function to aggregate to mean
#######

agg_to_mean <- function(data, by)  aggregate(data, by=by, FUN="mean", na.rm=TRUE)

#######
# Function to report mean in two groups and standardized difference
#######

balance_function <- function(Y, X, W = rep(1, length(X))){
  
  K <- (!is.na(Y[X==0]) & !is.na(W[X==0]))
  c(
    control   = weighted.mean(Y[X==0], W[X==0], na.rm = TRUE),
    treatment = weighted.mean(Y[X==1], W[X==1], na.rm = TRUE),
    d_stat    = (weighted.mean(Y[X==1], W[X==1], na.rm = TRUE) -
                 weighted.mean(Y[X==0], W[X==0], na.rm = TRUE))/
                (wt.sd(Y[X==0][K], W[X==0][K])),
    N         = sum(!is.na(Y[X==1 | X==0]))
  )}


#######
# Function to do mean effects
#######


normalize <- function(.data,
                      .treat,
                      .weight,
                      .outcome,
                      .stats = NULL) {
  # create temporary dataset  without missngness in
  # .weight, .outcome and filtered by .treat == 0
  .temp <-
    .data[!is.na( eval(parse(text = paste0(".data$", .weight))) ) &
            !is.na( eval(parse(text = paste0(".data$", .outcome))) ) &
            eval(parse(text = paste0(".data$", .treat))) == 0, ]
  # get the weighted mean of .outcome variable
  Ymean <-  weighted.mean(x = .temp[ , .outcome],
                          w = .temp[ , .weight],
                          na.rm = TRUE)
  # get the analytic weighted std. dev. of .outcome variable
  Yvar <- ( nrow(.temp) / (sum(.temp[ , .weight]) * (nrow(.temp) - 1)) ) *
    sum( .temp[ , .weight] * (.temp[ , .outcome] - Ymean)^2 )
  # return normalized vector of .outcome
  if (is.null(.stats)) return( (.data[ , .outcome] - Ymean)/sqrt(Yvar) )
  else return(list(".outcome_norm" = (.data[ , .outcome] - Ymean)/sqrt(Yvar),
                   ".outcome_mean" = Ymean,
                   ".outcome_sd" = sqrt(Yvar)))
}

# weighted mean effects

wmeaneffects <- function(.data,
                         .treat,
                         .weight,
                         .outcomes,
                         .varname = "index_new",
                         .cond = NULL) {
  require(magrittr)
  require(plyr)
  require(dplyr)
  
  if (is.null(.cond)) {
    .subset <- rep(TRUE, times = nrow(.data))
    .temp <- .data
  } else {
    .subset <- eval(expr = parse(text = .cond),
                    envir = .data,
                    enclos = parent.frame())
    .temp <- .data[.subset, ]
  }
  
  .out <- rep(NA, times = nrow(.data)) %>% cbind
  colnames(.out) <- .varname
  .out[.subset,.varname] <-
    .temp %>%
    sapply(X = .outcomes,
           FUN = normalize,
           .data = .,
           .treat = .treat,
           .weight = .weight) %>%
    apply(MARGIN = 1,
          FUN = mean,
          na.rm = TRUE) %>%
    cbind(.temp, "meaneff" = . ) %>%
    as.data.frame %>%
    normalize(.data = .,
              .treat = .treat,
              .weight = .weight,
              .outcome = "meaneff") %>%
    round(digits = 6) %>%
    as.vector
  return(.out)
}




#######
# Function to 
# "fixdec"
#######

fixdec <- function(.x, .dec = 3, .brac = FALSE) {
  if (.brac) format(round(.x, .dec), nsmall = .dec) %>% paste0("(", . , ")")
  else format(round(.x, .dec), nsmall = .dec)
}

####### 
# Function to extract tables of different results for different conditions
# Function to extract results  -- used for making tables; note that "take" takes 4 for interaction terms
# of one can also put in the name of the variables which is generally safer
####### 

get.results = function(L, 
                       take = c(rep("TUUNGANE",2),"TUUNGANE:RA",rep("TUUNGANE",1),"TUUNGANE:CHIEFTRUE",rep("TUUNGANE",10),"TUUNGANE:IDS_RAPID"), 
                       elements = 1:length(take),
                       round = 2
) {
  x<-t({sapply(elements, function(i) {
    if(is.numeric(take[i]))  out <- c(row.names(L[[i]][[1]])[take[i]], round(L[[i]][[1]][take[i],],round))
    if(!is.numeric(take[i])) out <- c(take[i], round(L[[i]][[1]][row.names(L[[i]][[1]])==take[i],],round)) 
    return(out)})
  })
  
  out <- apply(x[,-1], 2,as.numeric)
  row.names(out) <- paste0(elements,"_",x[,1])
  return(out)
}

get.results.tab = function(L, 
                           take = c(rep("TUUNGANE",2),"TUUNGANE:RA",rep("TUUNGANE",1),"TUUNGANE:CHIEFTRUE",rep("TUUNGANE",10),"TUUNGANE:IDS_RAPID"), 
                           main = c(rep(NA,2),"RA",rep(NA,1),"CHIEFTRUE",rep(NA,10),"IDS_RAPID"), 
                           elements = 1:length(take),
                           rounder = 3
) {
  x<-t({sapply(elements, function(i) {
    X1 <- L[[i]][[1]]
    X2 <- L[[i]][[2]]
    if(is.numeric(take[i]))  out <- c(row.names(X1)[take[i]], X1[take[i],])
    if(!is.numeric(take[i])) out <- c(take[i], X1[row.names(X1)==take[i],]) 
    if(is.na(main[i]))    out <- c(out, control = X2[1,2], N = sum(X2[,4]))
    if(!is.na(main[i]))   out <- c(out, control = X1[row.names(X1)==main[i],][1], N = sum(X2[,4]))
    return(  unlist(out))})
  })
  
  # Format for table
  x0 <- apply(x[,-1], 2,as.numeric)
  sds <- paste("(", round(x0[,2],rounder), ifelse(x0[,4] < 0.05, "**", ""),")", sep = "")
  cbind(x[,1], round(x0[,c(5,1)],rounder), sds,x[,7])
}

mat_to_tex <- function(mat, rownames = NULL, add_slashes = TRUE) {
  if(!is.null(rownames)) mat <- cbind(rownames, mat)
  x <- t(sapply(1:nrow(mat), function(j)  paste(mat[j,], collapse  = "&")))
  if(add_slashes) x <- paste(x, rep("\\\\", nrow(mat)))
  as.matrix(x, nrow(mat),1)
}


tablr <- function(x){
  x <- paste(x,"  \n",sep="")
  cat(x)
}



#######
# Function to generate a pattern of rounding and bracketing
#######

out_line <- function(.out_list,
                     .dec_pattern = c(2,2,2,0),
                     .brac_pattern = c(F,F,T,F)) {
  mapply(fixdec,
         c(.out_list[[1]][c(dim(.out_list[[1]])[1] - 1,
                            dim(.out_list[[1]])[1]),1],
           .out_list[[1]][dim(.out_list[[1]])[1],2],
           sum(.out_list[[2]][,4])),
         .dec_pattern,
         .brac_pattern) %>%
    unname() %>%
    return()
}

#######
# Function to 
#######

out_balance <- function(.out_list,
                        .dec_pattern = c(2,2,2,0),
                        .brac_pattern = c(F,F,F,F)) {
  mapply(fixdec,
         c(as.matrix(.out_list[[2]])[c(2,1),2],
           .out_list[[1]][dim(.out_list[[1]])[1],1],
           sum(.out_list[[2]][,4])),
         .dec_pattern,
         .brac_pattern) %>%
    unname() %>%
    return()
}

#######
# Function to 
#######

out_robust <- function(.out_list,
                       .dec_pattern = c(2,2),
                       .brac_pattern = c(F,T),
                       .coef_pattern = 4) {
  if (dim(.out_list[[1]])[1] > 2)  {
    mapply(fixdec,
           .out_list[[1]][.coef_pattern,c(1,2)],
           .dec_pattern,
           .brac_pattern) %>%
      unname() %>%
      paste0(collapse = " ") %>%
      return()
  } else {
    mapply(fixdec,
           .out_list[[1]][dim(.out_list[[1]])[1],c(1,2)],
           .dec_pattern,
           .brac_pattern) %>%
      unname() %>%
      paste0(collapse = " ") %>%
      return()
  }
}

#######
# Generate MDE plots
#######

mde_plot <- function(diagnoses, main = "", xlim = c(0,1)){
  plot(abs(diagnoses[[2]]$mean_estimand), diagnoses[[2]]$power, ylim = c(0,1), xlim = xlim, 
       main = main, xlab = "ATE (Absolute Value)", ylab = "power", type = "b")
  abline(a = .8, b = 0)
}

#######
# Report mean in two groups and standardized difference 
#######

balance_function <- function(Y,X,W = rep(1, length(X))){
  
  K <- (!is.na(Y[X==0]) & !is.na(W[X==0]))
  c(
    weighted.mean(Y[X==0], W[X==0], na.rm = TRUE),
    weighted.mean(Y[X==1], W[X==1], na.rm = TRUE),
    (weighted.mean(Y[X==1], W[X==1], na.rm = TRUE) - weighted.mean(Y[X==0], W[X==0], na.rm = TRUE))/
      (wt.sd(Y[X==0][K], W[X==0][K])),
    sum(!is.na(Y[X==1 | X==0]))
  )}

#####
# Load data set
#####

load_file <- function(data_file = "", path = local_datapath){
  haven::read_dta(file = paste(path, data_file, sep = "/"))
}

#####
# spillover weights
#####

# A general function to figure weights -- needed for RI 
# Cases will be dropped if weight = Inf or weight = 1
# cap of weight = 20 placed on weights
gen_weight05 = function(D, I, data = gps, cap = 20){
  w  <- rep(NA, nrow(data))
  w[D ==1 & I ==1] <- data$wDI_05[D ==1 & I ==1]
  w[D ==0 & I ==1] <- data$w0I_05[D ==0 & I ==1]
  w[D ==1 & I ==0] <- data$wD0_05[D ==1 & I ==0]
  w[D ==0 & I ==0] <- data$w00_05[D ==0 & I ==0]
  w[w == Inf] <- NA
  w[w == 1]   <- NA
  w[w > cap] <- cap
  return(w)
}

gen_weight20 = function(D, I, data = gps, cap = 20){
  w  <- rep(NA, nrow(data))
  w[D ==1 & I ==1] <- data$wDI_20[D ==1 & I ==1]
  w[D ==0 & I ==1] <- data$w0I_20[D ==0 & I ==1]
  w[D ==1 & I ==0] <- data$wD0_20[D ==1 & I ==0]
  w[D ==0 & I ==0] <- data$w00_20[D ==0 & I ==0]
  w[w == Inf] <- NA
  w[w == 1]   <- NA
  w[w > cap] <- cap
  return(w)
}

#####
# function to assess how many treated villages from outside CDC within distance X?
#####

indirect <- function(df = gps, d = 5, adjac = adj, treated = gps$TUUNGANE, binary = TRUE){
  same_cdc <- sapply(df$CDCCODE, function(i) df$CDCCODE==i)   # adjacency matrix for same CDC
  close <- (adjac <= d) & !same_cdc 
  out <- treated %*% close
  if(binary) out <- 1*(out > 0) #REVIEWED OLD: 1*(out > 1)
  return(as.numeric(out))
}


## IS NA or NAN

isNA <- function(x) is.na(x)  | is.nan(x)

#####
# tidy results
#####

tidy_results <- function(x, name, alt_treat = FALSE){ 
  tidy = tidy(x); rownames(tidy) = tidy$term;
  treat <- ifelse(alt_treat, "IRC_TUUNGANE","TUUNGANE")
  round(c(Control  = tidy["(Intercept)", "estimate"],
                 Effect    = tidy[treat, "estimate"], 
                 std_error = tidy[treat, "std.error"], 
                 N         = x$N ),3)
}

####
# Generate village means aggregating for sampling weights at village level
####
genVillmeans <-  function(y, d){
  d$Y <- d[,y, drop = TRUE]
  # Weighted average  
  d <- d %>% group_by(IDV) %>% mutate(use_weight   = ifelse(is.na(Y), NA, IDS_HH_SAMP_WEIGHT_DML),
                                      tot_weight   = sum(use_weight, na.rm = TRUE), 
                                      Y_unweighted = mean(Y, na.rm = TRUE),
                                      Y_weighted   = sum(Y*use_weight/tot_weight, na.rm = TRUE)) %>% 
    data.frame
  dvill     <- d[,names(d) %in% c("IDV", "Y_weighted", "Y_unweighted", "TUUNGANE", "LOTT_BIN", "VILL_WEIGHT", "WEIGHT", "IDS_CDCCODE")]
  dvill[!duplicated(dvill),]
}

genVilldiff <-  function(y, d){
  
  if(!(y == "qr2830_list_experiment" | y == "Correct_B_projet")) stop("only for Correct_B_projet and qr2830_list_experiment")
  
  # subset interactions
  if(y == "qr2830_list_experiment")   {d$i0 <- d$RB    == 0; d$i1 <- d$RB    == 1}
  if(y == "Correct_B_projet")         {d$i0 <- d$CHIEF == 0; d$i1 <- d$CHIEF == 1}
  
  # compute subsetted villmeans 
  
  villmean_0 <- genVillmeans("i0", d)
  colnames(villmean_0)[colnames(villmean_0)=="Y_unweighted"] <- "i0_unweighted"
  colnames(villmean_0)[colnames(villmean_0)=="Y_weighted"]   <- "i0_weighted"
  
  villmean_1 <- genVillmeans("i1", d)
  colnames(villmean_1)[colnames(villmean_1)=="Y_unweighted"] <- "i1_unweighted"
  colnames(villmean_1)[colnames(villmean_1)=="Y_weighted"]   <- "i1_weighted"
  
  villmean <- merge(villmean_0, select(villmean_1, IDV, i1_unweighted, i1_weighted), by = "IDV")
  
  villmean <- mutate(villmean,
                     Y_weighted   = i1_weighted   - i0_weighted,
                     Y_unweighted = i1_unweighted - i0_unweighted)
  villmean       
}

# Produces robustness table, and heterogeneous effects table

splice_sds <- function(M) {
  m <- nrow(M)
  out <- matrix(NA, 2*m,1)
  out[2*(1:m) - 1,1] <- M[,1]
  out[2*(1:m),1] <- M[,2]
  out
}
splice_sds(M = matrix(1:4,2))



#####
# FUNCTIONS FOR SPILLOVER ANALYSIS
#####

# Test statistic is the meas square error (actually, sqrt of this)
gps.analysis <- function(Y, 
                         DIR = gps$TUUNGANE, 
                         IND = gps$indirect05,
                         weight = gps$gps_weight05, 
                         coef = FALSE,               #If TRUE return the coefficients, if false just return the MSE
                         blocks = gps$LOTT_BIN){
  .dir <- DIR - mean(DIR, na.rm = TRUE)   
  .ind <- IND - mean(IND, na.rm = TRUE)   
  .int <- .dir*.ind
  if(is.null(blocks))  M   <- summary(lm(Y ~ .dir + .ind + .int, weight = weight))
  if(!is.null(blocks)) M   <- summary(lm(Y ~ .dir + .ind + .int + as.factor(blocks), weight = weight))
  if(coef)  out <- list(coef(M)[2:3, 1:2], sqrt(mean(M$residuals^2)),  length(M$residuals))
  if(!coef) out <- sqrt(mean(M$residuals^2))    # MSE CALCULATION
  return(out)
}

# Get the MSE for assignment i: Note that weights need to be produced for each assignment
test.stats  <- function(i, 
                        Y, 
                        directs=dir, 
                        indirects=ind05,
                        blocks = gps$LOTT_BIN, 
                        weight_matrix){
  diri <- directs[,i]
  indi <- indirects[,i]
  wi   <- weight_matrix[,i]
  gps.analysis(Y, DIR = diri, IND = indi, weight = wi, blocks = blocks, coef = FALSE)
}

## Randomization Inference Analysis Function

ri.analysis <- function(Y, 
                        DIR       = gps$TUUNGANE,   
                        directs   = dir,     
                        IND       = gps$indirect05, 
                        indirects = ind05, 
                        weight    = gps$gps_weight05, 
                        weight_matrix, 
                        spilloversims = 10,
                        blocks    = NULL){
  
  real          <- gps.analysis(Y, DIR = DIR, IND = IND, weight = weight, coef = TRUE, blocks = blocks)
  
  tstats05      <- sapply(1:spilloversims,
                          function(i) test.stats(i = i, Y = Y,
                                                 directs = directs, 
                                                 indirects = indirects,
                                                 blocks = blocks, 
                                                 weight_matrix = weight_matrix))
  
  out <- c(real[[1]][1,], real[[1]][2,], real[[2]], "MSE(p)"=mean(tstats05 <= real[[2]]), real[[3]])
  names(out) <- c("d", "se_d", "in", "se_in", "RMSE", "p", "N")
  return(out)
}


## END ##

