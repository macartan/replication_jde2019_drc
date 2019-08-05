

# Throughout the analysis, we combine data sets into three main ones:
# ind (INDIVIDUAL) comprises 10640 individual observations for 1120 villages
# vill (VILLAGE) comprises 1120 observations for 1120 villages, including audit measures for 560 villages
# disc (DISCUSSION) comprises 7761 obs of 457 villages

# Consolidate data --------------------------------------------------------
abd_vill$IDV %<>% as.numeric
abd_disc$IDV %<>% as.numeric
abd_ind$IDV %<>% as.numeric
# irc_tuungane$IDV %<>% as.numeric
tuungane$IDV %<>% as.numeric
audit$IDV %<>% as.numeric

# Create village robustness variables and assignment ----------------------
# Below, we create a set of variables that is merged with each of the raw data sets
# which includes tuungane treatment assignment, village adjusted propensity weights,
# village-level measures for school absence (NOSCHOOLS), inheritance (INHERITED), and committee absence (NOCOMITTEE)

SCHOOLS_INHERITED <- abd_vill %>% mutate(
  # Create variable NOSCHOOLS from cq024_b1_initiative_2006
  # Villages with little infrastructure (ie that state that they had no schools; "not applicable" removed) in 2007
  NOSCHOOLS = case_when(cq024_b1_initiative_2006 > 0 ~ 0,
                        cq024_b1_initiative_2006 == 0 ~ 1),
  # Create variable INHERITED from cq048_how_former_chief
  # Father of chief had his position inherited (=="3 Inherited")
  INHERITED = case_when(cq048_how_former_chief=="3 Inherited" ~ 1,
                        !cq048_how_former_chief %in% c("", "3 Inherited", "-8 NA") ~ 0))

# Indicator for villages with at least one committee that already existed in 2007
# Measure is whether we have a record of historical committees
# IDS_TYPES=="DC" & q5362_a_committee_exist1!=""
committees <- c("Agriculture", "Church", "Development", "Education", "Health", "Infrastructure", "None", "Other", "Security", "Watsan", "Women")

COMM <- abd_ind %>% subset(IDS_TYPES == "DC" & q5362_a_committee_exist1!="") #review: here I don't understand why dropping q5362_a_committee_exist1==""
COMM$LOTT_BIN <- as.factor(COMM$IDS_LOTT_BIN)

COMM$HAD_COMMITTEE <- rep(0, nrow(COMM))
for(j in 1:11){
  COMM$HAD_COMMITTEE[(COMM[paste0("q5362_a_committee_exist",j)][[1]]  %in%  committees) &
                       (COMM[paste0("q5362_c_committee_since",j)][[1]] > 5 )] <- 1
}
COMM$NOCOMMITTEE <- 1 - COMM$HAD_COMMITTEE

# IRC_TUUNGANE
# IRC_TUUNGANE <- irc_tuungane %>%
#   dplyr::select(IDV, ROBUSTNESS_TUUNGANE) %>%
#   dplyr::rename(IRC_TUUNGANE = ROBUSTNESS_TUUNGANE)
attr(irc_tuungane$IDV, "format.stata") <- NULL

# Join all village-level variables which will be merged to different data sets
robust_vars <- full_join(COMM[,c("IDV", "NOCOMMITTEE")],
                         SCHOOLS_INHERITED[,c("IDV","INHERITED","NOSCHOOLS")], by = "IDV")
assignment <- full_join(tuungane, irc_tuungane, by = "IDV")
village_add <- full_join(robust_vars, assignment, by = "IDV")

# Weights
village_add <- left_join(village_add, abd_vill[,c("IDV_RAPID","IDV","IDV_LOTT_BIN", "IDV_PROPENSITY_WEIGHT_ADJ")], by = "IDV")
village_add$VILL_WEIGHT <- village_add$IDV_PROPENSITY_WEIGHT_ADJ

# PWEIGHT2
village_add$LOTT_BIN <- village_add$IDV_LOTT_BIN
village_add$PWEIGHT2 <- ave(village_add$TUUNGANE, village_add$LOTT_BIN)
village_add$PWEIGHT2[village_add$TUUNGANE==1] <- 1/ village_add$PWEIGHT2[village_add$TUUNGANE==1]
village_add$PWEIGHT2[village_add$TUUNGANE==0] <- 1/ (1-village_add$PWEIGHT2[village_add$TUUNGANE==0])

# IPW_RAPID
IPW_RAPID_1 <- ave(village_add$TUUNGANE, village_add$LOTT_BIN, FUN = sum)
IPW_RAPID_0 <- ave(village_add$TUUNGANE, village_add$LOTT_BIN, FUN = length) - IPW_RAPID_1
village_add$IPW_RAPID <- NA
village_add$IPW_RAPID[village_add$IDV_RAPID==1] <- 1/IPW_RAPID_1[village_add$IDV_RAPID==1]
village_add$IPW_RAPID[village_add$IDV_RAPID==0] <- 1/IPW_RAPID_0[village_add$IDV_RAPID==0]

# * Individual ------------------------------------------------------------
ind <- merge(abd_ind, village_add, by="IDV")

.revalue_map <-
  c("1 Strongly agree" = 6,
    "2 Agree" = 5,
    "3 Don't know" = 4,
    "4 Indifferent" = 3,
    "5 Against" = 2,
    "6 Strongly against" = 1)

# social desirability variables
SOCDES <- ind %>%
  dplyr::mutate(q083_inconsistent_yes =
                  plyr::revalue(q083_inconsistent_yes,
                                .revalue_map) %>% as.integer,
                q083_inconsistent_not =
                  plyr::revalue(q083_inconsistent_not,
                                .revalue_map) %>% as.integer) %>%
  dplyr::filter(!is.na(q083_inconsistent_yes) | !is.na(q083_inconsistent_not)) %>%
  dplyr::mutate(daccord1 = ifelse(is.na(q083_inconsistent_yes), NA, q083_inconsistent_yes>=5),
                daccord0 = ifelse(is.na(q083_inconsistent_not), NA, q083_inconsistent_not>=5),
                v5_is_PA = ifelse(v5_contradiction=="PA" | v5_contradiction=="PB",
                                  v5_contradiction=="PA", NA),
                FIRST_ANSWER = ifelse(v5_is_PA, daccord0, ifelse(!v5_is_PA, daccord1, NA)),
                POS_PROMPT = 1 - v5_is_PA,
                NEG_PROMPT = as.integer(v5_is_PA),
                NOTUUNGANE = ifelse(is.na(TUUNGANE), NA, 1 - TUUNGANE),
                # WEIGHT = IDS_HH_SAMP_PROP_W_ANY,
                IDS_TUUNGANE_POS = TUUNGANE*POS_PROMPT,
                IDS_TUUNGANE_NEG = NOTUUNGANE*NEG_PROMPT) %>%
  dplyr::select(IDS, IDV, NOTUUNGANE, POS_PROMPT,
                NEG_PROMPT, FIRST_ANSWER, IDS_TUUNGANE_POS,
                IDS_TUUNGANE_NEG, v5_is_PA)

ind <- left_join(ind, SOCDES, by = c("IDS", "IDV"))

# * Village ---------------------------------------------------------------
vill <- full_join(abd_vill, village_add, by=intersect(names(abd_vill), names(village_add)))

# Add audit
vill <-  full_join(vill, audit, by=intersect(names(audit), names(vill)))

vill %<>% dplyr::mutate(
  # Clean Public goods present in July 2006 (from chief)
  wells = ifelse(cq023_a1_wells_2006 < 0, NA, cq023_a1_wells_2006),
  schools = ifelse(cq024_a1_schools_2006 < 0, NA, cq024_a1_schools_2006),
  clinics = ifelse(cq025_a1_cliniques_2006 < 0, NA, cq025_a1_cliniques_2006),
  churches = ifelse(cq026_a1_churches_2006 < 0, NA, cq026_a1_churches_2006),
  halls = ifelse(cq027_a1_halls_2006 < 0, NA, cq027_a1_halls_2006),
  
  # Migration into the village in 2006
  idp = ifelse(cq0136_idp_2006 < 0, NA, cq0136_idp_2006),
  idp = ifelse(cq0136_idp_2006 > 200, NA, idp),
  idpret = ifelse(cq0137_idp_returned_2006 < 0, NA, cq0137_idp_returned_2006),
  idpret = ifelse(cq0137_idp_returned_2006 > 200, NA, idpret),
  ref = ifelse(cq0138_refugie_2006 < 0, NA, cq0138_refugie_2006),
  ref = ifelse(cq0138_refugie_2006 > 200, NA, ref),                
  refret = ifelse(cq0139_refugie_repat_2006 < 0, NA, cq0139_refugie_repat_2006),
  refret = ifelse(cq0139_refugie_repat_2006 > 200, NA, refret)) %>%
  group_by(IDV) %>%
  mutate(
    public2006 = sum(wells, schools, clinics, churches, halls, na.rm = TRUE),
    mig2006 = sum(idp, idpret, ref, refret, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # How did the previous chief come to power? (CQ 48 maybe a dummy for popular enthronement options 4 and 5)
  mutate(
    former_chief_elec = case_when(
      cq048_how_former_chief == "4 Elected by inhabitants" ~ 1,
      cq048_how_former_chief == "5 Plebiscite by inhabitants" ~ 1,
      cq048_how_former_chief == "1 Elderly choose" ~ 0,
      cq048_how_former_chief == "2 King and other chiefs" ~ 0,
      cq048_how_former_chief == "3 Inherited" ~ 0,
      cq048_how_former_chief == "6 By force" ~ 0,
      cq048_how_former_chief == "7 Selected by pol or trad leadership" ~ 0),
    
    # Distance to the village mine (chief survey, CQ 22)
    dist_mine = ifelse(cq022_minutes_from_mine < 0, NA, cq022_minutes_from_mine/60) 
  )

no_infra <- with(vill, is.na(wells) & is.na(schools), is.na(clinics) & is.na(churches) & is.na(halls))
no_mig <- with(vill, is.na(idp) & is.na(idpret) & is.na(ref) & is.na(refret))

vill$public2006[no_infra] <- NA
vill$mig2006[no_mig] <- NA

# Mineral composition (chief survey, CQ 20)
recode_yes_no <- function(col){
  case_when(col %in% c(-8, 0) ~ 0,
            col == 1 ~ 1)
}

# Mineral Composition
mineral_cols <- grep("cq020_", names(vill))
MINE <- as.data.frame(apply(vill[,mineral_cols], 2, recode_yes_no)) 
names(MINE) <- gsub("cq020_", "", names(MINE))
MINE$mineral_index <- rowSums(MINE)

vill <- cbind(vill, MINE)

# Generate distance from chiefdom head town (mean of respondents in the village)
distance <- ind %>% dplyr::mutate(
  qe013_hrs = ifelse(qe013_k_chef_chefferie_hrs < 0,
                     NA, qe013_k_chef_chefferie_hrs),
  qe013_mns = ifelse(qe013_k_chef_chefferie_mns < 0,
                     NA, qe013_k_chef_chefferie_mns),
  distance = qe013_hrs + qe013_mns/60) %>%
  dplyr::group_by(IDV) %>%
  dplyr::summarize(distance = mean(distance, na.rm = TRUE))

vill <- left_join(vill, distance, by = "IDV")

# Existence comittee or association in 2006 (from chief)
# WHY NOT BASED ON CQ 23 - 27 ?

COM <-  ind  %>%
  dplyr::filter(IDS_TYPES=="DC") %>%
  dplyr::select(IDV,  starts_with("q5362_c_committee_since"))

COUNT <- 1*(COM[,-1] > 6) 
COUNT[COM[,-1] < 0] <- NA
COM$com2006 = rowSums(COUNT, na.rm = TRUE)

# com2006 needs to be "NA" if all are NAs                
COM$com2006[rowSums(!is.na(COUNT))==0] <- NA

vill <- merge(vill, COM[,c("IDV", "com2006")], by = "IDV")

# Really? but that means that if there is no  committee we say NA rather than 0. Why?
# Note weirdly this selects on places with a committee surviving; 
#hist(COM$com2006)

# * Discussion ------------------------------------------------------------
disc <- left_join(abd_disc, village_add, by = intersect(names(abd_disc), names(village_add)))


# * Roster ----------------------------------------------------------------

STATS <- roster %>%
  dplyr::filter(IDO==101, IDS_TYPES!="DC", IDS_TYPES!="DCDV") %>%
  dplyr::select(IDV, IDS, qf007_gender, qf009_birthyear) %>%
  dplyr::mutate(
    gender = qf007_gender,
    age = qf009_birthyear %>%
      ifelse(. < 1900 | . > 2012, yes = NA, no = .) %>%
      "-"(2011,.) %>%
      ifelse(. > stats::quantile(., probs = .99, na.rm=TRUE), yes = NA, no = .)) 

STATS <- left_join(STATS, village_add, by = "IDV")

# Prepare main outcomes (CAPTURE) -----------------------------------------
# * Financial Irregularities ----------------------------------------------

# clean da109_not_verifiable (Financial Irregularities)
vill$da109_not_verifiable[vill$da109_not_verifiable == 3680] <- 368
vill$da109_not_verifiable[vill$da109_not_verifiable > 1000 |
                            vill$da109_not_verifiable < 0] <- NA
vill %<>% mutate(
  # Financial Irregularities
  da109_not_verifiable = da109_not_verifiable/1000)

# * Embezzlement: DIRECT --------------------------------------------------

# adds WEIGHT

# * Embezzlement: LIST ----------------------------------------------------

# v8_sensitive_rapid: respondent received RA or RB. RA = ABCD,WXZ RB = ABD, WXYZ. C and Y are sensitive.
# RA is treatment in first list, control in second list. 
# RB is control in first list, treatment in second list 
# qr2729_list_experiment: response to AB(C)D  
# qr2830_list_experiment: response to WX(Y)Z
# qr026i_fund_misuse: direct question   

# v8_sensitive_rapid has been taken out of ABD_INDIV. Needs to be merged back in
# ABD_INDIV_LIST <- ABD_INDIV

# REVIEW: ABD_INDIV already has variable v8_sensitive_rapid, but 6 observations differ from v8 data
# ABD_INDIV_LIST <- merge(ABD_INDIV_LIST, v8, by = "IDS")
# diff <- ABD_INDIV_LIST$v8_sensitive_rapid.x!=ABD_INDIV_LIST$v8_sensitive_rapid.y
# ABD_INDIV_LIST$v8_sensitive_rapid.x[diff]
# ABD_INDIV_LIST$v8_sensitive_rapid.y[ABD_INDIV_LISTdiff]

# ind$WEIGHT <- ind$IDS_PROPENSITY_WEIGHT_ADJ #IDS_HH_SAMP_PROP_W_DML * 5 / 2

# ca 1900  in A and 1900 in B; only 3179 responses in 27/29 and 3700 (OK) responses to 2830

# Clean: asked to both DML and DMC
# 17 known errors removed here:
# 11 have "4" while they are control in List A
# 6 have "4" while they are control in List B	
errors <- ind$v8_sensitive_rapid %in% c("RB", "RA") & ind$qr2729_list_experiment==4
ind$qr2729_list_experiment[errors] <- NA

ind %<>% mutate(
  RA = case_when(v8_sensitive_rapid=="RA" ~ 1,
                 v8_sensitive_rapid=="RB" ~ 0),
  RB = 1 - RA,
  tuu_RA = TUUNGANE * RA,
  tuu_RB = TUUNGANE * RB)

# * Inequality of benefits ------------------------------------------------

#REVIEW: IDS_HH_SAMP_PROP_W_ANY = mean(IDS_HH_SAMP_PROP_W_ANY, na.rm = TRUE))
# This was just being averaged. The data is aggregated at the village level, so we should create a separate var for the values averaged by sampling weights

BENEFITS <- ind %>%
  dplyr::filter(IDS_RAPID == 1,
                IDS_TYPES == "DMC" | IDS_TYPES == "DML" | IDS_TYPES == "") %>%
  dplyr::mutate(
    qr003_bis_value = as.numeric(qr003_bis_value),
    qr003_bis_value = case_when(qr003_bis_value < 0 ~ 0,
                                qr003_bis_value > 100 ~ qr003_bis_value/900, #REVIEW: arbitrary recoding of values > 100 so max is a 100? 50000 = 56
                                qr003_bis_value <= 100 & qr003_bis_value >=0 ~ qr003_bis_value)) %>%
  group_by(IDV) %>%
  dplyr::summarize(distribution = sum(qr003_bis_value, na.rm = TRUE),
                   stdev_benefits = sd(qr003_bis_value, na.rm = TRUE),
                   stdev_benefits_samp_wt = SDMTools::wt.sd(qr003_bis_value, wt = IDS_HH_SAMP_PROP_W_ANY))

vill <- full_join(vill, BENEFITS, by = "IDV")


# * Dominance of Preferences ----------------------------------------------
# Most complete measure uses Step D to measure actual project choice

# if(local_data){
  project_recode <- function(var){
    car::recode(var, "
              1:2='watsan'; 
              c(15, 20)='education';
              c(16,21)='health';
              c(7,26,46)='transport';
              c(9, 25,27, 34, 41, 45, 19,23,8,32,13,24,11,33,10, 42)='agric';
              c(28,6,18,12) = 'private';
              c(3, 14, 22, 31,29,43,5,4, 44, 49) = 'other'")
  }
# }else{
#   project_recode <- function(var){
#     dplyr::case_when(var %in% c("wells (other water)", "irrigation") ~ 'watsan',
#                      var %in% c("distribution school material", "school/ renovation") ~ 'education',
#                      var %in% c("distribution medicines","health center / renovation") ~ 'health',
#                      var %in% c("road","market","bridge") ~ 'transport',
#                      var %in% c("goats raising", "cows raising", "poultry raising", "other raising",
#                                 "distribution corn seeds", "mill/ renovation", "distribution soil nutritions",
#                                 "distribution agriculture tools", "community field", "Pisciculture",
#                                 "distribution peanuts seeds", "distribution bean seeds", "distribution choux seeds",
#                                 "distribution rice seeds", "distribution other seeds", "riserie") ~ 'agric',
#                      var %in% c("credit system", "distribution money", "other distribution", "private project") ~ 'private',
#                      var %in% c("religious center/ renovation", "distribution building construction", 
#                                 "toilets/ renovation", "other","briques creation","electricity",
#                                 "other construction/ renovation","social center/ renovation", 
#                                 "education/training", "actions against sexual violence") ~ 'other')
#   } 
# }

CHOICE_D <- ind %>%
  select(qr001_project_choice, IDS, IDV)

CHOICE_B <- vill %>% 
  select(b_23_project_has_been_chosen, IDV)

# if(is.numeric(ind$qr001_project_choice)){
  
  CHOICE_D %<>% mutate(projetChoice_d = ifelse(qr001_project_choice<0, NA, qr001_project_choice)) %>%
    mutate(coarseChoice_d = project_recode(projetChoice_d)) 
  
  CHOICE_B %<>% mutate(projetChoice_b = ifelse(b_23_project_has_been_chosen<0, NA, b_23_project_has_been_chosen)) %>%
    mutate(coarseChoice_b = project_recode(projetChoice_b))
  
# }else{
  
#   CHOICE_D %<>%
#     mutate(projetChoice_d = ifelse(qr001_project_choice %in% c("don't know", "not applicable", "refuse to respond"), 
#                                    NA, as.character(qr001_project_choice)),
#            coarseChoice_d = project_recode(qr001_project_choice)) 
#   
#   CHOICE_B %<>%
#     mutate(projetChoice_b = ifelse(b_23_project_has_been_chosen %in% c("don't know", "not applicable", "refuse to respond"), 
#                                    NA, as.character(b_23_project_has_been_chosen)),
#            coarseChoice_b = project_recode(b_23_project_has_been_chosen))
# }


CHOICE_VILL <- aggregate(cbind(coarseChoice_d, projetChoice_d) ~ IDV, data = CHOICE_D, Mode) %>%
  merge(CHOICE_B, by = "IDV", all = TRUE)

# Check shows great consistency between steps B and D
# table(CHOICE_VILL$coarseChoice_d, CHOICE_VILL$coarseChoice_b)

# Get a measure of chief dominance using the answer from 5 people 
# answering project preference from av_14

CHIEF_DOM <- ind %>%
  dplyr::filter(IDS_TYPES == "DMC" | IDS_TYPES == "DC")

# Add in Chief's preference from ac_17 and rename pref vars
CHIEF_DOM <- merge(CHIEF_DOM, select(vill, IDV,  ac_17_project_preferences_1), by = "IDV")

# Add in actual project choice
CHIEF_DOM <- merge(CHIEF_DOM, CHOICE_VILL, by = "IDV")

CHIEF_DOM %<>% mutate(
  projetPref_chief = as.numeric(ac_17_project_preferences_1),
  coarsePref_chief = project_recode(projetPref_chief),
  projetPref_pop   = as.numeric(av_14_bis_project_1),
  coarsePref_pop   = project_recode(projetPref_pop),
  # Generate correct measure for population
  Correct_B_coarse = (coarsePref_pop == coarseChoice_b),
  Correct_D_coarse = (coarsePref_pop == coarseChoice_d),
  Correct_B_projet = (projetPref_pop == projetChoice_b),
  Correct_D_projet = (projetPref_pop == projetChoice_d),
  CHIEF            = (IDS_TYPES == "DC")
)

# Sub in correct measure for chief
CHIEF_DOM <- within(CHIEF_DOM, {
  Correct_B_coarse[CHIEF] <- (coarsePref_chief == coarseChoice_b)[CHIEF]
  Correct_D_coarse[CHIEF] <- (coarsePref_chief == coarseChoice_d)[CHIEF]
  Correct_B_projet[CHIEF] <- (projetPref_chief == projetChoice_b)[CHIEF]
  Correct_D_projet[CHIEF] <- (projetPref_chief == projetChoice_d)[CHIEF]
})

# Merge with individual data
ind <- left_join(ind,
                 select(CHIEF_DOM, starts_with("Correct"), CHIEF, IDV, IDS),
                 by = c("IDV", "IDS"))

# Prepare mechanism (PARTICIPATION) ---------------------------------------
# * Meeting attendance ----------------------------------------------------
# Remove entry error
vill$am_16_women_end[vill$am_16_women_end == 2023] <- NA
# Add women and men participation
vill$PART_A1 <- with(vill, am_16_women_end + am_17_men_end)


# * Interventions and Dominance of men in discussion ----------------------
## Drop observations for which ad1_2_individual_id != 2. There are also 20 NAs
AB_DISC <- dplyr::filter(disc, ad1_2_individual_id != 2 | is.na(ad1_2_individual_id))  #6587 observations; 6607 if include NAs

AB_DISC %<>% 
  group_by(IDV) %>%
  summarize(N_INTERV = sum(ad1_2_individual_id != 1, na.rm = TRUE),
            N_CHIEF = sum(ad1_2_individual_id == 1 & ad1_3_gender == 1),
            N_INTERVM = sum(ad1_3_gender == 1 & ad1_2_individual_id != 1, na.rm = TRUE),
            MALE_DOM = 100* (N_INTERVM / (N_CHIEF + N_INTERV)))


#merge with village data
vill <- left_join(vill, AB_DISC, by = "IDV")

# * Participatory selection methods ---------------------------------------

vill %<>% mutate(
  ELECTIONS1          = ifelse((b_32_elections_comittee_menFG3 > 0 &  !is.na(b_32_elections_comittee_menFG3)) | 
                                 (b_32_elections_comittee_womenFG3 > 0 &  !is.na(b_32_elections_comittee_womenFG3)), 1, 0),
  ELECTIONS1          = ELECTIONS1 * 100,
  ELECTIONS_LOTT1     = ifelse((b_32_elections_comittee_menFG3 > 0 & !is.na(b_32_elections_comittee_menFG3))| 
                                 (b_32_elections_comittee_womenFG3 > 0 &  !is.na(b_32_elections_comittee_womenFG3)) | 
                                 (b_32_lottery_comittee_menFG3 > 0 &  !is.na(b_32_lottery_comittee_menFG3)) | 
                                 (b_32_lottery_comittee_womenFG3 > 0 & !is.na(b_32_lottery_comittee_womenFG3)), 1, 0),
  ELECTIONS_LOTT_CON1 = ifelse(ELECTIONS_LOTT1==1 | 
                                 (b_32_consensus_comittee_womenFG3 > 0 | b_32_consensus_comittee_menFG3 > 0), 1, 0),
  ELECTIONS_LOTT_CON1 = ELECTIONS_LOTT_CON1 * 100,
  ELECTIONS2          =  ifelse((b_33_election_project_menFG3 > 0 &  !is.na(b_33_election_project_menFG3)) | 
                                  (b_33_election_project_womenFG3 > 0 &  !is.na(b_33_election_project_womenFG3)),
                                1, 0),
  ELECTIONS2          = ELECTIONS2 * 100,
  ELECTIONS_LOTT2     = ifelse((b_33_election_project_menFG3 > 0 &  !is.na(b_33_election_project_menFG3)) | 
                                 (b_33_election_project_womenFG3 > 0 &  !is.na(b_33_election_project_womenFG3)) | 
                                 (b_33_lottery_project_menFG3 > 0 &  !is.na(b_33_lottery_project_menFG3)) | 
                                 (b_33_lottery_project_womenFG3 > 0 & b_33_lottery_project_womenFG3!=0), 1, 0),
  ELECTIONS_LOTT_CON2 = ifelse(ELECTIONS_LOTT2==1 | (b_33_consensus_project_menFG3 > 0 | b_33_consensus_project_womenFG3 > 0), 1, 0),
  ELECTIONS_LOTT_CON2 = ELECTIONS_LOTT_CON2 * 100
)

# Create index

SEL <- vill %>% 
  dplyr::filter(!is.na(b_32_elections_comittee_menFG3)
                & !is.na(b_32_elections_comittee_womenFG3)
                & !is.na(b_32_consensus_comittee_womenFG3)
                & !is.na(b_32_consensus_comittee_menFG3)
                & !is.na(b_33_election_project_menFG3)
                & !is.na(b_33_consensus_project_menFG3)
                & !is.na(b_33_consensus_project_womenFG3)
                & !is.na(b_33_election_project_womenFG3))

# REVIEW: WEIGHT
SEL$MFI_SELECTION <-
  wmeaneffects(.data = as.data.frame(SEL),
               .treat = "TUUNGANE",
               .weight = "VILL_WEIGHT",
               .outcomes = c("ELECTIONS1",
                             "ELECTIONS2",
                             "ELECTIONS_LOTT_CON1",
                             "ELECTIONS_LOTT_CON2"),
               .cond = "!is.na(TUUNGANE)",
               .varname = "MFI_SELECTION") %>% as.vector

vill <- left_join(vill, SEL[,c("IDV","MFI_SELECTION")], by = "IDV")

# * Committee composition -------------------------------------------------

COMPOS <- tidyr::gather(data = vill,
                        key = REP,
                        value = b13_rep_gender,
                        contains("b13_rep_gender"))
COMPOS %<>% filter(!is.na(b13_rep_gender))

COMPOS %<>%
  group_by(IDV) %>%
  dplyr::summarize(N_FEM = sum((b13_rep_gender == 0), na.rm = TRUE),
                   N_MAL = sum((b13_rep_gender == 1), na.rm = TRUE),
                   TOT = n(),
                   SHARE = N_FEM/TOT)


# Merge with vill data
vill <- left_join(vill, COMPOS, by = "IDV")

# Create index
vill$MFI_COMPOSITION <-
  wmeaneffects(.data = as.data.frame(vill),
               .treat = "TUUNGANE",
               .weight = "VILL_WEIGHT",
               .outcomes = c("N_FEM",
                             "N_MAL",
                             "TOT",
                             "SHARE"),
               .cond = "!is.na(TUUNGANE)",
               .varname = "MFI_COMPOSITION") %>% as.vector

# Prepare mechanism (ACCOUNTABILITY) --------------------------------------
# * Accountability Mechanisms ---------------------------------------------

D_AUDIT_MECHS <- vill %>%
  group_by(IDV, VILL_WEIGHT, TUUNGANE) %>%
  summarize(
    DA_MECHANISMS_EXT = ifelse(da019_a_committee_men > 0 | da019_a_committee_women > 0, 1, 0),
    DA_MECHANISMS_EXT = ifelse(is.na(da019_a_committee_men)  & is.na(da019_a_committee_women), NA, DA_MECHANISMS_EXT),
    DA_MECHANISMS_COMMIT = ifelse(da019_b_rapid_men > 0 | da019_b_rapid_women > 0, 1, 0),
    DA_MECHANISMS_COMMIT = ifelse(is.na(da019_b_rapid_men) & is.na(da019_b_rapid_women), NA, DA_MECHANISMS_COMMIT),
    DA_MECHANISMS_COMMUN = ifelse(da019_c_community_men > 0 | da019_c_community_women > 0, 1, 0),
    DA_MECHANISMS_COMMUN = ifelse(is.na(da019_c_community_men)  & is.na(da019_c_community_women), NA, DA_MECHANISMS_COMMUN),
    DA_NO_MECHANISM = ifelse(da019_d_nothing_men > 0 | da019_d_nothing_women > 0, 1, 0),
    DA_NO_MECHANISM = ifelse(is.na(da019_d_nothing_men)  & is.na(da019_d_nothing_women), NA, DA_NO_MECHANISM),
    DA_MECHANISMS_EXT = DA_MECHANISMS_EXT * 100,
    DA_MECHANISMS_COMMUN = DA_MECHANISMS_COMMUN * 100,
    DA_NO_MECHANISM = DA_NO_MECHANISM * 100)

ABD_VILL_LONG <- vill %>%
  dplyr::select(IDV,
                IDV_PROPENSITY_WEIGHT_ADJ,
                TUUNGANE,
                IDV_CDCCODE,
                starts_with("dr31_")) %>%
  as.data.frame() %>%
  reshape(direction = "long",
          varying = list(c(5,11,17),
                         c(6,12,18),
                         c(7,13,19),
                         c(8,14,20),
                         c(9,15,21),
                         c(10,16,22)),
          v.names = c("dr31_a_Rep",
                      "dr31_b_rapid_Rep",
                      "dr31_c_comm_Rep",
                      "dr31_d_aucun_Rep",
                      "dr31_e_autre_Rep",
                      "dr31_f_nsp_Rep"))

ABD_VILL_LONG %<>% dplyr::filter(!is.na(dr31_a_Rep) |
                                   !is.na(dr31_b_rapid_Rep) |
                                   !is.na(dr31_c_comm_Rep) |
                                   !is.na(dr31_d_aucun_Rep) |
                                   !is.na(dr31_e_autre_Rep) |
                                   !is.na(dr31_f_nsp_Rep))

ABD_VILL_LONG[,c("dr31_a_Rep",
                 "dr31_b_rapid_Rep",
                 "dr31_c_comm_Rep",
                 "dr31_d_aucun_Rep",
                 "dr31_e_autre_Rep",
                 "dr31_f_nsp_Rep")] %<>%
  apply(2, function(x) ifelse(x == -9 | x == -8 | x == 7, NA, x))

ABD_VILL_LONG %<>%
  dplyr::mutate(DR_MECHANISMS_EXT = dr31_a_Rep * 100,
                DR_SUM_MECHANISMS = dr31_a_Rep + dr31_c_comm_Rep + dr31_e_autre_Rep,
                DR_MECHANISMS_COMM = dr31_c_comm_Rep * 100,
                DR_NO_MECHANISM = dr31_d_aucun_Rep,
                DR_REALLYNO_MECHANISM  = ifelse(dr31_b_rapid_Rep == 1, 100, dr31_d_aucun_Rep * 100))

ABD_VILL_LONG %<>%
  group_by(IDV) %>%
  summarize(DR_MECHANISMS_EXT = mean(DR_MECHANISMS_EXT, na.rm = TRUE),
            DR_MECHANISMS_COMM = mean(DR_MECHANISMS_COMM, na.rm = TRUE),
            DR_REALLYNO_MECHANISM = mean(DR_REALLYNO_MECHANISM, na.rm = TRUE))

### From M Rapid
ABD_INDIV_TEMP <- ind %>%
  mutate(qr015_NONE = ifelse(qr015_committee == 1, 100, qr015_none*100),
         qr015_external_committee = qr015_external_committee * 100,
         qr015_community = qr015_community * 100) %>%
  group_by(IDV) %>%
  summarize(qr015_external_committee = mean(qr015_external_committee, na.rm = TRUE),
            qr015_community = mean(qr015_community, na.rm = TRUE),
            qr015_NONE = mean(qr015_NONE, na.rm = TRUE))

# merge all intermediary measures
ABD_MERGE <- left_join(ABD_INDIV_TEMP, ABD_VILL_LONG, by = "IDV") %>%
  left_join(., D_AUDIT_MECHS, by = "IDV") %>%
  mutate(nDA_NO_MECHANISM =-DA_NO_MECHANISM,
         nDR_REALLYNO_MECHANISM =-DR_REALLYNO_MECHANISM,
         nqr015_NONE =-qr015_NONE) 

# Create index
ABD_MERGE$MFI_MECHANISMS <- wmeaneffects(.data = as.data.frame(ABD_MERGE),
                                         .treat = "TUUNGANE",
                                         .weight = "VILL_WEIGHT",
                                         .outcomes = c("DA_MECHANISMS_EXT",
                                                       "DA_MECHANISMS_COMMUN",
                                                       "nDA_NO_MECHANISM",
                                                       "DR_MECHANISMS_EXT",
                                                       "DR_MECHANISMS_COMM",
                                                       "nDR_REALLYNO_MECHANISM",
                                                       "qr015_external_committee",
                                                       "qr015_community",
                                                       "nqr015_NONE"),
                                         .cond = "!is.na(TUUNGANE)",
                                         .varname = "MFI_MECHANISMS") %>%
  as.vector

# merge with village data
vill <- left_join(vill, ABD_MERGE[,c("IDV", "MFI_MECHANISMS")], by = "IDV")

# * Private Complaints ----------------------------------------------------

ind$MFI_COMPLAINTS <- wmeaneffects(.data = as.data.frame(ind),
                                   .treat = "TUUNGANE",
                                   .weight = "VILL_WEIGHT",
                                   .outcomes = c("qr026a_length",
                                                 "qr026b_rapid_behavior",
                                                 "qr026c_unimportant",
                                                 "qr026d_reduced_help",
                                                 "qr026e_no_influence",
                                                 "qr026f_disagreement",
                                                 "qr026g_steps",
                                                 "qr026h_lack_info",
                                                 "qr026i_fund_misuse",
                                                 "qr026j_allocation",
                                                 "qr026k_conflicts",
                                                 "qr026l_controled_chef",
                                                 "qr026m_unrepresented"),
                                   .cond = "!is.na(TUUNGANE)",
                                   .varname = "MFI_COMPLAINTS") %>% as.vector

# Prepare mechanism (TRANSPARENCY) ----------------------------------------
# * Knowledge of project amount -------------------------------------------

ABD_INDIV_KNOWS <- ind %>%
  dplyr::filter(!(IDS_TYPES == "DC" | IDS_TYPES == "DCDV") & ## Deletes 2,240 obs
                  !is.na(qr002_project_amount) & !(IDS_RAPID == 0) )

ABD_INDIV_KNOWS %<>%
  dplyr::mutate(
    qr002_project_amount = as.numeric(qr002_project_amount),
    qr002_project_amount = case_when(qr002_project_amount %in% c(9000, 900000, -900) ~ 900,
                                     qr002_project_amount %in% c(1000000, 10000) ~ 1000,
                                     qr002_project_amount == 9 ~ -9,
                                     qr002_project_amount >= 0 & qr002_project_amount < 9000 ~ qr002_project_amount),
    qr002CORRECT = ifelse(qr002_project_amount == 1000, 100, 0),
    qr002CORRECT = ifelse(is.na(qr002CORRECT), 0, qr002CORRECT), #REVIEW: assuming missing same as getting ammount wrong?
    DISTANCE_FROM_1000 = abs(qr002_project_amount - 1000)) %>%
  select(IDS, IDV, qr002_project_amount, qr002CORRECT, DISTANCE_FROM_1000)

# merge with individual data
ind <- left_join(ind, ABD_INDIV_KNOWS, by = c("IDS", "IDV"))

# * Willingness to seek information ---------------------------------------

ind %<>% mutate(
  qi003_accept = ifelse(IDS %in% c(113726, 224816, 417216, 430816, 434128), NA, qi003_accept),
  qi003_accept = qi003_accept*100)

# * Quality of accounting -------------------------------------------------

vill[,c("da027_got_accounting_form",
        "da028_contrib",
        "da031_money_available_total",
        "da032_total_amount",
        "da033_justified",
        "da034_credibly_justified")] %<>% apply(2, function(x) ifelse(x %in% c(-9, -8, 7), NA, x))

#REVIEW: Why is it bounding measures arbitrarily and multiplying by 100, dividing by 1000 if it's a normalized index?

vill %<>% dplyr::mutate(
  ACCOUNTING_FORM = da027_got_accounting_form,
  ACCOUNTING_FORM = ACCOUNTING_FORM * 100,
  
  da028_contrib = ifelse(da028_contrib > 10000, da028_contrib/900, da028_contrib), #REVIEW: again, arbitrarily rescaling only partial data
  da028_contrib = ifelse(da028_contrib < 0, NA, da028_contrib),
  
  da031_money_available_total = ifelse(!is.na(da028_contrib) & da028_contrib > 0,
                                       da031_money_available_total - da028_contrib, da031_money_available_total),
  
  ACCOUNTED_EVAL = (da031_money_available_total)/1000,
  ACCOUNTED_EVAL = ifelse(ACCOUNTED_EVAL < 0 & !is.na(ACCOUNTED_EVAL), 0, ACCOUNTED_EVAL),
  ACCOUNTED_EVAL = ifelse(ACCOUNTED_EVAL > 1 & !is.na(ACCOUNTED_EVAL), 1, ACCOUNTED_EVAL),
  ACCOUNTED_EVAL = ACCOUNTED_EVAL * 100,
  
  da032_total_amount = ifelse(!is.na(da028_contrib) & da028_contrib > 0,
                              da032_total_amount - da028_contrib, da032_total_amount),
  
  ACCOUNTED_COMM = da032_total_amount/1000,
  ACCOUNTED_COMM = ifelse(ACCOUNTED_COMM<0 & !is.na(ACCOUNTED_COMM), 0, ACCOUNTED_COMM),
  ACCOUNTED_COMM = ifelse(test = ACCOUNTED_COMM > 1 & !is.na(ACCOUNTED_COMM), 1, ACCOUNTED_COMM),
  ACCOUNTED_COMM = ACCOUNTED_COMM * 100,
  
  PART_JUSTIFIED = da033_justified/1000,
  PART_CREDIBLY_JUSTIFIED = da034_credibly_justified/1000,
  PART_CREDIBLY_JUSTIFIED = PART_CREDIBLY_JUSTIFIED * 100)

vill$MFI_ACCOUNTING <-
  wmeaneffects(.data = as.data.frame(vill),
               .treat = "TUUNGANE",
               .weight = "VILL_WEIGHT",
               .outcomes = c("ACCOUNTING_FORM",
                             "ACCOUNTED_EVAL",
                             "ACCOUNTED_COMM",
                             "PART_JUSTIFIED",
                             "PART_CREDIBLY_JUSTIFIED"),
               .cond = "!is.na(TUUNGANE)",
               .varname = "MFI_ACCOUNTING") %>% as.vector
