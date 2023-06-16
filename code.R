health <- haven::read_dta("health_status_and_functioning.dta")
family <- haven::read_dta("family_information.dta")
background <- haven::read_dta("demographic_background.dta")
blood <- haven::read_dta("blood_test.dta")
biomarker <- haven::read_dta("biomarker.dta")

background_label <- as.data.frame(Hmisc::label(background))
colnames(background_label) <- "meaning"
biomarker_label <- as.data.frame(Hmisc::label(biomarker))
colnames(biomarker_label) <- "meaning"
blood_label <- as.data.frame(Hmisc::label(blood))
colnames(blood_label) <- "meaning"
family_label <- as.data.frame(Hmisc::label(family))
colnames(family_label) <- "meaning"
health_label <- as.data.frame(Hmisc::label(health))
colnames(health_label) <- "meaning"
# background$rgender: gender (1-male, 2-female)
# health$da026_2: the age when you began the menarche
# health$da028_2: the age started menopause
# biomarker$qa003, qa007, qa011: systolic pressures
# biomarker$qa004, qa008, qa012: diastolic pressures
# health$da011s1: take chinese traditional medicine for hypertension
# health$da011s2: take western modern medicine for hypertension
# health$da011s3: none of the above for hypertension
# background$ba002_1: birth year
# background$ba004: age
# background$bd001: highest level of education attained (1-no formal education / illiterate, 2-did not finish primary school but capable of reading or writing, 3-sishu / home school, 4-elementary school, 5-middle school, 6-high school, 7-vocational school, 8-two/three-year college / associate degree, 9-four-year college / bachelor’s degree, 10-post-graduate / master’s degree, 11-post-graduate / doctoral degree)
# background$be001: marital status (1-married with spouse present, 2-married but not living with spouse temporarily, 3-separated, 4-divorced, 5-widowed, 6-never married)
# background$bb006: where did you mainly live before 16 (1-village, 2-city/town)
# health$da059: smoke or not (1-yes, 2-no)
# health$da061: still smoking or not (1-still smoking, 2-not)
# health$da067: did you drink any alcoholic beverages last year (1-drink more than once a month, 2-drink but less than once a month, 3-none of these)
# health$da069: did you drink alcoholic beverages ever (1-i never had a drink, 2-i used to drink less than once a month, 3-i used to drink more than once a month)
# biomarker$qi002: height
# biomarker$ql002: weight
# family$cb001: how many biological children
# blood$newglu: glucose
# blood$newhba1c: glycated hemoglobin
# blood$newldl: LDL cholesterol
# blood$newhdl: HDL cholesterol
# blood$newtg: triglycerides

woman_id_menarche <- health$ID[is.na(health$da026_2) == FALSE & health$da026_2 > 0 & health$da026_2 < 100]
woman_id_menopause <- health$ID[is.na(health$da028_2) == FALSE & health$da028_2 < 100]
woman_id_excluded <- health$ID[is.na(health$da026_2) == FALSE & is.na(health$da028_2) == FALSE & (health$da026_2 <= 0 | health$da028_2 >= 100 | health$da028_2 < health$da026_2)]
woman_id_menarche_or_menopause <- sort(setdiff(union(woman_id_menarche, woman_id_menopause), woman_id_excluded))

background_tp <- background[background$ID %in% woman_id_menarche_or_menopause, ]
background_tp <- background_tp[order(background_tp$ID), ]
biomarker_tp <- biomarker[biomarker$ID %in% woman_id_menarche_or_menopause, ]
biomarker_tp <- biomarker_tp[order(biomarker_tp$ID), ]
blood_tp <- blood[blood$ID %in% woman_id_menarche_or_menopause, ]
blood_tp <- blood_tp[order(blood_tp$ID), ]
family_tp <- family[family$ID %in% woman_id_menarche_or_menopause, ]
family_tp <- family_tp[order(family_tp$ID), ]
health_tp <- health[health$ID %in% woman_id_menarche_or_menopause, ]
health_tp <- health_tp[order(health_tp$ID), ]

df <- data.frame(ID = woman_id_menarche_or_menopause, 
                 SP = rep(NA, length(woman_id_menarche_or_menopause)), 
                 DP = rep(NA, length(woman_id_menarche_or_menopause)), 
                 HTN = rep(NA, length(woman_id_menarche_or_menopause)), 
                 age_at_menarche = rep(NA, length(woman_id_menarche_or_menopause)), 
                 age_at_menopause = rep(NA, length(woman_id_menarche_or_menopause)), 
                 reproducive_year = rep(NA, length(woman_id_menarche_or_menopause)), 
                 age = rep(NA, length(woman_id_menarche_or_menopause)), 
                 education = rep(NA, length(woman_id_menarche_or_menopause)), 
                 marital_status = rep(NA, length(woman_id_menarche_or_menopause)), 
                 residence = rep(NA, length(woman_id_menarche_or_menopause)), 
                 smoking = rep(NA, length(woman_id_menarche_or_menopause)), 
                 drinking = rep(NA, length(woman_id_menarche_or_menopause)), 
                 antihypertensive_use = rep(NA, length(woman_id_menarche_or_menopause)), 
                 BMI = rep(NA, length(woman_id_menarche_or_menopause)), 
                 child_number = rep(NA, length(woman_id_menarche_or_menopause)), 
                 GLU = rep(NA, length(woman_id_menarche_or_menopause)), 
                 HbA1c = rep(NA, length(woman_id_menarche_or_menopause)), 
                 LDL_C = rep(NA, length(woman_id_menarche_or_menopause)), 
                 HDL_C = rep(NA, length(woman_id_menarche_or_menopause)), 
                 TG = rep(NA, length(woman_id_menarche_or_menopause)))

biomarker_tp$SP <- (biomarker_tp$qa003 + biomarker_tp$qa007 + biomarker_tp$qa011) / 3
df$SP[df$ID %in% biomarker_tp$ID] <- biomarker_tp$SP

biomarker_tp$DP <- (biomarker_tp$qa004 + biomarker_tp$qa008 + biomarker_tp$qa012) / 3
df$DP[df$ID %in% biomarker_tp$ID] <- biomarker_tp$DP

health_tp$antihypertensive_use <- rep(NA, nrow(health_tp))
health_tp$antihypertensive_use[is.na(health_tp$da011s3) == TRUE] <- "Yes"
health_tp$antihypertensive_use[is.na(health_tp$da011s3) == FALSE] <- "No"
df$antihypertensive_use[df$ID %in% health_tp$ID] <- health_tp$antihypertensive_use
df$antihypertensive_use <- as.factor(df$antihypertensive_use)

df$HTN[df$SP >= 140 | df$DP >= 90 | df$antihypertensive_use == "Yes"] <- "Yes"
df$HTN[df$SP < 140 & df$DP < 90 & df$antihypertensive_use == "No"] <- "No"
df$HTN <- as.factor(df$HTN)

df$age_at_menarche[df$ID %in% health_tp$ID] <- health_tp$da026_2
df$age_at_menopause[df$ID %in% health_tp$ID] <- health_tp$da028_2
df$reproducive_year <- df$age_at_menopause - df$age_at_menarche

background_tp$age <- rep(NA, nrow(background_tp))
background_tp$age[is.na(background_tp$ba002_1) == FALSE] <- 2011 - background_tp$ba002_1[is.na(background_tp$ba002_1) == FALSE]
background_tp$age[is.na(background_tp$ba002_1) == TRUE] <- background_tp$ba004[is.na(background_tp$ba002_1) == TRUE]
df$age[df$ID %in% background_tp$ID] <- background_tp$age

background_tp$education <- rep(NA, nrow(background_tp))
background_tp$education[background_tp$bd001 == 1] <- "illiterate"
background_tp$education[background_tp$bd001 == 2 | background_tp$bd001 == 3] <- "less than elementary school"
background_tp$education[background_tp$bd001 == 4] <- "elementary school"
background_tp$education[background_tp$bd001 == 5] <- "middle school"
background_tp$education[background_tp$bd001 == 6] <- "high school"
background_tp$education[background_tp$bd001 >= 7] <- "above vocational school"
df$education[df$ID %in% background_tp$ID] <- background_tp$education
df$education <- as.factor(df$education)

background_tp$marital_status <- rep(NA, nrow(background_tp))
background_tp$marital_status[background_tp$be001 == 1] <- "married and living with spouse"
background_tp$marital_status[background_tp$be001 > 1] <- "others"
df$marital_status[df$ID %in% background_tp$ID] <- background_tp$marital_status
df$marital_status <- as.factor(df$marital_status)

background_tp$residence <- rep(NA, nrow(background_tp))
background_tp$residence[background_tp$bb006 == 1] <- "rural"
background_tp$residence[background_tp$bb006 == 2] <- "urban"
df$residence[df$ID %in% background_tp$ID] <- background_tp$residence
df$residence <- as.factor(df$residence)

health_tp$smoking <- rep(NA, nrow(health_tp))
health_tp$smoking[health_tp$da059 == 1 & health_tp$da061 == 1] <- "current smoker"
health_tp$smoking[health_tp$da059 == 1 & health_tp$da061 == 2] <- "former smoker"
health_tp$smoking[health_tp$da059 == 2 & (health_tp$da061 == 2 | is.na(health_tp$da061) == TRUE)] <- "never smoker"
df$smoking[df$ID %in% health_tp$ID] <- health_tp$smoking
df$smoking <- as.factor(df$smoking)

health_tp$drinking <- rep(NA, nrow(health_tp))
health_tp$drinking[(health_tp$da069 == 2 | health_tp$da069 == 3) & (health_tp$da067 == 1 | health_tp$da067 == 2)] <- "current drinker"
health_tp$drinking[(health_tp$da069 == 2 | health_tp$da069 == 3) & health_tp$da067 == 3] <- "former drinker"
health_tp$drinking[health_tp$da069 == 1 & health_tp$da067 == 3] <- "never drinker"
df$drinking[df$ID %in% health_tp$ID] <- health_tp$drinking
df$drinking <- as.factor(df$drinking)

biomarker_tp$BMI <- biomarker_tp$ql002 / (biomarker_tp$qi002 / 100) ^ 2
df$BMI[df$ID %in% biomarker_tp$ID] <- biomarker_tp$BMI

family_tp$child_number <- family_tp$cb001
df$child_number[df$ID %in% family_tp$ID] <- family_tp$child_number

blood_tp$GLU <- blood_tp$newglu
df$GLU[df$ID %in% blood_tp$ID] <- blood_tp$GLU

blood_tp$HbA1c <- blood_tp$newhba1c
df$HbA1c[df$ID %in% blood_tp$ID] <- blood_tp$HbA1c

blood_tp$LDL_C <- blood_tp$newldl
df$LDL_C[df$ID %in% blood_tp$ID] <- blood_tp$LDL_C

blood_tp$HDL_C <- blood_tp$newhdl
df$HDL_C[df$ID %in% blood_tp$ID] <- blood_tp$HDL_C

blood_tp$TG <- blood_tp$newtg
df$TG[df$ID %in% blood_tp$ID] <- blood_tp$TG

# all women
# model 1
# SP ~ age of menarche + age + education + marital status + residence + smoking + drinking + antihypertensive use
lm_fit_1111 <- lm(SP ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + antihypertensive_use, data = df)
summary(lm_fit_1111)
# DP ~ age of menarche + age + education + marital status + residence + smoking + drinking + antihypertensive use
lm_fit_1121 <- lm(DP ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + antihypertensive_use, data = df)
summary(lm_fit_1121)
# HTN ~ age of menarche + age + education + marital status + residence + smoking + drinking
lm_fit_1131 <- glm(HTN ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking, family = "binomial", data = df)
summary(lm_fit_1131)
exp(coef(lm_fit_1131)) # OR
exp(confint(lm_fit_1131)) # 95% CI
# model 2
# SP ~ age of menarche + age + education + marital status + residence + smoking + drinking + antihypertensive use + BMI
lm_fit_1211 <- lm(SP ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + antihypertensive_use + BMI, data = df)
summary(lm_fit_1211)
# DP ~ age of menarche + age + education + marital status + residence + smoking + drinking + antihypertensive use + BMI
lm_fit_1221 <- lm(DP ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + antihypertensive_use + BMI, data = df)
summary(lm_fit_1221)
# HTN ~ age of menarche + age + education + marital status + residence + smoking + drinking + BMI
lm_fit_1221 <- glm(HTN ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + BMI, family = "binomial", data = df)
summary(lm_fit_1221)
exp(coef(lm_fit_1221)) # OR
exp(confint(lm_fit_1221)) # 95% CI
# model 3
# SP ~ age of menarche + age + education + marital status + residence + smoking + drinking + antihypertensive use + BMI + child number + GLU + HbA1c + LDL-C + HDL-C + TG
lm_fit_1311 <- lm(SP ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + antihypertensive_use + BMI + child_number + GLU + HbA1c + LDL_C + HDL_C + TG, data = df)
summary(lm_fit_1311)
# DP ~ age of menarche + age + education + marital status + residence + smoking + drinking + antihypertensive use + BMI + child number + GLU + HbA1c + LDL-C + HDL-C + TG
lm_fit_1321 <- lm(DP ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + antihypertensive_use + BMI + child_number + GLU + HbA1c + LDL_C + HDL_C + TG, data = df)
summary(lm_fit_1321)
# HTN ~ age of menarche + age + education + marital status + residence + smoking + drinking + BMI + child number + GLU + HbA1c + LDL-C + HDL-C + TG
lm_fit_1331 <- glm(HTN ~ age_at_menarche + age + education + marital_status + residence + smoking + drinking + BMI + child_number + GLU + HbA1c + LDL_C + HDL_C + TG, family = "binomial", data = df)
summary(lm_fit_1331)
exp(coef(lm_fit_1331)) # OR
exp(confint(lm_fit_1331)) # 95% CI


