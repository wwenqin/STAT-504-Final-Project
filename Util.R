
baseDir <- 'C:/users/matthew/desktop'

cwFile  <- file.path(baseDir, 'Data/MSA/CountyCrossWalk_Zillow.csv')
mspFile <- file.path(baseDir, 'Data/MSA/Metro_MedianSoldPricePerSqft_AllHomes.csv')
mrFile  <- file.path(baseDir, 'Data/MSA/Metro_Zri_AllHomesPlusMultifamily.csv')
popFile14 <- file.path(baseDir, 'Data/MSA/ACS_14_Population.csv')
popFile10 <- file.path(baseDir, 'Data/MSA/ACS_10_Population.csv')
invtFile <- file.path(baseDir, 'Data/MSA/InventoryMeasure_SSA_Metro_Public.csv')
salFile10 <- file.path(baseDir, 'Data/MSA/Salary_2010.csv')co
salFile14 <- file.path(baseDir, 'Data/MSA/Salary_2014.csv')
unemployFile14 <- file.path(baseDir, 'Data/MSA/2014_Unemployment.csv')
unemployFile10 <- file.path(baseDir, 'Data/MSA/2010_Unemployment.csv')

cw <- read.csv(cwFile)
cw <- unique(subset(cw, select=c('MetroName_Zillow', 'CBSACode')))
colnames(cw) <- c('RegionName', 'CBSACode')
cw <- cw[cw$RegionName != 'NULL',]
cw$Region <- sapply(as.character(cw$RegionName), function(m) {
  st <- substr(m, nchar(m)-1, nchar(m))
  ifelse(st %in% c('WA', 'OR', 'CA', 'NV', 'ID', 'UT', 'AZ', 'NM', 'CO', 'WY', 'MT', 'HI', 'AK'), 'West',
  ifelse(st %in% c('ME', 'NH', 'VT', 'MA', 'CT', 'RI', 'NJ', 'PA', 'NY'), 'NorthEast',
  ifelse(st %in% c('ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO', 'WI', 'IL', 'IN', 'MI', 'OH'), 'MidWest',
  ifelse(st %in% c('TX', 'OK', 'AR', 'LA', 'MS', 'AL', 'GA', 'FL', 'SC', 'NC', 'TN', 'KY', 'WV', 'VA', 'DC', 'MD', 'DE'), 'South', ''))))
})

# PctChangeIn_MedainSalePrice/SqFt ~ PctChangeSalary2010-Current + PctChangePopulation2010-current + ppChangeInUnemployment2010-current +
#                                     PctChangeInventory2010-Current + PctChangeRentalprice2010-current



# Median Sale Price/sqft- Response
# Look at pct change 2007 (peak) to Septempber 2015 (to get more data)
msp <- read.csv(mspFile)
msp <- subset(msp, select=c('RegionName', 'X2010.01', 'X2014.12'))
msp <- merge(msp, cw, by = 'RegionName')
colnames(msp) <- c('RegionName', 'Jan2010MSP', 'Dec2014MSP', 'CBSACode')

msp$PctChngMSP <- (msp$Dec2014MSP - msp$Jan2010MSP)/msp$Dec2014MSP

msp <- subset(msp, select=c('CBSACode', 'PctChngMSP'))


# Population
pop14 <- read.csv(popFile14, skip = 1)
moeCols <- which(grepl('Margin.of.Error', colnames(pop14)))
femaleCols <- which(grepl('Female', colnames(pop14)))
maleCols <- which(grepl('Male', colnames(pop14)))
pop14 <- pop14[, setdiff(1:ncol(pop14), c(moeCols, femaleCols, maleCols))]
pop14 <- pop14[, c(2, 4, 12, 14, 16, 18, 20, 22, 24, 26, 28)]

colnames(pop14) <- c('CBSACode', paste0(c('PopulationTotal',
                   'Population20to24', 'Population25to34',
                   'Population35to44', 'Population45to54',
                   'Population55to59', 'Population60to64',
                   'Population65to74', 'Population75to84',
                   'Population85Over'), 2014))
pop14$Young2014 <- pop14$Population20to242014 + pop14$Population25to342014
pop14$Mid2014   <- pop14$Population35to442014 + pop14$Population45to542014
pop14$Old2014   <- pop14$Population55to592014 + pop14$Population60to642014 + pop14$Population65to742014 + pop14$Population75to842014 + pop14$Population85Over2014



pop10 <- read.csv(popFile10, skip = 1)
moeCols <- which(grepl('Margin.of.Error', colnames(pop10)))
femaleCols <- which(grepl('Female', colnames(pop10)))
maleCols <- which(grepl('Male', colnames(pop10)))
pop10 <- pop10[, setdiff(1:ncol(pop10), c(moeCols, femaleCols, maleCols))]
pop10 <- pop10[, c(2, 4, 14, 16, 18, 20, 22, 24, 26, 28, 30)]

colnames(pop10) <- c('CBSACode', paste0(c('PopulationTotal',
                     'Population20to24', 'Population25to34',
                     'Population35to44', 'Population45to54',
                     'Population55to59', 'Population60to64',
                     'Population65to74', 'Population75to84',
                     'Population85Over'), 2010))
pop10$Young2010 <- pop10$Population20to242010 + pop10$Population25to342010
pop10$Mid2010   <- pop10$Population35to442010 + pop10$Population45to542010
pop10$Old2010   <- pop10$Population55to592010 + pop10$Population60to642010 + pop10$Population65to742010 + pop10$Population75to842010 + pop10$Population85Over2010

pop <- merge(pop10, pop14, by = 'CBSACode')
pop$PopPctChange <- (pop$PopulationTotal2014 - pop$PopulationTotal2010)/pop$PopulationTotal2010
pop$PopPctChangeYoung <- (pop$Young2014 - pop$Young2010)/pop$Young2010
pop$PopPctChangeMid <- (pop$Mid2014 - pop$Mid2010)/pop$Mid2010
pop$PopPctChangeOld <- (pop$Old2014 - pop$Old2010)/pop$Old2010

pop <- subset(pop, select=c('CBSACode', 'PopPctChange', 'PopPctChangeYoung', 'PopPctChangeMid', 'PopPctChangeOld'))


# Pct change in rental prices
mr <- read.csv(mrFile)
mr <- subset(mr, select=c('RegionName', 'X2010.11', 'X2014.12'))
mr <- merge(mr, cw, by = 'RegionName')
colnames(mr) <- c('RegionName', 'Nov2010MR', 'Dec2014MR', 'CBSACode')
mr$PctChangeMR <- (mr$Dec2014MR - mr$Nov2010MR)/mr$Nov2010MR

mr <- subset(mr, select=c('CBSACode', 'PctChangeMR'))

# Pct change in inventory
invt <- read.csv(invtFile)
invt <- subset(invt, select=c('RegionName', 'X2010.01', 'X2014.12'))
invt <- merge(invt, cw, by = 'RegionName')
colnames(invt) <- c('RegionName', 'Jan2010Invt', 'Dec2014Invt', 'CBSACode')
invt$PctChangeInvt <- (invt$Dec2014Invt - invt$Jan2010Invt)/invt$Jan2010Invt

invt <- subset(invt, select=c('CBSACode', 'PctChangeInvt'))


# Salary
sal10 <- read.csv(salFile10, skip = 1)
sal10 <- sal10[, c(2, 4)]
colnames(sal10) <- c('CBSACode', 'MedianIncome2010')

sal14 <- read.csv(salFile14, skip=1)
sal14 <- sal14[, c(2, 4)]
colnames(sal14) <- c('CBSACode', 'MedianIncome2014')

sal <- merge(sal10, sal14, by = 'CBSACode')
sal$PctChangeSalary <- (sal$MedianIncome2014 - sal$MedianIncome2010)/sal$MedianIncome2010

sal <- subset(sal, select=c('CBSACode', 'PctChangeSalary'))

# Unemployment
ue14 <- read.csv(unemployFile14, skip=1)
ue14 <- ue14[, c(2, 10)]
colnames(ue14) <- c('CBSACode', 'UnemploymentRate2014')

ue10 <- read.csv(unemployFile10, skip=1)
ue10 <- ue10[, c(2, 10)]
colnames(ue10) <- c('CBSACode', 'UnemploymentRate2010')

unemploy <- merge(ue14, ue10, by = 'CBSACode')
unemploy$ChangeUnemploy <- unemploy$UnemploymentRate2014 - unemploy$UnemploymentRate2010

unemploy <- subset(unemploy, select=c('CBSACode', 'ChangeUnemploy'))

# Combine and derive some new features

dat <- merge(msp, pop, by = 'CBSACode')
dat <- merge(dat, mr, by = 'CBSACode')
dat <- merge(dat, invt, by = 'CBSACode')
dat <- merge(dat, sal, by = 'CBSACode')
dat <- merge(dat, unemploy, by = 'CBSACode')

dat <- merge(dat, cw, by = 'CBSACode')

dat <- dat[complete.cases(dat), ]

save(dat, file = file.path(baseDir, 'Data', 'dat.RData'))

model1=lm(PctChngMSP~PopPctChange+PopPctChangeYoung+PopPctChangeMid+PopPctChangeOld+PctChangeMR+PctChangeInvt+PctChangeSalary+ChangeUnemploy+Region,data=data2)
model1=lm(PctChngMSP~PopPctChange+PopPctChangeMid+PopPctChangeOld+PctChangeMR+PctChangeInvt+PctChangeSalary+Region,data=data2)


