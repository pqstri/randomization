require(randomizeR)
require(tidyverse)

# study participants
total_sample_size <- 100

# number of features to stratify the population for
n_strata <- 2 # tp = surgical or tp = medical

# feature ratio
sur_to_med_ratio <- 0.5

# reproducibility
seed <- 1234
set.seed(seed)
seeds <- sample(1:10000, 2)

# generate randomization list

# surgical
surg_list <- data.frame(patient_internal_id = 1:round(total_sample_size*sur_to_med_ratio), strata = "surgery")
RAND1 <- randomizeR::rpbrPar(N = round(total_sample_size*sur_to_med_ratio), rb = 2, ratio = c(1, 1), groups = c("INTERVENTION", "CONTROL"))
R1 <- randomizeR::genSeq(RAND1, seed = seeds[1])
surg_list$assignment <- as.vector(randomizeR::getRandList(R1))

# medical
medi_list <- data.frame(patient_internal_id = round(total_sample_size*sur_to_med_ratio)+1:round(total_sample_size*(1-sur_to_med_ratio)), strata = "medical")
RAND2 <- randomizeR::rpbrPar(N = round(total_sample_size*(1-sur_to_med_ratio)), rb = 2, ratio = c(1, 1), groups = c("INTERVENTION", "CONTROL"))
R2 <- randomizeR::genSeq(RAND2, seed = seeds[2])
medi_list$assignment <- as.vector(randomizeR::getRandList(R2))

# joining
rlist <- rbind(surg_list, medi_list)

# generating patient identifier
rlist$patient_external_id <- sprintf("%02d%02d%03d", 1, # center
                                     rlist$strata == "surgery",
                                     rlist$patient_internal_id)

rlist$given <- F
rlist$given_at <- NA
rlist$assigned_to <- NA

# evaluate result consistency
table(rlist$strata, rlist$assignment) %>% addmargins()

# new assignation example
s1 <- "surgery"
unique_code <- "CRMLCU88O47R624R"

selected <- rlist %>%
  filter(!given, strata == s1) %>%
  arrange(patient_internal_id) %>%
  {.$patient_external_id[1]}

rlist[rlist$patient_external_id == selected,]$given_at = date()
rlist[rlist$patient_external_id == selected,]$given = T
rlist[rlist$patient_external_id == selected,]$assigned_to = digest::digest(unique_code, "sha256", serialize = T)



# check example
control_code <- "CRMLCU88O47R624R"

selected <- rlist %>%
  filter(assigned_to == digest::digest(control_code, "sha256", serialize = T)) %>%
  arrange(patient_internal_id) %>%
  {.$patient_external_id[1]}



