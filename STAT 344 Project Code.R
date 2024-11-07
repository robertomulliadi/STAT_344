# Target population = All documented meteorites
# Two Parameters: Mean mass, Proportion of Meteorites > 5kg
# Stratification: stony, stony-iron, iron

meteorites <- read.csv("meteorites.csv", header = T)
head(meteorites, 5)
meteorites_clean <- na.omit(meteorites)

N <- nrow(meteorites_clean)
N
n <- 1043 # To obtain population proportion within 3% error

set.seed(344)
SRS.index <- sample.int(N, n, replace = FALSE)
meteorites_sample <- meteorites_clean[SRS.index, ]
head(meteorites_sample, 5)

summary(meteorites_sample$mass)
hist(meteorites_sample$mass, main = "Histogram of Sample Mass", xlab = "Mass", ylab = "Frequency")

# Estimating Mean Mass
# Vanilla
vanilla_estimate_mean_mass <- mean(meteorites_sample$mass)
vanilla_estimate_mean_mass

# Standard Error for Vanilla Estimation (with FPC)
se_vanilla_srs <- sqrt(1-n/N)*sd(meteorites_sample$mass) / sqrt(n)
se_vanilla_srs

# Confidence Interval for Vanilla Estimation
margin_of_error_vanilla <- qt(0.975, df = n - 1) * se_vanilla
ci_lower_vanilla <- vanilla_estimate_mean_mass - margin_of_error_vanilla
ci_upper_vanilla <- vanilla_estimate_mean_mass + margin_of_error_vanilla
cat("Vanilla Estimate: ", round(vanilla_estimate_mean_mass, 2), "\n")
cat("95% Confidence Interval: (", round(ci_lower_vanilla, 2), ", ", round(ci_upper_vanilla, 2), ")\n")

# Proportion of meteorites 
meteorites_sample$mass_kg <- meteorites_sample$mass / 1000
proportion_estimate_srs <- sum(meteorites_sample$mass_kg > 5) /1043
proportion_estimate_srs

# Standard Error for Proportion Estimate
se_proportion_srs <- sqrt(1-n/N) * sqrt((proportion_estimate_srs*(1-proportion_estimate_srs))/n)
se_proportion_srs



# Stratified Sampling
unique_classes_all <- unique(meteorites_clean$class)
num_unique_classes <- length(unique_classes_all)
num_unique_classes

unique_classes_sample <- unique(meteorites_sample$class)
unique_classes_sample

# Defining Stratas: Iron, Stony-iron and Sony meteorites
stony <- c("L5", "L6", 'LL3-6', 'L', 'LL5', 'L/LL4', 'H6', 'H5', 'H4', 'H', 'EH4', 'Acapulocoite',' Diogenite-pm',' CR2-an', 
           'CI1', 'Eucrite-mmict', 'CV3', 'L3', 'LL6', 'L4', 'CM2', 'LL4', 'C2-ung', 'LL3.8', 'Diogenite', 'LL3.15', 'LL3.9', 
           'H/L3.9', 'H4-an', 'L/LL6', 'OC', 'H/L4', 'H4-5', 'L3.7', 'LL3.4', 'EL6', 'H3.8', 'H3-5', 'H5-6', 'H5-7', 'L3-6', '
           H4-6', 'CO3.3', 'H3', 'EH3/4-an','L/LL5', 'H3.7', 'CBa', 'H4/5', 'H3/4', 'H?', 'H3-6', 'L3.4', 'L3.7-6', 'EH7-an', 
           'CR2', 'CO3.2', 'K3', 'L5/6', 'CK4', 'L3.6', 'LL3.2',' CO3.5', 'Lodranite', 'L3-4', 'H5/6', 'L5-6', 'CO3.6', 'LL3.6', 
           'C3-ung', 'H3-4', 'CO3.4', 'EH3',' LL', 'R3.8-6', 'L4-6', 'EH5', 'LL3.00', 'H3.4', 'LL3.3', 'C', 'H/L3.6', 'LL7', 'LL4-6',
           'CO3.7', 'L/LL6-an', 'H3.9/4',' L3.8', 'LL5-6', 'LL3.8-6', 'L3.9', 'L4-5', 'L3-5', 'LL4/5', 'L4/5', 'H3.9', 'H3.6-6', 
           'H3.8-5',' H3.8/4',' H3.9-5', 'CH3', 'R3.8-5', 'L3.9/4', 'E4', 'CO3', 'Chrondite-ung', 'H~5', 'H~6', 'L/LL3.10', 'El5',
           'LL3', 'L~6', 'L~3', 'H~4', 'L(LL)3.5-3.7', 'H3.6', 'L3.4-3.7', 'L3.5', 'CM1/2', 'LL7(?)', 'LL6(?)', 'H3.5-4', 'El3',
           'R3.6', 'H3.5', 'CM1', 'L/LL3', 'H7', 'L(?)3', 'L3.2', 'L3.7-3.9', 'LL3.7', 'CO3.0', 'LL3.5', 'L3.7-4', 'CV3-an', 'L3.3', 
           'CK6', 'L3.1', 'Ck5', 'H3.3', 'H3.7-6', 'E6', 'H3.0', 'H3.1', 'L3.0', 'L/LL3.4', 'C6', 'L/LL~6', 'L/LL~5', 'L~4', 'L/LL~4',
           'LL(L)3', 'H3.2', 'H6_melt breccia', 'H5-melt breccia', 'H-melt rock', 'LL5/6', 'LL3/4', 'H3.4/3.5', 'H/L5', 'H(5?)', 
           'LL-imp melt', 'H~4/5', 'L6-melt breccia', 'L3.5-3.7', 'L3.3-3.7', 'L3.2-3.6', 'L3.3-3.6','CK5/6', 'L3.05', 'C2', 'C4/5', 
           'L/LL3.2', 'L3.5-5', 'L/LL(?)3', 'H4(?)', 'EL4/5', 'L5-7', 'Diogenite-an', 'L-melt rock', 'CR1', 'E', 'H-metal', 'L-metal', 
           'L/LL5/6', 'H3.8-4' ,'CBb', 'EL6/7', 'EL7', 'CH/CBb',' CO3.8', 'H/L~4', 'R5', 'H4/6', 'H3.7-5' ,'LL3.7-6', 'H3.7/3.8', 
           'L3.7/3.8', 'EH-imp melt', 'R', 'Aubrite-an', 'R6', 'LL-melt rock', 'L3.5-3.9', 'L3.2-3.5', 'L3.3-3.5', 'L3.0-3.7', 'E3-an', 
           'K', 'E3', 'Acapulcoite/lodranite', 'CK4-an', 'L(LL)3.05', 'L3.10', 'CB', 'Diogenite-olivine', 'EL-melt rock', 'EH6', 'L/LL4/5',
           'L3.8-an', 'C5/6-ung', 'CV2', 'Lunar (bas. breccia)', 'L3.8-6', 'R3/4', 'R3.9', 'CK', 'LL3.10', 'R4/5', 'L3.8-5',' Enst achon', 
           'H/L3-4', 'L(H)3', 'LL6/7', 'LL3.1', 'OC3', 'R3.7', 'LL~4', 'LL~4/5', 'L(LL)~4', 'H3.05', 'H3.10', 'Impact melt breccia', 
           'LL3-5', 'H/L3.7', 'LL3-4', 'CK3/4', 'Martian', 'CO3.1', 'Lunar (bas/gab brec)', 'Achondrite-prim', 'LL<3.5', 'CK3.8', 
           'L/LL-melt rock', 'H6/7', 'CM2-an', 'R3-5', 'L4-melt rock', 'L6-melt rock', 'H/L4/5', 'EL3/4', 'H/L6-melt rock', 'Enst achon-ung', 
           'L3-7', 'R3.4', 'LL3.05', 'LL4/6', 'LL3.8-4', 'H3.15', 'C3.0-ung', 'LL-melt breccia', 'LL6-melt breccia', 'L5-melt breccia', 
           'LL(L)3.1', 'LL6-an', 'L4-melt breccia, Howardite-an', 'H4-melt breccia', 'Martian (basaltic breccia)', 'L3-melt breccia', 
           'L~4-6', 'LL~5', 'R3.5-4', 'CR7', 'H-melt breccia', 'Lunar (norite)', 'L3.00', 'H3.0-3.4', 'L/LL4-6', 'CM', 'EH7', 'L4-an',
           'E-an', 'H3.8/3.9', 'L3.9-5', 'H3.8-6', 'H3.4-5', 'L3.0-3.9', 'L3.5-3.8', 'H3.2-3.7', 'L3.6-4', 'C3/4-ung', 'L/LL3.5', 
           'L/LL3.6/3.7', 'H/L4-5', 'LL~3', 'LL5-7', 'LL3.9/4', 'H3.8-an', 'CR-an', 'L/LL5-6', 'L(LL)5', 'L(LL)6', 'LL3.1-3.5', 'E5', 
           'Lodranite-an', 'H3.2-6', 'EH', 'H(?)4', 'E5-an', 'H3.2-an', 'EH6-an', 'Stone-ung', 'C1/2-ung', 'L/LL', 'Ureilite-an', 'Stone-uncl', 
           'Angrite', 'Aubrite', 'Eucrite-pmict', 'Eucrite', 'Eucrite-cm', 'Eucrite-br', 'Eucrite-Mg rich', 'Eucrite-unbr', 'Eucrite-an', 
           'Martian (OPX)', 'Martian (shergottite)', 'Martian (nakhlite)', 'Martian (chassignite)', 'Howardite', 'Acapulcoite/Lodranite', 
           'Winonaite', 'Achondrite-ung', 'Chondrite-ung', 'Brachinite', 'Lunar (anorth)', 'Lunar', 'Lunar (basalt)', 'Lunar (bas/anor)', 
           "LL3.0", "Lunar (gabbro)", "R4", "C4", "H5-an", "EH4/5", "R3-6", "L6/7", "L-imp melt", "CK3", "H3-an", "R3.8", "L~5", "C4-ung",
           "R3.5-6", "H3.9-6", "Ureilite-pmict", "LL~6", "CK4/5", "EL4", "Lunar (feldsp. breccia)", "L3.9-6", "H-an", "L/LL3-6", "L/LL3-5", 
           "H/L3.5", "H/L3", "R3-4", "CK3-an", "LL4-5", "H/L6", "L3/4", "H-imp melt", "CR", "Chondrite-fusion crust", "H(L)3-an", "L(LL)3",
           "H(L)3", "R3", "L7", "CM-an", "Relict OC", 'Chrondite-fusion crust'
)

stony_iron <- c("Mesosiderite-C2", "Pallasite, ungrouped", "Mesosiderite-C", "Pallasite?", "Mesosiderite-A1", "Mesosiderite", 
                "Mesosiderite-A3/4", "Mesosiderite-A3", "Mesosiderite-B2", "Mesosiderite-B1", "Mesosiderite?", "Mesosiderite-B", 
                "Mesosiderite-A4", "Pallasite", "Pallasite, PMG", "Pallasite, PMG-an", "Mesosiderite-B4", "Mesosiderite-an", "Mesosiderite-A2", 
                "Pallasite, PES", "Mesosiderite-A" )

iron <- c("Iron, IVA", "Iron, ungrouped", "Iron, IIAB", "Iron, IAB-sLL", "Iron, IAB-MG", "Iron?", "Iron, IIIAB", "Iron, IID", "Iron, IIE",
          "Iron, IAB-sHL", "Iron", "Iron, IIE-an", "Iron, IAB-ung", "Iron, IIF", "Iron, IIIAB-an", "Iron, IIIE-an", "Iron, IAB-sLM", "Iron,
          IC", "Iron, IID-an", "Iron, IIIE", "Iron, IVA-an", "Iron, IIAB-an", "Iron, IIIAB?", "Iron, IAB-sHH", "Iron, IC-an", "Iron, 
          IAB-sHL-an", "Iron, IIE?", "Iron, IIG", "Iron, IIC", "Iron, IIIF", "Iron, IVB", "Iron, IAB?", "Iron, IAB-sLH", "Relict iron" )

meteorites_sample$strata <- ""

for(x in 1:nrow(meteorites_sample)){
  if(meteorites_sample$class[x] %in% stony){
    meteorites_sample$strata[x] = "Stony"
  } else if(meteorites_sample$class[x] %in% stony_iron) {
    meteorites_sample$strata[x] = "Stony-Iron" 
  }else {
    meteorites_sample$strata[x] = "Iron"
  }
}

head(meteorites_sample)
attach(meteorites_sample)
N.h <- tapply(mass, strata, length)
N.h
classes <- names(N.h)
classes
detach(agpop)

# Generating stratified sample









