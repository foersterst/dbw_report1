Dry body mass from linear measures: report \#1
================
Stenio Foerster
22 Nov 2021

# Introduction

In this section I am going to load the input files and perform some
necessary modifications. You can skip all the commands not followed by a
comment, because they are mostly string modifications necessary to run
the following steps.

Loading input files

``` r
dat.jj <- read.csv(file = 'body_weight_jj.csv', header = T) # lab-reared individuals
dat.lp <- read.csv(file = 'photo_measu_jj.csv', header = T) # specimens from lepiforum
dat.ug <- read.csv(file = 'uganda_data_sh.csv', header = T) # Ugandan species
dat.gm <- read.csv(file = 'geom_bs.csv', header = T, row.names = 1) # Species present in the Geometridae mundi
phy.sh <- read.tree(file = 'geom_tree_sh.tre') # phylogenetic tree provided by Sille (373 species) 
```

Doing some necessary modifications

``` r
dat.ug$species <- dat.ug$species %>% gsub(pattern = '_', replacement = ' ') 
phy.sh$tip.label <- gsub(pattern = '_', replacement = ' ', x = phy.sh$tip.label)

# Putting the first letter in upper case for species' names (Juhan data)
dat.jj$species %>% strsplit(split = ' ') %>% 
  sapply(function(x) paste(x[1], sep = ' ')) %>% 
  str_to_title() -> ge
dat.jj$species %>% strsplit(split = ' ') %>% 
  sapply(function(x) paste(x[2], sep = ' ')) -> sp
dat.jj$species <- paste(ge, sp, sep = ' ')

# Putting the first letter in upper case for species' names (Lepiforum data)
dat.lp$species %>% strsplit(split = ' ') %>% 
  sapply(function(x) paste(x[1], sep = ' ')) %>% 
  str_to_title() -> ge
dat.lp$species %>% strsplit(split = ' ') %>% 
  sapply(function(x) paste(x[2], sep = ' ')) -> sp
dat.lp$species <- paste(ge, sp, sep = ' ')

# Removing wingless specimens from lepiforum data
dat.lp <- dat.lp[which(dat.lp$wingless == 0), ] # Wingless individuals are uninformative (are they?)
```

# Scaling lepiforum specimens to match lab-reared individuals

The scaling procedure adopted here is based on the instructions provided
by Juhan:  
“*For this scaling, a difference will be calculated for each sex within
each species, by dividing the mean wing length of the respective lab
subsample by the mean winglenth of the respective photo subsample, and
then multiplying all the mean linear measures of that subsample with
this coefficient.*”.

``` r
# Sex 0 (Juhan)
# Here, I first filtered the data table to keep only sex 0. Then I calculated the mean wing length for each species
dat.jj[which(dat.jj$sex == 0), ] %>%
  group_by(species) %>% 
  summarise('mean_wl_lab' = mean(wing_length, na.rm = T)) %>% 
  as.data.frame() -> dat.jj.0

# Sex 0 (lepiforum)
# Here I did the same, but I applied a command to calculate the mean values of all linear metrics per species at once
dat.lp[which(dat.lp$sex == 0), ] %>% 
  group_by(species) %>% 
  summarise_all(mean) %>% 
  as.data.frame() -> dat.lp.0

# Now I organized the data sets (Juhan, and lepiforum) to keep only the shared species between them
dat.lp.0 <- dat.lp.0[which(dat.lp.0$species %in% dat.jj.0$species), ]
dat.jj.0 <- dat.jj.0[which(dat.jj.0$species %in% dat.lp.0$species), ]
```

Let’s check if all the species present in Juhan’s data set are present
in the lepiforum and vice-versa

``` r
dat.jj.0$species == dat.lp.0$species # OK! 
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [46] TRUE TRUE TRUE TRUE

Once both data sets have the same species, I can merge them in a single
object

``` r
dat.ok.0 <- merge(x = dat.jj.0, y = dat.lp.0, by = 'species')
dat.ok.0 <- dat.ok.0[, c(-3:-5)]
```

Ok, according to the scaling procedure mentioned by Juhan, we need to
divide the mean wing length of the lab-reared individuals for a given
species by the mean wing length of the respective lepiforum species.
Note however that the lepiforum species have mean wing length values for
both wings (columns “wing\_right” and “wing\_left”).

``` r
head(dat.ok.0)
```

    ##                 species mean_wl_lab wingspan_widest wingspan_tips wing_right
    ## 1 Abraxas grossulariata    20.70000            41.0      39.92666   21.36844
    ## 2  Aethalura punctulata    14.03333            23.5      21.70081   12.72827
    ## 3      Alcis repandatus    19.60000            39.6      37.86122   21.51549
    ## 4   Arichanna melanaria    18.23750            36.0      33.91222   19.07622
    ## 5    Cabera exanthemata    13.20556            27.0      24.71811   13.90666
    ## 6        Cabera pusaria    13.03333            28.5      26.65116   14.90850
    ##   wing_left body_length abdomen_width
    ## 1  20.80806   13.693706      3.193663
    ## 2  12.78055    7.696730      2.274044
    ## 3  21.78351   14.460196      3.277378
    ## 4  19.00144   13.574299      2.556297
    ## 5  13.69231    9.388602      2.584374
    ## 6  14.69089   10.143621      2.317855

As I did not know which wing to use, I took the mean of
the two wings for each species, something like this: (wing\_right +
wing\_left)/2. These values were added to a new column called
“mean\_wl\_lep”.

``` r
dat.ok.0[, c('wing_right', 'wing_left')] %>% 
  apply(mean, MARGIN = 1) -> dat.ok.0$mean_wl_lep # mean wing length measured from both wings
```

With the column containing the mean wing length values for the lepiforum
species (“mean\_wl\_lep”), we can now create a new column
(“mean\_wl\_coeff”) to harbor the coefficients from the division: mean
wing length of lab-reared individuals / mean wing length of lepiforum
individuals.

``` r
dat.ok.0$mean_wl_coeff <- dat.ok.0$mean_wl_lab/dat.ok.0$mean_wl_lep # wing length coefficients
```

Now we just need to multiply all the raw linear measures taken from
lepiforum by the coefficients calculated in the previous command.

``` r
dat.ok.0 <- cbind('species' = dat.ok.0$species, dat.ok.0[, 3:8] * dat.ok.0$mean_wl_coeff) # scaled linear measures
colnames(dat.ok.0) <- c('species', paste(colnames(dat.ok.0[, -1]), 'sc', sep = '_')) # let's add the end "sc" to the variable names to say that they are "scaled" 
```

After that we can make sure that the species present in our linear
measures data set are those for which we have phylogenetic information

``` r
dat.ok.0 <- dat.ok.0[which(dat.ok.0$species %in% phy.sh$tip.label), ] # linear measure data now have only species present in the phylogenetic tree
phy.sh.0 <- drop.tip(phy.sh, tip = setdiff(phy.sh$tip.label, dat.ok.0$species)) # the original phylogenetic tree was pruned to retain only those species present in the linear measures data set
```

Here is the scaled linear measures for some species. You can try to
scale a single linear measure for one of the species showed below so
that we can compare our results.

``` r
head(dat.ok.0)
```

    ##                 species wingspan_widest_sc wingspan_tips_sc wing_right_sc
    ## 1 Abraxas grossulariata           40.24516         39.19159      20.97503
    ## 2  Aethalura punctulata           25.85642         23.87682      14.00458
    ## 4   Arichanna melanaria           34.48479         32.48489      18.27332
    ## 5    Cabera exanthemata           25.83792         23.65424      13.30812
    ## 6        Cabera pusaria           25.09849         23.47031      13.12915
    ## 9     Catarhoe cuculata           23.16577         22.63962      12.86361
    ##   wing_left_sc body_length_sc abdomen_width_sc
    ## 1     20.42497      13.441597         3.134866
    ## 2     14.06209       8.468505         2.502069
    ## 4     18.20168      13.002969         2.448704
    ## 5     13.10299       8.984516         2.473142
    ## 6     12.93751       8.932966         2.041216
    ## 9     12.93639       8.746675         1.799735

# Mismatching species

Here I will provide a brief summary of the species included and excluded
from our data sets. To do that, I need to run the same analyses
illustrated above to the sex 1 (remember that all that we have done
until now was for the sex 0 only). As we are just applying the same
commands to the sex 1, I will omit these commands here just to save
space.

But check the scaled linear measures for some species in sex 1 as well

``` r
head(dat.ok.1)
```

    ##                 species wingspan_widest_sc wingspan_tips_sc wing_right_sc
    ## 1 Abraxas grossulariata           39.74395         39.15508      20.91063
    ## 2  Aethalura punctulata           25.41476         23.76778      14.22626
    ## 3  Agriopis aurantiaria           32.22973         30.09750      18.25189
    ## 4   Arichanna melanaria           31.59469         30.42759      16.84444
    ## 5      Biston betularia           38.34010         37.97955      19.66112
    ## 6    Cabera exanthemata           26.81842         25.15001      13.79840
    ##   wing_left_sc body_length_sc abdomen_width_sc
    ## 1     21.18937      15.372806         2.252195
    ## 2     14.25556       9.594766         1.837901
    ## 3     18.34811      10.680866         2.321608
    ## 4     16.84444      13.394639         2.167980
    ## 5     19.93888      15.414285         4.311865
    ## 6     13.80160       9.840694         1.673037

After all the steps showed above, we ended up with 48 species. These are
the species for which we have both linear measures, dry body weight and
phylogenetic information. Below are some descriptive summaries

How many species are there in total (both sexes)?

``` r
c(dat.ok.0$species, dat.ok.1$species) %>% 
  unique() %>% 
  length() # 48 species in total
```

    ## [1] 48

What are these 48 species?

``` r
intersect(intersect(dat.lp$species, dat.jj$species), phy.sh$tip.label)
```

    ##  [1] "Abraxas grossulariata"    "Aethalura punctulata"    
    ##  [3] "Agriopis aurantiaria"     "Arichanna melanaria"     
    ##  [5] "Biston betularia"         "Cabera exanthemata"      
    ##  [7] "Cabera pusaria"           "Catarhoe cuculata"       
    ##  [9] "Cepphis advenaria"        "Cleora cinctaria"        
    ## [11] "Colotois pennaria"        "Crocallis elinguaria"    
    ## [13] "Cyclophora albipunctata"  "Ecliptopera silaceata"   
    ## [15] "Ectropis crepuscularia"   "Electrophaes corylata"   
    ## [17] "Ematurga atomaria"        "Ennomos autumnaria"      
    ## [19] "Ennomos fuscantaria"      "Epirrhoe alternata"      
    ## [21] "Epirrhoe tristata"        "Epirrita autumnata"      
    ## [23] "Erannis defoliaria"       "Eupithecia virgaureata"  
    ## [25] "Gymnoscelis rufifasciata" "Horisme tersata"         
    ## [27] "Hypomecis punctinalis"    "Jodis putata"            
    ## [29] "Lomaspilis marginata"     "Lomographa bimaculata"   
    ## [31] "Lomographa temerata"      "Lycia hirtaria"          
    ## [33] "Lycia pomonaria"          "Mesoleuca albicillata"   
    ## [35] "Operophtera brumata"      "Opisthograptis luteolata"
    ## [37] "Paradarisa consonaria"    "Pelurga comitata"        
    ## [39] "Philereme vetulata"       "Plagodis pulveraria"     
    ## [41] "Pseudopanthera macularia" "Rheumaptera cervinalis"  
    ## [43] "Selenia dentaria"         "Selenia tetralunaria"    
    ## [45] "Trichopteryx carpinata"   "Xanthorhoe biriviata"    
    ## [47] "Xanthorhoe ferrugata"     "Xanthorhoe spadicearia"

How many species per sex

``` r
unique(dat.ok.0$species) %>% length() # sex 0: 39 species
```

    ## [1] 39

``` r
unique(dat.ok.1$species) %>% length() # sex 1: 46 species
```

    ## [1] 46

What are the species for sex 0?

``` r
unique(dat.ok.0$species)
```

    ##  [1] "Abraxas grossulariata"    "Aethalura punctulata"    
    ##  [3] "Arichanna melanaria"      "Cabera exanthemata"      
    ##  [5] "Cabera pusaria"           "Catarhoe cuculata"       
    ##  [7] "Cepphis advenaria"        "Cleora cinctaria"        
    ##  [9] "Colotois pennaria"        "Crocallis elinguaria"    
    ## [11] "Cyclophora albipunctata"  "Ecliptopera silaceata"   
    ## [13] "Ectropis crepuscularia"   "Electrophaes corylata"   
    ## [15] "Ematurga atomaria"        "Ennomos autumnaria"      
    ## [17] "Epirrhoe alternata"       "Epirrhoe tristata"       
    ## [19] "Eupithecia virgaureata"   "Gymnoscelis rufifasciata"
    ## [21] "Horisme tersata"          "Hypomecis punctinalis"   
    ## [23] "Jodis putata"             "Lomaspilis marginata"    
    ## [25] "Lomographa temerata"      "Lycia hirtaria"          
    ## [27] "Opisthograptis luteolata" "Paradarisa consonaria"   
    ## [29] "Pelurga comitata"         "Philereme vetulata"      
    ## [31] "Plagodis pulveraria"      "Pseudopanthera macularia"
    ## [33] "Rheumaptera cervinalis"   "Selenia dentaria"        
    ## [35] "Selenia tetralunaria"     "Trichopteryx carpinata"  
    ## [37] "Xanthorhoe biriviata"     "Xanthorhoe ferrugata"    
    ## [39] "Xanthorhoe spadicearia"

What are the species for sex 1?

``` r
unique(dat.ok.1$species)
```

    ##  [1] "Abraxas grossulariata"    "Aethalura punctulata"    
    ##  [3] "Agriopis aurantiaria"     "Arichanna melanaria"     
    ##  [5] "Biston betularia"         "Cabera exanthemata"      
    ##  [7] "Cabera pusaria"           "Catarhoe cuculata"       
    ##  [9] "Cepphis advenaria"        "Cleora cinctaria"        
    ## [11] "Colotois pennaria"        "Crocallis elinguaria"    
    ## [13] "Cyclophora albipunctata"  "Ecliptopera silaceata"   
    ## [15] "Ectropis crepuscularia"   "Ematurga atomaria"       
    ## [17] "Ennomos autumnaria"       "Ennomos fuscantaria"     
    ## [19] "Epirrhoe alternata"       "Epirrhoe tristata"       
    ## [21] "Epirrita autumnata"       "Erannis defoliaria"      
    ## [23] "Eupithecia virgaureata"   "Gymnoscelis rufifasciata"
    ## [25] "Horisme tersata"          "Hypomecis punctinalis"   
    ## [27] "Jodis putata"             "Lomaspilis marginata"    
    ## [29] "Lomographa bimaculata"    "Lomographa temerata"     
    ## [31] "Lycia hirtaria"           "Lycia pomonaria"         
    ## [33] "Mesoleuca albicillata"    "Operophtera brumata"     
    ## [35] "Opisthograptis luteolata" "Paradarisa consonaria"   
    ## [37] "Pelurga comitata"         "Philereme vetulata"      
    ## [39] "Plagodis pulveraria"      "Rheumaptera cervinalis"  
    ## [41] "Selenia dentaria"         "Selenia tetralunaria"    
    ## [43] "Trichopteryx carpinata"   "Xanthorhoe biriviata"    
    ## [45] "Xanthorhoe ferrugata"     "Xanthorhoe spadicearia"

Which species are in linear data sets (Juhan/lepiforum) but not in the
phylogeny?

``` r
setdiff(intersect(dat.lp$species, dat.jj$species), phy.sh$tip.label) # 11 species
```

    ##  [1] "Alcis repandatus"      "Calocalpe undulata"    "Calospilos sylvata"   
    ##  [4] "Cyclophora annulata"   "Itame fulvaria"        "Itame wauaria"        
    ##  [7] "Phigalia pedaria"      "Semiothisa alternaria" "Semiothisa clathrata" 
    ## [10] "Semiothisa liturata"   "Semiothisa notata"

# PCA of scaled linear measures

Here you can find the main results of the PCAs performed on the scaled
linear measures. First, let’s check the cumulative proportion (%) of the
total variance captured by each PC. For the sex 0, the two first PCs
captured 99% of the variation. For the sex 1, the two first PCs
accounted for 98%. Check the individual contribution of each PC on the
line “Proportion of Variance”.

``` r
# PCA for sex 0
pca.sex.0 <- prcomp(dat.ok.0[, -1], scale. = T)
summary(pca.sex.0)
```

    ## Importance of components:
    ##                           PC1     PC2     PC3     PC4     PC5     PC6
    ## Standard deviation     2.3879 0.48688 0.19218 0.14011 0.05979 0.02531
    ## Proportion of Variance 0.9504 0.03951 0.00616 0.00327 0.00060 0.00011
    ## Cumulative Proportion  0.9504 0.98987 0.99603 0.99930 0.99989 1.00000

``` r
# PCA for sex 1
pca.sex.1 <- prcomp(dat.ok.1[, -1], scale. = T)
summary(pca.sex.1)
```

    ## Importance of components:
    ##                           PC1     PC2     PC3     PC4     PC5     PC6
    ## Standard deviation     2.3498 0.57892 0.34253 0.14001 0.07618 0.02242
    ## Proportion of Variance 0.9203 0.05586 0.01955 0.00327 0.00097 0.00008
    ## Cumulative Proportion  0.9203 0.97613 0.99568 0.99895 0.99992 1.00000

Now, take a look at the correlation among each linear measure and the
first two PCs

``` r
# For sex 0
pca.sex.0$rotation[, 1:2]
```

    ##                           PC1         PC2
    ## wingspan_widest_sc -0.4167002  0.12392167
    ## wingspan_tips_sc   -0.4155465  0.15331536
    ## wing_right_sc      -0.4133371  0.28908764
    ## wing_left_sc       -0.4143153  0.24202595
    ## body_length_sc     -0.4124004  0.01164565
    ## abdomen_width_sc   -0.3756372 -0.90490551

``` r
# For sex 1
pca.sex.1$rotation[, 1:2]
```

    ##                           PC1         PC2
    ## wingspan_widest_sc -0.4228918  0.10746472
    ## wingspan_tips_sc   -0.4221006  0.13131446
    ## wing_right_sc      -0.4166444  0.30795716
    ## wing_left_sc       -0.4164286  0.30898118
    ## body_length_sc     -0.4043238 -0.09397325
    ## abdomen_width_sc   -0.3640201 -0.87867508

# Ugandan species

Here are some descriptive summaries about the species from Uganda. Which
species from Uganda (with known dry body weight) are present in the
Sille’s tree?

``` r
spp.ug <- intersect(dat.ug$species, phy.sh$tip.label)
spp.ug # 64 species
```

    ##  [1] "Epigynopteryx sp nr coffeae"   "Epigynopteryx ansorgei"       
    ##  [3] "Oedicentra gerydaria"          "Prasinocyma pedicata"         
    ##  [5] "Megadrepana cinerea"           "Cleora occidens"              
    ##  [7] "Cartaletis variabilis"         "Epigynopteryx sp SH05"        
    ##  [9] "Ennominae sp SH13"             "Epigynopteryx sp SH03"        
    ## [11] "Miantochora sp SH02"           "Cleora dactylata"             
    ## [13] "Epigynopteryx coffeae"         "Aphilopota sp SH01"           
    ## [15] "Scopula tenera"                "Cleora lima"                  
    ## [17] "Cyclophora diplosticta"        "Geolyces sp nr sanghana"      
    ## [19] "Geolyces sp SH01"              "Acrostatheusis reducta"       
    ## [21] "Cleora nigrisparsalis"         "Geolyces sylvana"             
    ## [23] "Hypomecis sp SH01"             "Ennominae sp SH05"            
    ## [25] "Geolyces linearis"             "Ennominae sp SH07"            
    ## [27] "Cleora pavlitzkiae"            "Geolyces sp nr variegata SH01"
    ## [29] "Geolyces smithi"               "Miantochora fletcheri"        
    ## [31] "Ennominae sp SH11"             "Ennominae sp SH14"            
    ## [33] "Ennominae sp SH17"             "Colocleora sp nr spuria SH01" 
    ## [35] "Ennominae sp SH16"             "Scopula sp SH07"              
    ## [37] "Biston abruptaria"             "Ennominae sp SH10"            
    ## [39] "Scopula sp nr laevipennis"     "Epigynopteryx sp SH01"        
    ## [41] "Epigynopteryx sp SH02"         "Ennominae sp SH03"            
    ## [43] "Ennominae sp SH12"             "Cleora dargei"                
    ## [45] "Chiasmia fulvimargo"           "Chiasmia sp nr ostentosaria"  
    ## [47] "Episteira sp SH01"             "Oedicentra albipennis"        
    ## [49] "Miantochora sp SH03"           "Colocleora sp nr expansa"     
    ## [51] "Scopula johnsoni"              "Miantochora picturata"        
    ## [53] "Cleora herbuloti"              "Zamarada sp nr rhamphis"      
    ## [55] "Metallospora catori"           "Miantochora venerata"         
    ## [57] "Dasymacaria plebeia"           "Pingasa sp SH01"              
    ## [59] "Cleora radula"                 "Ennominae sp SH02"            
    ## [61] "Ennominae sp SH21"             "Hypomecis sp SH03"            
    ## [63] "Cleora epiclithra"             "Cleora echinodes"

For which of those species do we have images in the Geometridae mundi
catalog?

``` r
intersect(spp.ug, dat.gm$species) # 26 species
```

    ##  [1] "Epigynopteryx ansorgei" "Oedicentra gerydaria"   "Prasinocyma pedicata"  
    ##  [4] "Megadrepana cinerea"    "Cleora dactylata"       "Epigynopteryx coffeae" 
    ##  [7] "Scopula tenera"         "Cleora lima"            "Acrostatheusis reducta"
    ## [10] "Cleora nigrisparsalis"  "Geolyces sylvana"       "Cleora pavlitzkiae"    
    ## [13] "Miantochora fletcheri"  "Biston abruptaria"      "Cleora dargei"         
    ## [16] "Chiasmia fulvimargo"    "Oedicentra albipennis"  "Scopula johnsoni"      
    ## [19] "Miantochora picturata"  "Cleora herbuloti"       "Metallospora catori"   
    ## [22] "Miantochora venerata"   "Dasymacaria plebeia"    "Cleora radula"         
    ## [25] "Cleora epiclithra"      "Cleora echinodes"

Ok, that’s all for now. Please contact me if you have any problems to
understand what was presented here.

See you!
