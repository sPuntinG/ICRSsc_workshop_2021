##############################################################################################
#
#   In this script I use a *DUMMY* data set (simulated PAM data) to show the power of Tidyverse 
#   to simulate a typical workflow of data wrangling.
#   This is necessary to prepare the raw data for statistical analysis and visualization
#   (=> a necessary and sensitive step in reproducible research!).
#   
#   Before running this script take a look at the "PAM_metadata.txt" file 
#   and "PAM_replication.png" to understand the data set.
#
#   Note that throughout the script I use '# ***' to highlight key functions,  
#   like the ones mentioned in the presentation.
#
#   This is NOT an in-depth tutorial! I just want to broadly show you how to do stuff in Tidyverse ;)
#   (because often then main obstacles are not knowing how to do something, 
#   but rather knowing what it can be done)
#
###############################################################################################

# This script runs on these versions:

# # sessionInfo()
# # getRversion()
# # R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"
# 
# # packageVersion("tidyverse") # ‘1.3.1’
# # packageVersion("here")      # ‘1.0.1’


# 1. Install & load packages ----------------------------------------

# Code to automatically install and load the packages specified
if (!require("pacman")) install.packages("pacman", .libPaths()[1])
p_unload("all") 
pacman::p_load(
  here, 
  tidyverse # in future scripts, you can simply add all the packages that you need
)

# # The code above replaces this:
# if (!require("tidyverse")) install.packages("tidyverse")


# 2. Create "output" folder - where to export outputs (see at the end of the script) -----------------

# This help to keep the folder tidy ;)
dir.create(here("output"))


# 3. Import data (.csv files) ---------------------------------------------------------------------
pam1 <- read_csv(here("raw_data", "pam_1.csv"))
pam2 <- read_csv(here("raw_data", "pam_2.csv"))
pam3 <- read_csv(here("raw_data", "pam_3.csv"))
pam4 <- read_csv(here("raw_data", "pam_4.csv"))

IDTT <- read_csv(here("raw_data", "IDTT.csv"))

# Tip: Look at "Column specification" in Console when importing
# you can already check if the data is of the right class


# 4. Quickly check the data ------------------------------------------------

# - "manually"
view(pam1) # or just click on it in the Environment
           # repeat for all 4
           # see that they all have the same structure
view(IDTT)

# - through the Console
pam1
summary(pam1)
structure(pam1) 
glimpse(pam1)

names(pam1)


# 5. Join multiple data frames into one - 'bind_*()' ---------------------------------------------------------------

# Easy to bind data frames together 
### (This works also if the cols have different order, but if names don't match it will create and extra col)
pam <- bind_rows(pam1, pam2, pam3, pam4)
 
  # can you see why is already better than doing this manually through a spreadsheet?


# Let's keep the R Environment clean 
rm(pam1) # one at a time ... (too much typing)
rm(list = setdiff(ls(), c("pam", "IDTT"))) # specify what to keep and remove all the rest



# 6.0. Explore the data ------------------------------------------------------
# Now that all the data is in one place, let's do a more thorough exploration to 
# make sure that everything is correct
# For this we need to look up at the info in "PAM_metadata.txt" 


# 6.1. Explore 'IDTT': info on ID, Treatment and Tanks  -------------------------------------------

# We want to join this to "pam", but first 
# let's check that everything is in order
IDTT %>% 
  group_by(trtm, tank) %>%  # ***
  summarise(                # ***
    n = n()
  ) %>%  
  view()
  # all good! numbers are correct!


# # But let's also check that the samples ("ID") are correctly
# # divided among species
# # Problem: we need a column (variable) to store the info about the species
# IDTT %>% 
#   mutate(    # ***
#     species = stringr::str_replace_all(ID, "[:digit:]", ""),        # extract the spp info
#     replicate = stringr::str_replace_all(ID, "[:alpha:]", "")) %>%  # extract the numerical info
#   view()
# 
# IDTT_2 <- IDTT %>% 
#   mutate(
#     species = stringr::str_replace_all(ID, "[:digit:]", ""),
#     replicate = stringr::str_replace_all(ID, "[:alpha:]", ""))
#   
# 
# # Let's check again
# IDTT_2 %>% 
#   group_by(species, trtm, tank) %>% 
#   summarise(
#     n = n()
#   ) %>%  
#   view()  # all good :)
# 
# rm(IDTT_2)


# 6.2. Explore 'pam': PAM measurements  ------------------------------------------------------

# We can already see that we have too many observations: 507 instead of 504.
# Maybe be repeated readings?

# We can start by checking the sample ID's
pam$ID %>% unique()
# pam %>% pull(ID) %>% unique() # same result (just fo curiosity)

# We can see that there is something off ...
# Seems like there's several typos in the ID names
# e.g., blank spaces, hyphens, letter with wrong case ...
# Also note that the output (Console) is sorted alphabetically and that
# we can notice some wrong ID names at the end of the vector.

# Using the 'filter()' we can subset the row of the values of our interest 

# Weird name, one by one
pam %>% 
  filter(ID == "Ppoc26") # values seem reasonable, probably typo  # ***

pam %>% 
  filter(ID == "wrong") # here seems like something went wrong with the reading ...


# Names that are too long 
pam %>% 
  filter(nchar(ID) > 5) # Seem they are mostly typos ... (but not all)

# But going one by one is annoying ...
# so let's first create a vector of weird names and then 
# use 'filter()' to select all of these weird ID's

# Create a vector containing the names that are too long
toolong <- pam %>% 
  filter(nchar(ID) > 5) %>% 
  pull(ID)

toolong

# Create another vector, this time containing the weird values at the end of the list 
# Note that not all of these values are clearly wrong 
# (e.g. "Acr6", "Acr20", "Acr39", "Poc12", seem reasonable)
# but if they were listed at the end that should make us suspicious ...
weird_tail <- pam$ID %>% unique()
weird_tail <- weird_tail[128:135]

weird_tail

weird_IDs <- c(toolong, weird_tail)
weird_IDs

rm(toolong, weird_tail)

# Use 'filter()' and the '%in%' ("within") operator to subset only these observations 
pam %>% 
  filter(ID %in% weird_IDs) %>%  # ***
  view() 
  # Look mostly typos
  # but "wrong" is clearly a measurement that went wrong -> let's remove it!

# 7. Fix obvious errors ---------------------------------------------------------------

# removing observations (rows) can also be done with 'filter()'
# by keeping everything but ...
# (note the '!=' operator)
pam %>% 
  filter(ID != "wrong") # Console: "A tibble: 505 x 6"  # ***

pam <- pam %>% 
  filter(ID != "wrong") # Environment: "505 obs." (before was 506, so it removed the row)


# 8. Detect and fix NON-obvious errors ----------------------------------------------------

# Cool, but let's say we want to inspect the weird ID names by 
# visually inspect them in the whole table.
# We need to "mark" them. 
# Use the function 'if_else()' with 'mutate()'
# (note the '%in%' operator)
pam <- pam %>% 
  mutate(to_check = if_else(ID %in% weird_IDs, "x", "")) %>%  # ***
  view()

# Let's write down (by hand, on a piece of paper) the corrections needed.
# Also note that some ID names look perfectly fine but are somehow marked for check ...
# e.g. "Acr6" (row 133, 260), "Acr20" (row 147, 274), "Acr39" (row 166, 419), "Poc12" (row 181, 434)
# how come? Tip: it has to do with capitalization ...



# Use 'recode()' to re-write values
pam$ID <- recode(pam$ID, "acr6" = "Acr6")  # ***
pam$ID %>% unique() # worked , so let's do the others as well

pam$ID <- recode(pam$ID, "acr20" = "Acr20")
pam$ID <- recode(pam$ID, "acr39" = "Acr39")
pam$ID <- recode(pam$ID, "poc12" = "Poc12")

pam$ID <- recode(pam$ID, "Acr399" = "Acr39")
pam$ID <- recode(pam$ID, "Ppoc26" = "Poc26")

# To get rid of any blank space and punctuation across all
# character values in the variable ID
# we can use 'str_replace()'
pam <- pam %>% 
  mutate(ID = stringr::str_replace(ID, " ", "")) %>% 
  mutate(ID = stringr::str_replace(ID, "[:punct:]", ""))

# And now let's check again
pam$ID %>% unique() %>% length() 
# 127 (instead of 126)
# we're almost done!
# just one instance to deal with: "Sty34b"

pam %>% 
  filter(ID == "Sty34" | ID == "Sty34b") %>% view()
  # we see that we have 5 rows (when we should have 4, as each sample was measured 
  # twice in light and twice in dark)
  # and one measurement was clearly re-done ... (see "PAM_metadata.txt")

# Remove the reading before Sty34b with 'ifelse()' and 'drop_na()'
pam <- pam %>% 
  mutate(ID = ifelse(ID == "Sty34" & FvFm == 0.000, NA, ID)) %>% # ***
  drop_na() %>% view() # (look at row 118)

# Correct the name
pam$ID <- recode(pam$ID, "Sty34b" = "Sty34")



# 9. Join multiple data frames into one by common variable(s) - '*_join()' -----------------------------------------

# This is a very handy function!
# you can learn more about the different *_join() functions here:
# https://dplyr.tidyverse.org/reference/mutate-joins.html
# or Google search for images as "tidyverse join()"

data <- left_join(pam, IDTT, by = ("ID"))  # ***


# 10. Final check: correct nr of replicates per group? --------------------------------------------

# Add variable "species"
# using 'mutate()' + 'case_when()'
data <- data %>%
  mutate(species = case_when(              # ***
    str_detect(ID, "Acr") ~ "Acropora",
    str_detect(ID, "Poc") ~ "Pocillopra",
    str_detect(ID, "Sty") ~ "Stylophora")  ) %>% 
  relocate(species, .after = ID) 

# Add "meas_type" to indicate whether measurement occurred in light or dark
# for this we need to check our metadata -> sunset time? 18:20
data <- data %>% 
  mutate(Meas_type = ifelse(Time < lubridate::hms("18:20:00"), "Light", "Dark")) # ***


data %>%
  group_by(species, Date, trtm, tank) %>%
  summarize(n = n()) %>%
  view()

# # Curiosity: this is a wrapper for the above code
# data %>%
#   group_by(species, Date, trtm, tank) %>%
#   count() %>%
#   view()


# Also, you can easily create a summary table of your data
data %>% 
  group_by(Treatment, Species, Meas_type) %>% 
  summarize(
    FvFm_mean = mean(FvFm),
    FvFm_sd = sd(FvFm),
    Fo_mean = mean(Fo),
    Fo_sd = sd(Fo),
    Fm_mean = mean(Fm),
    Fm_sd = sd(Fm)
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  view()


# clean-up environment
rm(list = setdiff(ls(), c("data")))



# 11. Prep the data for plotting -----------------------------------------------------

# Now that it seems like we have fixed all typos and eliminated wrong readings
# it's time to explore our data
# (later to find outliers)


# Let's tidy up the data a bit further:
# 1. remove the var that we don't need anymore
data <- data %>% 
  select(-to_check) # *** # note the minus symbol. What happens if we remove it?

# 2. reorder vars (not necessary, but looks nicer)
data <- data %>% 
  relocate(c(trtm, tank), .after = species)

# 3. rename some vars to be consistent in style (not necessary, but looks nicer)
data <- data %>% 
  rename(               # ***
    Treatment = trtm,
    Species = species,
    Tank = tank
  )

# 4. Specify class of variables
#    mostly change 'character' -> 'factor'
#   so we can specify the order (e.g. since R sorts alphabetically, "Dark" would come before "Light" unless I specify it)
data <- data %>% 
  mutate(
    Date = factor(x = Date, levels = c("21.06.2021", "22.06.2021"), labels = c("day1", "day2")), # note what 'labels' does
    Treatment = factor(x = Treatment, levels = c("C", "T")),
    Tank = factor(x = Tank, levels = c("C1", "C2", "C3", "T1", "T2", "T3")),
    Meas_type = factor(x = Meas_type, levels = c("Light", "Dark") )
  )



# 12.0. {ggplot2} for data exploration - layers explained ----------------------------------------------

# Only the outline of a plot
ggplot(data = data, aes(x = Species, y = FvFm))

# then let's add the data points
ggplot(data = data, aes(x = Species, y = FvFm)) +
  geom_point()

# and jitter (so we can actually see them)
ggplot(data = data, aes(x = Species, y = FvFm)) +
  geom_point(position = position_jitter(width = 0.3))

# color by treatment
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_point(position = position_jitter(width = 0.3))

# add boxplots
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_point(position = position_jitterdodge()) + # note the "jitterdodge", what does it do?
  geom_boxplot()

# change order of layers
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge()) 
 
# work on the aesthetics
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot(fill = NA) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) + 
  theme_bw()

# Now, look at the outliers (very low points) ...


# 12.1. {ggplot2} for data exploration - stats "on the fly" (without pre-calculations) - BOXPLOTS ------------------------

# Note how easy it is now to see that some values stand out:
# - some are very very small compared to the rest
# - others seem more distant from the "cloud" but are not that extreme
# How can we tell which ones are outliers (hence, to be removed)??

# Use geom_boxplot to explore the data distribution and potential OUTLIERs
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot(fill = NA, coef = 1.5, 
               outlier.shape = "?", 
               outlier.colour = "black", 
               outlier.size = 3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  theme_bw()

# play with 'coef' (length of the whiskers as multiple of IQR)
# see how many points are considered outliers now (IQR set to 3 instead of default 1.5)
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot(fill = NA, coef = 3,  # now 3 (default = 1.5)
               outlier.shape = "?", 
               outlier.colour = "black", 
               outlier.size = 3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  theme_bw()

# We can see that two points are clear outliers
# (while the others might be just extreme - yet realistic - values)
# = VISUAL DATA EXPLORATION :)

# Let's remove those outliers ...
data <- data %>% 
  filter(FvFm > 0.4)  # ***

# ... and check again visually
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot(fill = NA, coef = 3, # now 3 (default = 1.5)
               outlier.shape = "?", 
               outlier.colour = "black", 
               outlier.size = 3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  theme_bw()

# What to do with the other values marked at potential outliers ("?")
# Dealing with outliers is a delicate and complex topic ...
# (e.g. see this blog post https://statisticsbyjim.com/basics/remove-outliers/ )
# Removing data should be thoroughly thought through and justified 
# and (importantly!) reported and described in the methods!
# For now we decide to keep all values.

# 12.1.1 faceting --------------------------------------------------
# So now what about differences between day1 and day2?
# Use facet to
# split by day
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot(fill = NA, coef = 3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  theme_bw() +
  facet_grid(. ~ Date)

# split by day and Meas_type
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_boxplot(fill = NA, coef = 3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  theme_bw() +
  facet_grid(Meas_type ~ Date)


# 12.2. {ggplot2} for data exploration - more stats "on the fly" ---------------------------------------------------------

# MEAN by group
# (note that the aes() does the grouping)
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  stat_summary(
    fun = "mean",
    geom = "point",
    shape = 18,
    size = 2)

# MEAN on top of raw values
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, seed = 1), alpha = 0.5) +
  stat_summary(
    aes(group = Treatment),
    fun = "mean",
    geom = "point",
    shape = 18,
    size = 2,
    color = "black")

# MEDIAN on top of raw values
ggplot(data = data, aes(x = Species, y = FvFm, color = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, seed = 1), alpha = 0.5) +
  stat_summary(
    aes(group = Treatment),
    fun = "median",   # MEDIAN
    geom = "point",
    shape = 18,
    size = 2,
    color = "black")



# 12.3. {ggplot2} for data exploration - more stats "on the fly" - LINEAR REGRESSION --------------------------------------

# Let's focus on "Dark" measurements only
dark <- data %>% 
  filter(Meas_type == "Dark") 

# Fit a linear regression
lm_plot <- ggplot(data = dark, aes(x = Time, y = FvFm, shape = Treatment)) + # 
  geom_point(aes(color = Species)) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,  # y ~ x + I(x^2) + I(x^3),
    linetype = "dashed",
    color = "black",
    se = T
  )
lm_plot     # note I have assigned the plot to an object so that we can export it (later)
 
# Cool, right?
# The bug here is that we cannot extract the values of the regression model
# (ggplot() calculates it in order to draw the regression line, but we cannot access it ...)
# This is the only thing I don't like about ggplot()

# # To get the values of the model you have to calculate it yourself:
# For the CONTROL group
dark %>% 
  filter(Treatment == "C") %>% 
  lm(data = ., FvFm ~ Time) %>% 
  summary()

# For the TEMPERATURE TREATMENT group
dark %>% 
  filter(Treatment == "T") %>% 
  lm(data = ., FvFm ~ Time) %>% 
  summary()


# 13. Brief demo of 'pivot_longer()' --------------------------------------------------
# No time to go into details of this ... (sorry no time!)
# But just to show what you can do ...
long <- dark %>% 
  pivot_longer(
    cols = starts_with("F"),
    names_to = "Fluo_meas",
    values_to = "Values"
  ) 

# ... and why
ggplot(long, aes(x = Treatment, y = Values, color = Fluo_meas, shape = Species)) +
  geom_point(position = position_jitterdodge(
    jitter.width = 0.05,
    dodge.width = 0.7))




# 14. Export cleaned data and plots ------------------------------------------------------

# Export the table as csv
write_csv(data, here("output", "data.csv"), col_names = T)

# Export the plot as png (but you van also specify another format)
ggsave(lm_plot, filename = here("output", "lm_plot.png"), dpi = 210, units = "cm", width = 15, height = 15)


# 15. FINAL REMARKS ---------------------------------------------------

# So this was an example of how one could go about exploring a raw data. 
# Please note that for the sake of brevity I have overlooked some aspects 
# but in a real situation you will want to explore your data further!
# (e.g., check ALL possible variables and grouping factors - here we completely 
# ignored Fo and Fm for example)
# To do so you can basically re-use most of the functions used here and change the parameters.

# I will appreciate your feedback! 
# You can reach me on Twitter @sPuntinGi and GitHub https://github.com/sPuntinG




 