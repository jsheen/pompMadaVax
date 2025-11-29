# Code to characterize transmission of Newcastle disease virus and vaccine impact in village settings.
Accompanies Sheen, Nguyen, Ravelomanantso, Randriamady, Andriamananjara-Cunderlik, Metcalf Golden (2025) Characterizing Newcastle disease virus transmission dynamics and the impact of vaccination in village settings.

## Make two additional directories for .RData output and plot files:
- "vaxMada-data/objects/data_obj/" output all .RData files
- "vaxMada-data/plots/" output all .png files

# Files to conduct analyses, save and load .RData objects for analysis, as well as to plot results
- 0_helper_functions.R: helper functions used throughout analyses
- 2_prepDataHH.R: prep data for characterization
- 3a_characHH.R: creates .RData objects at household level of mortality rates and poultry husbandry.
- 4a_prep_pomp_vill1.R, 4b_prep_pomp_vill2.R, 4c_prep_pomp_vill3.R: prep data for maximum-likelihood estimation
- 5a_pomp_vill1.R, 5b_pomp_vill2.R, 5c_pomp_vill3.R: perform maximum-likelihood estimation
- 6a_plot_vill1.R, 6b_plot_vill2.R, 6c_plot_vill3.R: plot model performance of maximum-likelihood estimation
- 7_plot_additional.R: additional plots
- sens: sensitivity tests
