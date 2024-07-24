# How to engineer a habitable planet: The rise of marine ecosystem engineers through the Phanerozoic
Analyses associated with manuscript "How to engineer a habitable planet: The rise of marine ecosystem engineers through the Phanerozoic" by Alison Cribb and Simon Darroch

Corresponding author: A.T.Cribb@soton.ac.uk

 <i><b> This repository is updated for the most recent round of revision in Palaeontology. Pleaes do not use any data files or R scripts prior to publication without consulting A Cribb at A.T.Cribb@soton.ac.uk! </i></b>

This repository contains all data and R scripts needed to reproduce the analyses and figures in the manuscript main text and supplementary materials. You can either run the analyses and generate new Output files, or you may use the Output files already in the repository that are used to generate the published manuscript. Note that due to the subsampling procedures involving a random number generator, your results may vary slightly from the published results. If you download this repository, set it as your working directory and everything should run smoothly.

## Data
There are three datasets in <kbd>[Data](https://github.com/atcribb/Rise-of-the-Ecosystem-Engineers/tree/main/Data)</kbd>:

* ``Phanerozoic_clean_final.RData`` - contains the cleaned and stratigraphically binned (largely following Kocsis et al. 2019 ddPhanero protocol with additional cleaning of formation data) Phanerozoic data. The raw PBDB data was downloaded 1 November 2023. This dataset is used to generate a new list of bioturbators in each script, but you can also use the bioturbator dataset below. 
* ``Reef_Ecosystem_Engineers_final.RData`` - contains the full Phanerozoic reef dataset with reef-builder classifications assigned in an REE_Classification category (REE = Reef Ecosystem Engineer)
* ``Bioturbators_data.RData`` - contains the full Phanerozoic bioturbators dataset, which is also generated each time at the top of any analysis script that focuses on bioturbators. This is not called in any of the analyses, but it may be useful to have for reference.


## Analyses
There are five main scripts in <kbd>[Analyses](https://github.com/atcribb/Rise-of-the-Ecosystem-Engineers/tree/main/Analyses)</kbd>:

* ``EEDominance_bioturbators.R`` - analyses for the dominance of bioturbators to produce Figure 1
* ``EERelabund_bioturbators_feeding.R`` - analyses to get the relative abundance of bioturbator feeding ecologies to produce Figure 2
* ``EERelabund_bioturbators_taxa.R`` - analyses to get the relative abundance of bioturbator phyla to produce Figure 3
* ``EEDominance_reefs.R`` - analyses for the dominance of reef-builders to produce Figure 4
* ``EERelabund_reefs.R`` - analyses to get the relative abundance of reef-builder groups to produce Figure 5

Additionally, there are versions of ``EEDominance_bioturbators.R`` and ``EEDominance_reefs.R`` specific to shallow, deep, siliciclastic, and carbonate facies in the <kbd>[within environments](https://github.com/atcribb/Rise-of-the-Ecosystem-Engineers/tree/main/Analyses/within_environments)</kbd> folder. These will produce the output files needed for Supplementary Figures 1 and 2 for bioturbators and Supplementary Figures 4 and 5 for reef-builders.

## Output 
The <kbd>[Output](https://github.com/atcribb/Rise-of-the-Ecosystem-Engineers/tree/main/Output)</kbd> folder already contains the .RData files for the results in the manuscript. If you re-run the analyses, these will automatically be overwritten with your own results. Note that results may vary slightly due to the random number generator in the subsampling procedures.


## Figures
Finally, there are four scripts in <kbd>[Figures](https://github.com/atcribb/Rise-of-the-Ecosystem-Engineers/tree/main/Figures)</kbd>:

* ``PlottingOutput_Bioturbator_Dominance.R`` – generates Figure 1 and the within-environment results for Supplementary Figures 1 and 2
* ``PlottingOutput_Bioturbator_RelativeAbundance.R`` - generates the relative abundance figures for phyla and feeding ecologies in Figures 2 and 3, respectively
* ``PlottingOutput_Reef_Dominance.R`` - generates Figure 4, the within-environment results for Supplementary Figures 4 and 5, and contains the brief (raw) analysis and plotting output for Supplementary Figure 3
* ``PlottingOutput_Reef_RelativeAbundance.R`` - generates the relative abundance figures for reef-builder groups in Figure 5
