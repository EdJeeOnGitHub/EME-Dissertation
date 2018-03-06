# EME-Dissertation
The project first explores asset market responses to terror and then underlying event heterogeneity.

## Organisation:

- Pretty self-explanatory:
- Data in folders marked data.
- Scripts in the Script folder.
- Material used in presentations in the Presentation folder.
- Code used for stan models in Stan folder and their output in Stanfit folders for AnalysisScript1.
- All other stan output is in AWS Output folder as they were run on an Amazon Machine Image server.

## Requirements:

- Packrat should have a list of packages that need to be installed.
- Any of the Bayesian models that are run require separate (I think) installation from stan, see here: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started. There's a few Bayesian models in AnalysisScript1 (only under the Logit headings I think) and any script starting 'Heterogenous X' where X = {CAR4, R, etc.(but not 'Exploratory Heterogenous Analysis')} is full of these models. I highly recommend not running the latter scripts as stan can be a pain to install and they take ~4 hours to run with 16 CPUs working in parallel.

## Replication Steps:

1. AnalysisScript1 - Main analysis that doesn't involve heterogenous effects
2. TextAnalysis/TerrorCleaning - preparation for heterogenous effects
3. Heterogenous CAR4 Analysis (The others aren't actually used apart from robustness checks so maybe be a little out of date wrt relative paths.)
4. ExploratoryHeterogeneous Analysis/Any scripts that produce graphics or tables - Using analysis output to create any tables or graphs used.

None of this is strictly necessary in terms of order to use as all the output data should be saved anyway.