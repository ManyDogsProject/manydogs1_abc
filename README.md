# ManyDogs 1: A Multi-Lab Replication Study of Dogs’ Pointing Comprehension

This repository provides the reproducible research materials for the [ManyDogs1 Project](http://manydogs.org) that investigates dogs' understanding of human pointing gestures. This includes the following:

-   Data
-   R Markdown file with analyses embedded

Created on 2023-05-02 by Jeffrey R. Stevens (<jeffrey.r.stevens@gmail.com>)

Finalized on 2023-05-17

[GitHub repository](https://github.com/ManyDogsProject/manydogs1_abc)

[Open Science Framework Project](https://osf.io/9r5xf/)


## Citation

If you use any of these materials, please cite:

ManyDogs Project, Espinosa, J., Stevens, J.R., Alberghina, D., Alway, H.E.E., Barela, J.D., Bogese, M., Bray, E.E., Buchsbaum, D. Byosiere, S.-E., Byrne, M., Cavalli, C. M., Chaudoir, L. M., Collins-Pisano, C., DeBoer, H.J., Douglas L.E.L.C., Dror, S., Dzik, M.V., Ferguson, B., Fisher, L., Fitzpatrick, H.C., Freeman, M.S., Frinton, S.N., Glover, M.K., Gnanadesikan, G.E., Goacher, J.E.P., Golańska, M., Guran, C.-N.A., Hare, E., Hare, B. Hickey, M., Horschler, D.J., Huber, L., Jim, H.-L., Johnston, A.M., Kaminski, J. Kelly, D.M., Kuhlmeier, V.A., Lassiter, L., Lazarowski, L., Leighton-Birch, J., MacLean, E.L., Maliszewska, K., Marra, V., Montgomery, L.I., Murray, M.S., Nelson, E.K., Ostojić, L., Palermo, S.G., Parks Russell, A.E., Pelgrim, M.H., Pellowe, S.D., Reinholz, A., Rial, L.A., Richards, E.M., Ross, M.A., Rothkoff, L.G., Salomons, H., Sanger, J.K., Santos, L., Shirle, A.R., Shearer, S.J., Silver, Z.A., Silverman, J.M., Sommese, A., Srdoc, T., St. John-Mosse, H., Vega, A.C., Vékony, K., Völter, C.J., Walsh C.J., Worth, Y.A., Zipperling, L.M.I., Żołędziewska, B., & Zylberfuden, S.G. (forthcoming). ManyDogs 1: A multi-lab replication study of dogs’ pointing comprehension. Manuscript accepted for publication in _Animal Behavior and Cognition_. [doi:10.31234/osf.io/f86jq](https://doi.org/10.31234/osf.io/f86jq)


## Summary

In this project, pet dogs voluntarily participated in a two-alternative object choice task with ostensive and non-ostensive experimental conditions, along with warm-up (one cup, two cup) trials and an odor control condition. We collected pilot data from one site and full data from 20 sites. One data file (`md1_data_pilot.csv`) contains records from 61 dogs tested for the pilot experiment between 2020-08-19 and 2020-09-30. The second data file (`md1_data.csv`) contains records from 704 dogs tested for the main experiment between 2022-01-20 and 2023-01-23. For both data sets, each row represents a trial for an individual dog. In addition, there are two files that include genetic data for the heritability analysis. `md1_ibs_matrix.txt` includes the genome-wide identity by state matrix for dogs published in Parker et al., 2017, _Cell Reports_ (<https://doi.org/10.1016/j.celrep.2017.03.079>), and `md1_ibs_matrix_ids.txt` gives the animal identifiers for rows and columns of `md1_ibs_matrix.txt`.


## Files

### Data files

`md1_data_pilot.csv`

* subjectID - Subject ID
* purebred - Whether dog is purebred (No or Yes)
* breed - Dog breed of subject
* age - Dog age in years
* sex - Dog sex (F = Female, M = Male)
* desexed - Neuter status (Yes = spayed/neutered, No = intact)
* training - Mean response to training section of CBARQ questionnaire
* condition_order - Test condition experience first
* condition - Condition for trial
* trial - Trial number
* location - Side of treat location (L = left, R = right)
* correct - Choice of treat location (0 = incorrect, 1 = correct)

`md1_data.csv`

* site - Abbreviation for testing site
* handler_bias - Whether site had the possibility of handler bias (No or Yes)
* subjectID - Subject ID
* status - Status of subject (Included = included in analysis, Error = excluded due to experimenter error, Incomplete = excluded due to not completing all trials)
* purebred - Whether dog is purebred (Yes or No)
* breed - Dog breed of subject
* breed_group - 
* age - Dog age in years
* sex - Dog sex (F = Female, M = Male)
* desexed - Neuter status (Yes = spayed/neutered, No = intact)
* owner_status - 
* training - Mean response to training section of CBARQ questionnaire
* condition_order - Test condition experience first
* condition - Condition for trial
* trial - Trial number
* valid_trial - Trial number for completed trials (with choices---NA = no-choice)
* location - Side of treat location (L = left, R = right)
* choice - Side of choice (L = left, R = right, NC = no-choice)
* correct - Choice of treat location (0 = incorrect, 1 = correct, NA = no-choice)
* recoded_choice - Choice of treat location for trials recoded for inter-rater reliability


### R Markdown document

`md1_analysis.Rmd` - R Markdown document with R code embedded for main manuscript


### R script

`md1_functions.R` - R script with functions borrowed from [{apastat} package](https://github.com/JeffreyRStevens/apastat) and generously provided by Roger Mundry.


### Installation

To reproduce these results, first clone or unzip the Git repository into a folder. Then, ensure that subfolders named "figures" and "results" are in the folder. Next, open `md1_analysis.Rmd` in [RStudio](https://rstudio.com) or another R interface and ensure that all packages mentioned at the top of the document are installed, along with [{knitr}](https://yihui.org/knitr/) and [{rmarkdown}](https://rmarkdown.rstudio.com/). Once all packages are installed, use {knitr} to render the R Markdown document (control-shift-K).

Note: Compiling the full R Markdown document takes a long time (12 hours or more depending on your computer processor speed and number of cores). The current version of the document runs quickly (less than a minute) using already-calculated results for regression analyses, Bayesian analyses, bootstrapped confidence intervals, and model stability analyses. To run this version, download `md1_analyses_rr.zip` and unzip all files in this repository (including figures and results directories) before knitting the R Markdown document `md1_analyses.Rmd`. To run the full version, replace all instances of `eval = FALSE` in the R Markdown document with `eval = TRUE` before knitting.


## License

All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.

No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.


# Data Set Metadata

The following table is necessary for this dataset to be indexed by search
engines such as <a href="https://g.co/datasetsearch">Google Dataset Search</a>.
<div itemscope itemtype="http://schema.org/Dataset">
<table>
<tr>
<th>property</th>
<th>value</th>
</tr>
<tr>
<td>name</td>
<td><code itemprop="name">ManyDogs 1: A multi-lab replication study of dogs’ pointing comprehension</code></td>
</tr>
<tr>
<td>description</td>
<td><code itemprop="description">The dataset from the paper <a href="https://doi.org/10.31234/osf.io/f86jq">ManyDogs 1: A multi-lab replication study of dogs’ pointing comprehension</a>. In this project, pet dogs voluntarily participated in a two-alternative object choice task with ostensive and non-ostensive experimental conditions, along with warm-up (one cup, two cup) trials and an odor control condition. We collected pilot data from one site and full data from 20 sites. One data file (md1_data_pilot.csv) contains records from 61 dogs tested for the pilot experiment between 2020-08-19 and 2020-09-30. The second data file (md1_data.csv) contains records from 704 dogs tested for the main experiment between 2022-01-20 and 2023-01-23. For both data sets, each row represents a trial for an individual dog. In addition, there are two files that include genetic data for the heritability analysis. md1_ibs_matrix.txt includes the genome-wide identity by state matrix for dogs published in Parker et al., 2017, <i>Cell Reports</i> (<a href="https://doi.org/10.1016/j.celrep.2017.03.079">https://doi.org/10.1016/j.celrep.2017.03.079</a>), and md1_ibs_matrix_ids.txt gives the animal identifiers for rows and columns of md1_ibs_matrix.txt.</code></td>
</tr>
</tr>
<tr>
<td>url</td>
<td><code itemprop="url">https://github.com/ManyDogsProject/manydogs1_abc</code></td>
</tr>
<tr>
<td>sameAs</td>
<td><code itemprop="sameAs">https://github.com/ManyDogsProject/manydogs1_abc</code></td>
</tr>
<tr>
<td>citation</td>
<td><code itemprop="citation">https://doi.org/10.31234/osf.io/f86jq</code></td>
</tr>
<tr>
<td>license</td>
<td>
<div itemscope itemtype="http://schema.org/CreativeWork" itemprop="license">
<table>
<tr>
<th>property</th>
<th>value</th>
</tr>
<tr>
<td>name</td>
<td><code itemprop="name">CC BY-SA 4.0</code></td>
</tr>
<tr>
<td>url</td>
<td><code itemprop="url">https://creativecommons.org/licenses/by-sa/4.0/</code></td>
</tr>
</table>
</div>
</td>
</tr>
</table>
</div>