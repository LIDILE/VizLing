# Visualisation_linguistique

Visualisation_linguistique is a R program to compute and visualise linguistic profiles extracted from texts written by learners of English. Learner-specific Key Performance Indicators (KPI) are computed and displayed in the form of graphical information. Comparisons with control groups are possible.  



## Installation
Software requirements : 
R software packages must be installed. See requirements.txt

1. Download zip file.

2. First copy and unzip this project to new directory on your computer

3. Make sure that the packages listed in *requirements.txt* are installed.

4. Enter the right *project_directory* (Visualisation_linguistique-master default name). Open "main.R" in Visualisation_linguistique-master/codes_R . MODIFY path according to your system : windows or linux.

5. Add to your project directory these java software applications: "stanford-parser-full-2014-01-04" (from https://nlp.stanford.edu/software/lex-parser.shtml) and "stanford-tregex-2014-01-04" (from https://nlp.stanford.edu/software/tregex.html) and you have to use the exact same names as mentionned. Unzip each directory in the same location. Remove the zip files. 

6. Open Visualisation_linguistique-master/stanford-tregex-2014-01-04/tregex.sh. Modify memory allocation: change "java -mx100m ..." to "java -mx1500m ..."

7. Manually create INPUT data directories as explained in architecture.pdf

Now, the project is ready to be used.




## Usage

Input file : zip file from Moodle Assignment activity (file containing texts of the learners)

Place the dowloaded .zip file in /data/zip_from_moodle. The text file versions of all student writings will be created in the *corpusALE* folder. 

In the terminal, go *project_directory*

To process files and compute metrics, type the following command: 

```
Rscript codes_R/main.R
```
The visualisations are created in /feedbacks. One .pdf per student. 

## Change log

10/10/2019 
First integrated version of all scripts. One program runs the pre-processing and metrics computation scripts. 
The visualisation script is run separately. 

18/12/2019
Fully integrated pipeline including text import from Moodle assignment zip file to creating vizualisations in rada mode. 
Simplified metrics and created explanations to read each metric. 

5/02/2020
Updated readme. New *data* folder structure. 


## Credits, License and citations

This program includes an R implementation of L2SCA (make_parsed_files.r) and Stanford CoreNLP. 

- Lu, Xiaofei. 2010. “Automatic Analysis of Syntactic Complexity in Second Language Writing.” International Journal of Corpus Linguistics 15 (4): 474–496.
- Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and David McClosky. 2014. “The Stanford CoreNLP Natural Language Processing Toolkit.” In Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, 55–60. http://acl2014.org/acl2014/.


If you use this program for research purposes, please cite the following paper:
Gaillat, Thomas, and Nicolas Ballier. 2019. “Expérimentation de Feedback Visuel Des Productions Écrites d’apprenants Francophones de l’anglais Sous MOODLE.” In Actes de La Conférence EIAH2019. Paris , France: Association des Technologies de l’Information pour l’Éducation et la Formation.



This program was funded by the DUNE DESIR AMI project from the universities of Rennes, France. It is licensed under CreativeCommons licence.
<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />Ce(tte) œuvre est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Licence Creative Commons Attribution - Pas d’Utilisation Commerciale - Partage dans les Mêmes Conditions 4.0 International</a>.

Authors: Thomas Gaillat, Anas Knefati and Antoine Lafontaine

