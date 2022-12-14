affiliations_example
================
Hugo Fitipaldi
2022-05-06

### Retrieving country of affiliations with the affiliation toolset

In order to start retrieving affiliations from studies published in
PUBMED, there are two ways to load the functions that will be needed.
The first is by downloading this repository and using the command

    source("affiliation_functions.R")

The other is by installing the development version of our
<a href = https://github.com/hugofitipaldi/affiliation> affiliation
package</a>:

    install.packages("remotes")
    remotes::install_github("hugofitipaldi/affiliation")

For future reference, while the code in the `hugofitipaldi/affiliation`
repository will continually evolve, the one contained here won’t suffer
any major changes.

#### Example (publications \> 2014)

``` r
result <- get_affiliations(30297969)
head(result)
```

    ##   author_lastname author_firstname
    ## 1         Mahajan           Anubha
    ## 2          Taliun           Daniel
    ## 3         Thurner         Matthias
    ## 4       Robertson           Neil R
    ## 5          Torres          Jason M
    ## 6          Rayner        N William
    ##                               country_of_affiliation
    ## 1             United Kingdom_United Kingdom_NA_NA_NA
    ## 2                          United States_NA_NA_NA_NA
    ## 3             United Kingdom_United Kingdom_NA_NA_NA
    ## 4             United Kingdom_United Kingdom_NA_NA_NA
    ## 5                         United Kingdom_NA_NA_NA_NA
    ## 6 United Kingdom_United Kingdom_United Kingdom_NA_NA
    ##                                                                                                                                                                                                                                                                                                     affiliation_freetext
    ## 1                              Wellcome Centre for Human Genetics, Nuffield Department of Medicine, University of Oxford, Oxford, UK. anubha@well.ox.ac.uk._Oxford Centre for Diabetes, Endocrinology and Metabolism, Radcliffe Department of Medicine, University of Oxford, Oxford, UK. anubha@well.ox.ac.uk._NA_NA_NA
    ## 2                                                                                                                                                                                               Department of Biostatistics and Center for Statistical Genetics, University of Michigan, Ann Arbor, MI, USA._NA_NA_NA_NA
    ## 3                                                                          Wellcome Centre for Human Genetics, Nuffield Department of Medicine, University of Oxford, Oxford, UK._Oxford Centre for Diabetes, Endocrinology and Metabolism, Radcliffe Department of Medicine, University of Oxford, Oxford, UK._NA_NA_NA
    ## 4                                                                          Wellcome Centre for Human Genetics, Nuffield Department of Medicine, University of Oxford, Oxford, UK._Oxford Centre for Diabetes, Endocrinology and Metabolism, Radcliffe Department of Medicine, University of Oxford, Oxford, UK._NA_NA_NA
    ## 5                                                                                                                                                                                                     Wellcome Centre for Human Genetics, Nuffield Department of Medicine, University of Oxford, Oxford, UK._NA_NA_NA_NA
    ## 6 Wellcome Centre for Human Genetics, Nuffield Department of Medicine, University of Oxford, Oxford, UK._Oxford Centre for Diabetes, Endocrinology and Metabolism, Radcliffe Department of Medicine, University of Oxford, Oxford, UK._Department of Human Genetics, Wellcome Trust Sanger Institute, Hinxton, UK._NA_NA

``` r
split_into_multiple <- function(column, pattern = ", ", into_prefix){
  cols <- str_split_fixed(column, pattern, n = Inf)
  cols[which(cols == "")] <- NA
  cols <- tibble::as_tibble(cols)
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}

n_result <- nrow(result)

result_tb <- result %>% 
  mutate(author_fullname = paste0(author_firstname, " ", author_lastname)) %>%
  bind_cols(split_into_multiple(.$country_of_affiliation, "_", "country_of_affiliation")) %>% 
  # selecting those that start with 'type_' will remove the original 'type' column
  select(c(author_fullname, affiliation_freetext), starts_with("country_of_affiliation_"))  %>%
  gather(key, country_name, -c(author_fullname, affiliation_freetext), na.rm = T)
```

``` r
# A few snippets of code that might help to find any errors 
result_tb %>%
  filter(str_count(country_name, " ") > 2)

result_tb[str_detect(result_tb$country_name, ", "),]
```

``` r
library(ggflags)
library(countrycode)
library(ggplot2)

freq_tbl <- result_tb %>%
  filter(country_name != "NA") %>%
  add_count(author_fullname, name = 'name_occurence') %>%
  mutate(country_weighted = 1/name_occurence) %>%
  group_by(country_name) %>%
  dplyr::summarise(prop_country = sum(country_weighted)/n_result * 100) %>%
  ungroup()

freq_tbl$ISO2 <- countrycode(freq_tbl$country_name, origin = 'country.name', destination = 'iso2c')

freq_tbl %>%
  mutate(rank = min_rank(desc(prop_country)), Country_Rank = paste(rank, ":", country_name)) %>%
  ggplot(aes(x = reorder(Country_Rank, prop_country), y = prop_country)) +
  geom_bar(stat="identity", fill="dimgray") +
  labs(title = "", subtitle = "", x = "", y = "",
       caption = "") +
  theme_linedraw() +
  geom_flag(y = -1, aes(country = tolower(ISO2)), size = 7) +
  scale_y_continuous(expand = c(0.02, 1.2)) +
  geom_text(aes(reorder(Country_Rank, prop_country), prop_country + 1.5, label = paste0(sprintf("%2.2f", prop_country), "%")), 
            position = position_dodge(width = 1)) + 
  coord_flip() +
  theme(axis.text.x= element_text(colour = "black",  size = 10),
        axis.text.y = element_text(colour = "black",  size = 12),
        strip.text = element_text(size = 16)) 
```

![](affiliation_example_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The resulting dataframe has the surname and name of each author, the
predicted countries of affiliation and the original affiliation
free-text string from PUBMED.

#### Example (publications \< 2014 & studies with non-indexed affiliations \> 2014)

For studies before 2014 (PubMed did not include all author affiliation
information in their metadata) and for a few studies \> 2014 with
non-indexed affiliations, the above function won’t work properly. The
following wrapper text-mining function attempts to fill this gap
extracting information using the standard author/affiliation PMC PubMed
Central notation.

Authors’ names followed by numbers of affiliation and the numbered list
of affiliation must be stored in two separate objects as follow:

``` r
authors_names <- "Dongmei Yu,1,2 Jae Hoon Sul,3,4 Fotis Tsetsos,5,6 Muhammad S. Nawaz,7 Alden Y. Huang,3,4,8 Ivette Zelaya,3,4,8 Cornelia Illmann,1 Lisa Osiecki,1 Sabrina M. Darrow,9 Matthew E. Hirschtritt,10 Erica Greenberg,11 Kirsten R. Muller-Vahl,12 Manfred Stuhrmann,13 Yves Dion,14 Guy Rouleau,15 Harald Aschauer,16,17 Mara Stamenkovic,16 Monika Schlögelhofer, MA,17 Paul Sandor,18 Cathy L. Barr,19 Marco Grados,20 Harvey S. Singer,20 Markus M. Nöthen,21 Johannes Hebebrand,22 Anke Hinney,22 Robert A. King,23,24 Thomas V. Fernandez,23,24 Csaba Barta,25 Zsanett Tarnok,26 Peter Nagy,26 Christel Depienne,27,28 Yulia Worbe,28,29,30 Andreas Hartmann,28,29,30 Cathy L. Budman,31 Renata Rizzo,32 Gholson J. Lyon,33 William M. McMahon,34 James R. Batterson,35 Danielle C. Cath,36,37 Irene A. Malaty,38 Michael S. Okun,38 Cheston Berlin,39 Douglas W. Woods,40,41 Paul C. Lee,42 Joseph Jankovic,43 Mary M. Robertson,44 Donald L. Gilbert,45 Lawrence W. Brown,46 Barbara J. Coffey,47 Andrea Dietrich,48 Pieter J. Hoekstra,48 Samuel Kuperman,49 Samuel H Zinner,50 Pétur Luðvigsson,51 Evald Sæmundsen,52,53 Ólafur Thorarensen,51 Gil Atzmon,54,55,56 Nir Barzilai,54,55 Michael Wagner,57 Rainald Moessner,58 Roel Ophoff,3 Carlos N. Pato,59 Michele T. Pato,59 James A Knowles,59 Joshua L. Roffman,11,60 Jordan W. Smoller, ScD,1,61 Randy L. Buckner,11,60,62,63 Jeremy A. Willsey,10,64 Jay A. Tischfield,65 Gary A. Heiman,65 Hreinn Stefansson,7 Kári Stefansson,7,52 Danielle Posthuma,66 Nancy J. Cox,67 David L. Pauls,1 Nelson B. Freimer,3,4 Benjamin M. Neale,1,2,68 Lea K. Davis,67 Peristera Paschou,6 Giovanni Coppola,3,4 Carol A. Mathews,69 Jeremiah M. Scharf,1,2,70,71"

# It is important to add one space between the affiliation number and the affiliation
affiliation_dict <- "1 Psychiatric and Neurodevelopmental Genetics Unit, Center for Genomic Medicine, Department of Psychiatry, Massachusetts General Hospital, Boston, Massachusetts, USA
2 Stanley Center for Psychiatric Research, Broad Institute of MIT and Harvard, Cambridge, Massachusetts, USA
3 Semel Institute for Neuroscience and Human Behavior, David Geffen School of Medicine, University of California Los Angeles, Los Angeles, California, USA
4 Department of Psychiatry and Biobehavioral Sciences, University of California, Los Angeles, California, USA
5 Department of Molecular Biology and Genetics, Democritus University of Thrace, Xanthi, Greece
6 Department of Biological Sciences, Purdue University, West Lafayette, Indiana, USA
7 deCODE Genetics/Amgen, Reykjavik, Iceland
8 Bioinformatics Interdepartmental Program, University of California, Los Angeles, Los Angeles, California, USA
9 Department of Psychiatry, University of California, San Francisco, San Francisco, California, USA
10 Department of Psychiatry, UCSF Weill Institute for Neurosciences, University of California, San Francisco, San Francisco, California, USA
11 Department of Psychiatry, Massachusetts General Hospital, Boston, Massachusetts, USA
12 Clinic of Psychiatry, Social Psychiatry and Psychotherapy, Hannover Medical School, Hannover, Germany
13 Institute of Human Genetics, Hannover Medical School, Hannover, Germany
14 McGill University Health Center (MUHC), University of Montréal, Centre Universitaire de Santé de Montréal (CHUM), Montreal, Quebec, Canada
15 Montreal Neurological Institute, Department of Neurology and Neurosurgery, McGill University, Montreal, Quebec, Canada
16 Department of Psychiatry and Psychotherapy, Medical University Vienna, Vienna, Austria
17 Biopsychosocial Corporation, Vienna, Austria
18 University Health Network and Youthdale Treatment Centres University of Toronto, Toronto, Ontario, Canada
19 Krembil Research Institute, University Health Network, Hospital for Sick Children, and The University of Toronto, Toronto, Ontario, Canada
20 Johns Hopkins University School of Medicine, Baltimore, Maryland, USA
21 Institute of Human Genetics, University Hospital Bonn, University of Bonn Medical School, Bonn, Germany
22 Department of Child and Adolescent Psychiatry, Psychosomatics and Psychotherapy, University Hospital Essen, University of Duisburg-Essen, Essen, Germany
23 Yale Child Study Center, Yale University School of Medicine, New Haven, Connecticut, USA
24 Department of Psychiatry, Yale University School of Medicine, New Haven, Connecticut, USA
25 Institute of Medical Chemistry, Molecular Biology and Pathobiochemistry, Semmelweis University, Budapest, Hungary
26 Vadaskert Child and Adolescent Psychiatric Hospital, Budapest, Hungary
27 Institute of Human Genetics, University Hospital Essen, University Duisburg-Essen, Essen, Germany
28 Sorbonne Universités, UPMC Université Paris 06, UMR S 1127, CNRS UMR 7225, ICM, Paris, France
29 French Reference Centre for Gilles de la Tourette Syndrome, Groupe Hospitalier Pitié-Salpêtrière, Paris, France
30 Assistance Publique-Hôpitaux de Paris, Department of Neurology, Groupe Hospitalier Pitié-Salpêtrière, Paris, France
31 Zucker School of Medicine at Hofstra/Northwell, Hempstead, New York, USA
32 Neuropsichiatria Infantile. Dipartimento di Medicina Clinica e Sperimentale, Università di Catania, Catania, Italy
33 Stanley Institute for Cognitive Genomics, Cold Spring Harbor Laboratory, Cold Spring Harbor, New York, USA
34 Department of Psychiatry, University of Utah, Salt Lake City, Utah, USA
35 Children’s Mercy Hospital, Kansas City, Missouri, USA
36 Department of Psychiatry, University Medical Center Groningen & Rijksuniversity Groningen, Groningen, the Netherlands
37 Drenthe Mental Health Center, Groningen, the Netherlands
38 Department of Neurology, Fixel Center for Neurological Diseases, McKnight Brain Institute, University of Florida, Gainesville, Florida, USA
39 Pennsylvania State University College of Medicine, Hershey, Pennsylvania, USA
40 Marquette University, Milwaukee, Wisconsin, USA
41 University of Wisconsin-Milwaukee, Milwaukee, Wisconsin, USA
42 Tripler Army Medical Center, University of Hawai’i John A. Burns School of Medicine, Honolulu, Hawaii, USA
43 Parkinson’s Disease Center and Movement Disorders Clinic, Department of Neurology, Baylor College of Medicine, Houston, Texas, USA
44 Division of Psychiatry, Department of Neuropsychiatry, University College London, London, UK
45 Department of Pediatrics, Cincinnati Children’s Hospital Medical Center, Cincinnati, Ohio, USA
46 Children’s Hospital of Philadelphia, Philadelphia, Pennsylvania, USA
47 Department of Psychiatry and Behavioral Sciences, University of Miami Miller School of Medicine, Miami, Florida, USA
48 University of Groningen, University Medical Center Groningen, Department of Child and Adolescent Psychiatry, Groningen, The Netherlands
49 University of Iowa Carver College of Medicine, Iowa City, Iowa, USA
50 Department of Pediatrics, University of Washington, Seattle, Washington, USA
51 Department of Pediatrics, Landspitalinn University Hospital, Reykjavik, Iceland
52 Faculty of Medicine, University of Iceland, Reykjavík, Iceland
53 The State Diagnostic and Counselling Centre, Kópavogur, Iceland
54 Department of Genetics, Albert Einstein College of Medicine, Bronx, New York, USA
55 Department of Medicine, Albert Einstein College of Medicine, Bronx, New York, USA
56 Department of Human Biology, Haifa University, Haifa, Israel
57 Department of Psychiatry and Psychotherapy, University of Bonn, Bonn, Germany
58 Department of Psychiatry and Psychotherapy, University of Tuebingen, Tuebingen, Germany
59 SUNY Downstate Medical Center Brooklyn, New York, USA
60 Athinoula A. Martinos Center for Biomedical Research, Department of Radiology, Massachusetts General Hospital, Charlestown, Massachusetts, USA
61 Department of Epidemiology, Harvard T. H. Chan School of Public Health, Boston, Massachusetts, USA
62 Center for Brain Science, Harvard University, Cambridge, Massachusetts, USA
63 Department of Psychology, Harvard University, Cambridge, Massachusetts, USA
64 Institute for Neurodegenerative Diseases, UCSF Weill Institute for Neurosciences, University of California San Francisco, San Francisco, California, USA
65 Department of Genetics and the Human Genetics Institute of New Jersey, Rutgers, the State University of New Jersey, Piscataway, New Jersey, USA
66 Department of Complex Trait Genetics Center for Neurogenomics and Cognitive Research, VU University Amsterdam, Amsterdam, the Netherlands
67 Division of Genetic Medicine, Vanderbilt Genetics Institute, Vanderbilt University Medical Center, Nashville, Tennessee, USA
68 Analytic and Translational Genetics Unit, Department of Medicine, Massachusetts General Hospital, Boston, Massachusetts, USA
69 Department of Psychiatry, Genetics Institute, University of Florida, Gainesville, Florida, USA
70 Department of Neurology, Brigham and Women’s Hospital, Boston, Massachusetts, USA
71 Department of Neurology, Massachusetts General Hospital, Boston, Massachusetts, USA" 
```

Next, the objects are used as parameters in the function
`auth_aff_dict`:

``` r
dictionary_example <- auth_aff_dict(authors_names,affiliation_dict) 
head(dictionary_example)
```

    ## # A tibble: 6 × 3
    ## # Groups:   author_fullname [6]
    ##   author_fullname   country_of_affiliation                    affiliation_freet…
    ##   <chr>             <chr>                                     <chr>             
    ## 1 Dongmei Yu        United States_United States               1 Psychiatric and…
    ## 2 Jae Hoon Sul      United States_United States               Semel Institute f…
    ## 3 Fotis Tsetsos     Greece_United States                      Department of Mol…
    ## 4 Muhammad S. Nawaz Iceland                                   deCODE Genetics/A…
    ## 5 Alden Y. Huang    United States_United States_United States Semel Institute f…
    ## 6 Ivette Zelaya     United States_United States_United States Semel Institute f…

Free-text affiliations and identified country of affiliation are
separated by underscores. Each affiliation line pertains to one
geographical location 9one country of affiliation), but a dual result
can be yielded by the function due to similar country names and/or
university names. This error can be easily diagnosed and corrected by
the following few lines of code (it will be later introduced inside the
function definition in the development package `affiliation`):

``` r
# identifying issues (misclassification)
dictionary_example[str_detect(dictionary_example$country_of_affiliation, ", "),]
```

    ## # A tibble: 2 × 3
    ## # Groups:   author_fullname [2]
    ##   author_fullname   country_of_affiliation affiliation_freetext                 
    ##   <chr>             <chr>                  <chr>                                
    ## 1 Jay A. Tischfield Jersey, United States  Department of Genetics and the Human…
    ## 2 Gary A. Heiman    Jersey, United States  Department of Genetics and the Human…

``` r
# Changing missclassification manually
dictionary_example[str_detect(dictionary_example$country_of_affiliation, ", "),]$country_of_affiliation <- "United States"
```

``` r
library(ggflags)
library(countrycode)
library(ggplot2)

n_result <- nrow(dictionary_example)

freq_tbl2 <- dictionary_example %>%
  select(-affiliation_freetext) %>%
  mutate(country_of_affiliation = strsplit(as.character(country_of_affiliation), "_")) %>% 
  unnest(country_of_affiliation) %>%
  add_count(author_fullname, name = 'name_occurence') %>%
  mutate(country_weighted = 1/name_occurence) %>%
  group_by(country_of_affiliation) %>%
  dplyr::summarise(prop_country = sum(country_weighted)/n_result * 100) %>%
  ungroup()

freq_tbl2$ISO2 <- countrycode(freq_tbl2$country_of_affiliation, origin = 'country.name', destination = 'iso2c')

freq_tbl2 %>%
  mutate(rank = min_rank(desc(prop_country)), Country_Rank = paste(rank, ":", country_of_affiliation)) %>%
  ggplot(aes(x = reorder(Country_Rank, prop_country), y = prop_country)) +
  geom_bar(stat="identity", fill="dimgray") +
  labs(title = "", subtitle = "", x = "", y = "",
       caption = "") +
  theme_linedraw() +
  geom_flag(y = -1, aes(country = tolower(ISO2)), size = 7) +
  scale_y_continuous(expand = c(0.02, 1.2)) +
  geom_text(aes(reorder(Country_Rank, prop_country), prop_country + 2.2, label = paste0(sprintf("%2.2f", prop_country), "%")), 
            position = position_dodge(width = 1)) + 
  coord_flip() +
  theme(axis.text.x= element_text(colour = "black",  size = 10),
        axis.text.y = element_text(colour = "black",  size = 12),
        strip.text = element_text(size = 16)) 
```

![](affiliation_example_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
