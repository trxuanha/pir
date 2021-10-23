# Personalized intervention recommendation

A R implementation of Personalized Intervention Recommendation (PIR) in paper "Recommending Personalized Interventions to Increase Employability of Disabled Jobseekers". We use existing implementations of the baseline methods to compare with our method. They are:

* CT, TOT, ST, FT: The existing implementation is located at
https://github.com/susanathey/causalTree.
*  CF: The existing implementation is located at https://github.com/grf-labs/grf.
*  XL, DR: The existing implementation is located at
https://github.com/microsoft/EconML

# Public datasets

Adult census income (ACI). This dataset from Machine Learning Repository contains 48842
records. This dataset is frequently used to perform the analysis of income equality problems.
In our experiment, the goal is to recommend to individuals what they should improve to find
professional jobs. Main attributes used in our experiments are age, education, race, sex,
hours-per-week, workclass, marital-status, relationship, and occupation. The occupation
attribute is binarized to indicate whether an individual has a professional job or not. It is used
as the outcome.

Employee turnover (TO). This dataset has 1129 records about employee turnover. The dataset
has 16 variables describing the characteristics of Russian workers. Main attributes used in our
experiments are age, profession, coach, head gender, grey wage, way, extraversion, independ,
selfcontrol, anxiety, novator, and stag. The stag attribute is used as the outcome. It is the
employment time of workers.

HR (HR). This dataset has 15000 records about employment retention 2. Main attributes used
in our experiments are: satisfaction level, last evaluation, average montly hours, work
accident, salary and time spend company. The attribute of time spend company is used as the
outcome. The three datasets are included in the code appendix (folder input).

# Computing infrastructure

Software used to run experiments:
* R software used to run PIR: R version 3.6.3, pcalg version 2.7.1, Rgraphviz version
2.30.0, hash version 2.2.6.1, tidyr version 1.1.2, xgboost version 1.3.2.1, rpart.plot
version 2.0.9, nnet version 7.3.15, stringr version 1.4.0, graph version 1.64.0.
* R software required for the baseline methods: causalTree, grf version 1.2.0.
* python software required for the baseline methods: econml.
* R software required to process results: survival version 3.2.7, survminer version 0.4.8,
ggpubr version 0.4.0.
* python software required to process results: statistics version 1.0.3.5, scipy version
1.4.1.
* R software required to run experiments in parallel: foreach version 1.5.1, doParallel
version 1.0.16.

Infrastructure used to run experiments:
* OS: Red Hat Enterprise Linux, version 7.8.
* CPU: Intel(R) Xeon(R) Gold 6246 CPU @ 3.30GHz).
* RAM: 16 GB.
# Run experiments

There two options of generating results in the paper for the public datasets:
* To generate results in the paper from existing data (existing generated
recommendations), run shell script GenerateResults. The generated results are located
in folder output/PerformanceEval.
* To generate results in the paper from scratch, run shell script DoExperiments. The
generated results are located in folder output/PerformanceEval.

