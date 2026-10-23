# STAT5003 Football Match Outcome Prediction

This group project investigates how accurately pre-match information can classify football matches as a home win, draw or away win across Europe's top five domestic leagues.

## Models

- Multinomial Logistic Regression
- Linear Discriminant Analysis
- k-Nearest Neighbours
- Support Vector Machine
- Random Forest

All models use the same chronological train/test split, five expanding-window validation folds and Macro-F1 as the primary selection metric.

## Repository structure

- `member1 MLR.qmd` — multinomial logistic regression report
- `Member2_LDA.qmd` — linear discriminant analysis report
- `Member3_kNN.qmd` — k-nearest neighbours report
- `Member4_SVM.qmd` — support vector machine report
- `Member5_Random_Forest.qmd` — random forest report
- `R/` — shared feature engineering, splitting, evaluation and MLR pipeline
- `data/model_data.rds` — derived modelling dataset
- `data/cv_folds.rds` — shared expanding-window folds
- `outputs/` — final model results and comparison tables
- `archive/previous_assignment/` — earlier planning material

## Reproducing the analysis

1. Install R, Quarto and the packages declared in each QMD setup block.
2. Open the repository as the working directory.
3. Render an individual model report, for example:

   ```bash
   quarto render Member3_kNN.qmd
   ```

4. To rebuild the MLR results from source:

   ```bash
   Rscript R/member1_mlr.R
   ```

The repository excludes the raw third-party CSV files. Obtain the Football Data from Transfermarkt dataset from its original source before rebuilding `data/model_data.rds` with `R/build_features.R`.

## Current final-test comparison

Macro-F1 is the primary comparison metric. Based on the saved results, LDA currently has the highest Final Test Macro-F1, followed closely by MLR, kNN and Random Forest.

## Academic integrity

The final submitted report should include the group’s completed APA 7 references and AI Use Statement.
