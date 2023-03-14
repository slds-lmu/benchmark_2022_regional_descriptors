# Benchmarking repository of interpretable regional descriptors

## Structure

### 1) Data (data/)

- Loads the required datasets from OpenML: https://www.openml.org/
- Stores the datasets and the `x_interest` as lists in `.rds` files
- Main functions: `get_data.R`

### 2) Models (models/)

- Trains, tunes, and stores 4 models for each dataset: random forest, linear/logistic/multinomial model, neural network and hyperbox model
- Performs nested resampling (5-fold CV for the inner and outer loop) for estimating the performance of each (tuned) model on each dataset
- The neural network had to be saved differently due to keras (the autotuner could not be saved as usual; the models need to be stored as `.hdf5` files)
- Main functions: `train_models.R`, `resample.R`, `get_resample_results.R`

### 3) Evaluate Data using Model Info (dateval/)

- Creates metainfos that is used as an input for the IRD methods and for the evaluation
- For each model, dataset and x_interest the following is saved: (1) the largest, local box, (2) training data in the largest, local box, (3) sampled data in the largest local box, (4) connected, convex levelset L
- Main functions: `generate_dataeval.R`

### 4) Interpretable Regional Descriptors (ird/)

- Runs the IRD methods for all datasets, models and `x_interest`, and stores the IRDs as `RegDescMethod`s
- Methods: MaxBox, PRIM, Anchors, MAIRE
- Main functions: `find_ird.R`

### 5) Robustness (robustness/) 

- Evaluates the robustness of IRD methods by repeating the box building processes 5 times and computing the robustness measure which is saved in a sql_lite databsae (`db_robustness_x.db`)
- Main functions: `assess_robustness_x.R` and `db_setup_robustness.R`

### 6) Evaluation (evaluation/)

#### 6.1) DB setup

- Reads in the `RegDescMethod` objects, evaluates the irds and stores them in a sql_lite database (`db_evals.db`)
- Main functions: `db_setup.R`

#### 6.2) Analysis

- Creates box plots for comparing the irds of the different methods w.r.t to several evaluation measures
- Creates tables for maximality and efficiency, as well as for statistical tests
- All data are queried from the database `db_evals.db`
- Main functions: `analysis.R`
