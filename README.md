# JudgmentBench: Analysis and Tables Code

Reproduction code for the paper *JudgmentBench: Comparing Rubric and Preference
Evaluation for Quality Assessment*. Two R scripts regenerate every figure and
table reported in the paper from the released benchmark dataset.

The [JudgmentBench dataset](https://huggingface.co/datasets/judgmentbench/JudgmentBench)
is available on Hugging Face.

## Repository layout

```
.
├── README.md            # this file
├── LICENSE              # MIT
├── requirements.txt     # R packages used by the scripts
├── reproduce.sh         # one-shot reproduction wrapper
├── analysis/
│   └── analysis_vSubmit.R
└── tables/
    └── tables_vSubmit.R
```

## Specification of dependencies

- R ≥ 4.2.0
- The R packages listed in `requirements.txt`. Install them with:

  ```
  Rscript -e 'install.packages(c("dplyr","tidyr","readr","tibble","ggplot2","purrr","ragg","gtable","jsonlite","patchwork","stringr"))'
  ```

  `analysis_vSubmit.R` will install missing packages on the fly (controlled by
  the `install_missing_packages` flag near the top of the script);
  `tables_vSubmit.R` aborts with a clear message if any package is missing.

No GPU, accelerator, proprietary library, or network access is required.

## Data

Download the [JudgmentBench dataset](https://huggingface.co/datasets/judgmentbench/JudgmentBench)
via either of the following, then point `EAH_JB_DIR` at the resulting folder.

**Option A — `git clone` (requires `git-lfs`):**

```
git lfs install
git clone https://huggingface.co/datasets/judgmentbench/JudgmentBench
export EAH_JB_DIR=$(pwd)/JudgmentBench
```

**Option B — `huggingface-cli download`:**

```
pip install huggingface_hub
huggingface-cli download judgmentbench/JudgmentBench \
    --repo-type dataset --local-dir ./result-dataset
export EAH_JB_DIR=$(pwd)/result-dataset
```

Alternatively, place the downloaded folder next to this README and name it
`result-dataset` so the scripts pick it up automatically without setting
`EAH_JB_DIR`.

The scripts expect the released subdirectory layout inside `result-dataset/`:

```
result-dataset/
├── human/
│   ├── annotators.csv
│   ├── assignment_records.csv
│   ├── annotations_rubric.csv
│   ├── annotations_comparative_judgment.csv
│   └── rubric_item_scores.csv
├── autograders/
│   ├── gpt_5_4/
│   │   ├── annotations_rubric.csv
│   │   ├── annotations_comparative_judgment.csv
│   │   └── rubric_item_scores.csv
│   └── gpt_5_4_mini/   (same three files)
├── base/
│   ├── tasks.csv
│   └── rubric_items.csv
└── outputs/
    └── outputs.csv
```

Other files released alongside the dataset (`README.md`, `croissant.json`,
`base/documents.csv`, `human/annotator_experience_summary.csv`, the
per-task `documents/` folder) are not read by the scripts.

## Reproducing the paper

The fastest path is the wrapper script:

```
./reproduce.sh
```

This expects either `./result-dataset/` to exist or `EAH_JB_DIR` to be set,
and writes outputs to `./outputs/`.

Equivalent manual invocation:

```
mkdir -p outputs
export EAH_JB_DIR=$(pwd)/result-dataset
export EAH_ANALYSIS_OUTPUT_DIR=$(pwd)/outputs
export EAH_TABLE_OUTPUT_DIR=$(pwd)/outputs

Rscript analysis/analysis_vSubmit.R     # ~10–15 min on a laptop (B=2000)
Rscript tables/tables_vSubmit.R         # ~30 sec
```

### Mapping of paper artifacts to output files

| Paper artifact                          | Output file                                       | Script                  |
|-----------------------------------------|---------------------------------------------------|-------------------------|
| Figure 1 (main results)                 | `figure1_main_results.png`                        | `analysis_vSubmit.R`    |
| Figure 2 (bootstrap distribution)       | `figure2_bootstrap_distribution.png`              | `analysis_vSubmit.R`    |
| Figure 3 (quality-level score dists.)   | `figure3_quality_level_score_distributions.png`   | `analysis_vSubmit.R`    |
| Figure 4 (experience diagnostic)        | `figure4_experience_diagnostic.png`               | `analysis_vSubmit.R`    |
| Table 1 (sampled tasks by type)         | `table1_sampled_tasks_by_type.tex`                | `tables_vSubmit.R`      |
| Table 2 (benchmark tasks)               | `table2_benchmark_tasks.tex`                      | `tables_vSubmit.R`      |
| Table 3 (annotator information)         | `table3_annotator_information.tex`                | `tables_vSubmit.R`      |
| Table 4 (subgroup recovery)             | `table4_subgroup_recovery.tex`                    | `analysis_vSubmit.R`    |
| Table 5 (adjacent-pair recovery)        | `table5_adjacent_pair_recovery.tex`               | `analysis_vSubmit.R`    |

CSV companions are written alongside each `.tex` file. Diagnostics for the
descriptive tables go to `outputs/diagnostics/`.

## Configuration

All runtime configuration is via environment variables. None are required if
the dataset is at `./result-dataset/`.

| Variable                     | Used by             | Default                    | Purpose                                    |
|------------------------------|---------------------|----------------------------|--------------------------------------------|
| `EAH_JB_DIR`                 | both scripts        | `./result-dataset`         | Path to the released dataset directory.    |
| `EAH_ANALYSIS_OUTPUT_DIR`    | `analysis_vSubmit`  | script directory           | Where the analysis script writes outputs.  |
| `EAH_TABLE_OUTPUT_DIR`       | `tables_vSubmit`    | script directory           | Where the tables script writes outputs.    |
| `EAH_N_BOOT`                 | `analysis_vSubmit`  | `2000`                     | Bootstrap replicates (paper uses 2,000).   |
| `EAH_SAVE_PLOT`              | `analysis_vSubmit`  | `true`                     | Set to `false` to skip writing PNG files.  |
| `EAH_BOOTSTRAP_PLOT_PATH`    | `analysis_vSubmit`  | `<output_dir>/figure2_bootstrap_distribution.png` | Override path for the bootstrap PNG only. |

## Expected run time and resources

A 2,000-replicate bootstrap run on a recent laptop (CPU only, single thread)
takes roughly 10–15 minutes for `analysis_vSubmit.R` and well under a minute
for `tables_vSubmit.R`. Peak memory under 1 GB.

## License

MIT (see `LICENSE`).
