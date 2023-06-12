# Snakemake workflow: Groundhog Day

[![Snakemake](https://img.shields.io/badge/snakemake-≥6.3.0-brightgreen.svg)](https://snakemake.github.io)
[![Conventional Commits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-%23FE5196?logo=conventionalcommits&logoColor=white)](https://conventionalcommits.org)

A Snakemake workflow for the Groundhog Day project.

## Usage

To run this workflow, type the following instruction in the terminal from the main directory:

```bash
snakemake --cores 1
```

## TODO

* Replace `<owner>` and `<repo>` everywhere in the template (also under .github/workflows) with the correct `<repo>` name and owning user or organization.
* Replace `<name>` with the workflow name (can be the same as `<repo>`).
* Replace `<description>` with a description of what the workflow does.

## Question

Reflecting on the nature of feedback can lead to a deeper understanding and personal growth. Consider the following insights:

1. Feedback, to a certain extent, is independent of us: Recognize that the feedback we receive is influenced by various factors beyond our control. It may come from external sources, subjective perspectives, or changing circumstances.
2. Learning to obtain better feedback: Embrace the opportunity to learn and improve by seeking constructive feedback. By actively seeking different viewpoints and evaluating feedback, we can gain valuable insights and make adjustments to enhance our performance.
3. Adapting to changing rules: Acknowledge that rules and circumstances in the world are subject to change. Being open to learning and adapting to new rules enables us to navigate challenges more effectively and seize opportunities for growth.

In light of these reflections, engaging in mental flexibility becomes crucial. Embracing this practice allows us to be adaptable and resilient in the face of changing feedback and circumstances.

Furthermore, practicing "ego-distancing" exercises can be valuable. Recognize that even if we feel strongly attached to our choices, there may be alternative paths that yield better outcomes. This understanding fosters a mindset that is not overly tied to ego-driven decisions. By cultivating detachment and openness, we can experience greater contentment and happiness in our lives.

## Method

In a PRL project using the EMA approach, participants were instructed to recall the most significant episode of their day and reconsider their choice regarding that particular event. Specifically, they were asked if they would repeat the same action or if they would change it. This introspective process was framed as a PRL task, presenting participants with two options: "repeat the same action" or "change it".

During the initial 15 trials, choosing to "repeat" the action was associated with a reward probability of 0.85 (and a punishment probability of 0.15). Subsequently, between the 16th and 30th trials, opting to "change" the action was rewarded with a probability of 0.85 (and punished with a probability of 0.15).

At the start of each session, participants were asked to rate their current mood on a scale ranging from -50 (indicating a negative mood) to +50 (representing a fantastic mood). This initial rating served as the baseline, which could be positively influenced (in the score provided in each trial) by receiving feedback indicating a correct decision or negatively influenced by feedback indicating an incorrect decision.

Following each choice, participants provided a mood rating specific to that moment in time.

At the conclusion of the session, participants were once again asked to rate their mood, allowing for an assessment of any changes or fluctuations that may have occurred throughout the session.

## Data dictionary

* rank: session coded with 1 for the first notification day, 2 for the second notification day, and so on.
* context_1: rating of the mood at the time of the notification
* control_1: Assessment of the perceived level of control that participants believe they had over the significant event of the day
* post_context_1: rating of the mood at the end of the PRL session
* coin: the endpoint of the mood determined by the feedbacks obtained by the subject, from the starting point of context_1

## Folder structure

This is a personal way of structuring projects built through observation and personal experience that helped me planning and scaling up without getting lost.
```shell
.
├── config.yml
├── data
├── envs
├── LICENSE
├── README.md
├── reports
├── results
├── src
├── start_project.sh
└── workflows
```

## Rationale
- `config.yml`: hand-curated list of external files and parameters required for the project.
- `data`: keep raw and preprocessed data organized.  
- `envs`: conda environments to run the main project and, if necessary, create more.
- `LICENSE`
- `reports`: discuss your insights with a project [webpage](https://miqg.github.io/project_template/intro.html) created with `jupyter-book`.
- `results`: store files and final plots for every experiment.
- `src`: project modules.
- `workflows`: to download, preprocess, and analyze your data.


## Typical workflow
1. Modify `config.yml` to your taste adding variables that could be useful project-wide.
2. Create the workflows to download and preprocess your project's data at `workflows/download/` and `workflows/preprocess`, respectively. Make sure to distinguish between code that can be used project-wide -place it in the project's modules in `src/` and call the functions in your workflow-; or code that is only used specifically for that part of the project -place it in your workflow's `scripts/` subdirectory-.
3. Now, you can analyse your data creating different experiments as subdirectories of `workflows/analyses` that will get inputs from `data/` and will output at `results/your_experiment_name/`.
4. Commit your work, and consider adding README files.
5. Inspect and explore results creating jupyter notebooks at `reports/notebooks/` that can be rendered into static webpages with [`jupyter-book`](https://jupyterbook.org/intro.html). Structure your project's book by modifying `reports/_toc.yml`.
    

## Structure
```shell
.
├── config.yml
├── data
│   ├── prep
│   ├── raw
│   └── references
├── envs
│   └── main.yml
├── LICENSE
├── README.md
├── reports
│   ├── _config.yml
│   ├── images
│   │   └── logo.png
│   ├── notebooks
│   │   ├── example_notebook.md
│   │   ├── intro.md
│   │   └── README.md
│   ├── README.md
│   └── _toc.yml
├── results
│   ├── new_experiment
│   │   ├── files
│   │   │   └── output_example.tsv
│   │   └── plots
│   │       └── output_example.pdf
│   └── README.md
├── src
│   └── python
│       ├── setup.py
│       └── your_project_name
│           └── config.py
├── start_project.sh
└── workflows
    ├── analyses
    │   └── new_experiment
    │       ├── README.md
    │       ├── run_all.sh
    │       ├── scripts
    │       │   └── workflow_step.py
    │       └── snakefile
    ├── download
    │   ├── README.md
    │   ├── run_all.sh
    │   ├── scripts
    │   │   └── workflow_step.py
    │   └── snakefile
    ├── preprocess
    │   ├── README.md
    │   ├── run_all.sh
    │   ├── scripts
    │   │   └── workflow_step.py
    │   └── snakefile
    └── README.md
```
