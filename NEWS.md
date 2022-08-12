# englelab v1.1.0

Updated: 12 August, 2022

- Major update

- Added new functions to assist in processing and cleaning data.

- `center()` to calculate z-score or centered scores for a list of variables

- `composiste()` to calculate a composite score for a group of variables. Composites can be calculated using different methods:

    - mean, sum, factor analysis, or principal components
    
- `replace_outliers()` to replace outlier values with missing or some other method (e.g., mean). There is an option to save the outlier values to a data file to store a log of what rows in the data were removed.

- `remove_missing()` to remove subjects with too much missing data on a group of tasks for any given construct. 

- Added some extra columns to the output of `raw_symspan()`, `raw_rotspan()`, and `raw_ospan()`

    - MemoryTargets and Recalled to more easily see and analyze the sequence of memory items
    and the sequence of recalled items on each trial.

    - EditDistance.unit and EditDistance.load calculate the trial scores based on 
    the edit distance scoring method. See Gonthier et al. (2022).
    
    - Got rid of Recall.correct as it was redundant with Partial.load
    
- Added extra columns to the output of `score_symspan()`, `score_rotspan()`, and `score_ospan()`

    - EditDistanceScore, EditDistanceUnit, and EditDistanceLoad for scores based on 
    the edit distance scoring method. See Gonthier et al. (2022).
    
Gonthier, C. (2022). An easy way to improve scoring of memory span tasks: The edit distance, beyond “correct recall in the correct serial position.” Behavior Research Methods, 16. https://doi.org/10.3758/s13428-022-01908-2

# englelab v1.0.2

Updated: 5 May, 2022

- Minor update only

- Updated `score_visualarrays()`. Added a `taskname = ` argument for different types of visual arrays tasks

# englelab v1.0.1

Updated: 12 May, 2021

-   Minor update only

-   Adds PartialScore and AbsoluteScore variables to score\_ functions output for complex-span tasks

# englelab v1.0.0

Updated: 24 February, 2021

-   **This is a major update and will break previous versions** (*my apologies*)

-   **This is the first major release and will hopefully be stable after this**

-   The `raw_` functions work pretty much the same and might not break from previous versions

-   The `score_` functions will break and have been made to be more flexible, such as using them to calculate alternate span scores and reliability estimates.

-   The `score_` functions need to be used with `dplyr::group_by()` and so can be thought of as similar to `dplyr::summarise()`

-   See new Vignettes explaining this all:

    -   [Complex Span Tasks](https://englelab.github.io/englelab/articles/Complex_Span.html)

    -   [Attention Control Tasks](https://englelab.github.io/englelab/articles/Attention_Control.html)
