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
