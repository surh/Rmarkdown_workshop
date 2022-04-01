params <-
list(data_file = "data/phenotypes.tsv")

#' ---
#' title: "The effect of bacteria on the *A. thaliana* Pi starvation response"
#' author: "Sur Herrera Paredes"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_float: TRUE
#' bibliography: references.bib
#' params:
#'   data_file: "data/phenotypes.tsv"
#' ---
#' 
#' This document was created for the workshop **Data Management for Publishing:
#' Accessibility and Reproducibility**, to be taught on April 23rd, 2022. All
#' resources for the Rmarkdown session of the workshop, plus some additional
#' ones, can be found in the GitHub repository
#' [surh/Rmarkdown_workshop](https://github.com/surh/Rmarkdown_workshop).
#' 
#' The data and code in this document reproduce some of the analysis from
#' @HerreraParedes2018. The full data and code from that publication can be found
#' in the GitHub repository [surh/wheelP](https://github.com/surh/wheelP).
#' 
#' # Setup
#' 
#' The first step is to load the packages and data that we need for analysis.
#' We will use the [tidyverse](https://www.tidyverse.org/) collection of packages.
#' 
#' We do that by creating our first R code chunk, which should start with the
#' line ```` ```{r chunk_name} ````,  followed by any R code, and ends with a line
#' with a triple backtick (```` ``` ````). When we press `Ctrl+Shift+k` in
#' RStudio, any R code within the code chunk will be run and both the
#' code and its output will be rendered nicely into an output file.
#' 
Sys.Date()
## ----packages---------------------------------------------------------------------------------------------------------
library(tidyverse)

#' 
#' Now, we read the data which is in the `data` directory of the
#' [GitHub repository](https://github.com/surh/Rmarkdown_workshop)
#' of the workshop. Since the input file is a tab-delimited file,
#' we use the `read_tsv` function from the `readr` package (which
#' was automatically loaded when we loaded the `tidyverse` package).
#' 
#' After reading the file, we simply print the first few rows to inspect it.
#' 
## ----read_data--------------------------------------------------------------------------------------------------------
Dat <- read_tsv(params$data_file,
                col_types = cols(Pi_content = col_number(),
                                 Elongation = col_number(),
                                 Experiment = col_character(),
                                 EndP = col_character(),
                                 Bacteria = col_character(),
                                 Plate = col_character()))
Dat

#' 
#' This dataset contains `r nrow(Dat)` observations. The observations correspond to
#' a subset of the observations published by @HerreraParedes2018. Specifically,
#' the dataset indicates the shoot phosphate (Pi) content ($\frac{Pi\ mmol }{FW\ mg}$),
#' and main root elongation (cm) measurements of *Arabidopsis thaliana* plants that
#' were germinated in the absence of Pi, and then received concurrent treatments
#' of one of two levels of Pi ($30 \mu M$, $100 \mu M$), and one of three bacterial
#' cocktails (P1P2, I1I2, N2N3) or no bacteria (axenic).
#' 
#' # Sanity checks
#' 
#' When *A .thaliana* plants are starved for Pi, they suppress the elongation
#' of their main root, because in the soils where they live, Pi is located in
#' the topmost layers.
#' 
#' We confirm that we observe this general trend by plotting the `Elongation`
#' & `Pi_content` measurements in a scatter plot. We can use the `ggplot`
#' function from the `ggplot2` package (which also was loaded as part of the
#' `tidyverse` package).
#' 
#' Because there are two Pi conditions, we plot each the observations from
#' each condition in a different color (`col = EndP`), and we add a linear
#' regression line (i.e. a *trendline*) on each group of observations
#' with the `geom_smooth` function. Finally, we set the `theme_classic()`
#' theme which makes for less busy looking plots [^1].
#' 
#' [^1]: The `%>%` symbol in the code below is a pipe (or more formally a 
#' FIFO operator). It is provided by the `maggrittr` package and loaded with the
#' `tidyverse`. If you are familiar with UNIX/Linux systems, it works very similar
#' to the shell `|` operator. Its operation is very simple, it just takes the
#' output of its expression on the left, and it uses that output as the first
#' parameter of the expression on its right.
#' 
nrow(Dat)
## ----plot_phenotypes, fig.cap="Scatter plot of main root elongation (Elongation) and shoot Pi content (Pi_content)."----
Dat %>%
  ggplot(aes(x = Pi_content, y = Elongation, col = EndP)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_classic()

#' 
#' We observe that there are three apparent outliars in the bottom left corner
#' of the plot. It is important to investigate those further, so we sort the
#' dataset by Elongation to investigate further. We can do that with the `arrange`
#' function of the `dplyr` package, which is another one of those packages
#' automagically loaded when loading `tidyverse`. By default, `arrange` sorts
#' things in increasing order.
#' 
## ----explore_outliars-------------------------------------------------------------------------------------------------
Dat %>%
  arrange(Elongation)

#' 
#' When we examine the observation with the three smalles elongation
#' measurements, we note that all come from the same experiment and
#' treatment. It seems likely that something went wrong with those
#' observations, so we should remove them from further analysis.
#' 
#' There is more than one way to do it, but one convenient function is
#' `filter` which is part of the `dplyr` package, and allows us to specify
#' a condition that must be met for rows in our dataset to be kept, everything
#' else is removed.
#' 
## ----remove_outliars--------------------------------------------------------------------------------------------------
Dat <- Dat %>%
  filter(Elongation > 2)

#' 
#' After removing outliars, w have `r nrow(Dat)` observations (i.e. rows) in the
#' dataset.
#' 
#' It is always a good idea to check that the overall trends didn't change
#' with the removal of the outliars (and after any type of subsetting). So we
#' can make the same plot again with the modified dataset. This time,
#' we also save the plot to the `p1` variable.
#' 
#' Note that if we just copy the chunk from the first scatter plot (with the
#' outliars), we would get an error because the chunk names would be the same.
#' So we need to change the name to something unique before rendering the
#' RMarkdown file.
#' 
nrow(Dat)
## ----plot_phenotypes2, fig.cap="Scatter plot of main root elongation (Elongation) and shoot Pi content (Pi_content) with outliars removed."----
p1 <- Dat %>%
  ggplot(aes(x = Pi_content, y = Elongation, col = EndP)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_classic()
p1

#' 
#' Overall, the trends remain the same after we removed the outliars, which
#' was expected.
#' 
#' ## Is the association statiscally supported?
#' 
#' The plots above show that the relationship between plant shoot Pi content
#' and main root elongation, follow the expected trend, but how strong is the
#' relationship? and is it statistically supported?
#' 
#' We can quantify the correlation with the `cor` base R function, and
#' obtain a *p*-value with the `cor.test()` function which outputs a list
#' with an element named `p.value`.
#' 
## ----cor_pval, echo = FALSE-------------------------------------------------------------------------------------------
r <- cor(Dat$Pi_content, Dat$Elongation)
r.pval <- cor.test(Dat$Pi_content, Dat$Elongation)$p.value

#' 
#' With these tools, we calculate that the overall correlation between the
#' Pi content and root elongation phenotypes
#' is **`r cor(Dat$Pi_content, Dat$Elongation)`** which has a *p*-value
#' of `r cor.test(Dat$Pi_content, Dat$Elongation)$p.value` which is
#' `r ifelse(r.pval < 0.05, "", "not")` significant.
#' 
#' However, we want to calculate the correlation independently for each
#' condition.
#' 
#' * The correlation between the Pi content and root elongation in
#' the $30 \mu M$ Pi condition is 
#' **`r cor(Dat$Pi_content[ Dat$EndP == "30 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "30 uM,0%Suc" ])`**
#' which has a *p*-value of
#' `r cor.test(Dat$Pi_content[ Dat$EndP == "30 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "30 uM,0%Suc" ])$p.value`.
#' 
#' * The correlation between the Pi content and root elongation in
#' the $100 \mu M$ Pi condition is 
#' **`r cor(Dat$Pi_content[ Dat$EndP == "100 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "100 uM,0%Suc" ])`**
#' which has a *p*-value of
#' `r cor.test(Dat$Pi_content[ Dat$EndP == "100 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "100 uM,0%Suc" ])$p.value`.
#' 
#' Thus the effect correlation of between Pi starvation & main root elongation
#' is stronger when the Pi starvation is more severe.
#' 
#' ### Controling for covariates
#' 
#' While we compared the association between the plant phenotypes in two
#' conditions, the dataset contains another variable, which is the bacterial
#' cocktail (`Bacteria`) added to the plants, so a full analysis must consider
#' the effect of the bacterial treatment, as well as the experimental variation
#' between replicates.
#' 
#' Without going into details, a first approximation to this problem [^2] can
#' be achieved with the base R `lm` function, which allows us to fit a linear
#' model that attempts to explain the variation in main root elongation,
#' as a function of all the other variables (biological & technical).
#' 
#' The statistical model is defined by a `formula` object, which we save
#' to a variable `f1`. After fitting the model we display the results
#' with the R base function `summary`.
#' 
#' [^2]: A more complete and appropriate treatment would consider the full
#' dataset published by @HerreraParedes2018.
#' 
cor(Dat$Pi_content, Dat$Elongation)
cor.test(Dat$Pi_content, Dat$Elongation)$p.value
ifelse(r.pval < 0.05, "", "not")
cor(Dat$Pi_content[ Dat$EndP == "30 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "30 uM,0%Suc" ])
cor.test(Dat$Pi_content[ Dat$EndP == "30 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "30 uM,0%Suc" ])$p.value
cor(Dat$Pi_content[ Dat$EndP == "100 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "100 uM,0%Suc" ])
cor.test(Dat$Pi_content[ Dat$EndP == "100 uM,0%Suc" ], Dat$Elongation[ Dat$EndP == "100 uM,0%Suc" ])$p.value
## ----full_model-------------------------------------------------------------------------------------------------------
f1 <- Elongation ~ Pi_content + Bacteria + EndP + Experiment
m1 <- lm(f1, data = Dat)
summary(m1)

#' 
#' Here we can see that there are statistically signigicant (i.e. *p*-value < 0.05)
#' effects for the different bacterial treatments and for some experiments.
#' However there no effect of the Pi concentration in the media at the
#' second part of the experiment (EndP), and it seems like the shoot Pi content
#' is also not statistically significant [^3].
#' 
#' [^3]: Part of the lack of significance has to do with the size of the dataset
#' and the aggregation level of the data in this example. In the original
#' publication, there are many more observations and the measurements are less
#' aggregated.
#' 
#' Given the small sample size, models with a lot of parameters tend to
#' suffer of something called *over-fitting* which basically means that
#' the model confuses noise for signal. So in general, it is a good
#' practice to check if a simpler model can do an equally good job at
#' explaining your data.
#' 
#' We can thus check whether a model without the `EndP` variable, which had
#' the largest *p*-value in the previous model. To do that, we change the formula
#' by removing that variable, fit the new (`m2`) model and then we perform a
#' Likelihood Ratio Test (LRT) with the base R `anova` function. You can do
#' all this much more simply with the `lmtest` package, but we are restricting
#' ourselves here to base functions.
#' 
## ----nested_model-----------------------------------------------------------------------------------------------------
f2 <- Elongation ~ Pi_content + Bacteria + Experiment
m2 <- lm(f2, data = Dat)
anova(m1, m2, test = "LRT")

#' 
#' The *p*-value in the output above is clearly above 0.05 and thus it indicates
#' clearly that there is no significant difference in explanatory power
#' between the models. Thus we select the simpler one (`m2`) and we
#' display the results with the base R `summary` function.
#' 
## ----model_summary----------------------------------------------------------------------------------------------------
summary(m2)

#' 
#' The output above now shows that shoot Pi content does have a significant
#' effect on main root elongation as we expected, thus even after accounting for
#' other sources of variation, the relationship between these two plant
#' phenotypes remains.
#' 
#' The output of summary contains a lot of useful information, but sometimes
#' we are interested just in the table of coeffients and *p*-values, which
#' is what is typically reported in scientific publications. We could
#' simply `summary(m2)$coefficients` which access the `coefficients` table
#' produced by the `summary` function, but that still displays the
#' results as R output which is not necessarily the most easy to read or export.
#' 
#' We could also save the resutls to a file with functions like `write_tsv`
#' or `write_csv` of the readr package (or `write.table` from base R), but then
#' we lose the advantage of displaying code and output together.
#' 
#' Instead, we can convert any matrix or data.frame into a Markdown formatted
#' table with the `kable` function of the knitr package. Since we haven't
#' loaded this package we can access the function with the `package::function`
#' syntax which tells R in which package (or more technically namespace) a
#' function is. For the `kable` function to work we also need to set the
#' `results = "asis"` option in the code chunk options. Finally we can
#' add a caption with the `caption` option of the `kable` function.
#' 
## ----coef_table, echo = FALSE, results = "asis"-----------------------------------------------------------------------
knitr::kable(summary(m2)$coefficients,
             caption = paste("Coefficients of linear model (",
                             format(f2), ").",
                             "We observe that shoot Pi content has no significant",
                             "effect on main root elongation."))

#' 
#' # Summary
#' 
#' As conclusions from our analysis we have that:
#' 
#' 1. There is an effect of shot Pi content in main root elongation. This effect
#' seems to be stronger in more Pi limiting conditions but that difference is not
#' statistically significant.
#' 
#' 2. The specific bacterial cocktail used has the strongest effect on main
#' root elongation.
#' 
#' 3. In general, the presence of bacteria decreases main root elongation,
#' which is consistent with the published analysis by @HerreraParedes2018.
#' 
#' We can illustrate the first conclusion with the earlier scatter plot
#' between Pi content & root elongation (saved to `p1`), and create
#' a boxplot for conclusion 2. For conclusion 3, it would be nice to
#' include an external figure from the original publication. 
#' 
#' We can add a figure from an image file anywhere outside outside of a code
#' chunk with the syntax `![catpiton](path/to/image/file)`, but we lose some of
#' the ability to control its appearance. Instead, we can display the image
#' from a code chunk with the `include_graphics` function from the knitr package.
#' 
#' Finally, we can display multiple images side by side if we add the code
#' chunk option `fig.show="hold"` and we play with the code chunk option
#' `out.width`.
#' 
#' With this tools we can now make the final plot and display the three images
#' that illustrate our conclusions.
#' 
## ----  out.width="33%", fig.show='hold',fig.cap="**Left**: Correlation between shoot Pi content and main root elongation in different environmental Pi levels. **Center**: Distribution of main root elongation measurements stratified by bacterial treatment. Color indicates corresponding shoot Pi content. **Right**: figure 3C from @HerreraParedes2018 showing confirming that most bacterial cocktails decrease main root elongation."----
p1
Dat %>%
  ggplot(aes(x = Bacteria, y = Elongation)) +
  geom_boxplot(outlier.color = NA) +
  geom_point(aes(fill = Pi_content),
             position = position_jitter(width = 0.1),
             shape = 21, size = 3) +
  scale_fill_gradient2(low = "magenta",
                       mid = "white", high = "green",
                       midpoint = 10) +
  theme_classic()
knitr::include_graphics("figs/Fig3C.jpeg")

#' # Session Info
#' 
## ---------------------------------------------------------------------------------------------------------------------
sessionInfo()

#' 
#' 
#' 
#' # References
#' 
#' 
#' 
#' 
#' 
