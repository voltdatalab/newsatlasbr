# newsatlasbr

`newsatlasbr` is a package created to give easier access to the datasets of the [News Atlas](https://www.atlas.jor.br/english/) project, which is an initiative that researches and maps news organizations in the Brazilian territory.

The package facilitates data extraction from the project [open-data API](https://www.atlas.jor.br/plataforma/api/), developed by [Volt Data Lab](https://www.voltdata.info/en-lg).

## Installing and loading the package

Currently, the `newsatlasbr` package can be installed directly from its GitHub repository:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("voltdatalab/newsatlasbr")
```

Once installed, it can be loaded using the `library` function.

```r
library(newsatlasbr)
```

## Functions

`newsatlasbr` has three groups of functions:

* Sign-in to the API to retrieve a token;
* Get aggregate data from municipalities, states or regions and map those data;
* Get data on the news organizations.

For a complete overview of those groups, users can check out tutorials in [English](https://github.com/voltdatalab/newsatlasbr/blob/master/vignettes/Introduction.md) and [Portuguese](https://github.com/voltdatalab/newsatlasbr/blob/master/vignettes/Introducao.md).

To access News Atlas' API, users [should be registered](https://api.atlas.jor.br/login)  and insert their e-mail and password for authentication. `atlasbr_signin` function registers these information on the current R session, so that it can be used to obtain the Bearer token to extract data using the API.

As noted, the function sets the e-mail and password in the current R environment. Thus, users should repeat this operation on every new R session in which they intend to use the `newsatlasbr` package. *We stress that user and password are personal. Therefore, users should be careful when writing and saving them in R scripts, in order to avoid sharing these information*.

Two functions allows the users to extract data on every Brazilian municipality (`get_municipalities()`) and every news organizations in the country (`organizations_state(state = "all")`). Users should note, however, that there can be more efficient options depending on the request they wish to make. We reccomend checking the tutorials to learn about them.

## Authors

[Lucas Gelape](https://github.com/lgelape), for [Volt Data Lab](https://www.voltdata.info/en-lg).

## Contributors

[Sérgio Spagnuolo](https://github.com/sergiospagnuolo) and [Renata Hirota](https://github.com/rmhirota).
