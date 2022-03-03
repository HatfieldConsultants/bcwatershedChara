
bcwatershedCharac
============================

### Usage

  This is an [R application](https://shiny.rstudio.com/) for British Columbia Watershed Characterization. Follow these steps to install and initialize this application:  

  + Clone or download the repository
  + Double-click `bcwatershedchara.Rproj` to open the project in an R IDE, e.g. RStudio  
  + Use [renv](https://www.rstudio.com/blog/renv-project-environments-for-r/) to install all dependent packages by typing these lines in your Console:  
    
```{r}
install.packages("renv")  # renv is a new effort to bring project-local R dependency management to your projects 
library(renv)             # please read the renv(link) above for details (optional)
renv::restore()           # this may take a while
```
    
  + Double-click the `app.R` to open this file
  + Click the green arrow `Run App` button to load the application

#### Example

  Besides clicking the `Run App` button, you can also initialize the application from Console.

```{r}
library(shiny)
runApp()
```

  After initializing the application, you will see a **User Guide** on the third page of this application.


#### Maintain and update data instruction
  
  See `/maintain/How-to-maintain-this-app.html`. 


### Project Status

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/bcwatershedCharac/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
