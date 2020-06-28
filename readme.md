# rosr: Create academic R markdown projects for open science and reproducible research

## Introduction

'rosr' is an R package for creating reproducible academic project with integrated various academic elements, including data, bibliography, codes, images, manuscripts, dissertations, slides and so on. These elements are well connected so that they can be easily synchronized and updated. Users don't have to repeat copying and pasting their results and figures from time to time. It will be easy for the scientific researchers to use, even if they are R beginners, or even non-R-users.

## Quick start

### Installation

```R
# stable version (not yet):
# install.packages("rosr")
# or development version:
remotes::install_github("pzhaonet/rosr")
```

### For non-R users

'rosr' is friendly to non-R users. RStudio is recommended as an advanced user interface. After installation, click the 'Addins' button and you can find "Create a rosr project". Click the elements you want, and click 'create'

![](https://github.com/rbind/pzhao/raw/master/static/img/rosr-addin.png)

![](https://github.com/rbind/pzhao/raw/master/static/img/rosr-screenshot.png)

### For R users

```R
require('rosr')
create_rosr()
```


# License

Copyright [Peng Zhao](http://pzhao.org).

Released under the GPL-3 license.

