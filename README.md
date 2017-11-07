Transition Matrix Model using Moodys' Data
================

### Overview

Shiny Dashboard of this project can be found [here](https://rachelgu.shinyapps.io/zscore_moody/). *Its quite a big project so it takes a while to render. Please be patient.* 😄 😄

**Data source**:

[Moody's Annual Default Study](https://www.moodys.com/researchdocumentcontentpage.aspx?docid=PBC_1059749), includes data of default, loss and rating transition experience of corporate bond, loan and deposit issuers. This study covers financial institutions, corporates and regulated utilities that have long-term debt ratings.

**Methodology**:

[A one-parameter representation of credit risk and transition matrices](https://www.z-riskengine.com/media/1032/a-one-parameter-representation-of-credit-risk-and-transition-matrices.pdf).This paper presents a formal methodology for gauging the credit risk of financial institutions.

### Exploratory Analysis

### Z Score Generation

`delta.transition(rho, Zt, cml)`

`rho.iteration(max_itr, seg, error = 3, weight = 1)`

### Model Generation

`model.select(y, x, n, tol, criterion)`

`Sign.Check_3v(Model.Data)` `Sign.Check_2v(Model.Data)`

### Model Implementation

`df.backtest(balcnt, zlist, rho, cml)`

`df.scenario(prerate, zlist, rho, cml)`
