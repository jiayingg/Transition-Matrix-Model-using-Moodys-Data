Transition Matrix Model using Moodys' Data
================

### 1. Overview

Shiny Dashboard of this project can be found [here](https://rachelgu.shinyapps.io/zscore_moody/). *Its quite a big project so it takes a while to render. Please be patient.* 😄 😄

**Data source**:

[Moody's Annual Default Study](https://www.moodys.com/researchdocumentcontentpage.aspx?docid=PBC_1059749), includes data of default, loss and rating transition experience of corporate bond, loan and deposit issuers. This study covers financial institutions, corporates and regulated utilities that have long-term debt ratings.

**Methodology**:

[A one-parameter representation of credit risk and transition matrices](https://www.z-riskengine.com/media/1032/a-one-parameter-representation-of-credit-risk-and-transition-matrices.pdf).This paper presents a formal methodology for gauging the credit risk of financial institutions.

### 2. Exploratory Analysis

### 3. Z Score Generation

**Key formula:**

<center>
<img src="https://latex.codecogs.com/gif.latex?X%3D%5Csqrt%5Crho%20Z%20&plus;%20%5Csqrt%7B1-%5Crho%7D%5C%20%5Cepsilon"/>
</center>
<center>
<img src="https://latex.codecogs.com/gif.latex?P%28G%2Cg%7CZ_%7Bt%7D%29%3D%5CPhi%28%5Cfrac%7BX_%7B%28g&plus;1%29%7D%5EG-%5Csqrt%7B%5Crho%7DZ_%7Bt%7D%7D%7B%5Csqrt%7B1-%5Crho%7D%7D%29-%5CPhi%5Cfrac%7B%28X_g%5EG-%5Csqrt%5Crho%20Z_%7Bt%7D%29%7D%7B%5Csqrt%7B1-%5Crho%7D%7D"/>
</center>
**Key function:**

`delta.transition(rho, Zt, cml)`

`rho.iteration(max_itr, seg, error = 3, weight = 1)`

### 4. Model Generation

**Key function:**

`model.select(y, x, n, tol, criterion)`

`Sign.Check_3v(Model.Data)` `Sign.Check_2v(Model.Data)`

### 5. Model Implementation

**Key function:**

`df.backtest(balcnt, zlist, rho, cml)`

`df.scenario(prerate, zlist, rho, cml)`
