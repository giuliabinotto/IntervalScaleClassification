# Performance metrics for ordinal and interval scale classification

In the field of supervised machine learning, the precise evaluation of classification models stands as a fundamental pursuit. This necessitates the utilization of robust performance metrics. This repository focuses specifically on the evaluation of classification models within the contexts of ordinal and interval scale classifications. It contains the implementation designed to compute the results expounded upon in the article "Adapting Ordinal Performance Metrics for Interval Scaling: Length Matters" by G. Binotto and R. Delgado (preprint, 2024). Precisely, it facilitates the computation of two ordinal metrics, namely Mean Absolute Error (MAE) and Total Cost (TC), alongside their interval scale counterparts. A specific section is designed to address scenarios in which the rightmost interval is unbounded.


## Requirements

To create the graphs in Simulations.R, the following libraries are needed: ggplot, reshape2, latex2exp and viridis.
To create the graphs in Examples.R, the following libraries are needed: ggplot and latex2exp.

## Structure
- The ```MAE.R``` script computes $MAE$ and normalized $\widehat{MAE}$ metrics.
- The ```TC.R``` script computes $TC$ and normalized $\widehat{TC}$ metrics.
- The ```MAEintervals.R``` and ```TCintervals.R``` scripts compute $\widehat{MAE^{int}}$ and $\widehat{TC^{int}}$ metrics, respectively.
- The ```MAEunbounded.R``` and ```TCunbounded.R``` scripts compute $\widehat{MAE^{int}}$ and $\widehat{TC^{int}}$ metrics, respectively, in the case of unbounded rightmost interval.
- The ```Examples.R``` script presents some examples to test the functions, including the cases proposed in George et al. (2016).
- The ```Simulations.R``` script includes the code to generate the plots of section 4.2.

## Authors
Giulia Binotto & Rosario Delgado (Universitat Autònoma de Barcelona, Spain).

