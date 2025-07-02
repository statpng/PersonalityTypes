# PersonalityTypes

Please see the `main.R` file.

- This begins with preprocessing raw survey data and performing Exploratory Factor Analysis (EFA) to derive latent factor scores. 
- It then applies Gaussian Mixture Models (GMM) to these scores, using the Normalized Entropy Criterion (NEC) and Normalized Variation of Information (NVI) to identify the optimal and most stable cluster solution, representing distinct 
personality types.
- It also includes methods for validating cluster robustness via permutation-based enrichment testing.
- Finally, it facilitates the interpretation of these types by linking them to external outcome variables through Compositional Regression, which correctly handles cluster membership probabilities as compositional predictors.
- A suite of specialized visualization functions is provided for each stage of the analysis.
