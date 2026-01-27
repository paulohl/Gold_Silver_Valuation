# Gold Silver Valuation
Data-driven financial/economic analysts of the profitability Gold and Silver as investment
# Historical Precious Metals Analytics
_Abstract_

_This repository documents a quantitative and historical analysis of gold and silver price dynamics using macroeconomic, financial, and geopolitical indicators. The project originated as a methodological exercise in statistical analysis using R and evolved into a broader examination of commonly asserted claims regarding silver valuation. The study emphasizes reproducibility, long-horizon analysis, and system-level economic drivers rather than narrative or speculative reasoning._

## Scope and Objectives

The primary objectives of this project are:     

    - To analyze historical gold and silver prices within a macroeconomic framework
    - To identify statistically relevant drivers of precious metals price behavior
    - To evaluate commonly repeated claims about silver valuation against historical data
    - To document methodologies suitable for replication and extension

The scope is descriptive and analytical, not predictive. The project does not attempt to forecast prices or propose trading strategies.

## Research Motivation

Public discourse surrounding silver frequently exhibits a high degree of narrative uniformity and low empirical rigor. Assertions are often presented as self-evident truths, relying on selective historical references, simplified ratios, or unverifiable claims of market manipulation.

This repository was developed to examine silver using the same analytical standards applied to other asset classes, namely:      
- Long-term time series analysis
- Macroeconomic context
- Cross-asset comparison
- Explicit treatment of uncertainty and model limits

The aim is not to refute beliefs, but to evaluate whether such claims are supported by observable historical behavior.

## Analytical Framework

Silver and gold are analyzed as components of a broader economic system rather than as isolated monetary artifacts.

The framework incorporates the following categories:</br>
__Financial Markets__
- Equity indices (S&P 500, NASDAQ in later periods)
- Risk-on / risk-off capital allocation dynamics

__Monetary Variables__
- Nominal and real interest rates
- Inflation and inflation expectations

__Commodities and Energy__
- Broad commodity indices
- Crude oil prices as a transmission variable

__Geopolitical Context__
- Major geopolitical cycles as exogenous shocks
- Emphasis on observable economic effects rather than narrative causality

No variable is treated as deterministically causal in isolation.

  
Repository Structure      
/data        # Raw and cleaned historical datasets     
/src         # R scripts for analysis and visualization     
/notebooks   # Exploratory and explanatory notebooks      
/docs        # Methodology and supporting documentation      


## Limitations
- Correlation is not interpreted as causation
- Structural breaks are explicitly acknowledged
- Data availability constrains early historical resolution
- Results are contingent on chosen indicators and sampling windows

## Status

This repository is under active development. Analytical results, documentation, and visualizations will be expanded iteratively.

## License

[Specify license]

Frequently Repeated Claims vs. Data
Common Claim	Typical Assertion	Empirical Observation	Analytical Outcome
Gold–Silver Ratio	Ratio implies inevitable mean reversion	Ratio varies across regimes and monetary systems	No stable equilibrium ratio observed
Silver Price Suppression	Prices are artificially capped by coordinated actors	Long-term prices correlate with macro drivers	No independent evidence of persistent suppression
Imminent Price Explosion	Silver will rapidly reprice by an order of magnitude	Historical spikes are transient and regime-bound	Claim not supported by time-series behavior
Physical Scarcity	Limited supply guarantees repricing	Above-ground stock and recycling mitigate scarcity	Scarcity alone insufficient
Roman Era Ratios	Ancient ratios define “true” value	Ancient economies are non-comparable	Historical analogy invalid
Fiat Collapse	Paper currency collapse ensures metal repricing	Precious metals fluctuate under fiat systems	No monotonic relationship
Interest Rate Irrelevance	Rates do not affect precious metals	Strong inverse relationship observed	Claim contradicted by data
