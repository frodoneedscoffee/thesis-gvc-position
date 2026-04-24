# Firm-Level Determinants of GVC Upgrading in Indian Manufacturing

This repository contains the code, data workflows, and documentation for my Master's thesis examining the firm-level determinants of upgrading (movement to higher value-added segments of the production chain) within Global Value Chains (GVCs) in the context of Indian manufacturing.

The study develops a novel firm-level measure of GVC position, inspired by Koopman et al. (2010) and Stemberger & Kejžar (2025), and applies it to firm-level data to analyze how productivity, financial constraints and firm capabilities influence movement toward higher value-added segments of the value chain.

The analysis integrates insights from:
- Trade theory (firm heterogeneity and productivity)
- Global Value Chain (GVC) governance
- Resource-Based View (VRIN capabilities)

# Research Questions
1. Does firm productivity facilitate upgrading within GVCs?
2. Do financial constraints limit firms’ ability to upgrade?
3. Do firm capabilities — particularly innovation and marketing — enable upgrading?

# Key Findings
- Productivity is the dominant driver of upgrading, explaining both cross-sectional differences and within-firm movement.
- Leverage matters, but primarily across firms, not within-firm dynamics.
- Liquidity is not statistically significant in the baseline results.
- Financial constraints are sector-specific, with strong heterogeneity across industries.
- Advertising (market-facing capability) drives upgrading, while innovation does not show significant effects.
- ICT has no standalone impact, but enhances the effectiveness of advertising through interaction effects.

# Data Sources
- CMIE Prowess for firm-level data
  - covers Indian firms
  - includes firmographics, firm financials, and trade activity
  - sample restricted to manufacturing firms (NIC 10–33)
- OECD Inter-Country Input-Output (ICIO) Tables (1995–2022) for industry-level data
  - used to construct proxies for forward participation
  - necessary due to lack of firm-level intermediate export data

# Methodology
- Construction of a firm-level GVC position index using backward participation (imported inputs) and forward participation (exported intermediates, proxied)
- Empirical approach: Panel data regressions with fixed effects models to control for unobserved heterogeneity
- Diagnostic tests including Hausman test, Wooldridge autocorrelation test, BP test for heteroscedasticity and VIF tests

# Key variables:
- Productivity (TFP)
- Financial constraints (liquidity, leverage)
- Capabilities (R&D, advertising, ICT)
- Controls (firm size, age, business group affiliation)

# Reproducibility
- To replicate the analysis, obtain access to:
  - CMIE Prowess data (subscription required)
  - OECD ICIO tables (publicly available)
- Install required R packages: tidyverse, fixest, corrplot, prodest, modelsummary, broom, kableExtra, ggplot2, ggrepel, plm, lmtest, car

# Limitations
- Forward participation is proxied using industry-level data, not directly observed at the firm level
- Results may be sensitive to measurement of capabilities (especially innovation)
- Sectoral heterogeneity suggests that effects are not uniform across industries

# Future Research
- Classify industries as upstream vs downstream and test capability-specific effects
- Use transaction-level or firm-to-firm trade data for better measurement
- Explore dynamic upgrading paths over time
- Incorporate GVC governance structures explicitly

# License
This project is for academic and research purposes only.
Data usage is subject to CMIE and OECD licensing terms.
