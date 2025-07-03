# Analysis Scripts  
**Companion scripts** for the manuscript *[Your Paper Title]*, implementing the analyses presented in the manuscript using the [`RangeShifts`](https://github.com/EcoLabBET/RangeShifts.git) framework on a toy dataset.

### 🔄 Repo Structure  
ExOrChiST/
├── Toy_Dataset/ # Generated dataset of species occupancy at hectad level for Ireland
├── scripts/ # Analysis scripts used in the paper
├── Examples/ # Usage examples of the RangeShifts framework

### 📁 Scripts  
- `ExOrChiST-OrchidTrends-DiversityMetrics.R`: Calculates diversity metrics presented in the paper
- `ExOrChiST-OrchidTrends-TopCWE_boxplots.R`: Generates box plots for the CWE metric presented in the paper
- `ExOrChiST-OrchidTrends-TopSR_violinplots.R`: Generates violin plots for the SR metric presented in the paper
- `RosePlots.py`: Supporting scripts for Rose Plot visualizations (see /Examples/Measures-of-Centrality_Rose-Plots.ipynb)

### 📁 Examples
- `Trend_Analysis_with_pink-noise.ipynb`: Showcase of trend tables presented in the manuscript using the [`RangeShifts`](https://github.com/EcoLabBET/RangeShifts.git) framework
- `Measures-of-Centrality_Rose-Plots.ipynb`: Showcase of using [`RangeShifts`](https://github.com/EcoLabBET/RangeShifts.git) to calculate centrality points and visualize them in Rose Plots

### 📜 Reference  
If using these scripts, please cite:  
> *[Paper Citation]*  

### Funding
This research was supported by the Hellenic Foundation for Research and Innovation (H.F.R.I.) under the "2nd Call for H.F.R.I. Research Projects to support Faculty Members & Researchers" (Project Number: 3972).
