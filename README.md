
# Hemipneustes Morphological Analysis

This project contains an R script developed as part of an internship at the **Natural History Museum of Maastricht**. The script performs statistical analysis of the morphological traits of the sea urchin genus *Hemipneustes*, comparing data across different geological time periods.

## Access the Script
The script can be accessed here and downloaded into your R environment. 
➡️ [Script_Hemipneustes.R](./Script_Hemipneustes.R)

## Features
- **Data Loading and Preprocessing**: Reads Excel files, filters data, and computes derived variables for analysis.
- **Principal Component Analysis (PCA)**: Identifies key patterns and variations in the data.
- **Feature Engineering**: Calculates relative measurements and performs angular transformations.
- **Visualization**: Creates PCA plots to explore morphological differences.

## Requirements
To use the script, ensure you have the following installed:
- **R** (version 4.0 or higher recommended)
- R libraries:
  - `readxl`
  - `FactoMineR`
  - `ggplot2`
  - `dplyr`

## How to Use
1. **Prepare the Dataset**:  
   Ensure your dataset is an Excel file (`.xlsx`) containing the required columns for analysis (e.g., `TL`, `TW`, `TH`, `degrees`, etc.). Update the file path in the script to point to your local file.

2. **Install Required Libraries**:  
   In R, run the following commands to install the necessary libraries:
   ```R
   install.packages(c("readxl", "FactoMineR", "ggplot2", "dplyr"))
   ```

3. **Run the Script**:  
   Open the script in your preferred R editor (e.g., RStudio) and execute it. The script will:
   - Clean and preprocess the data.
   - Perform PCA on selected features.
   - Generate plots for exploratory data analysis.

4. **Explore Results**:  
   Check the PCA plots and summary statistics to gain insights into the morphological traits of *Hemipneustes* and their variations across time periods.

## Customization
You can adapt the script to:
- Analyze additional features by modifying the PCA input variables.
- Visualize other relationships by creating additional plots.
- Use data from different sources by updating the file path and column names.

## Acknowledgments
This work was completed during an internship at the **Natural History Museum of Maastricht**. Special thanks to the museum staff for their guidance and access to the data.

## License
This project is open-source and available under the MIT License. Feel free to modify and use it for your research or educational purposes.

