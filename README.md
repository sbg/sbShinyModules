# sbShinyModules

## Overview

**sbShinyModules** is an R package designed to simplify the development of
Shiny applications specifically for hosting as on-demand Shiny applications on
**Seven Bridges Platforms**. This package aims to assist both internal and external
developers by providing pre-built, reusable Shiny modules tailored to fit
Seven Bridges hosting infrastructure.

By using these modules, developers can focus on the unique aspects of their
applications without needing to research and implement common functionalities
themselves. Modules such as file pickers and file exporters are designed to
work seamlessly within on-demand Data Studio hosting solution, allowing for
easy integration.

A detailed guideline for integrating modules into your Shiny apps and 
publishing them on our Seven Bridges Platform can be found [here](inst/Guideline_for_publishing_RShiny_on_demand_apps.pdf).

## Installation 

Install the development version from [GitHub](github) using the `remotes`
package:

```r
# Install the remotes package if you haven't already
# install.packages("remotes")

# Install sbShinyModules from GitHub
remotes::install_github("sbg/sbShinyModule@develop") 
```

## Note on Dependencies 

The sbShinyModules package depends on the **xattrs package**, which in turn
requires the **libattr1-dev** system library. 
If you encounter issues installing xattrs, it might be due to the absence of
libattr1-dev.

To resolve this, install the libattr1-dev library using your system's package
manager, then retry the installation of sbShinyModules.


## Features

### File Picker Module

The File Picker Module enables users to select files from Platform projects
within a Shiny app. It supports both single and multiple file selections with
a user-friendly interface.

```r
# UI part
mod_file_picker_ui("file_picker_1") 
```

```r
# Server part
mod_file_picker_server("file_picker_1", files_df, selection = "single") 
```
<br>
<center>
  ![Single file picker](vignettes/figures/file_picker/single_file_picker.png)
</center>
<br>


### Plot Exporter Module

The Plot Exporter Module allows users to save and export plots generated
within a Shiny app. It supports various output formats and integrates with
Seven Bridges Platform for seamless project file management.

```r
# UI part
mod_plot_exporter_ui("plot_exporter_1", save_button_title = "Save Plot")
```

```r
# Server part
mod_plot_exporter_server( 
  id = "plot_exporter_1", 
  plot_reactVals = helper_reactive, 
  output_formats = c("png", "pdf"), 
  module_title = "Export Plot", 
  sbg_directory_path = "/sbgenomics", 
  btns_div_width = 12 
) 
```
<br>
<center>
  ![Plot exporter](vignettes/figures/plot_exporter/save_plot_modal_dialog.png)
</center>
<br>


### Generic File Exporter Module

The Generic File Exporter Module provides a flexible solution for saving and
exporting a variety of file types from a Shiny app to the Platform project.
It supports different file formats and export functions.

```r
# UI part
mod_save_file_generic_ui("file_exporter_1")
```

```r
# Server part
mod_save_file_generic_server(
  id = "file_exporter_1",  
  reac_vals = list( 
    FUN = write.table, 
    args = list(x = my_data_frame, file = "my_file.csv"), 
    filename = "my_file", 
    extension = "csv", 
    overwrite = TRUE 
  ), 
 	sbg_directory_path = "/sbgenomics")  
```
<br>


### get_all_project_files() Function

This utility function simplifies file management by efficiently retrieving
detailed information about files within a specified directory.

```r
# Create a mock directory on the sbgenomics/project-files path and populate it
# with some test fles to test the get_all_project_files() function
all_files_df <- sbShinyModules::get_all_project_files(
  path = "/sbgenomics/project-files"
)

head(all_files_df)
```


