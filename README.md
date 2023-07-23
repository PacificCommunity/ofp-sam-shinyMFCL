shinyMFCL
=========

A collection of Shiny app [examples](examples) used to view MULTIFAN-CL stock
assessments.

General Design
--------------

 Step | Label      | File Input                               | Functions                                 | File Output
----- | ---------- | ---------------------------------------- | ----------------------------------------- | -------------------
1     | Lengths    | `length.fit`                             | `read_length_fit_file`                    | `lfits_dat.RData`
2     | Weights    | `weight.fit`                             | `read_length_fit_file`                    | `wfits_dat.RData`
3     | Movement   | `*.par`                                  | `read.MFCLRegion`                         | `move_coef.RData`
4     | General    | `*.par`, `*.rep`                         | `read.MFCLPar`, `read.MFCLRep`            | `other_data.RData`
5     | Likelihood | `*.par`, `test_plot_output`              | `read.MFCLParBits`, `read.MFCLLikelihood` | `ll_tab_data.RData`
6     | Tagging    | `*.par`, `*.tag`, `temporary_tag_report` | `read.MFCLPar`, `read.MFCLTag`            | `tag_data.RData`
