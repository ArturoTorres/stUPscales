# stUPscales
 Spatio-Temporal Uncertainty Propagation Across Multiple Scales - R package
 
 Copyright 2010 - 2020 Luxembourg Institute of Science and Technology.

What is new!

AggProp package:
  - 2016-10-22, version 1.0: 
    - several functions for temporal aggregation and uncertainty propagation via Monte Carlo simulation.
    - version for the QUICS deliverable 2.3 (ESR3).
    
STUnc package:
  - 2017-10-04, version 1.0.2: in agreement with nomenclature after Simon's suggestions e.g. V_Tank by V_Chamber
  - 2017-07-28, version 1.0.1:
    - added function for discrete sampling. New case 6 in MC.setup.R.
  - 2016-10-22, version 1.0: 
    - several functions for temporal aggregation and uncertainty propagation via Monte Carlo simulation.

stUPscales package:
  - 2017-10-18, named changed from STUnc to stUPscales
  - 2017-10-04, version 1.0.2: in agreement with nomenclature after Simon's suggestions e.g. V_Tank by V_Chamber
  - 2018-03-02, version 1.0.3: in agreement with EmiStatR version 1.2.0.6 (folderOutput dismissed)
  - 2018-06-11, version 1.0.3.1: Namespace file updated to export parallel functions
  - 2018-07-04, version 1.0.3.2: Removed function Level2Volume and corresponding author contribution
  - 2018-08-09, version 1.0.3.3: Edited package description (more environmental models approach)
  - 2019-05-04, version 1.0.3.4: Added new datasets from DWD and ASTA; added MC.analysis_generic function (14.02.2019). In CRAN.
                                 Checked to work seamlessly with EmiStatR_1.2.2.0 (staged-install).
  - 2019-05-30, version 1.0.4.0: Included new implementations (by Benedikt) for data preparation of radar
  imagery from DWD and precipitation gauge data. Created functions Correct.radar, IsReg.files, Smooth.ts and
  xts2STFDF. Removed routine for plotting inside Agg_t.R function. Edited DESCRIPTION file to renumber 
  package version.
  - 2019-06-28, version 1.0.5.0: Added MC.summary.output.R file for MC summary, estimation and 
  standard errors according to Dennis Boos.
  - 2019-11-12, version 1.0.5.0: Added Create.grid.R file for Create.grid function.
  - 2019-11-25, version 1.0.5.0: Added Aux2Diss.R file for Create.grid, 
  Subset.stfdf, Split.stfdf, Create.grid.split functions.
  - 2019-11-26, version 1.0.5.0: Added Split.grid and krigeST.parallel functions.
  - 2020-04-13, version 1.0.5.0: Modified MC.estatistic function before HESS submission. 
  Modified Split.grid and krigeST.parallel functions.
  - 2020-07-10, version 1.0.5.1: Removed EmiStatR dependency. TODO: check build.
  Added Bbox.offset function in Aux2Diss. Added dataset Goe_catchment.