# multiFlowR

`multiflowR` is a project to develop tools for Quantitative linguistics (QL) in general and multi-modal linguistics in particular.

## dfMaker

`dfMaker` is a function which structures data from [`OpenPose`](https://github.com/CMU-Perceptual-Computing-Lab/openpose) computational vision software to study human body language. It also permits to integrate data from file names when the UCLA's tag structure when `extra.var= TRUE` (date and the searched expression).

> ***Example UCLA's tag structure:*** 2020-02-21_2100_US_CNN_The_Lead_With_Jake_Tapper_1244-1250_subsequently.mp4

`dfMaker` is not a complete function yet and the idea is to incorporate data from other software programs to create multi-modal `data frames` in linguistics.

## OpenPoseDataCleaning

This sections contains functions to clean and normalize data generated by OpenPose. The idea is to provide tools for computational linguistics studies.
