(* ::Package:: *)

(* ::Section:: *)
(*BeginPackage*)


BeginPackage["Tungsten`"];


(* ::Section:: *)
(*Usage*)


(* ::Section:: *)
(*Constants*)


PACKAGEDIRECTORYNAME = $InputFileName // DirectoryName;

MODULEFOLDER = FileNameJoin[{PACKAGEDIRECTORYNAME, "Functions"}]

MODULES = FileNames @ MODULEFOLDER;

LOADALL = False;


(* ::Section:: *)
(*Functions*)


(* ::Section:: *)
(*Main*)


If[LOADALL,
    Get /@ MODULES
    ,
    Get /@ (FileNameJoin[{MODULEFOLDER, #}]& /@ {"Numerical.wl", "Probability.wl",
         "Utility.wl", "FeatureFinding.wl", "Plotting.wl"})
];

EndPackage[];
