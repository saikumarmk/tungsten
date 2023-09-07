(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Keyboard Shortcuts*)


SetOptions[$FrontEndSession, 
 InputAutoReplacements ->{
 "m11"->ToBoxes[Defer[FullSimplify/@{}]],
 "m12"->ToBoxes[Defer[Solve[a,x]]],
 "m13"->ToBoxes[Defer[Reduce/@{}]],
 "m14"->ToBoxes[Defer[ComplexExpand/@{}]],
"m15"->ToBoxes[Defer[ArcLength[{x[t],y[t]},{t,a,b}]]],
"m16"->ToBoxes[Defer[Euler[exp,{x,y},{Subscript[x, o],Subscript[y, o]},h,Subscript[x,F]]]],
"m17"->ToBoxes@Defer[Solve[D[y,x],y'[x]]],
"m18"->ToBoxes[Defer[Det[{{},{},{}}]]],
"m19"->ToBoxes[Defer[Integrate[y,{x,a,b}]]],
"m21"->ToBoxes[Defer[Plot[fn,{x,-10,10},PlotRange->10]]],
"m22"->ToBoxes[Defer[ContourPlot[rel,{x,-10,10},{y,-10,10},Frame->False,Axes->True]]],
"m23"->ToBoxes[Defer[StreamPlot[{1,dy/dx},{x,-10,10},{y,-10,10},Frame->False,Axes->True]]],
"m23a"->ToBoxes[Defer[StreamPlot[{1,#},{x,-10,10},{y,-10,10},Frame->False,Axes->True]&/@{}]],
"m24"->ToBoxes[Defer[ParametricPlot[{x[t],y[t]},{t,a,b}]]],
"m25"->ToBoxes[Defer[DetailPlot[fn,{x,-10,10},PlotRange->10]]],
"m25a"->ToBoxes[Defer[DetailPlot[#,{x,-10,10},PlotRange->10]&/@{}]],
"m26"->ToBoxes[Defer[PolarPlot[r[\[Theta]],{\[Theta],0,2\[Pi]}]]],
"m27"->ToBoxes[Defer[RelationPlot[rel,{x,-5,5},{y,-5,5}]]],
"m27a"->ToBoxes[Defer[CRPlot[rel,{x,-5,5},{y,-5,5}]]],
"m28"->ToBoxes[Defer[DetailPP[{x[t],y[t]},{t,a,b}]]],
"mplot"->ToBoxes[Defer[AManipulate[Plot[f[x,a],{x,-10,10}],{a,-10,10}]]]
}]


(* ::Section:: *)
(*End*)


End[]
