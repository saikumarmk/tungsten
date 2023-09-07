(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


VecPlot::usage = "Plots a list of vectors on an i,j plane.";

DetailBase::usage = "Returns the components required for DetailPlot.";

RelationBase::usage = "Returns the components required for RelationPlot.";

CalloutCoordinate::usage = "Applies a callout to a point.";

CRBase::usage = "Returns the components required for CRPlot.";

DetailPlot::usage = "Plots asymptotes and other important details of a graph.";

TooltipCoordinate::usage = "Creates a tooltip expression for a list of coordinates.";

RelationPlot::usage = "Plots relations returning axial intercepts.";

CRPlot::usage = "Plots complex subsets in terms of z.";

Va::usage = "Returns a vertical asymptote at x=a.";

CListPlot::usage = "Plots a list of complex numbers on an Argand diagram.";

PPBase::usage = "Returns the components required for DetailPP.";

DetailPP::usage = "Plots a set of parametric equations with features such as intercepts and intersections labeled.";

HoleQ::usage = "Tests whether a hole exists at a point a.";

Endpoint::usage = "Tooltips and colors an endpoint.";

InversePlot::usage = "Plots a function along with its inverse. IPlot[f[x],{x,a,b},{y,c,d}].";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]

(*Requires FeatureFinding to be loaded in first.*) 


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*DetailPlot*)


(* ::Subsubsection:: *)
(*Va (Vertical Asymptote)*)


Va[x_] :=
    InfiniteLine[{x, 0}, {0, -x}];


(* ::Subsubsection:: *)
(*HoleQ*)


HoleQ[fx_, x_, a_] :=
    (*Determines if a function has a hole at a point. NOT OPERATIONAL.*)Quiet @
        
        Module[{L, val},
            L = Limit[fx, x -> a];
            val = fx /. x -> a;
            If[L === val,
                Return[False]
                ,
                Return[True]
            ]
        ]


(* ::Subsection:: *)
(*TrigQ*)


TrigQ[exp_] :=
    ContainsAny[Join[Head /@ Level[exp, -1], {Head[exp]}], {ArcCos, ArcSin,
         ArcTan, Log, Log2, Sqrt, ArcSec, ArcCot, ArcCsc}];


(* ::Subsubsection:: *)
(*Endpoint*)


Endpoint[fx_, x_, a_] :=
    Module[{real, L},
        L = Evaluate @ Limit[fx, x -> a];
        If[Internal`RealValuedNumericQ[L],
            If[!HoleQ[fx, x, a],
                Return[TooltipCoordinate[{{a, L}}, Orange]]
                ,
                Return[TooltipCoordinate[{{a, L}}, White]]
            ]
            ,
            Return[{}]
        ]
    ]


(* ::Subsubsection:: *)
(*TooltipCoordinate*)


TooltipCoordinate[points_, color_, size_:0.013] :=
    Graphics[{PointSize[size], color, #}]& /@ (Tooltip[Point[#], #]& 
        /@ points)

CalloutCoordinate[points_] :=
    Callout[#, ToString[#, TraditionalForm], CalloutMarker -> "Circle"
        ]& /@ points


(* ::Subsubsection:: *)
(*DetailBase*)


DetailBase[exp_, {x_, min_, max_}, opts : OptionsPattern[]] :=
    Module[
        {newexp, xint, xintP, yint, yintP, stat, statP, newexpD, inflec,
             inflecP, term, residue, obliq, vert, line, vaP, vaN, obliqStr, domain,
             holex, holeP, va, vaC, asym, cond, newexpDDD, start, end, sp, ep, trigcheck,
             asymcheck, verticalAsymptote, contourVertical, oblique, absCheck}
        ,
        (*Pre check*)
        newexp = exp /. Abs -> RealAbs;
        If[(FunctionDomain[newexp, x] /. x -> \[Infinity]) \[Or] (FunctionDomain[newexp,
             x] /. x -> -\[Infinity]),
            asymcheck = True
            ,
            asymcheck = False
        ];
        trigcheck = TrigQ[newexp];
        domain = FunctionDomain[{newexp, min <= x <= max}, x];
        absCheck = ContainsAny[Join[Head /@ Level[newexp, -1], {Head[
            newexp]}], {RealAbs, Abs}];
        (*Oblique asymptote condition*)
        cond =
            If[FunctionDomain[newexp, x] == True,
                True
                ,
                False
            ];
        newexpD =
            D[
                If[trigcheck || absCheck,
                    newexp
                    ,
                    Refine[ComplexExpand[newexp], domain]
                ]
                ,
                x
            ];
        newexpDDD =
            D[
                If[trigcheck || absCheck,
                    newexp
                    ,
                    Refine[ComplexExpand[newexp], domain]
                ]
                ,
                {x, 3}
            ];
        (*Points*)
        term =
            If[Length[newexp] > 1,
                Level[Apart @ newexp, 1]
                ,
                Apart @ newexp
            ];
        xintP = TooltipCoordinate[XIntercept[newexp, {x, domain}], Red
            ];
        (*Y intercept*)
        yintP = TooltipCoordinate[YIntercept[newexp, {x, domain}], Red
            ];
        (*Stationary points*)
        statP = TooltipCoordinate[FullSimplify /@ StationaryPoint[{newexp,
             newexpD}, {x, domain}], Blue];
        (*Inflection points*)
        inflecP = TooltipCoordinate[FullSimplify /@ InflectionPoint[{
            newexp, newexpD, newexpDDD}, {x, domain}], Green];
        (*Start point*)
        sp = Endpoint[exp, x, min];
        ep = Endpoint[exp, x, max];
        (*Asymptotes*)
        oblique = SlantAsymptote[newexp, x];
        verticalAsymptote = VerticalAsymptote[exp, {x, min, max}];
        contourVertical =
            Quiet @
                ContourPlot[
                    Evaluate[Thread[x == verticalAsymptote]]
                    ,
                        (*Then tries to solve for the reciprocal being
                        
                         
                        0*)
                    {x, -10^4, 10^4}
                    ,
                    {y, -10^4, 10^4}
                    ,
                    ContourStyle -> {Directive[Red, Dashed]}
                ];
        (*Graphics*)
        asym = Plot[Evaluate[Tooltip[#, ToString[#, TraditionalForm]]&
             /@ oblique], {x, min, max}, PlotStyle -> Directive[Red, Dashed], Evaluate[
            FilterRules[{opts}, {Except[PlotStyle], Options[Plot]}]]];
        Flatten[{asym, xintP, yintP, statP, inflecP, sp, ep, contourVertical
            }]
    ];


(* ::Subsubsection:: *)
(*DetailPlot*)


DetailPlot[exp_, {x_, min_, max_}, opts : OptionsPattern[]] :=
    Quiet @
        Module[{bases, asym, base, reVar, ints, cords, ccords, intP},
            
            If[ListQ[exp],
                (*If multiple expressions are given*)
                bases = DetailBase[#, {x, min, max}]& /@ exp;
                (*Determine points of intersection*)
                ints = Chop /@ Table[FullSimplify /@ ForceSolve[exp[[
                    n]] == # && min <= x <= max, x]& /@ Cases[exp, Except @ exp[[n]]], {n,
                     1, Length[exp]}];
                cords = Chop /@ Table[Thread[{Flatten[ints[[n]]], ReplaceAll[
                    exp[[n]], x -> #]& /@ Flatten[ints[[n]]]}], {n, 1, Length[ints]}];
                ccords = DeleteDuplicates[FlattenAt[cords, Table[{n},
                     {n, 1, Length[cords]}]]];
                intP = TooltipCoordinate[ccords, Purple];
                base = Plot[Tooltip[exp], {x, min, max}, Evaluate[FilterRules[
                    {opts}, Options[Plot]]]];
                Show[Flatten[{base, bases, intP}]]
                ,
                (*If only one expression is given*)
                bases = DetailBase[exp, {x, min, max}];
                base = Plot[Tooltip[exp], {x, min, max}, Evaluate[FilterRules[
                    {opts}, Options[Plot]]]];
                Show[Flatten[{base, bases}]]
            ]
        ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


Options[DetailBase] = Options[Plot];

Options[DetailPlot] = Options[Plot];


(* ::Subsection:: *)
(*RelationPlot*)


(* ::Subsubsection::Closed:: *)
(*RelationPlot*)


RelationPlot[exp_, {x_, min_, max_}, {y_, yin_, yang_}, opts : OptionsPattern[
    ]] :=
    Quiet @
        Module[{bases, asym, base, reVar},
            If[ListQ[exp],
                (*Multiple expressions*)
                bases = RelationBase[#, {x, min, max}, {y, yin, yang}
                    ]& /@ exp;
                base = ContourPlot[Tooltip[exp], {x, min, max}, {y, yin,
                     yang}, Axes -> True, Frame -> False, Evaluate[FilterRules[{opts}, Options[
                    ContourPlot]]]];
                Show[Flatten[{base, bases}]]
                ,
                (*one expression*)
                bases = RelationBase[exp, {x, min, max}, {y, min, max
                    }];
                base = ContourPlot[Tooltip[exp], {x, min, max}, {y, yin,
                     yang}, Axes -> True, Frame -> False, Evaluate[FilterRules[{opts}, Options[
                    Plot]]]];
                Show[Flatten[{base, bases}]]
            ]
        ]


(* ::Subsubsection:: *)
(*RelationBase*)


RelationBase[exp_, {x_, min_, max_}, {y_, yin_, yang_}, opts : OptionsPattern[
    ]] :=
    Quiet @
        Module[{crel, ex, exf, xint, xintP, yint, yintP, stat, statP,
             expD, base, vert, vertP, diff, hor, horP, flatlineq},
            exf =
                If[AnyTrue[Thread[Or[MatchQ[#, _Abs]& /@ Level[exp, -
                    1], MatchQ[#, _Arg]& /@ Level[exp, -1], MatchQ[#, _Re]& /@ Level[exp,
                     -1], MatchQ[#, _Im]& /@ Level[exp, -1]]], TrueQ],
                    FullSimplify @ ComplexExpand[exp]
                    ,
                    exp
                ];
            flatlineq = AnyTrue[Thread[Or[MatchQ[#, _Re]& /@ Level[exp,
                 -1], MatchQ[#, _Im]& /@ Level[exp, -1]]], TrueQ];
            (*Points*)
            xint = ForceSolve[exf && y == 0 && min <= x <= max, {x, y
                }]; (*limit domain for trig functions*)
            xintP = TooltipCoordinate[xint, Red];
            yint = ForceSolve[exf && x == 0 && yin <= y <= yang, {x, 
                y}];
            yintP = TooltipCoordinate[yint, Red];
            diff = y'[x] /. Solve[D[exf /. y -> y[x], x], y'[x]][[1]]
                 /. y[x] -> y;
            vert = ForceSolve[Denominator[diff] == 0 && exf && min <=
                 x <= max && yin <= y <= yang, {x, y}];
            vertP =
                If[Length[vert[[1]]] == 1,
                    Nothing
                    ,
                    TooltipCoordinate[vert, Blue]
                ];
            hor = Refine[#, min < x < max && yin < y < yang]& /@ ForceSolve[
                Numerator[diff] == 0 && exf && min <= x <= max && yin <= y <= yang, {
                x, y}];
            horP =
                If[Length[hor[[1]]] == 1,
                    Nothing
                    ,
                    TooltipCoordinate[hor, Blue]
                ];
            {
                xintP
                ,
                yintP
                ,
                If[Not[flatlineq],
                    {vertP, horP}
                    ,
                    Nothing
                ]
            }
        ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


Options[RelationPlot] = Options[ContourPlot];

Options[RelationBase] = Options[ContourPlot];


(* ::Subsection:: *)
(*CRPlot*)


(* ::Subsubsection:: *)
(*CRPlot*)


CRPlot[exp_, {xv_, min_, max_}, {yv_, yin_, yang_}, opts : OptionsPattern[
    ]] :=
    Quiet @
        Module[{bases, asym, base, reVar},
            If[ListQ[exp],
                bases = CRBase[#, {x, min, max}, {y, yin, yang}]& /@ 
                    exp;
                base = ContourPlot[Tooltip[Evaluate[(exp) /. Symbol["z"
                    ] -> xv + I yv]], {xv, min, max}, {yv, yin, yang}, Axes -> True, Frame
                     -> False, Evaluate[FilterRules[{opts}, Options[ContourPlot]]]];
                Show[Flatten[{base, bases}]]
                ,
                bases = CRBase[exp, {xv, min, max}, {yv, min, max}];
                base = ContourPlot[Tooltip[Evaluate[exp /. Symbol["z"
                    ] -> xv + I yv]], {xv, min, max}, {yv, yin, yang}, Axes -> True, Frame
                     -> False, Evaluate[FilterRules[{opts}, Options[Plot]]]];
                Show[Flatten[{base, bases}]]
            ]
        ]


(* ::Subsubsection:: *)
(*CRBase*)


CRBase[exp_, {x_, min_, max_}, {y_, yin_, yang_}, opts : OptionsPattern[
    ]] :=
    Quiet @
        Module[{re, crel, ex, exf, xint, xintP, yint, yintP, stat, statP,
             expD, base, vert, vertP, diff, hor, horP, flatlineq},
            re = exp /. Symbol["z"] -> x + I y;
            exf =
                If[AnyTrue[Thread[Or[MatchQ[#, _Abs]& /@ Level[re, -1
                    ], MatchQ[#, _Arg]& /@ Level[re, -1], MatchQ[#, _Re]& /@ Level[re, -1
                    ], MatchQ[#, _Im]& /@ Level[re, -1]]], TrueQ],
                    FullSimplify @ ComplexExpand[re]
                    ,
                    re
                ];
            flatlineq = AnyTrue[Thread[Or[MatchQ[#, _Re]& /@ Level[re,
                 -1], MatchQ[#, _Im]& /@ Level[re, -1]]], TrueQ];
            (*Points*)
            xint = ForceSolve[exf && y == 0 && min <= x <= max, {x, y
                }]; (*limit domain for trig functions*)
            xintP = TooltipCoordinate[xint, Red];
            yint = ForceSolve[exf && x == 0, {x, y}];
            yintP = TooltipCoordinate[yint, Red];
            diff = y'[x] /. Solve[D[exf /. y -> y[x], x], y'[x]][[1]]
                 /. y[x] -> y;
            vert = ForceSolve[Denominator[diff] == 0 && exf && min <=
                 x <= max && yin <= y <= yang, {x, y}];
            vertP =
                If[Length[vert[[1]]] == 1,
                    Nothing
                    ,
                    TooltipCoordinate[vert, Blue]
                ];
            hor = Refine[#, min < x < max && yin < y < yang]& /@ ForceSolve[
                Numerator[diff] == 0 && exf && min <= x <= max && yin <= y <= yang, {
                x, y}];
            horP =
                If[Length[hor[[1]]] == 1,
                    Nothing
                    ,
                    TooltipCoordinate[hor, Blue]
                ];
            {
                xintP
                ,
                yintP
                ,
                If[Not[flatlineq],
                    {vertP, horP}
                    ,
                    Nothing
                ]
            }
        ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


Options[CRPlot] = Options[ContourPlot];

Options[CRBase] = Options[ContourPlot];


(* ::Subsection:: *)
(*DetailPP*)


(* ::Subsubsection:: *)
(*PPBase*)


PPBase[exp_, {t_, min_, max_}] :=
    Module[{xdom, ydom, xint, xt, xP, yint, yt, yP, stat, vert, st, statP,
         sP, vP, vt, eP},
        xdom = FunctionDomain[{exp[[1]], min <= t <= max}, t];
        ydom = FunctionDomain[{exp[[2]], min <= t <= max}, t];
        xt = ForceSolve[exp[[1]] == 0 && xdom && ydom, t];
        xint = DeleteDuplicates[Thread[exp /. t -> xt]];
        xP = Graphics[{PointSize[0.014], Red, #}]& /@ (Tooltip[Point[
            #], #]& /@ xint);
        yt = ForceSolve[exp[[2]] == 0 && ydom && xdom, t];
        yint = DeleteDuplicates[Thread[exp /. t -> yt]];
        yP = Graphics[{PointSize[0.014], Red, #}]& /@ (Tooltip[Point[
            #], #]& /@ yint);
        stat = ForceSolve[D[exp[[2]], t] / D[exp[[1]], t] == 0 && xdom
             && ydom, t];
        st = DeleteDuplicates[Thread[exp /. t -> stat]];
        statP = Graphics[{PointSize[0.013], Blue, #}]& /@ (Tooltip[Point[
            #], #]& /@ st);
        vert = ForceSolve[D[exp[[1]], t] / D[exp[[2]], t] == 0 && ydom
             && xdom, t];
        vt = DeleteDuplicates[Thread[exp /. t -> vert]];
        vP = Graphics[{PointSize[0.014], Blue, #}]& /@ (Tooltip[Point[
            #], #]& /@ vt);
        If[Internal`RealValuedNumericQ[Total[exp /. t -> min]],
            sP = Graphics[{PointSize[0.014], Orange, #}]& /@ (Tooltip[
                Point[#], #]& /@ ({exp /. t -> min}))
            ,
            sP = Nothing
        ];
        If[Internal`RealValuedNumericQ[Total[exp /. t -> max]],
            eP = Graphics[{PointSize[0.014], Orange, #}]& /@ (Tooltip[
                Point[#], #]& /@ ({exp /. t -> max}))
            ,
            eP = Nothing
        ];
        {xP, yP, statP, vP, sP, eP}
    ];


(* ::Subsubsection:: *)
(*DetailPP*)


DetailPP[exp_, {t_, min_, max_}, opts : OptionsPattern[]] :=
    Quiet @
        Module[{bases, asym, base, ints, cords, ccords, intP, ccrp, crp,
             crods, ct, crP},
            If[MatrixQ[exp],
                bases = PPBase[#, {t, min, max}]& /@ exp;
                base = ParametricPlot[Tooltip[exp], {t, min, max}, Evaluate[
                    FilterRules[{opts}, Options[ParametricPlot]]]] /. Line[x_] :> Sequence[
                    Arrowheads[Table[.02, {10}]], Arrow @ Line[x]];
                ints = Table[FullSimplify /@ ForceSolve[exp[[n]] == #
                     && min <= t <= max, t]& /@ Cases[exp, Except @ exp[[n]]], {n, 1, Length[
                    exp]}];
                cords = Table[Thread[exp[[n]] /. t -> Flatten[ints[[n
                    ]]]], {n, 1, Length[ints]}];
                ccords = DeleteDuplicates[FlattenAt[cords, Table[{n},
                     {n, 1, Length[cords]}]]];
                intP = Graphics[{PointSize[0.013], Purple, #}]& /@ (Tooltip[
                    Point[#], #]& /@ ccords);
                crp = Table[FullSimplify /@ ForceSolve[exp[[n]] == (#
                     /. t -> s) && min <= t <= max && min <= s <= max, {t, s}]& /@ Cases[
                    exp, Except @ exp[[n]]], {n, 1, Length[exp]}];
                ct = (DeleteDuplicatesBy[#[[1]], Equal]& /@ crp)[[All,
                     All, 1]];
                ccrp =
                    Table[
                        Thread[
                            exp[[n]] /.
                                t ->
                                    If[Length[ct[[n]]] != 1,
                                        ct[[n]]
                                        ,
                                        ct[[n, 1]]
                                    ]
                        ]
                        ,
                        {n, 1, Length[ct]}
                    ];
                crods = DeleteDuplicates[ccrp];
                crP = Graphics[{PointSize[0.013], Yellow, #}]& /@ (Tooltip[
                    Point[#], #]& /@ crods);
                Show[Flatten[{base, bases, crP, intP}]]
                ,
                bases = PPBase[exp, {t, min, max}];
                base = ParametricPlot[Tooltip[exp], {t, min, max}, Evaluate[
                    FilterRules[{opts}, Options[ParametricPlot]]]] /. Line[x_] :> Sequence[
                    Arrowheads[Table[.02, {10}]], Arrow @ Line[x]];
                Show[Flatten[{base, bases}]]
            ]
        ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


Options[DetailPP] = Options[ParametricPlot];


(* ::Subsection:: *)
(*InversePlot*)


(* ::Subsubsection:: *)
(*InversePlot*)


InversePlot[exp_, {x_, min_, max_}, {y_, ymin_, ymax_}, opts : OptionsPattern[
    ]] :=
    Module[{},
        ContourPlot[{y == exp, Evaluate[x == (exp /. x -> y)]}, {x, min,
             max}, {y, ymin, ymax}, Axes -> True, Frame -> False, Evaluate[FilterRules[
            {opts}, Options[Plot]]]]
    ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


Options[InversePlot] = Options[ContourPlot];


(* ::Section:: *)
(*End*)


End[]
