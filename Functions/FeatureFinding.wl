(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


XIntercept::usage = "Finds the x intercepts of a function. Input is xIntercept[function,{x,domain}]";

YIntercept::usage = "Finds the y intercepts of a function. Input is yIntercept[function,{x,domain}]";

StationaryPoint::usage = "Finds stationary points of a uni-variate real valued function.";

InflectionPoint::usage = "Finds inflexion points of a real valued function.";

TrigQ::usage = "Determines if a function contains trigonometric terms.";

SlantAsymptote::usage = "Attempts to find vertical, horizontal and oblique asymptotes.";

NatureDetermine::usage = "Helper function used in NatureTest.";

LogAsymptote::usage = "Find the vertical asymptotes of logarithmic functions.";

VerticalAsymptote::usage = "";

NatureTest::usage = "Determines the nature of all stationary points via the Sign test. NatureTest[f[x],x].";

FInfo::usage = "Returns important features of a function. Input can be Finfo[f[x],x] or Finfo[f[x],{x,min,max}]";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Feature Finding*)


(* ::Subsubsection:: *)
(*XIntercept*)


XIntercept[exp_, {x_, domain_}] :=
    Module[{},
        Chop /@ Thread[{ForceSolve[{exp == 0, domain}, x], 0}]
    ]

XIntercept[exp_, x_] :=
    Module[{},
        Chop /@ Thread[{ForceSolve[{exp == 0}, x], 0}]
    ]


(* ::Subsubsection:: *)
(*YIntercept*)


YIntercept[exp_, {x_, domain_}] :=
    Module[{},
        Solve[y == exp && x == 0 && domain, {x, y}, Reals][[All, All,
             2]]
    ]


(* ::Subsubsection:: *)
(*StationaryPoint*)


StationaryPoint[{exp_, expD_}, {x_, domain_}] :=
    Module[{candidates},
        candidates = ForceSolve[{expD == 0, domain}, x];
        Chop /@ Thread[{candidates, ReplaceAll[exp, x -> #]& /@ candidates
            }]
    ]

StationaryPoint[exp_, x_] :=
    Module[{candidates},
        candidates = ForceSolve[{D[exp, x] == 0}, x];
        Chop /@ Thread[{candidates, ReplaceAll[exp, x -> #]& /@ candidates
            }]
    ]

StationaryPoint[exp_, {x_, domain_}] :=
    Module[{candidates},
        candidates = ForceSolve[{D[exp, x] == 0, domain}, x];
        Chop /@ Thread[{candidates, ReplaceAll[exp, x -> #]& /@ candidates
            }]
    ]


(* ::Subsubsection:: *)
(*InflectionPoint*)


InflectionPoint[{exp_, expD_, expDDD_}, {x_, domain_}] :=
    Module[{candidates},
        candidates = ForceSolve[{D[expD, x] == 0, domain, expDDD != 0
            }, x];
        Chop /@ Thread[{candidates, ReplaceAll[exp, x -> #]& /@ candidates
            }]
    ]

InflectionPoint[exp_, {x_, domain_}] :=
    Module[{candidates},
        candidates = ForceSolve[{D[exp, {x, 2}] == 0, domain, D[exp, 
            {x, 3}] != 0}, x];
        Chop /@ Thread[{candidates, ReplaceAll[exp, x -> #]& /@ candidates
            }]
    ]

InflectionPoint[exp_, x_] :=
    Module[{candidates},
        candidates = ForceSolve[{D[exp, {x, 2}] == 0, D[exp, {x, 3}] 
            != 0}, x];
        Chop /@ Thread[{candidates, ReplaceAll[exp, x -> #]& /@ candidates
            }]
    ]


(* ::Subsubsection:: *)
(*SlantAsymptote*)


SlantAsymptote[exp_, x_] :=
    Module[{series, vaP, vaN, residue, obliq, cond, domTerms, domTermsN,
         obliqN, seriesN},
        cond =
            If[FunctionDomain[exp, x] == True,
                True
                ,
                False
            ];
        If[NumericQ @ Limit[exp, x -> \[Infinity]],
            vaP = Limit[exp, x -> \[Infinity]]
            ,
            vaP = Nothing
        ];
        If[NumericQ @ Limit[exp, x -> -\[Infinity]],
            vaN = Limit[exp, x -> -\[Infinity]]
            ,
            vaN = Nothing
        ];
        series = Normal @ Series[exp, {x, \[Infinity], 10}];
        domTerms =
            If[Length[exp] > 1,
                Select[series, (0 != Limit[#, x -> \[Infinity]])&]
                ,
                exp
            ];
        obliq =
            If[!NumericQ[Limit[exp, x -> \[Infinity]]],
                If[SameQ[Apart @ exp, domTerms],
                    Nothing
                    ,
                    domTerms
                ]
                ,
                Limit[exp, x -> \[Infinity]]
            ];
        seriesN = Normal @ Series[exp, {x, -\[Infinity], 10}];
        domTermsN =
            If[Length[exp] > 1,
                Select[seriesN, (0 != Limit[#, x -> -\[Infinity]])&]
                ,
                exp
            ];
        obliqN =
            If[!NumericQ[Limit[exp, x -> -\[Infinity]]],
                If[SameQ[Apart @ exp, domTermsN],
                    Nothing
                    ,
                    domTermsN
                ]
                ,
                Limit[exp, x -> -\[Infinity]]
            ];
        If[cond,
            obliq = Nothing;
            obliqN = Nothing
        ];
        DeleteDuplicates @ {vaP, vaN, obliq, obliqN}
    ]


(* ::Subsubsection:: *)
(*LogAsymptote*)


LogAsymptote[fx_, {x_, min_, max_}] :=
    Module[{case, psol},
        case = Cases[fx + abcde, _Log, Infinity];
        If[Length[case] == 0,
            Return[Nothing]
        ];
        psol = Flatten[Solve[{# == 0, min <= x <= max}, x, Reals]& /@
             case[[All, 1]]][[All, 2]];
        Thread[x == Select[psol, !Internal`RealValuedNumericQ[Limit[fx,
             x -> #]]&]]
    ]

LogAsymptote[fx_, x_] :=
    Module[{case, psol},
        case = Cases[fx + abcde, _Log, Infinity];
        If[Length[case] == 0,
            Return[Nothing]
        ];
        psol = Flatten[Solve[{# == 0}, x, Reals]& /@ case[[All, 1]]][[
            All, 2]];
        Thread[x == Select[psol, !Internal`RealValuedNumericQ[Limit[fx,
             x -> #]]&]]
    ]

LogAsymptote[fx_, {x_, domain_}] :=
    Module[{case, psol},
        case = Cases[fx + abcde, _Log, Infinity];
        If[Length[case] == 0,
            Return[Nothing]
        ];
        psol = Flatten[Solve[{# == 0, domain}, x, Reals]& /@ case[[All,
             1]]][[All, 2]];
        Thread[x == Select[psol, !Internal`RealValuedNumericQ[Limit[fx,
             x -> #]]&]]
    ]


(* ::Subsubsection:: *)
(*VerticalAsymptote*)


VerticalAsymptote[exp_, {x_, domain_}] :=
    Module[{term, residue, newexp, vaC, logAsymptote, asymptoteList, 
        logVariable, verticalAsymptote, filterIndeterminate},
        newexp = exp /. RealAbs -> Abs;
        term =
            If[Length[newexp] > 1,
                Level[FullSimplify[newexp], 1]
                ,
                FullSimplify[newexp]
            ];
        residue =
            If[Length[newexp] > 1,
                Plus @@ Select[term, (0 == Limit[#, x -> \[Infinity]] && 0 == Limit[
                    #, x -> -\[Infinity]])&]
                ,
                newexp
            ];
        logAsymptote = LogAsymptote[exp, {x, domain}];
        verticalAsymptote = Union[Complement[Flatten @ DeleteCases[ForceSolve[
            FullSimplify @ TrigReduce[Together[1 / #]] == 0 && domain, x]& /@ Flatten[
            {term}], {}, Infinity], Flatten @ ForceSolve[{TrigReduce @ newexp == 
            0, domain}, x]], Flatten @ ForceSolve[Denominator[newexp] == 0 && domain,
             x]];
        filterIndeterminate = Quiet @ Select[verticalAsymptote, !ReplaceAll[
            exp, x -> #] === Indeterminate&];
        asymptoteList =
            If[logAsymptote === Nothing,
                filterIndeterminate
                ,
                Flatten @ Join[filterIndeterminate, logAsymptote[[All,
                     2]]]
            ];
        Return[asymptoteList]
    ]

VerticalAsymptote[exp_, {x_, min_, max_}] :=
    VerticalAsymptote[exp, {x, min <= x <= max}]

VerticalAsymptote[exp_, x_] :=
    VerticalAsymptote[exp, {x, True}]


(* ::Subsubsection:: *)
(*NatureDetermine*)


NatureDetermine[exp_, {x_, val_}] :=
    Module[{point},
        If[Head[val] === ConditionalExpression,
            Return["Unknown"]
        ];
        point = D[exp, {x, 2}] /. x -> val;
        Switch[Sign[Re[point]],
            -1,
                "Maximum"
            ,
            0,
                "Inflexion"
            ,
            1,
                "Minimum"
        ]
    ]


(* ::Subsubsection:: *)
(*NatureTest*)


NatureTest[f_, x_, e_:0.1] :=
    Module[{derivative, StationaryPoint, typeAllocation},
        derivative = D[f, x];
        StationaryPoint = ForceSolve[derivative == 0, x];
        typeAllocation = <|-1 -> "\\", 1 -> "/", 0 -> "-"|>;
        Column @ Table[Grid[{StringReplace[{"f'(b)", "f'(a)", "f'(c)"
            }, {"a" -> ToString[StationaryPoint[[index]], TraditionalForm], "b" ->
             ToString[StationaryPoint[[index]] - e, TraditionalForm], "c" -> ToString[
            StationaryPoint[[index]] + e, TraditionalForm]}], {derivative /. x ->
             StationaryPoint[[index]] - e, Chop[derivative /. x -> StationaryPoint
            [[index]]], derivative /. x -> StationaryPoint[[index]] + e}, {typeAllocation[
            Sign[derivative /. x -> StationaryPoint[[index]] - e]], typeAllocation[
            Sign[Chop[derivative /. x -> StationaryPoint[[index]]]]], typeAllocation[
            Sign[derivative /. x -> StationaryPoint[[index]] + e]]}, {"", NatureDetermine[
            f, {x, StationaryPoint[[index]]}], ""}}, Frame -> All], {index, Length[
            StationaryPoint]}]
    ]


(* ::Subsection:: *)
(*FInfo*)


(* ::Subsubsection:: *)
(*FInfo - Domain*)


FInfo[exp_, {x_, dom_}] :=
    Module[{newexp, xint, xintP, yint, yintP, stat, statP, newexpD, inflec,
         inflecP, fPeriod, absCheck, fRange, term, residue, obliq, vert, line,
         vaP, vaN, obliqStr, plt, domain, holex, holeP, va, vaC, oblf, asym, 
        cond, newexpDDD, start, end, sp, ep, trigcheck, asymcheck, va1, va2, 
        basestring, range, domainD, newstring, obliqs, vaCs},
        newexp = exp /. Abs -> RealAbs;
        absCheck = ContainsAny[Join[Head /@ Level[newexp, -1], {Head[
            newexp]}], {RealAbs, Abs}];
        fPeriod =
            If[FunctionPeriod[exp, x] === 0,
                "This function does not oscillate."
                ,
                ToString[FunctionPeriod[exp, x], TraditionalForm]
            ];
        If[(FunctionDomain[newexp, x] /. x -> \[Infinity]) \[Or] (FunctionDomain[newexp,
             x] /. x -> -\[Infinity]),
            asymcheck = True
            ,
            asymcheck = False
        ];
        trigcheck = TrigQ[newexp];
        domain = FunctionDomain[{newexp, dom}, x];
        domainD =
            If[BooleanQ[FunctionDomain[exp, x]],
                "All real numbers"
                ,
                ToString[domain, TraditionalForm]
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
        xint = XIntercept[newexp, {x, domain}];
        xintP =
            If[Length[xint] == 0,
                "No x intercept."
                ,
                "Horizontal intercepts:\n" <> ToString[TableForm[ToString[
                    #, TraditionalForm]& /@ xint], TraditionalForm]
            ];
        fRange = Quiet @ Check[FunctionRange[{exp, domain}, x, Global`y
            ], FunctionRange[{exp}, x, Global`y]];
        range =
            If[BooleanQ[fRange] == True,
                " All real numbers"
                ,
                ToString[fRange, TraditionalForm]
            ];
        yint = YIntercept[newexp, {x, domain}];
        yintP =
            If[Length[yint] == 0,
                "No y intercept."
                ,
                "Vertical intercepts:\n" <> ToString[TableForm[ToString[
                    #, TraditionalForm]& /@ yint], TraditionalForm]
            ];
        (*Stationary points*)
        stat = FullSimplify /@ Chop /@ Thread[{StationaryPoint[{newexp,
             newexpD}, {x, domain}], NatureDetermine[newexp, {x, #}]& /@ ForceSolve[
            {newexpD == 0, domain}, x]}];
        statP =
            If[Length[stat] == 0,
                "No stationary points."
                ,
                "Stationary points:\n" <> ToString[TableForm[ToString[
                    #, TraditionalForm]& /@ stat], TraditionalForm]
            ];
        inflec = InflectionPoint[{newexp, newexpD, newexpDDD}, {x, domain
            }];
        inflecP =
            If[Length[inflec] == 0,
                "No points of inflexion."
                ,
                "Inflexion points:\n" <> ToString[TableForm[ToString[
                    FullSimplify /@ #, TraditionalForm]& /@ inflec], TraditionalForm]
            ];
        vaP =
            If[NumericQ @ Limit[newexp, x -> \[Infinity]],
                "Right sided asymptote: " <> " y = " <> ToString[Limit[
                    newexp, x -> \[Infinity]], TraditionalForm]
                ,
                "No right sided asymptote."
            ];
        vaN =
            If[NumericQ @ Limit[newexp, x -> -\[Infinity]],
                "Left sided asymptote: " <> " y = " <> ToString[Limit[
                    newexp, x -> -\[Infinity]], TraditionalForm]
                ,
                "No left sided asymptote."
            ];
        obliq = SlantAsymptote[newexp, x];
        obliqs =
            If[obliq === {},
                "No oblique asymptote."
                ,
                "Oblique asymptote(s): " <> " y = " <> ToString[obliq,
                     TraditionalForm]
            ];
        vaC = Thread[x == VerticalAsymptote[newexp, {x, dom}]];
        vaCs =
            If[Length[vaC] == 0,
                "No vertical asymptotes."
                ,
                "Vertical asymptotes:\n" <> ToString[TableForm[ToString[
                    FullSimplify /@ #, TraditionalForm]& /@ vaC], TraditionalForm]
            ];
        basestring = "The function is FN\nThe Derivative is:DRV\nDomain: DMN\nRange: RNE\nPeriod: PERI\n"
            ;
        newstring = StringReplace[basestring, {"FN" -> ToString[exp, 
            TraditionalForm], "DRV" -> ToString[D[exp, x], TraditionalForm], "DMN"
             -> domainD, "RNE" -> range, "PERI" -> fPeriod}];
        newstring <> xintP <> "\n" <> yintP <> "\n" <> statP <> "\n" 
            <> inflecP <> "\n" <> vaP <> "\n" <> vaN <> "\n" <> obliqs <> "\n" <>
             vaCs
    ]

FInfo[exp_, {x_, min_, max_}] :=
    FInfo[exp, {x, min <= x <= max}]


(* ::Subsubsection:: *)
(*FInfo - General*)


FInfo[exp_, x_] :=
    FInfo[exp, {x, True}]


(* ::Section:: *)
(*End*)


End[]
