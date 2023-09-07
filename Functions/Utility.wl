(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


LinearSystem::usage = "Given a set of simultaneous equations, LinearSystem finds instances where there are infinite or no intersections. Usage is LinearSystem[{equations},{x,y},param]";

FTest::usage = "Tests a function against given statements about the function. Usage is FTest[f[x],{prop1,prop2...},{f,x}]";

RFTest::usage = "Tests various functions against one statement to check if it is true. Usage is RFTest[prop,{f1[x],f2[x]...},f,x]";

Bisector::usage = "Creates the perpendicular bisector from two points. Bisector[p1,p2]";

RightSum::usage = "Generates the right Riemann sum for a function f[x] over the interval [a,b] with width \[Delta]. RightSum[f[x],{x,a,b,\[Delta]}]";

LeftSum::usage = "Generates the left Riemann sum for a function f[x] over the interval [a,b] with width \[Delta]. LeftSum[f[x],{x,a,b,\[Delta]}]";

TangentLine::usage = "
TangentLine[function,variable,point] returns the equation of the tangent at a point."

NormalLine::usage = "
NormalLine[function,variable,point] returns the equation of the normal at a point."

QuickLine::usage = "Returns the equation of a line from two points. Input is QuickLine[a,b] where a and b are coordinates.";

AverageValue::usage = "Calculates the average value of a function f on the interval [a,b]. Input is AverageValue[f[x],{x,a,b}].";

AverageRate::usage = "Calculates the average rate of change of a function f on the interval [a,b]. Input is AverageRate[f[x],{x,a,b}].";

RatDenom::usage = "Rationalizes an expression.";

Cis::usage = "A function for the polar form of a complex number. Usage is Cis[\[Theta]]=Cos[\[Theta]]+\[ImaginaryI] Sin[\[Theta]]";

PEliminate::usage = "Returns the cartesian equation of a parametric set of equations.";

Substitute::usage = "Performs a u substitution, changing the bounds of an integral.";

FMax::usage = "Returns the coordinates of a global/local maximum.";

FMin::usage = "Returns the coordinates of a global/local minimum.";

FSlopeSketch::usage = "Sketches five slope fields.";

ScalarRes::usage = "Gives the scalar resolute of u onto v. Usage is ScalarRes[u,v]";

ToDeg::usage = "Converts radians to degrees.";

ToRad::usage = "Converts degrees to radians.";

TurningPointForm::usage = "Convert standard form quadratic to turning point form.";

Transform::usage = "Applies a transformation to a function. Transform[function,{x,y},M,b] where M is the 2x2 dilation matrix and b is the 2x1 translation column.";

InverseFunc::usage = "Returns the inverse function within a certain region. Usage is InverseFunc[f[x],{x,y},{a,b}]";

DomRan::usage = "Returns the domain and range of a function.";

Dp::usage = "Gives a number to n decimal places. Input is Dp[expression,n]";

CircleForm::usage = "Returns the radius and centre of a complex subset.";

ToCircle::usage = "Converts a circle into its standard form. Input is ToCircle[expressionOfTheCircle]";

ComplexDrive::usage = "Given a complex subset in terms of z, it will return the cartesian equation/relation. Usage is ComplexDrive[zrel,{x,y}]";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Line Construction*)


(* ::Subsubsection:: *)
(*TangentLine*)


TangentLine[f_, var_, a_, y_:y] :=
    Expand[y /. Solve[y - (f /. var -> a) == (D[f, var] /. var -> a) 
        (var - a), y, Reals][[1]]];

TangentLine[eq_, {x_, y_}, {a_, b_}] :=
    Module[{gradient},
        gradient = ImpDiff[eq, {x, y, 1}][[2]] /. {x -> a, y[x] -> b}
            ;
        Return[Solve[y - b == gradient (x - a), y][[1, 1]] /. Rule ->
             Equal]
    ]

TangentLine[eq_, {x_, y_}, a_] :=
    Module[{yvalues},
        yvalues = y /. Solve[(eq /. x -> a), y];
        Column @ Thread[{Thread[{a, yvalues}], TangentLine[eq, {x, y},
             {a, #}]& /@ yvalues}]
    ]


(* ::Subsubsection:: *)
(*NormalLine*)


NormalLine[f_, var_, a_, y_:y] :=
    Expand[y /. Solve[y - (f /. var -> a) == -(D[f, var] /. var -> a)
         ^ -1 (var - a), y, Reals][[1]]];

NormalLine[eq_, {x_, y_}, {a_, b_}] :=
    Module[{gradient},
        gradient = ImpDiff[eq, {x, y, 1}][[2]] /. {x -> a, y[x] -> b}
            ;
        Return[Solve[y - b == -1 / gradient (x - a), y][[1, 1]] /. Rule
             -> Equal]
    ]

NormalLine[eq_, {x_, y_}, a_] :=
    Module[{yvalues},
        yvalues = y /. Solve[(eq /. x -> a), y];
        Column @ Thread[{Thread[{a, yvalues}], NormalLine[eq, {x, y},
             {a, #}]& /@ yvalues}]
    ]


(* ::Subsubsection:: *)
(*QuickLine*)


QuickLine[a_, b_] :=
    Module[{},
        Solve[Global`y - a[[2]] == (a[[2]] - b[[2]]) / (a[[1]] - b[[1
            ]]) (Global`x - a[[1]]), Global`y][[1, 1]] /. Rule -> Equal
    ]


(* ::Subsubsection:: *)
(*Bisector*)


Bisector[a_, b_] :=
    Module[{},
        Solve[Global`y - (a[[2]] + b[[2]]) / 2 == -((a[[2]] - b[[2]])
             / (a[[1]] - b[[1]])) ^ -1 (Global`x - (a[[1]] + b[[1]]) / 2), Global`y
            ][[1, 1]] /. Rule -> Equal
    ]


(* ::Subsection:: *)
(*Transform*)


(* ::Subsubsection:: *)
(*Transform*)


Transform[f_, {x_, y_}, m_, b_] :=
    Module[{tvec, xd, yd},
        tvec = Solve[{xd, yd} == m . {x, y} + b, {x, y}][[1, All, 2]]
            ;
        Solve[tvec[[2]] == f /. x -> tvec[[1]], yd][[1, 1, 2]] /. {yd
             -> y, xd -> x}
    ]

Transform[f_, {x_, y_}, matrix_] :=
    Module[{tvec, xd, yd},
        tvec = Solve[{xd, yd} == matrix, {x, y}][[1, All, 2]];
        Solve[tvec[[2]] == f /. x -> tvec[[1]], yd][[1, 1, 2]] /. {yd
             -> y, xd -> x}
    ]


(* ::Subsection:: *)
(*Average Value and Average Rate*)


(* ::Subsubsection:: *)
(*AverageValue*)


AverageValue[f_, {x_, a_, b_}] :=
    Module[{},
        1 / (b - a) Integrate[f, {x, a, b}]
    ]


(* ::Subsubsection:: *)
(*AverageRate*)


AverageRate[f_, {x_, a_, b_}] :=
    Module[{upperValue, lowerValue},
        upperValue = f /. x -> b;
        lowerValue = f /. x -> a;
        (upperValue - lowerValue) / (b - a)
    ]


(* ::Subsection:: *)
(*TurningPointForm*)


(* ::Subsubsection:: *)
(*TurningPointForm*)


TurningPointForm[a_, b_, c_] :=
    TraditionalForm[a (Global`x + b / (2 a)) ^ 2 + ((4 a * c - b^2) /
         (4 a))];

TurningPointForm[f_, x_:x] :=
    TraditionalForm[a (x - b) ^ 2 + c /. SolveAlways[f == a (x - b) ^
         2 + c, x][[1]]];


(* ::Subsection:: *)
(*PEliminate*)


PEliminate[r_, t_] :=
    Eliminate[Global`x == (r)[[1]] && Global`y == (r)[[2]], t]

ClearAll[Substitute];

SetAttributes[Substitute, HoldAll];

Substitute[Integrate[f_, x_], u_ -> gx_] :=
    With[{sub = First @ Solve[u == gx, x] /. _C -> 0},
        With[{integrand = (f /. sub) D[x /. sub, u]},
            Defer @ Integrate[integrand, u]
        ]
    ];

Substitute[Integrate[f_, x_], {u_, sub : (x_ -> gu_)}] :=
    With[{integrand = (f /. sub) D[gu, u]},
        Defer @ Integrate[integrand, u]
    ];

Substitute[Integrate[f_, {x_, x1_, x2_}], u_ -> gx_] :=
    With[{sub = First @ Solve[u == gx, x] /. _C -> 0},
        With[{integrand = (f /. sub) D[x /. sub, u], u1 = gx /. x -> 
            x1, u2 = gx /. x -> x2},
            Defer @ Integrate[integrand, {u, u1, u2}]
        ]
    ];

Substitute[Integrate[f_, {x_, x1_, x2_}], {u_, sub : (x_ -> gu_)}] :=
    With[{integrand = (f /. sub) D[gu, u], u1 = u /. First @ Solve[gu
         == x1, u], u2 = u /. First @ Solve[gu == x2, u]},
        Defer @ Integrate[integrand, {u, u1, u2}]
    ]


(* ::Subsection:: *)
(*LinearSystem*)


(* ::Subsubsection:: *)
(*LinearSystem*)


LinearSystem[eqs_, {x_, y_}, par_] :=
    Module[{det, InfOrNone},
        det = Det[Normal[CoefficientArrays[eqs, {x, y}][[-1]]]];
        InfOrNone = Solve[det == 0, par][[All, 1, 2]];
        Table[
                If[Quiet @ Solve[eqs /. par -> InfOrNone[[i]], {x, y}
                    ] == {},
                    ToString[par] <> "=" <> ToString[InfOrNone[[i]], 
                        TraditionalForm] <> " - No Solution"
                    ,
                    ToString[par] <> "=" <> ToString[InfOrNone[[i]], 
                        TraditionalForm] <> " - Infinite solutions"
                ]
                ,
                {i, 1, Length[InfOrNone]}
            ] // TableForm
    ];


(* ::Subsection:: *)
(*FTest*)


(* ::Subsubsection:: *)
(*FTest*)


FTest[fx_, prop_, {f_:f, x_:x}] :=
    Module[{refx, g, reprop, dom},
        g[y_] := fx /. {x -> y};
        reprop = prop /. f[var_] :> g[var];
        reprop = FullSimplify /@ reprop;
        TableForm @
            Table[
                If[BooleanQ[reprop[[i]]] && TrueQ[reprop[[i]]],
                    ToString[prop[[i]], TraditionalForm] <> " is true"
                        
                    ,
                    ToString[prop[[i]], TraditionalForm] <> " is NOT true"
                        
                ]
                ,
                {i, 1, Length[prop]}
            ]
    ];


(* ::Subsection:: *)
(*RFTest*)


(* ::Subsubsection:: *)
(*RFTest*)


RFTest[prop_, fx_, {f_, x_}] :=
    Module[{doms, sub, result, variables},
        variables = DeleteDuplicates @ Cases[prop, _Symbol, \[Infinity]];
        doms = Table[FunctionDomain[# /. x -> variables[[i]], variables
            [[i]]], {i, 1, Length[variables]}]& /@ fx;
        sub = Table[prop //. f[var_] :> (fx[[i]] /. x -> var), {i, 1,
             Length[fx]}];
        result = Table[FullSimplify[sub[[i]], doms[[i]]], {i, 1, Length[
            sub]}];
        TableForm @
            Table[
                If[BooleanQ[result[[i]]] && TrueQ[result[[i]]],
                    ToString[fx[[i]], TraditionalForm] <> " satisfies the property."
                        
                    ,
                    ToString[fx[[i]], TraditionalForm] <> " DOES NOT satisfies the property."
                        
                ]
                ,
                {i, 1, Length[fx]}
            ]
    ]


(* ::Subsection:: *)
(*RiemannSum*)


(* ::Subsubsection:: *)
(*RightSum*)


RightSum[fx_, {x_, a_, b_, delta_}] :=
    Module[{intervalLength, numberOfRectangles},
        intervalLength = b - a;
        numberOfRectangles = intervalLength / delta;
        delta Sum[(fx /. x -> delta n + a), {n, 1, numberOfRectangles
            }]
    ]


(* ::Subsubsection:: *)
(*LeftSum*)


LeftSum[fx_, {x_, a_, b_, delta_}] :=
    Module[{intervalLength, numberOfRectangles},
        intervalLength = b - a;
        numberOfRectangles = intervalLength / delta;
        delta Sum[(fx /. x -> delta (n - 1) + a), {n, 1, numberOfRectangles
            }]
    ]


(* ::Subsubsection:: *)
(*ScalarRes*)


ScalarRes[u_, v_] :=
    Simplify @ Dot[u, Normalize[v]];


(* ::Subsubsection:: *)
(*Cis*)


Cis[x_] :=
    Cos[x] + I Sin[x];


(* ::Subsubsection:: *)
(*ToDeg*)


ToDeg[Radian_] :=
    N @ Radian * 180 / Pi;


(* ::Subsubsection:: *)
(*ToRad*)


ToRad[Degree_] :=
    N @ Degree * Pi / 180;


(* ::Subsubsection:: *)
(*FMax*)


FMax[fx_, x_] :=
    Maximize[fx, x]


(* ::Subsubsection:: *)
(*FMin*)


FMin[fx_, x_] :=
    Minimize[fx, x]


(* ::Subsubsection:: *)
(*FSlopeSketch*)


FSlopeSketch[list_, varx_:x, vary_:y] :=
    Quiet @ StreamPlot[{1, #}, {varx, -10, 10}, {vary, -10, 10}, Frame
         -> False, Axes -> True]& /@ list


(* ::Subsubsection:: *)
(*RatDenom*)


RatDenom[x_?NumericQ] :=
    Module[{y, nn, dd, f, g, c, k, blah},
        (
            y = Together[x];
            nn = Numerator[y];
            dd = Denominator[y];
            f = MinimalPolynomial[dd, t];
            c = f /. t -> 0;
            g = Factor[(c - f) / t];
            {k, blah} = FactorTermsList[Expand[nn * (g /. t -> dd)]];
                
            Sign[c] ((k / GCD[k, c]) * blah) / HoldForm[Evaluate @ Abs[
                c / GCD[k, c]]]
        )
    ]

RatDenom[x__] :=
    RatDenom[#]& /@ x


(* ::Subsubsection:: *)
(*DomRan*)


DomRan[f_, x_, y_] :=
    {FunctionDomain[f, x], FunctionRange[f, x, y]}


(* ::Subsection:: *)
(*InverseFunc*)


(* ::Subsubsection:: *)
(*InverseFunc*)


InverseFunc[func_, {x_, y_}, {x0_, y0_}] :=
    Module[
        {f, sol, eqSol, uc}
        ,
        (*Solves for an inverse function over a branch*)
        sol = Flatten @ Solve[y == func, {x}, Reals][[All]];
        eqSol = # /. {Rule -> Equal, x -> x0, y -> y0}& /@ sol;
        eqSol = ToRadicals /@ FullSimplify[eqSol];
        uc = Pick[sol, eqSol][[1]];
        uc = ToRadicals /@ uc /. {x -> y, y -> x};
        uc /. r_ ^ Rational[1, p_] -> Surd[r, p]
    ];


(* ::Subsection:: *)
(*Complex Subsets*)


(* ::Subsubsection:: *)
(*CircleForm*)


CircleForm[rel_, {x_:x, y_:y}] :=
    Module[{reg},
        reg = ImplicitRegion[ComplexExpand[rel], {x, y}];
        {RegionCentroid[reg], Sqrt[Area[reg] / \[Pi]]}
    ];

CircleForm[rel_] :=
    CircleForm[rel, Variables[rel]]


(* ::Subsubsection:: *)
(*ComplexDrive*)


ComplexDrive[zrel_, {x_:x, y_:y}] :=
    Module[{rel, userel, trel, circ},
        rel = zrel /. Symbol["z"] -> x + I y;
        If[Head[rel[[1]]] === Head[rel[[2]]],(*Meaning is it a line*)
            
            userel = Solve[ComplexExpand[rel], y][[1]] /. {Rule -> Equal
                };
            Return[userel]
            ,
            If[Head[rel[[1]]] === Abs || Head[rel[[2]]] === Abs, (*is
                 it a circle*)
                trel = {ReplacePart[rel, 0 -> GreaterEqual], ReplacePart[
                    rel, 0 -> LessEqual]};
                If[NumericQ[CircleForm[trel[[1]], {x, y}][[-1]]],
                    circ = CircleForm[trel[[1]], {x, y}];
                    Return[(x - circ[[1, 1]]) ^ 2 + (y - circ[[1, 2]]
                        ) ^ 2 == circ[[2]] ^ 2]
                    ,
                    circ = CircleForm[trel[[2]], {x, y}];
                    Return[(x - circ[[1, 1]]) ^ 2 + (y - circ[[1, 2]]
                        ) ^ 2 == circ[[2]] ^ 2]
                ]
                ,
                Return[ComplexExpand[rel]]
            ]
        ]
    ];

ComplexDrive[zrel_] :=
    ComplexDrive[zrel, {Global`x, Global`y}]


(* ::Subsubsection:: *)
(*ToCircle*)


ToCircle[rel_] :=
    Module[{solver},
        solver = SolveAlways[(rel /. Equal -> Subtract) == (x - h) ^ 
            2 + (y - k) ^ 2 - r^2, {x, y}];
        If[solver === {},
            Return["Not a Circle"]
            ,
            Return[(Global`x - h) ^ 2 + (Global`y - k) ^ 2 == r^2 /. 
                solver[[1]]]
        ]
    ]


(* ::Section:: *)
(*End*)


End[]
