(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


(* ::Subsection:: *)
(*Usage - <Probability>*)


(*Usage - Probability*)

HGD::usage = "Shortcut for HypergeometricDistribution. Input is number of trials, defectives and total population";

ND::usage = "Shortcut for NormalDistribution. Input is \[Mu] and \[Sigma].";

BD::usage = "Shortcut for BinomialDistribution. Input is n and p.";

SD::usage = "Computes the standard deviation of a random variable.";

Var::usage = "Computes the variance of a random variable.";

RVAdd::usage = "Adds two random variables.";

Pr::usage = "Shortcut for NProbability. Input is the Probability input.";

PD::usage = "Defines a ProbabilityDistribution.";

ED::usage = "EmpiricalDistribution shortcut.";

PropCI::usage="Returns a proportion confidence interval. PropCI[p,n,clevel]";

TProbability::usage = "TProbability[{success,failure},{succesname,failurename},initialstate,{possible states}]";

DProbability::usage = "DProbability[{success,failure},{successname,failurename},initialstate,combo]";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*TProbability*)


(* ::Subsubsection:: *)
(*TProbability*)


TProbability[{out1_, out2_}, {succ_, fail_}, init_, dess_] :=
    Total[DProbability[{out1, out2}, {succ, fail}, init, #]& /@ dess]


(* ::Subsubsection:: *)
(*DProbability*)


DProbability[{out1_, out2_}, {succ_, fail_}, init_, des_] :=
    Module[{a, b, c = 1, i, t, chk = {}},
        For[i = 0, i <= StringLength[des], ++i,
            If[i == 0,
                If[init == succ,
                    b = out1
                    ,
                    b = out2
                ]
                ,
                If[StringPart[des, i] == succ,
                    If[i == 1,
                        If[StringPart[des, i] == init,
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            b = out1
                            ,
                            AppendTo[chk, b];
                        ]
                        ,
                        If[StringPart[des, i] == StringPart[des, i - 
                            1],
                            b = out1;
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            AppendTo[chk, 1 - b];
                            b = out1;
                        ]
                    ]
                    ,
                    If[i == 1,
                        If[StringPart[des, i] == init,
                            b = out2;
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            AppendTo[chk, 1 - b];
                            b = out2;
                        ]
                        ,
                        If[StringPart[des, i] == StringPart[des, i - 
                            1],
                            b = out2;
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            AppendTo[chk, 1 - b];
                            b = out2;
                        ]
                    ]
                ]
            ]
        ];
        c
    ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


(* ::Subsubsection:: *)
(*PropCI*)


PropCI[n_, p_, clevel_:95] :=
    {p + Quantile[ND[], (1 - clevel / 100) * 0.5] Sqrt[(p (1 - p)) / 
        n], p + Quantile[ND[], (1 - clevel / 100) * 0.5 + clevel / 100] Sqrt[
        (p (1 - p)) / n]}


(* ::Subsection:: *)
(*Various Shortcuts*)


(* ::Subsubsection:: *)
(*Probability*)


HGD[n_, D_, N_] :=
    HypergeometricDistribution[n, D, N];

ND[\[Mu]_:0, \[Sigma]_:1] :=
    NormalDistribution[\[Mu], \[Sigma]];

BD[n_, p_] :=
    BinomialDistribution[n, p];

SD[x_] :=
    StandardDeviation[x];

Var[x_] :=
    Variance[x];

RVAdd[a_, b_] :=
    TransformedDistribution[a, b];

Pr[x_, y_] :=
    Probability[x, y];

PD[f_, {x_, a_, b_}] :=
    ProbabilityDistribution[f, {x, a, b}];

ED[a_, b_] :=
    EmpiricalDistribution[a -> b]


(* ::Section:: *)
(*End*)


End[]
