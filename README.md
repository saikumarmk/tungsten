 <!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/saikumarmk/tungsten">

  </a>

  <h3 align="center">tungsten</h3>

  <p align="center">
    A Mathematica library designed for high school assessments.
    <br />
    <br />
    <br />
  </p>
</div>


## About

This is a collection of functions that I wrote during my time in high school, where our choice of CAS was the Wolfram language, where we used Mathematica for calculations. The functions are partially cleaned up, documented, and sorted, but reflect my younger coding style, which was often inefficient, and would build on logic not always understood.

### Rationale

TBD.

## Getting Started

### Loading this module into your local install

Assuming you have a Mathematica install, navigate to the location that $UserBaseDirectory points to. For Windows, this a subfolder of the Roaming directory, for Macintosh systems it is `~/Library/Mathematica` and Linux would be `~/.Mathematica` by default. Navigate to the Applications directory, and then clone the repository into that folder.


You may then load Tungsten by running the following:

```
Needs["Tungsten`"]
```

This will load in the library.

### Paclet Compilation

Run `CreatePacletArchive[source,dir]` where `source` is the location of Tungsten, and `dir` is the directory you want the paclet to be made in. 



### Documentation

Coming soon. Refer to the Functions folder.



