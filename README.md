# reMLIC

Reimplementation of MLIC [1], an interpretable machine learning system in Common Lisp.

[1] Malioutov, Dmitry, and Kuldeep S. Meel. "Mlic: A maxsat-based framework for learning interpretable classification rules." International Conference on Principles and Practice of Constraint Programming. Springer, Cham, 2018.

## Installation

``` shell
ros install numcl/constantfold numcl/specialized-function numcl/gtype numcl/numcl guicho271828/remlic
```

## Usage

    mlic           input output rules &rest args &key accuracy sparsity diversity unlabeled oneshot verbose evaluate val-input val-output

Runs a MLIC algorithm on a bit array input and a bit-array output, both in [NUMCL-compatible format](https://github.com/numcl/numcl/blob/master/doc/DETAILS.org#representation).
The first dimension is treated as the batch dimension.

`accuracy` and `sparsity` controls the penalization weights in MLIC that
balances the accuracy and the interpretability.

MLIC input is, similar to a fully-connected NN, not structured. Therefore,
arrays with rank larger than 2 are internally reshaped into an 2D array.
MLIC also runs the training for each output dimension separately.


    mlic-core      input output rules &rest regularizers class evaluate verbose timelimit val-input val-output solver-args

This is the base function called by mlic / mlic-iterative. All keywords accepted
by this function is also accepted by mlic / mlic-iterative.

`solver-args` is lambda-list arguments to `cl-maxsat:solve`, which can specify the solver.
The default value is `(:maxsat-competition :year 2017 :track :complete :name :maxhs)`.

`class` specifies how we model the constraints. `cnf` is the standard mode used in the original MLIC paper.
However, we also add several other classes that may better characterize your dataset: `cnf`, `dnf`, `eqv`, and so on.
See the corresponding documentation string in `src/2constraints.lisp`.

    mlic-iterative input output rules &rest args &key initial-samples max-samples callback by timelimit method evaluate val-input val-output

Since MLIC is computationally intensive (using Max-SAT solver), we made an
iterative variant that starts form the small dataset and increase the sample
size until the validation accuracy meets a certain threshold.
`callback` specifies the function that specifies the condition.
For details, see the implementation of function `validation-above-90`.

## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.4.12 on X86-64 Linux 4.4.0-141-generic (author's environment)

Also, it depends on the following libraries:

+ cl-maxsat by *Masataro Asai* : Common Interface to MAX-SAT Solvers from Common Lisp
+ numcl by *Masataro Asai* : Numpy clone in Common Lisp
+ trivial-timeout by *Gary Warren King* : OS and Implementation independent access to timeouts
+ trivia by *Masataro Asai* :
    NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility




## Author, License, Copyright

Licensed under LGPL v3.

Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
