# generic-onell

A generic implementation of the (1+(λ,λ)) genetic algorithm.
This algorithm, as well as few simpler algorithms typically used in comparisons,
can now function on search spaces other than bit strings, e.g. permutations,
or bit strings with cardinality constraints that can be used to encode spanning trees.

## Build system and dependencies

This project is written in Scala 2 and uses `sbt` as the build system. Any recent `sbt` should work.

The dependencies are (detailed in `build.sbt` and automatically fetched by `sbt`):
- `scalatest` for running tests
- `jackson-core` for some JSON I/O
- `jgrapht-core` for solving vertex covers

The project should run well with Java version 11 and higher.

## Papers that use this repository

Note that journal versions of conference papers are listed as separate entries.

* Maxim Buzdalov, Benjamin Doerr:
  Runtime analysis of the (1+(λ,λ)) genetic algorithm on random satisfiable 3-CNF formulas.
  GECCO 2017: 1343-1350
* Anton O. Bassin, Maxim Buzdalov:
  The (1+(λ,λ)) genetic algorithm for permutations.
  GECCO Companion 2020: 1669-1677
* Denis Antipov, Maxim Buzdalov, Benjamin Doerr:
  Fast mutation in crossover-based algorithms.
  GECCO 2020: 1268-1276
* Denis Antipov, Maxim Buzdalov, Benjamin Doerr:
  First Steps Towards a Runtime Analysis When Starting with a Good Solution.
  PPSN (2) 2020: 560-573
* Denis Antipov, Maxim Buzdalov, Benjamin Doerr:
  Lazy parameter tuning and control: choosing all parameters randomly from a power-law distribution.
  GECCO 2021: 1115-1123
* Anton O. Bassin, Maxim Buzdalov:
  An Experimental Study of Operator Choices in the (1+(λ,λ)) Genetic Algorithm.
  MOTOR 2020: 320–335 
* Maxim Buzdalov:
  The (1+(λ,λ)) Genetic Algorithm on the Vertex Cover Problem: Crossover Helps Leaving Plateaus.
  CEC 2022: 1-10
* Denis Antipov, Maxim Buzdalov, Benjamin Doerr:
  Fast Mutation in Crossover-Based Algorithms.
  Algorithmica 84(6): 1724-1761 (2022)
* Denis Antipov, Maxim Buzdalov, Benjamin Doerr:
  Lazy Parameter Tuning and Control: Choosing All Parameters Randomly from a Power-Law Distribution.
  Algorithmica 86(2): 442-484 (2024)
* Denis Antipov, Maxim Buzdalov, Benjamin Doerr:
  First Steps Towards a Runtime Analysis When Starting with a Good Solution.
  ACM Transactions on Evolutionary Learning and Optimization: To appear (2024)
  - Code and data archived at Zenodo, DOI: `10.5281/zenodo.11622895`.
  - Reproducibility notes. To create your own version of the files, run the following commands (the `--threads` option specifies the number of threads to run, 0 means to use the number of CPUs available). Note that this project does not have a means to specify the seed, so your results, as well as the order of lines, will be different, but for each configuration, the results should be statistically the same.
    - `onemax-sqrt.json`: `sbt "run runtime bits:om:sqrt --from 5 --to 22 --runs 100 --out onemax-sqrt.json --threads 0"`
    - `onemax-log.json`: `sbt "run runtime bits:om:log --from 5 --to 22 --runs 100 --out onemax-log.json --threads 0"`
    - `onemax-1to21-unlimited.json`: `sbt "run runtime bits:om:lin --from 22 --to 22 --runs 100 --out onemax-1to21-unlimited.json --threads 0 --d 1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152"`
    - `onemax-1to14-limited.json`: `sbt "run runtime bits:om:lin* --from 22 --to 22 --runs 100 --out onemax-1to14-limited.json --threads 0 --d 1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384"`
    - `onemax-3d-experimentA.json`: `sbt "run runtime bits:om:sqrt3da --from 3 --to 14 --runs 100 --out onemax-3d-experimentA.json --threads 0"`
    - `onemax-3d-experimentB.json`: `sbt "run runtime bits:om:sqrt3db --from 14 --to 14 --runs 100 --out onemax-3d-experimentB.json --threads 0"`


## Acknowledgments

The following contributors would like to acknowledge the support of this research by the [Russian Scientific Foundation](http://рнф.рф/en),
agreement [17-71-20178](http://рнф.рф/en/enprjcard/?rid=17-71-20178):

* [Anton Bassin](https://github.com/BASSIN)
* [Maxim Buzdalov](https://github.com/mbuzdalov)
