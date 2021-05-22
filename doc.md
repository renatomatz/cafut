---
project: cafut
summary: Unit Testing for Coarray Fortran
project_github: https://github.com/renatomatz/cafut.git
author: Renato Zimmermann
author_email: renato.zimmermann@mail.utoronto.ca
github: https://github.com/renatomatz
src_dir: ./src
exclude_dir:./test
source: true
proc_internals: true
sort: permission-alpha
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
---

The point of this library is to provide a simple, object-oriented unit testing framework meant for applications using Coarray Fortran. As it is further developped, we place more importance in brevity than to features, and for that reason, this project aims at maintaining all code within a single module that can be placed with the tested scripts.

Other unit testing frameworks for Fortran (that I know of) give unorganized outputs when the program is meant to run in parallel using Coarrays. This ultimately comes down to coordinating test results amongst images such that test summaries are more organized and informative when it comes to the results of each image. Workarounds that revolve around adapting tests to immitate parallel behavior will run slower and everything else that made you want to write a parallel program in the first place.

# Setup

To use cafut to its fullest, your compiler must have implemented Fortran Coarrays. In the examples shown bellow, I will be using [OpenCoarrays](https://github.com/sourceryinstitute/OpenCoarrays), though there are several other options available. 

If you wish to compile cafut for testing with a single image (such as using cafut for sequential programs), you can add the `-fcoarray=single` flag to gcc in order to compile the program.

### Quick and Dirty

If you want to use cafut for some quick tests on a specific file, start by cloning cafut and generating the necessary .mod file with:

```sh
git clone https://github.com/renatomatz/cafut.git
cd cafut/src/
caf -c cafut.f90
```

where `caf` is the OpenCoarray compiler for using cafut with multiple images.

Then, just add the `cafut.mod` file, which should be an output from the above command, to the same directory that the program you wish to test is located. To use cafut, simply add `use cafut` to the test program.

### Fortran Package Manager (fpm)

To use cafut for testing in your [fpm](https://github.com/fortran-lang/fpm) project without making it a build dependency, just add the following to your package manifest file (`fpm.toml`) under the relevant test descriptions (which in the example bellow, we call _main_):

```toml
[[ test ]]
name="main"
source-dir="test/"
main="main.f90"
[test.dependencies]
cafut = { git = "https://github.com/renatomatz/cafut.git" }
```

You can then `use` the package in your testing programs with `use cafut`.

# Objects

TODO: write description of the main objects.

# Examples

TODO: write examples
