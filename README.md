pdfs
====

Tester and regression tester for Cpdf.

Usage
-----

1. Run 'make' to build it. Requires OCaml to be installed.

2. Put some PDFs in PDFTests/ (one simple one is included)

3. Run ./cpdftest all >foo 2>&1 to run all tests on all files and capture the output

4. Run ./cpdftest testname all >foo 2>&1 to run a single test on all files and capture the output

5. Put a number instead of 'all' to run only the first n tests.

6. Run ./clean to clean up.
