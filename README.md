Problem solutions for [Rosalind](http://rosalind.info/), which provides challenging
bioinformatics problems.

Registration is encouraged; it allows the developer to download larger sample data
sets and verify their solutions.

Each Erlang module should have unit tests and uses a standard interface:

* `run()` with arguments per the problem specification
    + Will return the data as specified by the problem, but not formatted
      properly; see `o/1` and `o/2` below
* `file("input filename")` for longer input like [FASTA](http://en.wikipedia.org/wiki/FASTA_format)
* `o(run())` to generate output that matches the Rosalind specification
* `o(run(), FH)` to supply an output file handle for longer output
    + When needed, so far only for the `cons` problem
* `eunit:test(module)` to run the unit tests

Example from [Finding a Motif in DNA](http://rosalind.info/problems/subs/):


````erlang
1> eunit:test(subs).
  Test passed.
ok
2> subs:run("GATATATGCATATACTT", "ATAT").
[2,4,10]
3> subs:o(subs:run("GATATATGCATATACTT", "ATAT")).
2 4 10
ok
4> subs:o(subs:run("TCCACCGACTGGAATCGACATCACCGACACACCGACTATCTACACCGACTCACCGACGTCGTTCCACCGACCTCACCGACCACCGACCACCGACCACCGACTCAGGCACCGACACACCGACGGTACACCGACGGGGGCACCGACCCACCGACTATGCCACACCGACGCCACCGACCACCGACCACTTACTTCACCGACCACCGACGCACACCGACACGACAGCACCGACCCCCACCGACCACCGACCCCTCCACCGACTACACCGACCACCGACCTCAACGTCTCAAGACTCACCGACCCACCGACCACCGACCCCACCGACTTTTAAACACCGACACACCGACGCACCGACCCACCGACCTGGGCACCGACGTCTCCACCGACTCACCGACGAGAGGCACCGACCACCGACTCACCGACCGTCCACCGACCACCGACCCGCACCGACCACCGACCTGTTCGCAATGTACACCGACTTATCCACCGACGGCTCACCGACCACCGACTGGTATGGCGCCCACACCGACCGCCACCGACCACCGACCACCGACCACCGACACTTACGGTTCACCGACTACGCACCGACCCACCGACCACCGACGCACCGACGTTCACCGACCACCGACGGATCGCACCGACCCACCGACCACCGACACACCGACCACCGACCACCGACCGCACCGACTGCTGGCACCGACGTCCACCGTGGTACACCGACCCACCGACCCAGCCACCGACCACCGACCACCGACCTTCTCACCGACGGTCACCGACGTCCCACCGACTCACCGACAGGCCTTCACCGACCACCGAC", "CACCGACCA")).
74 81 88 169 176 192 233 261 300 399 425 442 493 531 538 545 588 613 641 656 663 732 739 801
ok
````

[My profile on Rosalind](http://rosalind.info/users/macintux/)
