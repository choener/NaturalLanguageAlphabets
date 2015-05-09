[![Build Status](https://travis-ci.org/choener/NaturalLanguageAlphabets.svg?branch=master)](https://travis-ci.org/choener/NaturalLanguageAlphabets)

# Natural Language Alphabets

Efficient, alphabet symbols. The symbols are interned, and hashed. This is
quite useful for k-gram scoring, where we have different sets of symbols with
different scores. IMMC symbols are internally represented via Ints in the range
[0..]. This makes it possible to use unboxed containers when handling IMMC
symbols.



#### Contact

Christian Hoener zu Siederdissen
choener@bioinf.uni-leipzig.de
Leipzig University, Leipzig, Germany

