import unittest
import os
import gzip
import json

import kappy

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

class KappaSnapshotTest(unittest.TestCase):

    def test_parse_string(self):
        dimer = kappy.KappaComplex.from_string("A(y[_\n], a[1]{u}), B(b{# }[1] /*lool*/x{   p},y)")
        assert(len(dimer) == 2)
        return

    def test_embeddings(self):
        # one agent
        assert(kappy.KappaComplex.from_string("A(a[_])") in
               kappy.KappaComplex.from_string("A(b[1] a[2]), A(b[3] a[2]), B(a[1] x{p}), B(a[3] x{u})"))
        # negative
        assert(not kappy.KappaComplex.from_string("A(b[.])") in
               kappy.KappaComplex.from_string("A(b[1] a[2]), A(b[3] a[2]), B(a[1] x{p}), B(a[3] x{u})"))
        assert(not kappy.KappaComplex.from_string("A(b[.])") in
               kappy.KappaComplex.from_string("C(b[.])"))
        # several agents
        nasty=kappy.KappaComplex.from_string("A(b[1] a[2]), A(b[3] a[2]), B(a[1] x{p}), B(a[3] x{u})")
        assert(len(nasty.find_pattern(kappy.KappaComplex.from_string("A(b[2]), B(a[2] x)"))) == 2)
        assert(len(nasty.find_pattern(kappy.KappaComplex.from_string("A(b[2]), B(a[2] x{u})"))) == 1)
        # symmetries
        assert(len(nasty.find_pattern(kappy.KappaComplex.from_string("A(a[5]), A(a[5])"))) == 2)
        # canonical representation killer
        assert(nasty ==
               kappy.KappaComplex.from_string("A(a[1] b[2]), A(a[1] b[3]), B(a[2] x{u}), B(a[3] x{p})"))
        # with loops
        assert(kappy.KappaComplex.from_string("A(b[1] c[2]), B(a[1] c[3]), C(a[2] b[3])") ==
               kappy.KappaComplex.from_string("B(a[1] c[2]), A(b[1] c[3]), C(a[3] b[2])"))

    def test_parse_1d(self):
        with gzip.open(os.path.join(THIS_DIR,"snap_1d_polymers.json.gz")) as f:
            snap = kappy.KappaSnapshot.from_JSONDecoder(json.load(f))
            # check repr
            re_entry = repr(snap)
            # get number of complexes
            num_complexes = len(snap.complexes)
            assert(num_complexes == 10), "Expected 10 complexes in %s" %(fname)
            # get the most abundant complex, which should exist in 10 copies
            big_size,[(big_abun, big_comp)] = snap.get_largest_complexes()
            # check that the biggest complex has abundance of 1
            assert(big_abun == 1), \
                "Expected biggest complex %s to have abundance of 1." %(big_comp)
            small_abun,[small_comp] = snap.get_most_abundant_complexes()
            # check that the smallest complex has abundance of 10
            assert(small_abun == 10), \
                "Expected smallest complex %s to have abundance of 10." %(small_comp)
            return
