import unittest
import os
import gzip
import json

import kappy

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

class KappaSnapshotTest(unittest.TestCase):

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
