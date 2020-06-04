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
            print (repr(snap))
        return
