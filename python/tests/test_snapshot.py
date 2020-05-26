##
## Test snapshot parsing capacities
##
import os
from util import _KappaClientTest, run_nose

import kappy
import kappy.kappa_snap as kappa_snap
from kappy.kappa_std import BIN_DIR
import unittest

SNAPSHOTS = ["snap_1d_polymers"]

SNAP_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        "snapshots")

if not os.path.isdir(SNAP_DIR):
    raise Exception("Expected snapshot directory at: %s" %(SNAP_DIR))

def get_snapshot_fname(snap_name):
    if snap_name not in SNAPSHOTS:
        raise Exception("No snapshot %s" %(snap_name))
    fname = os.path.join(SNAP_DIR, "%s.json.gz" %(snap_name))
    if not os.path.isfile(fname):
        raise Exception("Cannot find snapshot file %s" %(fname))
    return fname

# Not subclassing _KappaClientTest for now until other unit test errors are resolved
## TODO: we should try to fix this, unless not all test suites need to be part of _KappaClientTest.
## snapshot parsing should depend on calling any KaSim agents
class TestSnapshot(unittest.TestCase):#_KappaClientTest):
    """Test snapshot parsing capacities."""

    def getRuntime(self):
        ### This fails!
        return kappy.KappaStd(BIN_DIR)

    def test_snap_1d_polyers(self):
        """Test if we can parse a snapshot with 1-D array format 
           and extract statistics from it. This snapshot should have ten different complex
           types, with the most abundant complex having 10 copies."""
        fname = get_snapshot_fname("snap_1d_polymers")
        snap_obj = kappa_snap.KappaSnapshot(from_fname=fname)
        # get number of complexes
        num_complexes = len(snap_obj.complexes)
        assert(num_complexes == 10), "Expected 10 complexes in %s" %(fname)
        # get the most abundant complex, which should exist in 10 copies
        complexes_by_abun = snap_obj.get_complexes_by_abundance()
        abun, most_abun_comp = complexes_by_abun[0]
        # check that the most abundant complex has abundance of 10
        assert(abun == 10), \
          "Expected most abundance complex %s to have abundance of 10." %(most_abun_comp)
        # the complex object also stores its own abundance
        assert(most_abun_comp.abundance == 10), \
          "Expected most abundance complex %s to have abundance of 10." %(most_abun_comp)
          

if __name__ == '__main__':
    run_nose(__file__)

        
        


