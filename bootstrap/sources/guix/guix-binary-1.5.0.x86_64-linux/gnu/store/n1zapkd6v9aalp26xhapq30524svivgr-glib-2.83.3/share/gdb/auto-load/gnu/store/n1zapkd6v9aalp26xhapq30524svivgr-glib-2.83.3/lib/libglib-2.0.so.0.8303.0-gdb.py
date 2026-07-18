import sys
import gdb

# Update module path.
dir_ = '/gnu/store/n1zapkd6v9aalp26xhapq30524svivgr-glib-2.83.3/share/glib-2.0/gdb'
if not dir_ in sys.path:
    sys.path.insert(0, dir_)

from glib_gdb import register
register (gdb.current_objfile ())
