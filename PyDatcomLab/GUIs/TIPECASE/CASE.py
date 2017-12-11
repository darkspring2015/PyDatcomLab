import sys
from PyQt5 import QtCore, QtWidgets
from PyDatcom.DatcomWidgets.FLTCON import FLTCON
from PyDatcom.DatcomWidgets.SYNTHS import SYNTHS
from PyDatcom.DatcomWidgets.OPTIONS import OPTIONS
from PyDatcom.DatcomWidgets.BODY import BODY
from PyDatcom.DatcomWidgets.WGPLNF import WGPLNF
from PyDatcom.DatcomWidgets.HTPLNF import HTPLNF
from PyDatcom.DatcomWidgets.VTPLNF import VTPLNF


class CASE(QtWidgets.QWidget):
    def __init__(self):
        super(CASE,self).__init__()
        self.fltcon = FLTCON.FLTCON()
        self.synths = SYNTHS.SYNTHS()
        self.options = OPTIONS.OPTIONS()
        self.body = BODY.BODY()
        self.wgplnf = WGPLNF.WGPLNF()
        self.vtplnf = VTPLNF.VTPLNF()
        self.htplnf = HTPLNF.HTPLNF()

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    sys.exit(app.exec_())