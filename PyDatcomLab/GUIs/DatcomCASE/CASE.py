import sys
from PyQt5 import QtCore, QtWidgets
from PyDatcom.DatcomWidgets.DatcomCASE import CASEUI
from PyDatcom.DatcomWidgets.FLTCON import FLTCON
from PyDatcom.DatcomWidgets.SYNTHS import SYNTHS
from PyDatcom.DatcomWidgets.OPTIONS import OPTIONS
from PyDatcom.DatcomWidgets.BODY import BODY
from PyDatcom.DatcomWidgets.WGPLNF import WGPLNF
from PyDatcom.DatcomWidgets.HTPLNF import HTPLNF
from PyDatcom.DatcomWidgets.VTPLNF import VTPLNF
from xml.dom.minidom import Document

class CASE(QtWidgets.QWidget):
    def __init__(self):
        super(CASE,self).__init__()
        self.Ui = CASEUI.Ui_Form()
        self.Ui.setupUi(self)
        self.fltcon = FLTCON.FLTCON()
        self.synths = SYNTHS.SYNTHS()
        self.options = OPTIONS.OPTIONS()
        self.body = BODY.BODY()
        self.wgplnf = WGPLNF.WGPLNF()
        self.vtplnf = VTPLNF.VTPLNF()
        self.htplnf = HTPLNF.HTPLNF()

    def slotFLTCON(self):
        self.fltcon.show()
    def slotSYNTHS(self):
        self.synths.show()
    def slotOPTIONS(self):
        self.options.show()
    def slotBODY(self):
        self.body.show()
    def slotWGPLNF(self):
        self.wgplnf.show()
    def slotVTPLNF(self):
        if self.fltcon.Ui.spinBox_inputNMACH.text() != '':
            self.vtplnf.Ui.lineEdit_MACHN.setText(self.fltcon.Ui.spinBox_inputNMACH.text())
        else:
            self.vtplnf.Ui.lineEdit_MACHN.setText('')
        self.vtplnf.show()
    def slotVFPLNF(self):
        pass
    def slotHTPLNF(self):
        if self.fltcon.Ui.spinBox_inputNMACH.text()!='':
            self.htplnf.Ui.lineEdit_MACHN.setText(self.fltcon.Ui.spinBox_inputNMACH.text())
        else:
            self.htplnf.Ui.lineEdit_MACHN.setText('')
        self.htplnf.show()

    def slotSAVE(self):
        self.doc = Document()
        saveNameList = self.doc.createElement('SaveNameList')
        self.doc.appendChild(saveNameList)
        #FLTCON
        saveNameListFLTCON = self.fltcon.xmlFLTCON()
        saveNameList.appendChild(saveNameListFLTCON)

        saveNameListSYNTHS = self.synths.xmlSYNTHS()
        saveNameList.appendChild(saveNameListSYNTHS)

        saveNameListOPTIONS = self.options.xmlOPTIONS()
        saveNameList.appendChild(saveNameListOPTIONS)

        saveNameListBODY = self.body.xmlBODY()
        saveNameList.appendChild(saveNameListBODY)

        saveNameListWGPLNF = self.wgplnf.xmlWGPLNF()
        saveNameList.appendChild(saveNameListWGPLNF)

        saveNameListHTPLNF= self.htplnf.xmlHTPLNF()
        saveNameList.appendChild(saveNameListHTPLNF)

        saveNameListVTPLNF = self.vtplnf.xmlVTPLNF()
        saveNameList.appendChild(saveNameListVTPLNF)

        self.Ui.textBrowser.setText(self.doc.toprettyxml(indent=''))


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = CASE()
    window.show()
    sys.exit(app.exec_())