import  os,sys
from PyQt5 import QtCore,QtWidgets,QtGui
from PyDatcom.DatcomWidgets.WGPLNF import WGPLNFUI
from xml.dom.minidom import Document

class WGPLNF(QtWidgets.QWidget):
    def __init__(self):
        super(WGPLNF,self).__init__()
        self.Ui = WGPLNFUI.Ui_Form()
        self.Ui.setupUi(self)
        self.initiate()
    def initiate(self):
        path = os.path.abspath(os.path.dirname(sys.argv[0]))
        path = path.rstrip("\\WGPLNFz")  #remove the SYNTHS in the path
        path2 = "\\rc\\WGPLNF.png"
        filePath = path + path2
        icon = QtGui.QPixmap(filePath)
        icon2 = icon.scaled(500,400,0,0)
        self.Ui.image_WGPLNF.setPixmap(icon2)
        self.Ui.lineEdit_SSPNOP.setEnabled(False)
        self.Ui.lineEdit_SAVSO.setEnabled(False)
        self.Ui.lineEdit_CHRDBP.setEnabled(False)
        self.Ui.lineEdit_SSPNDD.setEnabled(False)
    def slot_enable(self):
        if self.Ui.comboBox_WShape.currentIndex()==0:
            self.Ui.lineEdit_SSPNOP.setEnabled(False)
            self.Ui.lineEdit_SAVSO.setEnabled(False)
            self.Ui.lineEdit_CHRDBP.setEnabled(False)
            self.Ui.lineEdit_SSPNDD.setEnabled(False)
        else:
            self.Ui.lineEdit_SSPNOP.setEnabled(True)
            self.Ui.lineEdit_SAVSO.setEnabled(True)
            self.Ui.lineEdit_CHRDBP.setEnabled(True)
            self.Ui.lineEdit_SSPNDD.setEnabled(True)
    def xmlWGPLNF(self):
        doc = Document()
        nameListWGPLNF = doc.createElement('NameList')
        nameListWGPLNF.setAttribute('Name', "WGPLNF")
        doc.appendChild(nameListWGPLNF)
        label = list()
        label = ['CHRDTP', 'SSPNE', 'SSPN', 'CHRDR', 'SAVSI', 'CHSTAT', 'TWISTA', 'DHDADI', 'DHDADO', 'SSPNOP', 'SAVSO','CHRDBP', 'SSPNDD']
        text = list()
        text = [self.Ui.lineEdit_CHRDTP.text(),
                self.Ui.lineEdit_SSPNE.text(),
                self.Ui.lineEdit_SSPN.text(),
                self.Ui.lineEdit_CHRDR.text(),
                self.Ui.lineEdit_SAVSI.text(),
                self.Ui.lineEdit_CHSTAT.text(),
                self.Ui.lineEdit_TWISTA.text(),
                self.Ui.lineEdit_DHDADI.text(),
                self.Ui.lineEdit_DHDADO.text(),
                self.Ui.lineEdit_SSPNOP.text(),
                self.Ui.lineEdit_SAVSO.text(),
                self.Ui.lineEdit_CHRDBP.text(),
                self.Ui.lineEdit_SSPNDD.text()]
        for i in range(0, 13):
            if text[i] != '':
                variable = doc.createElement('Variable')
                variable.setAttribute('Name', label[i])
                variable.setAttribute('StartIndex', "1")
                nameListWGPLNF.appendChild(variable)
                variable_value = doc.createElement('Value')
                variable_value_text = doc.createTextNode(text[i])
                variable.appendChild(variable_value)
                variable_value.appendChild(variable_value_text)
            else:   pass
        return (nameListWGPLNF)
if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = WGPLNF()
    window.show()
    sys.exit(app.exec_())