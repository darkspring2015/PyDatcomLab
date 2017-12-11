import  os,sys
from PyQt5 import QtCore,QtWidgets,QtGui
from PyDatcom.DatcomWidgets.HTPLNF import HTPLNFUI
from  xml.dom.minidom import Document
from PyDatcom.DatcomWidgets.DatcomCASE import CASE

class HTPLNF(QtWidgets.QWidget):
    def __init__(self):
        super(HTPLNF,self).__init__()
        self.Ui = HTPLNFUI.Ui_Form()
        self.Ui.setupUi(self)
        self.initiate()
    def initiate(self):
        path = os.path.abspath(os.path.dirname(sys.argv[0]))
        path = path.rstrip("\\HTPLNFz")  #remove the SYNTHS in the path
        path2 = "\\rc\\HTPLNF.png"
        filePath = path + path2
        icon = QtGui.QPixmap(filePath)
        icon2 = icon.scaled(500,400,0,0)
        self.Ui.image_HTPLNF.setPixmap(icon2)
        self.Ui.lineEdit_SSPNOP.setEnabled(False)
        self.Ui.lineEdit_SAVSO.setEnabled(False)
        self.Ui.lineEdit_CHRDBP.setEnabled(False)
        self.Ui.lineEdit_SSPNDD.setEnabled(False)

        self.Ui.lineEdit_CHRDTP.setText('')
        self.Ui.lineEdit_SSPNE.setText('')
        self.Ui.lineEdit_SSPN.setText('')
        self.Ui.lineEdit_CHRDR.setText('')
        self.Ui.lineEdit_SAVSI.setText('')
        self.Ui.lineEdit_CHSTAT.setText('')
        self.Ui.lineEdit_TWISTA.setText('')
        self.Ui.lineEdit_DHDADI.setText('')
        self.Ui.lineEdit_DHDADO.setText('')
        self.Ui.lineEdit_SSPNOP.setText('')
        self.Ui.lineEdit_SAVSO.setText('')
        self.Ui.lineEdit_CHRDBP.setText('')
        self.Ui.lineEdit_SSPNDD.setText('')

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
    def xmlHTPLNF(self):
        doc = Document()
        nameListHTPLNF = doc.createElement('NameList')
        nameListHTPLNF.setAttribute('Name', "HTPLNF")
        doc.appendChild(nameListHTPLNF)
        label = list()
        label = ['CHRDTP','SSPNE','SSPN','CHRDR','SAVSI','CHSTAT','TWISTA','DHDADI','DHDADO','SSPNOP','SAVSO','CHRDBP','SSPNDD']
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
        for i in range(0,13):
            if ((self.Ui.comboBox_WShape.currentIndex()==0)&(9<=i<=12 )):
                continue
            if text[i]!='':
                variable = doc.createElement('Variable')
                variable.setAttribute('Name', label[i])
                variable.setAttribute('StartIndex', "1")
                nameListHTPLNF.appendChild(variable)
                variable_value = doc.createElement('Value')
                variable_value_text = doc.createTextNode(text[i])
                variable.appendChild(variable_value)
                variable_value.appendChild(variable_value_text)
            else:pass
        return (nameListHTPLNF)

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = HTPLNF()
    window.show()
    sys.exit(app.exec_())