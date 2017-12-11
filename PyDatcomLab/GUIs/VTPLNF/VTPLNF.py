import  os,sys
from PyQt5 import QtCore,QtWidgets,QtGui
from PyDatcom.DatcomWidgets.VTPLNF import VTPLNFUI
from xml.dom.minidom import Document

class VTPLNF(QtWidgets.QWidget):
    def __init__(self):
        super(VTPLNF,self).__init__()
        self.Ui = VTPLNFUI.Ui_Form()
        self.Ui.setupUi(self)
        self.initiate()
    def initiate(self):
        path = os.path.abspath(os.path.dirname(sys.argv[0]))
        path = path.rstrip("\\VTPLNFz")  #remove the SYNTHS in the path
        path2 = "\\rc\\VTPLNF.png"
        filePath = path + path2
        icon = QtGui.QPixmap(filePath)
        icon2 = icon.scaled(500, 400, 0, 0)
        self.Ui.image_VTPLNF.setPixmap(icon2)
        self.Ui.lineEdit_SSPNOP.setEnabled(False)
        self.Ui.lineEdit_SAVSO.setEnabled(False)
        self.Ui.lineEdit_CHRDBP.setEnabled(False)
        self.Ui.tableWidget.setColumnCount(3)
        self.Ui.tableWidget.setColumnWidth(0, 50);self.Ui.tableWidget.setColumnWidth(1,50);self.Ui.tableWidget.setColumnWidth(2,50);self.Ui.tableWidget.setColumnWidth(3,50)
        self.Ui.tableWidget.setRowCount(1)
        self.Ui.tableWidget.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem('SVWB'))
        self.Ui.tableWidget.setHorizontalHeaderItem(1, QtWidgets.QTableWidgetItem('SVB'))
        self.Ui.tableWidget.setHorizontalHeaderItem(2, QtWidgets.QTableWidgetItem('SVHB'))
        self.Ui.tableWidget.setItem(0, 0, QtWidgets.QTableWidgetItem(''))
        self.Ui.tableWidget.setItem(0, 1, QtWidgets.QTableWidgetItem(''))
        self.Ui.tableWidget.setItem(0, 2, QtWidgets.QTableWidgetItem(''))
        self.Ui.tableWidget.setEnabled(False)
        self.Ui.lineEdit_CHRDTP.setText('')
        self.Ui.lineEdit_SSPNE.setText('')
        self.Ui.lineEdit_SSPN.setText('')
        self.Ui.lineEdit_CHRDR.setText('')
        self.Ui.lineEdit_SAVSI.setText('')
        self.Ui.lineEdit_CHSTAT.setText('')
        self.Ui.lineEdit_SSPNOP.setText('')
        self.Ui.lineEdit_CHRDBP.setText('')
        self.Ui.lineEdit_SAVSO.setText('')
    def slot_enable(self):
        if self.Ui.comboBox_WShape.currentIndex()==0:
            self.Ui.lineEdit_SSPNOP.setEnabled(False)
            self.Ui.lineEdit_SAVSO.setEnabled(False)
            self.Ui.lineEdit_CHRDBP.setEnabled(False)
        else:
            self.Ui.lineEdit_SSPNOP.setEnabled(True)
            self.Ui.lineEdit_SAVSO.setEnabled(True)
            self.Ui.lineEdit_CHRDBP.setEnabled(True)
    def slot_TabRow(self):
        if self.Ui.lineEdit_MACHN.text()=='':
            Num = 0
        else: Num = int(self.Ui.lineEdit_MACHN.text())
        self.Ui.tableWidget.setRowCount(Num)
        for i in range(0,Num):
            self.Ui.tableWidget.setItem(i, 0, QtWidgets.QTableWidgetItem(''))
            self.Ui.tableWidget.setItem(i, 1, QtWidgets.QTableWidgetItem(''))
            self.Ui.tableWidget.setItem(i, 2, QtWidgets.QTableWidgetItem(''))
    def xmlVTPLNF(self):
        doc = Document()
        nameListVTPLNF = doc.createElement('NameList')
        nameListVTPLNF.setAttribute('Name', "VTPLNF")
        doc.appendChild(nameListVTPLNF)
        label = list()
        label = ['CHRDTP', 'SSPNE', 'SSPN','CHRDR', 'SAVSI', 'CHSTAT','SSPNOP', 'CHRDBP','SAVSO']
        text = list()
        text = [self.Ui.lineEdit_CHRDTP.text(),
                self.Ui.lineEdit_SSPNE.text(),
                self.Ui.lineEdit_SSPN.text(),
                self.Ui.lineEdit_CHRDR.text(),
                self.Ui.lineEdit_SAVSI.text(),
                self.Ui.lineEdit_CHSTAT.text(),
                self.Ui.lineEdit_SSPNOP.text(),
                self.Ui.lineEdit_CHRDBP.text(),
                self.Ui.lineEdit_SAVSO.text()]
        for i in range(0, 9):
            if ((self.Ui.comboBox_WShape.currentIndex()==0)&(6<=i<=8 )):
                continue
            if text[i] != '':
                variable = doc.createElement('Variable')
                variable.setAttribute('Name', label[i])
                variable.setAttribute('StartIndex', "1")
                nameListVTPLNF.appendChild(variable)
                variable_value = doc.createElement('Value')
                variable_value_text = doc.createTextNode(text[i])
                variable.appendChild(variable_value)
                variable_value.appendChild(variable_value_text)
            else:
                pass

        if (self.Ui.lineEdit_MACHN.text()=='')|(self.Ui.tableWidget.isEnabled()==False):
            Ntab=0
        else:
            Ntab = int(self.Ui.lineEdit_MACHN.text())
            tab_column1 = doc.createElement('Variable')
            tab_column1.setAttribute('Name', "SVWB")
            tab_column1.setAttribute('StartIndex', "1")
            tab_column2 = doc.createElement('Variable')
            tab_column2.setAttribute('Name', "SVB")
            tab_column2.setAttribute('StartIndex', "1")
            tab_column3 = doc.createElement('Variable')
            tab_column3.setAttribute('Name', "SVHB")
            tab_column3.setAttribute('StartIndex', "1")
            tab_column1_value = []
            tab_column1_value_text = []
            tab_column2_value = []
            tab_column2_value_text = []
            tab_column3_value = []
            tab_column3_value_text = []
            valueMissing = 0
            for i in range(0, Ntab):
                if ((self.Ui.tableWidget.item(i, 0).text() == '')|(self.Ui.tableWidget.item(i,1).text()=='')|(self.Ui.tableWidget.item(i,2).text()=='')):
                    self.windowmMessage = QtWidgets.QMessageBox()
                    self.windowmMessage.setText("存在未输入数!")
                    self.windowmMessage.exec()
                    valueMissing = valueMissing + 1
                    tab_column1_value.clear()
                    tab_column1_value_text .clear()
                    tab_column2_value.clear()
                    tab_column2_value_text.clear()
                    tab_column3_value .clear()
                    tab_column3_value_text.clear()
                    break
                else:
                    tab_column1_value.append(doc.createElement('Value'))
                    tab_column1_value_text.append(doc.createTextNode(str(self.Ui.tableWidget.item(i, 0).text())))
                    tab_column2_value.append(doc.createElement('Value'))
                    tab_column2_value_text.append(doc.createTextNode(str(self.Ui.tableWidget.item(i, 1).text())))
                    tab_column3_value.append(doc.createElement('Value'))
                    tab_column3_value_text.append(doc.createTextNode(str(self.Ui.tableWidget.item(i, 2).text())))
            if valueMissing == 0:
                nameListVTPLNF.appendChild(tab_column1)
                nameListVTPLNF.appendChild(tab_column2)
                nameListVTPLNF.appendChild(tab_column3)
                for i in range(0, Ntab):
                    tab_column1.appendChild(tab_column1_value[i])
                    tab_column1_value[i].appendChild(tab_column1_value_text[i])
                    tab_column2.appendChild(tab_column2_value[i])
                    tab_column2_value[i].appendChild(tab_column2_value_text[i])
                    tab_column3.appendChild(tab_column3_value[i])
                    tab_column3_value[i].appendChild(tab_column3_value_text[i])
            else:pass
        return (nameListVTPLNF)
if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = VTPLNF()
    window.show()
    sys.exit(app.exec_())