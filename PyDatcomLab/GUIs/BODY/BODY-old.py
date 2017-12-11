import  os,sys
from PyQt5 import QtCore,QtWidgets,QtGui
from PyDatcom.DatcomWidgets.BODY import BODYUI
from xml.dom.minidom import Document


class BODY(QtWidgets.QWidget):
    def __init__(self):
        super(BODY,self).__init__()
        self.Ui = BODYUI.Ui_Form()
        self.Ui.setupUi(self)
        self.initiate()
    def initiate(self):
        path = os.path.abspath(os.path.dirname(sys.argv[0]))
        path = path.rstrip("\\BODYz")  # remove the BODY in the path
        path2 = "\\rc\\Body_SubSonic.png"
        filePath = path + path2
        icon = QtGui.QPixmap(filePath)
        icon2 = icon.scaled(500, 400, 0, 0)
        self.Ui.image_BODY.setPixmap(icon2)
        self.Ui.label_BNOSE.setEnabled(False)
        self.Ui.label_BTAIL.setEnabled(False)
        self.Ui.label_BLN.setEnabled(False)
        self.Ui.label_BLA.setEnabled(False)
        self.Ui.checkBox_DS.setEnabled(False)
        self.Ui.comboBox_BNOSE.setEnabled(False)
        self.Ui.comboBox_BTAIL.setEnabled(False)
        self.Ui.lineEdit_BLN.setEnabled(False)
        self.Ui.lineEdit_BLA.setEnabled(False)
        self.Ui.lineEdit_DS.setEnabled(False)
        self.Ui.comboBox_ITYPE.setEnabled(False)
        # initiate the table
        self.Ui.Tab_ComboVariables.setRowCount(1)
        self.Ui.Tab_ComboVariables.setColumnCount(1)
        self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem('截面半径'))
        self.Ui.Tab_ComboVariables.setItem(0, 0, QtWidgets.QTableWidgetItem())
        self.Ui.lineEdit_BODY_Num.setText('')
        self.Ui.lineEdit_BLN.setText('')
        self.Ui.label_BTAIL.setText('')
        self.Ui.lineEdit_DS.setText('')
    def slot_super_hyper(self):
        if 0<=self.Ui.comboBox_Conf.currentIndex()<=1:
            self.Ui.label_BNOSE.setEnabled(False)
            self.Ui.label_BTAIL.setEnabled(False)
            self.Ui.label_BLN.setEnabled(False)
            self.Ui.label_BLA.setEnabled(False)
            self.Ui.checkBox_DS.setEnabled(False)
            self.Ui.comboBox_BNOSE.setEnabled(False)
            self.Ui.comboBox_BTAIL.setEnabled(False)
            self.Ui.lineEdit_BLN.setEnabled(False)
            self.Ui.lineEdit_BLA.setEnabled(False)
            self.Ui.lineEdit_DS.setEnabled(False)
        else: #==2|3
            self.Ui.label_BNOSE.setEnabled(True)
            self.Ui.label_BTAIL.setEnabled(True)
            self.Ui.label_BLN.setEnabled(True)
            self.Ui.label_BLA.setEnabled(True)
            self.Ui.checkBox_DS.setEnabled(True)
            self.Ui.comboBox_BNOSE.setEnabled(True)
            self.Ui.comboBox_BTAIL.setEnabled(True)
            self.Ui.lineEdit_BLN.setEnabled(True)
            self.Ui.lineEdit_BLA.setEnabled(True)
            self.Ui.lineEdit_DS.setEnabled(False)
    def slot_Tab_ComboVariables(self):
        headerName = list()
        headerName.append('截面半径')
        headerName.append('截面积')
        headerName.append('截面周长')
        if self.Ui.lineEdit_BODY_Num.text()=='':Num=0
        else: Num = int(self.Ui.lineEdit_BODY_Num.text())
        self.Ui.Tab_ComboVariables.clear()
        self.Ui.Tab_ComboVariables.verticalHeader().show()
        if 0<=self.Ui.comboBox_ComboVariables.currentIndex()<=2:
            self.Ui.Tab_ComboVariables.setColumnCount(1)
            self.Ui.Tab_ComboVariables.setRowCount(Num)
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName[self.Ui.comboBox_ComboVariables.currentIndex()]))
            for i in range(0,Num):
                self.Ui.Tab_ComboVariables.setItem(i,0,QtWidgets.QTableWidgetItem())
        elif self.Ui.comboBox_ComboVariables.currentIndex() == 3:
            self.Ui.Tab_ComboVariables.setColumnCount(2)
            self.Ui.Tab_ComboVariables.setRowCount(Num)
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName[1]))
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(1,QtWidgets.QTableWidgetItem(headerName[2]))
            for i in range(0,Num):
                self.Ui.Tab_ComboVariables.setItem(i,0,QtWidgets.QTableWidgetItem())
                self.Ui.Tab_ComboVariables.setItem(i, 1, QtWidgets.QTableWidgetItem())
        elif self.Ui.comboBox_ComboVariables.currentIndex() == 4:
            self.Ui.Tab_ComboVariables.setColumnCount(2)
            self.Ui.Tab_ComboVariables.setRowCount(Num)
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName[2]))
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(1,QtWidgets.QTableWidgetItem(headerName[0]))
            for i in range(0,Num):
                self.Ui.Tab_ComboVariables.setItem(i, 0, QtWidgets.QTableWidgetItem())
                self.Ui.Tab_ComboVariables.setItem(i, 1, QtWidgets.QTableWidgetItem())
        else:#==5                                                                                                           $
            self.Ui.Tab_ComboVariables.setColumnCount(2)
            self.Ui.Tab_ComboVariables.setRowCount(Num)
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(0,QtWidgets.QTableWidgetItem(headerName[1]))
            self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(1, QtWidgets.QTableWidgetItem(headerName[0]))
            for i in range(0,Num):
                self.Ui.Tab_ComboVariables.setItem(i, 0, QtWidgets.QTableWidgetItem())
                self.Ui.Tab_ComboVariables.setItem(i, 1, QtWidgets.QTableWidgetItem())
    def xmlBODY(self):
        doc = Document()
        nameListBODY = doc.createElement('NameList')
        nameListBODY.setAttribute('Name', "BODY")
        doc.appendChild(nameListBODY)
        if self.Ui.lineEdit_BODY_Num.text()!='':
            NX = doc.createElement('Variable')
            NX.setAttribute('Name', "NX")
            NX.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(NX)
            NX_value = doc.createElement('Value')
            NX_value_text = doc.createTextNode(str(self.Ui.lineEdit_BODY_Num.text()))
            NX.appendChild(NX_value)
            NX_value.appendChild(NX_value_text)
        if self.Ui.comboBox_BNOSE.isEnabled()==True:
            BNOSE = doc.createElement('Variable')
            BNOSE.setAttribute('Name', "BNOSE")
            BNOSE.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(BNOSE)
            BNOSE_value = doc.createElement('Value')
            if self.Ui.comboBox_BNOSE.currentIndex() == 0:
                BNOSE_value_text = doc.createTextNode(str('1.0'))
            else:
                BNOSE_value_text = doc.createTextNode(str('2.0'))
            BNOSE.appendChild(BNOSE_value)
            BNOSE_value.appendChild(BNOSE_value_text)
        if self.Ui.comboBox_BTAIL.isEnabled() == True:
            BTAIL = doc.createElement('Variable')
            BTAIL.setAttribute('Name', "BTAIL")
            BTAIL.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(BTAIL)
            BTAIL_value = doc.createElement('Value')
            if self.Ui.comboBox_BTAIL.currentIndex() == 0:
                BTAIL_value_text = doc.createTextNode(str('1.0'))
            else:
                BTAIL_value_text = doc.createTextNode(str('2.0'))
            BTAIL.appendChild(BTAIL_value)
            BTAIL_value.appendChild(BTAIL_value_text)
        if self.Ui.lineEdit_BLN.text() != '':
            BLN = doc.createElement('Variable')
            BLN.setAttribute('Name', "BLN")
            BLN.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(BLN)
            BLN_value = doc.createElement('Value')
            BLN_value_text = doc.createTextNode(str(self.Ui.lineEdit_BLN.text()))
            BLN.appendChild(BLN_value)
            BLN_value.appendChild(BLN_value_text)
        if self.Ui.lineEdit_BLA.text() != '':
            BLA = doc.createElement('Variable')
            BLA.setAttribute('Name', "BLA")
            BLA.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(BLA)
            BLA_value = doc.createElement('Value')
            BLA_value_text = doc.createTextNode(str(self.Ui.lineEdit_BLA.text()))
            BLA.appendChild(BLA_value)
            BLA_value.appendChild(BLA_value_text)
        if self.Ui.lineEdit_DS.isEnabled()==True:
            DS = doc.createElement('Variable')
            DS.setAttribute('Name', "DS")
            DS.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(DS)
            DS_value = doc.createElement('Value')
            DS_value_text = doc.createTextNode(str(self.Ui.lineEdit_DS.text()))
            DS.appendChild(DS_value)
            DS_value.appendChild(DS_value_text)
        if self.Ui.comboBox_METHED.currentIndex()==1:
            METHOD = doc.createElement('Variable')
            METHOD.setAttribute('Name', "METHOD")
            METHOD.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(METHOD)
            METHOD_value = doc.createElement('Value')
            METHOD_value_text = doc.createTextNode(str('2'))
            METHOD.appendChild(METHOD_value)
            METHOD_value.appendChild(METHOD_value_text)
        if self.Ui.comboBox_ITYPE.isEnabled()==True:
            ITYPE = doc.createElement('Variable')
            ITYPE.setAttribute('Name', "ITYPE")
            ITYPE.setAttribute('StartIndex', "1")
            nameListBODY.appendChild(ITYPE)
            ITYPE_value = doc.createElement('Value')
            if self.Ui.comboBox_ITYPE.currentIndex() == 0:
                ITYPE_value_text = doc.createTextNode(str('1'))
            elif self.Ui.comboBox_ITYPE.currentIndex() == 1:
                ITYPE_value_text = doc.createTextNode(str('2'))
            else:
                ITYPE_value_text = doc.createTextNode(str('3'))
            ITYPE.appendChild(ITYPE_value)
            ITYPE_value.appendChild(ITYPE_value_text)
        if (self.Ui.lineEdit_BODY_Num.text()=='')|(self.Ui.lineEdit_BODY_Num.text()==0):
            pass
        else:
            if 0 <= self.Ui.comboBox_ComboVariables.currentIndex() <= 2:
                tab2 = doc.createElement('Variable')
                if self.Ui.comboBox_ComboVariables.currentIndex() == 0:
                    tab2.setAttribute('Name', "R")
                    tab2.setAttribute('StartIndex', "1")
                elif self.Ui.comboBox_ComboVariables.currentIndex() == 1:
                    tab2.setAttribute('Name', "S")
                    tab2.setAttribute('StartIndex', "1")
                else:
                    tab2.setAttribute('Name', "P")
                    tab2.setAttribute('StartIndex', "1")
                tab2_value = []
                tab2_value_text = []
                valueMissing = 0
                Ntab2 = int(self.Ui.lineEdit_BODY_Num.text() )
                for i in range(0, Ntab2):
                    if ((self.Ui.Tab_ComboVariables.item(i, 0).text() == '') ):
                        self.windowmMessage = QtWidgets.QMessageBox()
                        self.windowmMessage.setText("BODY项表格存在未输入数!")
                        self.windowmMessage.exec()
                        valueMissing = valueMissing + 1
                        tab2_value.clear()
                        tab2_value_text.clear()
                        break
                    else:
                        tab2_value.append(doc.createElement('Value'))
                        tab2_value_text.append(doc.createTextNode(self.Ui.Tab_ComboVariables.item(i, 0).text()))
                if valueMissing == 0:
                    nameListBODY.appendChild(tab2)
                    for i in range(0, Ntab2):
                        tab2.appendChild(tab2_value[i])
                        tab2_value[i].appendChild(tab2_value_text[i])
            else:
                tab2_column1 = doc.createElement('Variable')
                tab2_column2 = doc.createElement('Variable')
                if self.Ui.comboBox_ComboVariables.currentIndex() == 3:
                    tab2_column1.setAttribute('Name', "S")
                    tab2_column1.setAttribute('StartIndex', "1")
                    tab2_column2.setAttribute('Name', "P")
                    tab2_column2.setAttribute('StartIndex', "1")
                elif self.Ui.comboBox_ComboVariables.currentIndex() == 4:
                    tab2_column1.setAttribute('Name', "P")
                    tab2_column1.setAttribute('StartIndex', "1")
                    tab2_column2.setAttribute('Name', "R")
                    tab2_column2.setAttribute('StartIndex', "1")
                else:  # 5
                    tab2_column1.setAttribute('Name', "S")
                    tab2_column1.setAttribute('StartIndex', "1")
                    tab2_column2.setAttribute('Name', "R")
                    tab2_column2.setAttribute('StartIndex', "1")
                tab2_column1_value = []
                tab2_column1_value_text = []
                tab2_column2_value = []
                tab2_column2_value_text = []
                valueMissing = 0
                Ntab2 = self.Ui.lineEdit_BODY_Num.text()
                for i in range(0, Ntab2):
                    if (self.Ui.Tab_ComboVariables.item(i, 0).text() == ''| ( self.Ui.Tab_ComboVariables.item(i, 1).text() == '')):
                        self.windowmMessage = QtWidgets.QMessageBox()
                        self.windowmMessage.setText("BODY项表格存在未输入数!")
                        self.windowmMessage.exec()
                        valueMissing = valueMissing + 1
                        tab2_column1_value.clear()
                        tab2_column1_value_text.clear()
                        tab2_column2_value.clear()
                        tab2_column2_value_text.clear()
                        break
                    else:
                        tab2_column1_value.append(doc.createElement('Value'))
                        tab2_column1_value.append(doc.createTextNode(str(self.Ui.Tab_ComboVariables.item(i, 0).text())))
                        tab2_column2_value.append(doc.createElement('Value'))
                        tab2_column2_value.append(doc.createTextNode(str(self.Ui.Tab_ComboVariables.item(i, 1).text())))
                if valueMissing == 0:
                    nameListBODY.appendChild(tab2_column1)
                    nameListBODY.appendChild(tab2_column2)
                    for i in range(0, Ntab2):
                        tab2_column1.appendChild(tab2_column1[i])
                        tab2_column1[i].appendChild(tab2_column1_value_text[i])
                        tab2_column2.appendChild(tab2_column2[i])
                        tab2_column2[i].appendChild(tab2_column2_value_text[i])
        return (nameListBODY)

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = BODY()
    window.show()
    sys.exit(app.exec_())