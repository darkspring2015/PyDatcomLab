import  os,sys
from PyQt5 import QtCore,QtWidgets,QtGui
from PyDatcom.DatcomWidgets.FLTCON import FLTCONUI
from xml.dom.minidom import Document

class FLTCON(QtWidgets.QWidget):
    def __init__(self):
        super(FLTCON,self).__init__()
        self.Ui = FLTCONUI.Ui_Form()
        self.Ui.setupUi(self)
        self.initiate()
    def initiate(self):
        self.Ui.spinBox_inputNALT.setEnabled(False)
        self.Ui.inputFlightAngle.setEnabled(False)
        self.Ui.inputFlightWeight.setEnabled(False)
        self.Ui.tab1.setRowCount(1)
        self.Ui.tab1.setColumnCount(1)
        self.Ui.tab2.setRowCount(1)
        self.Ui.tab2.setColumnCount(1)
        self.Ui.tab3.setRowCount(1)
        self.Ui.tab3.setColumnCount(1)
        self.Ui.tab1.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem('攻角'))
        self.Ui.tab2.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem('马赫数'))
        self.Ui.tab3.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem('雷诺数'))
        for j in range(0, 1):
            for i in range(0,1):
                self.Ui.tab2.setItem(i, j, QtWidgets.QTableWidgetItem())
        for j in range(0, 1):
            for i in range(0, 1):
                self.Ui.tab3.setItem(i, j, QtWidgets.QTableWidgetItem())
        for j in range(0, 1):
            for i in range(0, 1):
                self.Ui.tab1.setItem(i, j, QtWidgets.QTableWidgetItem())


    def slot_enableALT(self):
        if self.Ui.comboBox_Loop.currentIndex()==0:
            self.Ui.spinBox_inputNALT.setEnabled(False)
        else:
            self.Ui.spinBox_inputNALT.setEnabled(True)
    def slot_makeTab1_row(self):
        rowsN = int(self.Ui.spinBox_inputNALPHA.value())
        self.Ui.tab1.setRowCount(rowsN)
    def slot_makeTab2and3(self):
        headerName0 = str( '马赫数')
        headerName1 = str('雷诺数')
        headerName2 = str('高度数')
        headerName3 = str('自由流速')
        headerName4 = str('自由流静压')
        headerName5 = str('自由流温度')
        if self.Ui.comboBox_Variables.currentIndex() == 0:
            Ntab3 = self.Ui.spinBox_inputNMACH.value()
        else: Ntab3 = self.Ui.spinBox_inputNALT.value()
        if self.Ui.comboBox_Variables.currentIndex() == 0:
            self.Ui.tab2.clear()
            self.Ui.tab3.clear()
            self.Ui.tab2.setRowCount(self.Ui.spinBox_inputNMACH.value())
            self.Ui.tab3.setRowCount(Ntab3)
            self.Ui.tab2.setColumnCount(1)
            self.Ui.tab3.setColumnCount(1)
            self.Ui.tab2.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName0))
            self.Ui.tab3.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName1))
            for j in range (0,1):
                for i in range(0,self.Ui.spinBox_inputNMACH.value()):
                    self.Ui.tab2.setItem(i,j,QtWidgets.QTableWidgetItem())
            for j in range (0,1):
                for i in range(0,Ntab3):
                    self.Ui.tab3.setItem(i,j,QtWidgets.QTableWidgetItem())
        elif  self.Ui.comboBox_Variables.currentIndex() == 1:
            self.Ui.tab2.clear()
            self.Ui.tab3.clear()
            self.Ui.tab2.setRowCount(self.Ui.spinBox_inputNMACH.value())
            self.Ui.tab3.setRowCount(Ntab3)
            self.Ui.tab2.setColumnCount(1)
            self.Ui.tab3.setColumnCount(1)
            self.Ui.tab2.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName0))
            self.Ui.tab3.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName2))
            for j in range (0,1):
                for i in range(0,self.Ui.spinBox_inputNMACH.value()):
                    self.Ui.tab2.setItem(i,j,QtWidgets.QTableWidgetItem())
            for j in range (0,1):
                for i in range(0,Ntab3):
                    self.Ui.tab3.setItem(i,j,QtWidgets.QTableWidgetItem())
        elif self.Ui.comboBox_Variables.currentIndex() == 2:
            self.Ui.tab2.clear()
            self.Ui.tab3.clear()
            self.Ui.tab2.setRowCount(self.Ui.spinBox_inputNMACH.value())
            self.Ui.tab3.setRowCount(Ntab3)
            self.Ui.tab2.setColumnCount(1)
            self.Ui.tab3.setColumnCount(1)
            self.Ui.tab2.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName3))
            self.Ui.tab3.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName2))
            for j in range(0, 1):
                for i in range(0, self.Ui.spinBox_inputNMACH.value()):
                    self.Ui.tab2.setItem(i, j, QtWidgets.QTableWidgetItem())
            for j in range(0, 1):
                for i in range(0, Ntab3):
                    self.Ui.tab3.setItem(i, j, QtWidgets.QTableWidgetItem())
        elif self.Ui.comboBox_Variables.currentIndex() == 3:
            self.Ui.tab2.clear()
            self.Ui.tab3.clear()
            self.Ui.tab2.setRowCount(self.Ui.spinBox_inputNMACH.value())
            self.Ui.tab3.setRowCount(Ntab3)
            self.Ui.tab2.setColumnCount(1)
            self.Ui.tab3.setColumnCount(2)
            self.Ui.tab2.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName3))
            self.Ui.tab3.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName4))
            self.Ui.tab3.setHorizontalHeaderItem(1, QtWidgets.QTableWidgetItem(headerName5))
            for j in range(0, 1):
                for i in range(0, self.Ui.spinBox_inputNMACH.value()):
                    self.Ui.tab2.setItem(i, j, QtWidgets.QTableWidgetItem())
            for j in range(0, 2):
                for i in range(0, Ntab3):
                    self.Ui.tab3.setItem(i, j, QtWidgets.QTableWidgetItem())
        else:
            self.Ui.tab2.clear()
            self.Ui.tab3.clear()
            self.Ui.tab2.setRowCount(self.Ui.spinBox_inputNMACH.value())
            self.Ui.tab3.setRowCount(Ntab3)
            self.Ui.tab2.setColumnCount(1)
            self.Ui.tab3.setColumnCount(2)
            self.Ui.tab2.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName0))
            self.Ui.tab3.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem(headerName4))
            self.Ui.tab3.setHorizontalHeaderItem(1, QtWidgets.QTableWidgetItem(headerName5))
            for j in range(0, 1):
                for i in range(0, self.Ui.spinBox_inputNMACH.value()):
                    self.Ui.tab2.setItem(i, j, QtWidgets.QTableWidgetItem())
            for j in range(0, 2):
                for i in range(0, Ntab3):
                    self.Ui.tab3.setItem(i, j, QtWidgets.QTableWidgetItem())

    def xmlFLTCON(self):
        doc = Document()
        nameListFLTCON = doc.createElement('NameList')
        nameListFLTCON.setAttribute('Name', "FLTCON")
        doc.appendChild(nameListFLTCON)
        Ntab1 = self.Ui.spinBox_inputNALPHA.value()
        Ntab2 = self.Ui.spinBox_inputNMACH.value()
        if self.Ui.comboBox_Variables.currentIndex() == 0:
            Ntab3 = self.Ui.spinBox_inputNMACH.value()
        else:
            Ntab3 = self.Ui.spinBox_inputNALT.value()

        LOOP = doc.createElement('Variable')
        LOOP.setAttribute('Name', "LOOP")
        LOOP.setAttribute('StartIndex', "1")
        nameListFLTCON.appendChild(LOOP)
        LOOP_value = doc.createElement('Value')
        if self.Ui.comboBox_Loop.currentIndex()==0:
            LOOP_value_text = doc.createTextNode(str(1))
        elif self.Ui.comboBox_Loop.currentIndex()==1:
            LOOP_value_text = doc.createTextNode(str(2))
        else:
            LOOP_value_text = doc.createTextNode(str(3))
        LOOP.appendChild(LOOP_value)
        LOOP_value.appendChild(LOOP_value_text)

        NMACH = doc.createElement('Variable')
        NMACH.setAttribute('Name', "NMACH")
        NMACH.setAttribute('StartIndex', "1")
        nameListFLTCON.appendChild(NMACH)
        NMACH_value = doc.createElement('Value')
        NMACH_value_text = doc.createTextNode(str(self.Ui.spinBox_inputNMACH.value()))
        NMACH.appendChild(NMACH_value)
        NMACH_value.appendChild(NMACH_value_text)

        NALPHA = doc.createElement('Variable')
        NALPHA.setAttribute('Name', "NALPHA")
        NALPHA.setAttribute('StartIndex', "1")
        nameListFLTCON.appendChild(NALPHA)
        NALPHA_value = doc.createElement('Value')
        NALPHA_value_text = doc.createTextNode(str(self.Ui.spinBox_inputNALPHA.value()))
        NALPHA.appendChild(NALPHA_value)
        NALPHA_value.appendChild(NALPHA_value_text)

        NALT = doc.createElement('Variable')
        NALT.setAttribute('Name', "NALT")
        NALT.setAttribute('StartIndex', "1")
        nameListFLTCON.appendChild(NALT)
        NALT_value = doc.createElement('Value')
        NALT_value_text = doc.createTextNode(str(self.Ui.spinBox_inputNALT.value()))
        NALT.appendChild(NALT_value)
        NALT_value.appendChild(NALT_value_text)

        if(self.Ui.checkBox_FlightWeight.checkState() == QtCore.Qt.Checked):
            WT = doc.createElement('Variable')
            WT.setAttribute('Name', "WT")
            WT.setAttribute('StartIndex', "1")
            nameListFLTCON.appendChild(WT)
            WT_value = doc.createElement('Value')
            WT_value_text = doc.createTextNode(str(self.Ui.inputFlightWeight.text()))
            WT.appendChild(WT_value)
            WT_value.appendChild(WT_value_text)
        if (self.Ui.checkBox_FlightPathAngle.checkState() == QtCore.Qt.Checked):
            GAMMA = doc.createElement('Variable')
            GAMMA.setAttribute('Name', "GAMMA")
            GAMMA.setAttribute('StartIndex', "1")
            nameListFLTCON.appendChild(GAMMA)
            GAMMA_value = doc.createElement('Value')
            GAMMA_value_text = doc.createTextNode(str(self.Ui.inputFlightAngle.text()))
            GAMMA.appendChild(GAMMA_value)
            GAMMA_value.appendChild(GAMMA_value_text)
        STMACH = doc.createElement('Variable')
        STMACH.setAttribute('Name', "STMACH")
        STMACH.setAttribute('StartIndex', "1")
        nameListFLTCON.appendChild(STMACH)
        STMACH_value = doc.createElement('Value')
        STMACH_value_text = doc.createTextNode(str(self.Ui.Input_HyperS_UpperLimit.text()))
        STMACH.appendChild(STMACH_value)
        STMACH_value.appendChild(STMACH_value_text)
        TSMACH = doc.createElement('Variable')
        TSMACH.setAttribute('Name', "TSMACH")
        TSMACH.setAttribute('StartIndex', "1")
        nameListFLTCON.appendChild(TSMACH)
        TSMACH_value = doc.createElement('Value')
        TSMACH_value_text = doc.createTextNode(str(self.Ui.Input_SubS_LowerLimit.text()))
        TSMACH.appendChild(TSMACH_value)
        TSMACH_value.appendChild(TSMACH_value_text)
        HYPERS = doc.createElement('Variable')
        if self.Ui.comboBox_HYPERS.currentIndex()==0:
            HYPERS.setAttribute('Name', "HYPERS")
            HYPERS.setAttribute('StartIndex', "1")
            nameListFLTCON.appendChild(HYPERS)
            HYPERS_value = doc.createElement('Value')
            HYPERS_value_text = doc.createTextNode(str('.TRUE.'))
            HYPERS.appendChild(HYPERS_value)
            HYPERS_value.appendChild(HYPERS_value_text)

        # table1 value
        if Ntab1 == 0:
            pass
        else:
            tab1 = doc.createElement('Variable')
            tab1.setAttribute('Name', "ALSCHD")
            tab1.setAttribute('StartIndex', "1")
            tab1_value = []
            tab1_value_text = []
            valueMissing = 0
            for i in range(0, Ntab1):
                if (self.Ui.tab1.item(i, 0).text() == ""):
                    self.windowmMessage = QtWidgets.QMessageBox()
                    self.windowmMessage.setText("存在未输入的攻角数!")
                    self.windowmMessage.exec()
                    valueMissing = valueMissing + 1
                    tab1_value.clear()
                    tab1_value_text.clear()
                    break
                else:
                    tab1_value.append(doc.createElement('Value'))
                    tab1_value_text.append(doc.createTextNode(self.Ui.tab1.item(i, 0).text()))
            if valueMissing == 0:
                nameListFLTCON.appendChild(tab1)
                for i in range(0, self.Ui.spinBox_inputNALPHA.value()):
                    tab1.appendChild(tab1_value[i])
                    tab1_value[i].appendChild(tab1_value_text[i])
            else:
                pass
        # end table1 value
        #table2 value
        if Ntab2==0:
            pass
        else:
            tab2 = doc.createElement('Variable')
            if(2<=self.Ui.comboBox_Variables.currentIndex()<=3):
                tab2.setAttribute('Name', "VINF")
                tab2.setAttribute('StartIndex', "1")
            else:
                tab2.setAttribute('Name', "MACH")
                tab2.setAttribute('StartIndex', "1")
            tab2_value = []
            tab2_value_text = []
            valueMissing = 0
            for i in range(0,Ntab2):
                if(self.Ui.tab2.item(i, 0).text() == ""):
                    self.windowmMessage = QtWidgets.QMessageBox()
                    self.windowmMessage.setText("表2存在未输入数!")
                    self.windowmMessage.exec()
                    valueMissing = valueMissing + 1
                    tab2_value.clear()
                    tab2_value_text.clear()
                    break
                else:
                    tab2_value.append(doc.createElement('Value'))
                    tab2_value_text.append(doc.createTextNode(self.Ui.tab2.item(i, 0).text()))
            if valueMissing == 0:
                nameListFLTCON.appendChild(tab2)
                for i in range(0,Ntab2):
                    tab2.appendChild(tab2_value[i])
                    tab2_value[i].appendChild(tab2_value_text[i])
            else:pass
        # end table2 value
        # table3 value
        if Ntab3 == 0:
            pass
        else:
            tab3_column1 = doc.createElement('Variable')
            if self.Ui.comboBox_Variables.currentIndex()==0:
                tab3_column1.setAttribute('Name', "ReyNold")
            elif 1<=self.Ui.comboBox_Variables.currentIndex()<=2:
                tab3_column1.setAttribute('Name', "ALT")
            else:
                tab3_column1.setAttribute('Name', "PINF")
            tab3_column1.setAttribute('StartIndex', "1")
            tab3_column2 = doc.createElement('Variable')
            tab3_column2.setAttribute('Name', "TINF")
            tab3_column2.setAttribute('StartIndex', "1")
            tab3_column1_value = []
            tab3_column2_value = []
            tab3_column1_value_text = []
            tab3_column2_value_text = []
            valueMissing = 0
            if 0 <= self.Ui.comboBox_Variables.currentIndex() <= 2:
                for i in range(0, Ntab3):
                    if (self.Ui.tab3.item(i, 0).text() == ""):
                        self.windowmMessage = QtWidgets.QMessageBox()
                        self.windowmMessage.setText("存在未输入的雷诺数!")
                        self.windowmMessage.exec()
                        valueMissing = valueMissing + 1
                        tab3_column1_value.clear()
                        tab3_column1_value_text.clear()
                        break
                    else:
                        tab3_column1_value.append(doc.createElement('Value'))
                        tab3_column1_value_text.append(doc.createTextNode(self.Ui.tab3.item(i, 0).text()))
                if valueMissing == 0:
                    nameListFLTCON.appendChild(tab3_column1)
                    for i in range(0, Ntab3):
                        tab3_column1.appendChild(tab3_column1_value[i])
                        tab3_column1_value[i].appendChild(tab3_column1_value_text[i])
                else:
                    pass
            else:
                for i in range(0, Ntab3):
                    if ((self.Ui.tab3.item(i, 0).text() == "")|(self.Ui.tab3.item(i, 1).text() == "")):
                         self.windowmMessage = QtWidgets.QMessageBox()
                         self.windowmMessage.setText("存在未输入的雷诺数!")
                         self.windowmMessage.exec()
                         valueMissing = valueMissing + 1
                         tab3_column1_value.clear()
                         tab3_column2_value.clear()
                         tab3_column1_value_text.clear()
                         tab3_column2_value_text.clear()
                         break
                    else:
                         tab3_column1_value.append(doc.createElement('Value'))
                         tab3_column1_value_text.append(doc.createTextNode(self.Ui.tab3.item(i, 0).text()))
                         tab3_column2_value.append(doc.createElement('Value'))
                         tab3_column2_value_text.append(doc.createTextNode(self.Ui.tab3.item(i, 1).text()))
                if valueMissing == 0:
                    nameListFLTCON.appendChild(tab3_column1)
                    nameListFLTCON.appendChild(tab3_column2)
                    for i in range(0, Ntab3):
                        tab3_column1.appendChild(tab3_column1_value[i])
                        tab3_column1_value[i].appendChild(tab3_column1_value_text[i])
                        tab3_column2.appendChild(tab3_column2_value[i])
                        tab3_column2_value[i].appendChild(tab3_column2_value_text[i])
                else:pass
        return (nameListFLTCON)

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = FLTCON()
    window.show()
    sys.exit(app.exec_())