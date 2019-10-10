'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se reciben por el puerto serial a una
' memoria SD
'


$version 0 , 1 , 14
$regfile = "m328Pdef.dat"
$crystal = 16000000
$hwstack = 80
$swstack = 80
$framesize = 80
$baud = 9600


$projecttime = 26



'Declaracion de constantes



'Configuracion de entradas/salidas
Led1 Alias Portb.5                                          'LED ROJO
Config Led1 = Output


'Configuración de Interrupciones
'TIMER0
Config Timer1 = Timer , Prescale = 64                       'Ints a 100.1603Hz si Timer1=&HF63C
On Timer1 Int_timer1
Enable Timer1
Start Timer1

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

Config Scl = Portc.5                                        ' used i2c pins
Config Sda = Portc.4
Config Twi = 100000

I2cinit

$lib "i2c_twi.lbx"
$lib "glcdSSD1306-I2C.lib"

Config Graphlcd = Custom , Cols = 128 , Rows = 64 , Lcdname = "SSD1306"


Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "WATP_archivos.bas"



'Programa principal

Cls
Showpic 0 , 0 , Pic
Wait 1
Cls
Setfont Font12x16
Lcdat 1 , 1 , "WATP 2019"
Lcdat 3 , 1 , "AC=OK"

Call Inivar()


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Newseg = 1 Then
      Reset Newseg
      Incr Cntrdisp
      Cntrdisp = Cntrdisp Mod 4
      Select Case Cntrdisp
         Case 0:
            Lcdat 5 , 1 , "/"

         Case 1:
            Lcdat 5 , 1 , "-"

         Case 2:
            Lcdat 5 , 1 , "\"

         Case 3:
            Lcdat 5 , 1 , "l"

      End Select


   End If

Loop




