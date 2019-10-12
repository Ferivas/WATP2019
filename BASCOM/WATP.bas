'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se reciben por el puerto serial a una
' memoria SD
'


$version 0 , 1 , 48
$regfile = "m328Pdef.dat"
$crystal = 16000000
$hwstack = 80
$swstack = 80
$framesize = 80
$baud = 9600


$projecttime = 77



'Declaracion de constantes



'Configuracion de entradas/salidas
Led1 Alias Portb.5                                          'LED ROJO
Config Led1 = Output

Buzzer Alias Portc.3
Config Buzzer = Output
Set Buzzer

' Configura resistencias de pullup para leer pines de entrada
Set Portb.0
Set Portb.1
Set Portb.2
Set Portb.3

'*******************************************************************************
'Vacin Alias Pinc.0
Vacin Alias Pind.3
Config Vacin = Input
Set Portd.3
'set portc.0

Puerta Alias Pinc.1
Config Puerta = Input
Set Portc.1


Relac Alias Portd.4
Config Relac = Output
Reltmp Alias Portd.5                                        'NO
Config Reltmp = Output
Relpwr Alias Portd.6
Config Relpwr = Output

Relpta Alias Portd.7
Config Relpta = Output



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


'Sensor DS18B20
Config 1wire = Portc.0

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
Setfont Font8x8tt
Lcdat 1 , 1 , "**  WATP 2019 **"
Lcdat 3 , 1 , Version(1)
Lcdat 5 , 1 , Version(2)
Lcdat 7 , 1 , Version(3)
Wait 2
Cls

Setfont Font12x16
Call Inivar()


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Newseg = 1 Then
      Reset Newseg
      If Alarmac = 1 Then
         Incr Cntrac
         If Cntrac.0 = 1 Then
            Lcdat 1 , 70 , "AC=NO" , 1
         Else
            Lcdat 1 , 70 , "AC=NO" , 0
         End If
      End If

      If Relpta = 1 Then
         Incr Cntrpta
         If Cntrpta.0 = 1 Then
            Lcdat 4 , 1 , "P=A" , 1
         Else
            Lcdat 4 , 1 , "P=A" , 0
         End If
      End If

      Incr Cntrdisp
      Cntrdisp = Cntrdisp Mod 16
      Setfont Font8x8tt
      Call Displcd()

      Incr Cntrtemp
      Cntrtemp = Cntrtemp Mod 5
      Setfont Font12x16
      If Cntrtemp = 0 Then
         Call Leer_ds18b20()
         If T1 <> T1ant Then
            T1ant = T1
            Lcdat 1 , 1 , "     "
            Lcdat 1 , 1 , "T=" ; Fusing(t1 , "#.#")
         End If
      End If


   End If

   Call Leer_vac()
   Call Leer_pta()

Loop


