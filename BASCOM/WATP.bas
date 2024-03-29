'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se reciben por el puerto serial a una
' memoria SD
'


$version 0 , 1 , 150
$regfile = "m328Pdef.dat"
$crystal = 16000000
$hwstack = 120
$swstack = 120
$framesize = 120
$baud = 9600


$projecttime = 201



'Declaracion de constantes
Const Posxtmax = 5
Const Posytmax = 74


'Configuracion de entradas/salidas
Led1 Alias Portb.5                                          'LED ROJO
Config Led1 = Output

Led2 Alias Portb.4                                          'LED ROJO
Config Led2 = Output



' Configura resistencias de pullup para leer pines de entrada
Set Portb.0
Set Portb.1
Set Portb.2
Set Portb.3

'*******************************************************************************
Vacin Alias Pinc.0
Config Vacin = Input
Set Portc.0

Puerta Alias Pinc.1
Config Puerta = Input
Set Portc.1


Relac Alias Portd.7
Config Relac = Output
Reltmp Alias Portd.6                                        'NO
Config Reltmp = Output
Relpwr Alias Portd.5
Config Relpwr = Output

Relpta Alias Portd.4
Config Relpta = Output

Buzzer Alias Portc.3
Config Buzzer = Output
Set Buzzer

'Configuración de Interrupciones
'TIMER1
Config Timer0 = Timer , Prescale = 8                        'Ints a 4032Hz si Timer0=&Hc2
On Timer0 Int_timer0
Enable Timer0
Start Timer0

'TIMER1
Config Timer1 = Timer , Prescale = 64                       'Ints a 100Hz si Timer1=&HF63C
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
$lib "glcdSSD1306-I2C.lib"                                  'OLED 0.9"
'$lib "glcdSH1106-I2C.lib"                                   'OLED 1.3"

Config Graphlcd = Custom , Cols = 128 , Rows = 64 , Lcdname = "SSD1306"


'Sensor DS18B20
Config 1wire = Portd.2

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "WATP_archivos.bas"



'Programa principal

Cls
Showpic 0 , 0 , Pic
Waitms 200
Cls
Setfont Font8x8tt
Lcdat 1 , 1 , "**  WATP 2019 **"
Lcdat 3 , 1 , Version(1)
Lcdat 5 , 1 , Version(2)
Lcdat 7 , 1 , Version(3)
Wait 1
Cls


Call Inivar()
Setfont Font12x16

Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   Do
      Call Proctec()
   Loop Until Initout = 0
   Teclaset = 0

   If Newseg = 1 Then
      Setfont Font12x16
      Reset Newseg
      If Alarmac = 1 Then
         Incr Cntrac
         If Cntrac.0 = 1 Then
            Lcdat 1 , 1 , "AC=NO" , 1
         Else
            Lcdat 1 , 1 , "AC=NO" , 0
         End If
      End If

      If Relpta = 1 Then
         Incr Cntrpta
         If Cntrpta.0 = 1 Then
            Lcdat 1 , 78 , "P=A" , 1
         Else
            Lcdat 1 , 78 , "P=A" , 0
         End If
      End If

      Incr Cntrdisp
      Cntrdisp = Cntrdisp Mod 16
      Setfont Font8x8tt
      Call Displcd()
      Setfont Font12x16


       Call Leer_ds18b20()
       If Alarmtemp = 0 Then
          If T1 <> T1ant Then
             T1ant = T1
             Lcdat 4 , 1 , "     "
             Lcdat 4 , 1 , "T=" ; Fusing(t1 , "#.#")
             Print #1 , T1
          End If
       End If
'         Tempestr4 = Fusing(t1 , "##.")
'         Tempe = Val(tempestr4)
       If T1 > Tmax Then
          Incr Cntrmax
          If Cntrmax = 5 Then
             Set Alarmtemp
             Set Reltmp
             Cntrmax = 0
          End If
       Else
          Reset Alarmtemp
          Reset Reltmp
       End If

       If Alarmtemp = 1 Then
          Incr Cntrttemp
          If Cntrttemp.0 = 1 Then
             Lcdat 4 , 1 , "     "
             Lcdat 4 , 1 , "T=" ; Fusing(t1 , "#.#") , 1
          Else
             Lcdat 4 , 1 , "     "
             Lcdat 4 , 1 , "T=" ; Fusing(t1 , "#.#") , 0
          End If
       End If

       If Errortemp = 1 Then
          Reset Relpwr
          Set Reltmp
       End If

       If Inivariables = 1 Then
         Reset Inivariables
         Call Inivar()
       End If

   End If

   Call Leer_vac()
   Call Leer_pta()

Loop