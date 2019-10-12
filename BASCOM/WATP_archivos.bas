'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  SD_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile
$projecttime = 81


'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Leer_ds18b20()
Declare Sub Leer_vac()
Declare Sub Displcd()
Declare Sub Leer_pta()


'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte

Dim Cmdtmp As String * 6
Dim Atsnd As String * 200
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 52

Dim Cntrdisp As Byte

Dim Bklsta As Byte
Dim Alarmac As Bit , Alarmtemp As Bit , Vacinant As Bit , Puertaant As Bit
Dim Cntrac As Byte , Cntrpta As Byte , Cntrt As Byte
'Variables TIMER1
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim T1c As Byte
Dim Newseg As Bit

'DS18B20
Dim Crc As Byte
Dim T1 As Single , Bint As Integer , Tr As Byte , Ti As Byte
Dim T1ant As Single
Dim Bt(9) As Byte
Dim Cntrtemp As Byte
Dim Signo As String * 2
'Dim Tempestr4 As String * 4 , Signo As String * 1 , Tempe As Byte

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(34) As String * 20
Dim Serdata As String * 200 , Serrx As Byte , Serproc As String * 200



'*******************************************************************************
'* END public part                                                             *
'*******************************************************************************


Goto Loaded_arch

'*******************************************************************************
' INTERRUPCIONES
'*******************************************************************************

'*******************************************************************************
' Subrutina interrupcion de puerto serial 1
'*******************************************************************************
At_ser1:
   Serrx = Udr

   Select Case Serrx
      Case "$":
         Ser_ini = 1
         Serdata = ""

      Case 13:
         If Ser_ini = 1 Then
            Ser_ini = 0
            Serdata = Serdata + Chr(0)
            Serproc = Serdata
            Sernew = 1
            'Enable Timer0
         End If

      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If

   End Select

Return


Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer1:
   Timer1 = &HF63C
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      'Toggle Iluminar
      Led1 = Iluminar
      Incr Num_ventana
   End If

   Incr T1c
   T1c = T1c Mod 100
   If T1c = 0 Then Set Newseg

Return





'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************

'*******************************************************************************
' Inicialización de variables
'*******************************************************************************
Sub Inivar()
Reset Led1
Print #1 , "************ DRIVER AUDIO ************"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)
Estado_led = 1
T1ant = 99
If Vacin = 0 Then
   Set Vacinant
Else
   Reset Vacinant
End If
Lcdat 1 , 1 , "T=-- "

If Puerta = 0 Then
   Set Puertaant
Else
   reSet Puertaant
End If



End Sub


'*****************************************************************************
' Subrutina para leer VAC
'*****************************************************************************
Sub Leer_vac()
   'Locate 2 , 1
   If Vacin = 0 Then
      If Vacin <> Vacinant Then
         Lcdat 1 , 70 , "AC=OK"
         Vacinant = Vacin
      End If
      Reset Relac
      Alarmac = 0
      If Alarmtemp = 0 Then
         Bklsta = 1
      End If
   Else
      If Vacin <> Vacinant Then
         Lcdat 1 , 70 , "AC=NO"
         Vacinant = Vacin
      End If
      Set Relac
      Alarmac = 1
      Bklsta = 2
   End If

End Sub

'*****************************************************************************
' Subrutina para leer la puerta
'*****************************************************************************
Sub Leer_pta()
   'Locate 1 , 14
'   If Puerta = 0 Then
   If Puerta = 1 Then
      If Puerta <> Puertaant Then                           'Cambiado con el hardware AVR
         Puertaant = Puerta
         Lcdat 4 , 1 , "P=A"
      End If
      Set Relpta
   Else
      If Puerta <> Puertaant Then                           'Cambiado con el hardware AVR
         Puertaant = Puerta
         Lcdat 4 , 1 , "P=C"
      End If
      Reset Relpta
   End If

End Sub

'*****************************************************************************
' Subrutina para timer1
'*****************************************************************************
Sub Displcd()
   'Locate 1 , 1
'(
   If Alarmac = 1 Then
      If Alarmtemp = 1 Then
         Dispcntr2 = 30
      Else
         Dispcntr2 = 40
      End If
   Else
      If Alarmtemp = 1 Then
         Dispcntr2 = 20
      Else
         Dispcntr2 = Dispcntr
      End If
   End If
')
   Select Case Cntrdisp
      Case 0:
         Lcdat 7 , 1 , ">              <"
      Case 1:
         Lcdat 7 , 1 , " >            < "
      Case 2:
         Lcdat 7 , 1 , "  >          <  "
      Case 3:
         Lcdat 7 , 1 , "   >        <   "
      Case 4:
         Lcdat 7 , 1 , "    >      <    "
      Case 5:
         Lcdat 7 , 1 , "     >    <     "
      Case 6:
         Lcdat 7 , 1 , "      >  <      "
      Case 7:
         Lcdat 7 , 1 , "       ><       "
      Case 8:
         Lcdat 7 , 1 , "       <>       "
      Case 9:
         Lcdat 7 , 1 , "      <  >      "
      Case 10:
         Lcdat 7 , 1 , "     <    >     "
      Case 11:
         Lcdat 7 , 1 , "    <      >    "
      Case 12:
         Lcdat 7 , 1 , "   <        >   "
      Case 13:
         Lcdat 7 , 1 , "  <          >  "
      Case 14:
         Lcdat 7 , 1 , " <            > "
      Case 15:
         Lcdat 7 , 1 , "<              >"
'      Case 20:
'         Lcd "TEMP ALARM!  "
'      Case 30:
'         Lcd "AC&T  ALARM! "
'      Case 40:
'         Lcd " AC ALARM!   "
   End Select
End Sub
'*****************************************************************************


'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   Print #1 , "$" ; Serproc
   Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   If Numpar > 0 Then
      For Tmpb = 1 To Numpar
         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
      Next
   End If

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Select Case Cmdtmp
         Case "LEEVFW"
            Cmderr = 0
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">, Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"


         Case "SETLED"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 17 Then
                  Cmderr = 0
                  Atsnd = "Se configura setled a " + Str(tmpb)
                  Estado_led = Tmpb

               Else
                  Cmderr = 5
               End If

            Else
               Cmderr = 4

            End If

         Case Else
            Cmderr = 1

      End Select

   Else
        Cmderr = 2
   End If

   If Cmderr > 0 Then
      Atsnd = Lookupstr(cmderr , Tbl_err)
   End If

   Print #1 , Atsnd

End Sub


'*****************************************************************************
' Subrutina para leer temperatura del DS18B20
'*****************************************************************************
Sub Leer_ds18b20()
1wreset
1wwrite &HCC
1wwrite &H44
Waitms 800
'Call Rdramds18b20()

 1wreset                                                    ' reset the bus
 1wwrite &HCC
 1wwrite &HBE                                               ' read 9 data bytest
 For Tr = 1 To 9
    Bt(tr) = 1wread()
 Next                                                       ' read bytes in array
 1wreset

'Call Crcit                                                    'Check CRC

 Crc = 0
 For Ti = 1 To 9
 Tr = Crc Xor Bt(ti)
 Crc = Lookup(tr , Crc8)
 Next

If Crc = 0 Then                                             ' if is OK, calculate for
  Bint = Makeint(bt(1) , Bt(2))
  If Bt(2).3 = 0 Then                                       'Temp postiva
     T1 = Bint / 16
     Signo = "+"
  Else
     Bint = Not Bint                                        ' Comprobar esta subrutina
     Bint = Bint + 1
     T1 = Bint / 16
     Signo = "-"
  End If
   Print #1 , Signo ; Fusing(t1 , "#.##")
Else
   Print #1 , "CRC ERR"
End If

End Sub
'*****************************************************************************


'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************

Crc8:
Data 0 , 94 , 188 , 226 , 97 , 63 , 221 , 131 , 194 , 156
Data 126 , 32 , 163 , 253 , 31 , 65 , 157 , 195 , 33 , 127
Data 252 , 162 , 64 , 30 , 95 , 1 , 227 , 189 , 62 , 96
Data 130 , 220 , 35 , 125 , 159 , 193 , 66 , 28 , 254 , 160
Data 225 , 191 , 93 , 3 , 128 , 222 , 60 , 98 , 190 , 224
Data 2 , 92 , 223 , 129 , 99 , 61 , 124 , 34 , 192 , 158
Data 29 , 67 , 161 , 255 , 70 , 24 , 250 , 164 , 39 , 121
Data 155 , 197 , 132 , 218 , 56 , 102 , 229 , 187 , 89 , 7
Data 219 , 133 , 103 , 57 , 186 , 228 , 6 , 88 , 25 , 71
Data 165 , 251 , 120 , 38 , 196 , 154 , 101 , 59 , 217 , 135
Data 4 , 90 , 184 , 230 , 167 , 249 , 27 , 69 , 198 , 152
Data 122 , 36 , 248 , 166 , 68 , 26 , 153 , 199 , 37 , 123
Data 58 , 100 , 134 , 216 , 91 , 5 , 231 , 185 , 140 , 210
Data 48 , 110 , 237 , 179 , 81 , 15 , 78 , 16 , 242 , 172
Data 47 , 113 , 147 , 205 , 17 , 79 , 173 , 243 , 112 , 46
Data 204 , 146 , 211 , 141 , 111 , 49 , 178 , 236 , 14 , 80
Data 175 , 241 , 19 , 77 , 206 , 144 , 114 , 44 , 109 , 51
Data 209 , 143 , 12 , 82 , 176 , 238 , 50 , 108 , 142 , 208
Data 83 , 13 , 239 , 177 , 240 , 174 , 76 , 18 , 145 , 207
Data 45 , 115 , 202 , 148 , 118 , 40 , 171 , 245 , 23 , 73
Data 8 , 86 , 180 , 234 , 105 , 55 , 213 , 139 , 87 , 9
Data 235 , 181 , 54 , 104 , 138 , 212 , 149 , 203 , 41 , 119
Data 244 , 170 , 72 , 22 , 233 , 183 , 85 , 11 , 136 , 214
Data 52 , 106 , 43 , 117 , 151 , 201 , 74 , 20 , 246 , 168
Data 116 , 42 , 200 , 150 , 21 , 75 , 169 , 247 , 182 , 232
Data 10 , 84 , 215 , 137 , 107 , 53
'*****************************************************************************

Tbl_err:
Data "OK"                                                   '0
Data "Comando no reconocido"                                '1
Data "Longitud comando no valida"                           '2
Data "Numero de usuario no valido"                          '3
Data "Numero de parametros invalido"                        '4
Data "Error longitud parametro 1"                           '5
Data "Error longitud parametro 2"                           '6
Data "Parametro no valido"                                  '7
Data "ERROR8"                                               '8
Data "ERROR SD. Intente de nuevo"                           '9

Tabla_estado:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000011&                    'Estado 1
Data &B00000000000000000000000000110011&                    'Estado 2
Data &B00000000000000000000001100110011&                    'Estado 3
Data &B00000000000000000011001100110011&                    'Estado 4
Data &B00000000000000110011001100110011&                    'Estado 5
Data &B00000000000011001100000000110011&                    'Estado 6
Data &B00001111111111110000111111111111&                    'Estado 7
Data &B01010101010101010101010101010101&                    'Estado 8
Data &B00110011001100110011001100110011&                    'Estado 9
Data &B01110111011101110111011101110111&                    'Estado 10
Data &B11111111111111000000000000001100&                    'Estado 11
Data &B11111111111111000000000011001100&                    'Estado 12
Data &B11111111111111000000110011001100&                    'Estado 13
Data &B11111111111111001100110011001100&                    'Estado 14
Data &B11111111111111000000000000001100&                    'Estado 15
Data &B11111111111111111111111111110000&                    'Estado 16


Pic:
$bgf "WATCHING.bgf"

$include "font8x8TT.font"
$include "Font12x16.font"

Loaded_arch: