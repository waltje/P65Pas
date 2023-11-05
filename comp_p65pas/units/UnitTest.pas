{Unit used to test operations of the compiler. 
Include simple functions to generate tones, using a buzzer or led in the pin PORTB.0 or GPIO.0,
like a way of detect errors on the results of the operations.
                                                  By TIto Hinostroza 05/06/2017}
unit UnitTest;
interface
var
  PORTB   : byte absolute $06;   //This will work on 12 bits instructions (GPIO) and 14 bits instructions (PORTB)
  pinLed  : boolean absolute PORTB;
 
  procedure good;
  procedure bad;
  
implementation

  procedure good;
  begin
    pinLed := true;
    delay_ms(word(30));
    pinLed := false;
    delay_ms(word(30));
  end;

  procedure bad;
  begin
    pinLed := false;
    delay_ms(1500);
    pinLed := true;
  end;

end.

