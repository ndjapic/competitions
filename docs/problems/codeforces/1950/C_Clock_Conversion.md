# Problem: C_Clock_Conversion.pas

```pascal
program C_Clock_Conversion;
{$H+}
const
    z = ord('0');
var
    ntc, tci: int16;
    s, t: string;
    time: int16;
    h, m: int8;

begin
    setlength(t, 8);
    t[3] := ':';
    t[6] := ' ';
    t[8] := 'M';

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);

        time := ord(s[1]) - z;
        time := 10 * time + ord(s[2]) - z;
        time := 6 * time + ord(s[4]) - z;
        time := 10 * time + ord(s[5]) - z;

        h := time div 60;
        m := time mod 60;

        if h < 12 then
            t[7] := 'A'
        else begin
            t[7] := 'P';
            dec(h, 12);
        end;

        if h = 0 then h := 12;

        t[1] := chr(z + h div 10);
        t[2] := chr(z + h mod 10);
        t[4] := chr(z + m div 10);
        t[5] := chr(z + m mod 10);

        writeln(t);

    end;
end.

```
