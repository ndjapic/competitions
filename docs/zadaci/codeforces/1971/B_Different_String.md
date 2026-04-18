# Задатак: B_Different_String.pas

```pascal
program B_Different_String;
{$H+}
var
    ntc, tci: int16;
    n, i: int8;
    s: string;
    ch: char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        i := 1;
        while (i < n) and (s[i] = s[i+1]) do inc(i);

        if i < n then begin
            writeln('YES');
            ch := s[i];
            s[i] := s[i+1];
            s[i+1] := ch;
            writeln(s);
        end else
            writeln('NO');

    end;
end.

```
