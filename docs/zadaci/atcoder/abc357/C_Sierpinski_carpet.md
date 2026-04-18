# Задатак: C_Sierpinski_carpet.pas

```pascal
program C_Sierpinski_carpet;
{$H+}
var
    n, k: int8;
    i, j, a: int16;
    ch: char;
    s: array [1 .. 729] of string;

begin
    readln(n);

    a := 1;
    setlength(s[1], 1);
    s[1][1] := '#';

    for k := 1 to n do begin
        for i := 1 to a do begin

            setlength(s[i], 3*a);
            setlength(s[i+a], 3*a);
            setlength(s[i+a*2], 3*a);

            for j := 1 to a do begin
                ch := s[i][j];
                s[i][j+a] := ch;
                s[i][j+a*2] := ch;
                s[i+a][j] := ch;
                s[i+a][j+a] := '.';
                s[i+a][j+a*2] := ch;
                s[i+a*2][j] := ch;
                s[i+a*2][j+a] := ch;
                s[i+a*2][j+a*2] := ch;
            end;

        end;
        a := a*3;
    end;

    for i := 1 to a do writeln(s[i]);
end.

```
