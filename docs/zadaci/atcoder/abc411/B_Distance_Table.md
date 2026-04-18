# Задатак: B_Distance_Table.pas

```pascal
program B_Distance_Table;
const
    nn = 50;
var
    n, i, j: int8;
    d: array [1 .. nn] of int8;
    s: array [1 .. nn] of int16;

begin
    readln(n);

    s[1] := 0;
    for i := 1 to n-1 do begin
        read(d[i]);
        s[i+1] := s[i] + d[i];
    end;
    readln;

    for i := 1 to n-1 do begin
        for j := 1 to n-i do write(s[i+j] - s[i], ' ');
        writeln;
    end;
end.

```
