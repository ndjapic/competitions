# Problem: C_Nene_s_Magical_Matrix.pas

```pascal
program C_Nene_s_Magical_Matrix;
const
    maxn = 500;
var
    ntc, tci: int16;
    n, i, j: int16;
    c: int8;
    s: array [0 .. maxn] of int32;

begin
    s[0] := 0;
    for n := 1 to maxn do s[n] := s[n-1] + n * (2*n-1);

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        writeln(s[n], ' ', 2*n);

        for i := n downto 1 do
            for c := 1 to 2 do begin
                write(c, ' ', i);
                for j := 1 to n do write(' ', j);
                writeln;
            end;

    end;
end.

```
