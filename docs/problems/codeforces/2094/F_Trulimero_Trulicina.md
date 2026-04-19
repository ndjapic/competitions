# Problem: F_Trulimero_Trulicina.pas

```pascal
program F_Trulimero_Trulicina;
var
    ntc, tci: int16;
    n, m, i, j: int32;
    k: int64;
    a: array of array of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

        setlength(a, n);
        for i := 0 to n-1 do setlength(a[i], m);

        if m mod k = 0 then
            for i := 0 to n-1 do
                for j := 0 to m-1 do
                    a[i][j] := (i + j) mod k + 1
        else
            for i := 0 to n-1 do begin
                for j := 0 to m-1 do begin
                    a[i][j] := (i*m + j) mod k + 1
                end;
            end;

        for i := 0 to n-1 do begin
            for j := 0 to m-2 do write(a[i][j], ' ');
            writeln(a[i][m-1]);
        end;

    end;
end.

```
