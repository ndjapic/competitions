# Problem: B_Reverse_Proxy.pas

```pascal
program B_Reverse_Proxy;
var
    n, x, q, k, i: int8;
    a: array [1 .. 100] of int8;

begin
    readln(n, q);
    for x := 1 to n do a[x] := 0;

    for k := 1 to q do begin

        read(x);
        if x = 0 then begin

            x := 1;
            for i := 1 to n do
                if a[x] > a[i] then x := i;

        end;

        inc(a[x]);
        write(x);
        if k < q then write(' ');

    end;
    readln;
    writeln;
end.

```
