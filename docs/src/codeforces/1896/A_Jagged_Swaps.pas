program A_Jagged_Swaps;
uses
    math;
const
    maxn = 10;
var
    ntc, tci: int16;
    n, i, x: int8;
    found: boolean;
    a: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]);
        readln;

        found := true;
        while found do begin

            i := n-1;
            while (i > 1) and not ((a[i-1] < a[i]) and (a[i] > a[i+1])) do dec(i);
            found := i > 1;

            if found then begin
                x := a[i];
                a[i] := a[i+1];
                a[i+1] := x;
            end;

        end;

        i := 1;
        while (i < n) and (a[i] < a[i+1]) do inc(i);
        if i < n then
            writeln('NO')
        else
            writeln('YES');

    end;
end.
