program A_Bazoka_and_Mocha_s_Array;
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, j: int8;
    found: boolean;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do begin
            read(a[i]);
            a[i+n] := a[i];
        end;
        readln;

        j := 1;
        found := false;
        while not found and (j <= n) do begin
            i := j+1;
            while (i < j+n) and (a[i-1] <= a[i]) do inc(i);
            found := i = j+n;
            inc(j);
        end;

        if found then
            writeln('Yes')
        else
            writeln('No');

    end;
end.
