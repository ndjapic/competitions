program B_AND_Reconstruction;
uses
    math;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    a, b: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n-1 do read(b[i]);
        readln;

        a[1] := b[1];
        a[n] := b[n-1];

        for i := 2 to n-1 do a[i] := b[i-1] or b[i];

        i := 1;
        while (i < n) and (b[i] = a[i] and a[i+1]) do inc(i);

        if i < n then
            writeln(-1)
        else begin
            for i := 1 to n-1 do write(a[i], ' ');
            writeln(a[n]);
        end;

    end;
end.
