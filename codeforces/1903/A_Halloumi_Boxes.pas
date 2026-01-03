program A_Halloumi_Boxes;
const
    maxn = 100;
var
    ntc, tci: int8;
    n, k, i: int8;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        for i := 1 to n do read(a[i]); readln;

        if k > 1 then
            writeln('YES')
        else begin

            i := 1;
            while (i < n) and (a[i] <= a[i+1]) do inc(i);

            if i < n then
                writeln('NO')
            else
                writeln('YES');

        end;

    end;
end.
