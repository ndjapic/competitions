program C_Theofanis_Nightmare;
uses
    math;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    ans: int64;
    a: array [0 .. maxn] of int64;

begin
    a[0] := 0;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
            inc(a[i], a[i-1]);
        end;
        readln;

        ans := a[n];
        for i := 1 to n-1 do
            inc(ans, max(0, a[n] - a[i]));

        writeln(ans);

    end;
end.
