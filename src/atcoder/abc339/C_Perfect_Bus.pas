program C_Perfect_Bus;
uses
    math;
var
    n, i, dx: int32;
    x, mn: int64;

begin
    readln(n);

    x := 0;
    mn := 0;

    for i := 1 to n do begin
        read(dx);
        inc(x, dx);
        mn := min(mn, x);
    end;

    writeln(x-mn);
end.
