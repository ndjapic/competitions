program C_Repunit_Trio;
var
    n, r, d, i: int16;
    a: array [0 .. 2] of int16;

function ncr_rep(n, r: int16): int16;
begin
    if r = 0 then
        ncr_rep := 1
    else
        ncr_rep := ncr_rep(n+1, r-1) * n div r;
end;

begin
    readln(n);

    for r := 2 downto 0 do begin
        a[r] := 1;
        d := ncr_rep(a[r], r);
        while n > d do begin
            dec(n, d);
            inc(a[r]);
            d := ncr_rep(a[r], r);
        end;
    end;

    for r := 2 downto 1 do dec(a[r], a[r-1]);
    for r := 2 downto 0 do
        for i := 1 to a[r] do write(3-r);
    writeln;
end.
