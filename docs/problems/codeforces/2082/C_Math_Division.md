# Problem: C_Math_Division.pas

```pascal
program C_Math_Division;
var
    ntc, tci: int16;
    x, mn, mx, n, m, i, j: int32;

function c1(x: int32): int8;
begin
    if x = 0 then
        c1 := 0
    else
        c1 := c1(x div 2) + x mod 2;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(x, n, m);

        mn := x;
        i := 0;
        j := 0;

        while (i < n) or (j < m) do
            if mn = 0 then begin
                i := n;
                j := m;
            end else if (j < m) and (mn = 1) then begin
                j := m;
            end else if i = n then begin
                mn := (mn+1) div 2;
                inc(j);
            end else if j = m then begin
                mn := mn div 2;
                inc(i);
            end else if odd(mn) then begin
                mn := mn div 2;
                inc(i);
            end else begin
                mn := (mn+1) div 2;
                inc(j);
            end;

        mx := x;
        i := 0;
        j := 0;

        while (i < n) or (j < m) do
            if mx = 0 then begin
                i := n;
                j := m;
            end else if (j < m) and (mx = 1) then begin
                j := m;
            end else if i = n then begin
                mx := (mx+1) div 2;
                inc(j);
            end else if j = m then begin
                mx := mx div 2;
                inc(i);
            end else if odd(mx) then begin
                mx := (mx+1) div 2;
                inc(j);
            end else begin
                mx := mx div 2;
                inc(i);
            end;

        writeln(mn, ' ', mx);

    end;

end.

```
