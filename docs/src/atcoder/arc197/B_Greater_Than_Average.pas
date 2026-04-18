program B_Greater_Than_Average;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, l, r, ave, score: int32;
    s: int64;
    a, cp: array [1 .. nn] of int32;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (a[il] <= a[ir]) then begin
                cp[i] := a[il];
                inc(il);
            end else begin
                cp[i] := a[ir];
                inc(ir);
            end;

        for i := l to r-1 do a[i] := cp[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;
        msort(1, n+1);

        l := 1;
        score := 0;
        s := 0;
        for r := 1 to n do begin
            inc(s, a[r]);
            ave := s div r;
            while (l <= r) and (a[l] <= ave) do inc(l);
            score := max(score, r-l+1);
        end;

        writeln(score);
    end;
end.
