# Problem: B_Collecting_Game.pas

```pascal
program B_Collecting_Game;
const
    maxn = 100 * 1000;
var
    ntc, tci, n, i, l, r, m: int32;
    a, ans, p, merge: array [1 .. maxn] of int32;
    s: array [0 .. maxn] of int64;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (a[p[il]] <= a[p[ir]]) then begin
                merge[i] := p[il];
                inc(il);
            end else begin
                merge[i] := p[ir];
                inc(ir);
            end;

        for i := l to r do p[i] := merge[i];

    end;
end;

function f(i: int32): int32;
begin
    f := a[p[i]];
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
            p[i] := i;
        end;
        readln;
        msort(1, n);

        s[0] := 0;
        for i := 1 to n do s[i] := s[i-1] + f(i);

        ans[p[n]] := n;
        for i := n-1 downto 1 do begin

            l := i;
            r := n+1;
            while r-l > 1 do begin

                m := (l+r) div 2;
                if f(m) <= s[i] then
                    l := m
                else
                    r := m;

            end;

            if l > i then
                ans[p[i]] := ans[p[l]]
            else
                ans[p[i]] := i;

        end;

        for i := 1 to n-1 do write(ans[i]-1, ' '); writeln(ans[n]-1);

    end;
end.

```
