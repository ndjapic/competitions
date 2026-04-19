# Problem: C_Bewitching_Stargazer.pas

```pascal
program C_Bewitching_Stargazer;
var
    ntc, tci: int32;
    n, k, c: int32;
    s: int64;

procedure dfs(l, r: int32; var c: int32; var s: int64);
var
    m, d, dc: int32;
    ds: int64;
begin
    c := 0;
    s := 0;
    d := r-l+1;
    if d >= k then begin
        m := (l+r) div 2;
        if odd(d) then begin
            c := 1;
            s := m;
            dfs(l, m-1, dc, ds);
        end else begin
            dfs(l, m, dc, ds);
        end;
        inc(c, 2*dc);
        inc(s, 2*ds + int64(m+1-l) * dc)
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        dfs(1, n, c, s);
        writeln(s);

    end;
end.

```
