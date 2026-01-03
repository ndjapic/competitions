program C_Anji_s_Binary_Tree; {$H+}
uses
    math;
const
    maxn = 300 * 1000;
var
    ntc, tci, n, i: int32;
    s: string;
    l, r, a: array [1 .. maxn] of int32;

procedure dfs(i: int32);
begin
    if (l[i] = 0) and (r[i] = 0) then
        a[i] := 0
    else begin

        a[i] := high(int32);

        if l[i] > 0 then begin
            dfs(l[i]);
            if s[i] <> 'L' then inc(a[l[i]]);
            a[i] := min(a[i], a[l[i]]);
        end;

        if r[i] > 0 then begin
            dfs(r[i]);
            if s[i] <> 'R' then inc(a[r[i]]);
            a[i] := min(a[i], a[r[i]])
        end;

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);
        for i := 1 to n do readln(l[i], r[i]);

        dfs(1);
        writeln(a[1]);

    end;
end.
