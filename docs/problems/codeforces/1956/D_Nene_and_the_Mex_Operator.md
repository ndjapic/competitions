# Problem: D_Nene_and_the_Mex_Operator.pas

```pascal
program D_Nene_and_the_Mex_Operator;
const
    maxn = 18;
    maxm = 500 * 1000;
var
    n, i, l, r: int8;
    mask, mask0, s, s0, m, k: int32;
    a: array [1 .. maxn] of int32;
    op: array [1 .. maxm] of record
        l, r: int8;
    end;

procedure append(l, r: int8);
begin
    inc(m);
    op[m].l := l;
    op[m].r := r;
end;

procedure dfs(l, r: int8);
begin
    if r-l+1 > 1 then begin
        dfs(l, r-1);
        append(l, r);
        append(l, r-1);
        dfs(l, r-1);
    end;
end;

begin
    readln(n);
    for i := 1 to n do read(a[i]); readln;

    mask0 := 0;
    s0 := 0;

    for mask := 0 to (int32(1) shl n) - 1 do begin

        l := 1;
        s := 0;
        for r := 1 to n do
            if odd(mask shr (r-1)) then begin
                inc(s, a[r]);
                l := r+1;
            end else
                inc(s, 2*(r-l+1)-1);

        if s0 < s then begin
            s0 := s;
            mask0 := mask;
        end;

    end;

    l := 1;
    m := 0;
    for r := 1 to n do
        if odd(mask0 shr (r-1)) then
            l := r+1
        else if (r = n) or odd(mask0 shr r) then begin
            for i := l to r do
                if a[i] > 0 then append(i, i);
            dfs(l, r);
            append(l, r);
        end;

    writeln(s0, ' ', m);
    for k := 1 to m do writeln(op[k].l, ' ', op[k].r);
end.

```
