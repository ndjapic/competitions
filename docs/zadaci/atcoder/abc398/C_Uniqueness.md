# Задатак: C_Uniqueness.pas

```pascal
program C_Uniqueness;
{$H+}
const
    nn = 300 * 1000;
var
    n, i, ioi: int32;
    istr: string;
    a, p, cp: array [1 .. nn] of int32;

function readdword(): dword;
var
    ans: dword;
begin
    ans := 0;
    while (istr[ioi] < '0') or (istr[ioi] > '9') do inc(ioi);
    while (istr[ioi] >= '0') and (istr[ioi] <= '9') do begin
        ans := ans * 10 + ord(istr[ioi]) - ord('0');
        inc(ioi);
    end;
    readdword := ans;
end;

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
            if (ir > r) or (il <= m) and (a[p[il]] >= a[p[ir]]) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r do p[i] := cp[i];

    end;
end;

begin
    readln(n);

    readln(istr);
    istr := istr + ' ';
    ioi := 1;

    for i := 1 to n do begin
        a[i] := readdword();
        p[i] := i;
    end;
    msort(1, n);

    i := 1;
    if (n > 1) and (a[p[1]] = a[p[2]]) then begin
        i := 2;
        while (i < n) and ((a[p[i-1]] = a[p[i]]) or (a[p[i]] = a[p[i+1]])) do
            inc(i);
        if (i = n) and (a[p[n-1]] = a[p[n]]) then inc(i);
    end;

    if i <= n then
        writeln(p[i])
    else
        writeln(-1);
end.

```
