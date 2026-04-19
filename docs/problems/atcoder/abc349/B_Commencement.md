# Problem: B_Commencement.pas

```pascal
program B_Commencement;
{$H+}
const
    maxn = 100;
var
    n, i, x: int8;
    ans: boolean;
    s: string;
    c, merge: array [0 .. 25] of int8;

procedure msort(l, r: int8);
var
    m, i, j, k: int8;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (c[j] >= c[k]) then begin
                merge[i] := c[j];
                inc(j);
            end else begin
                merge[i] := c[k];
                inc(k);
            end;

        for i := l to r-1 do c[i] := merge[i];

    end;
end;

begin
    readln(s);
    n := length(s);

    for x := 0 to 25 do c[x] := 0;

    for i := 1 to n do begin
        x := ord(s[i]) - ord('a');
        inc(c[x]);
    end;

    msort(0, 26);

    ans := true;
    for x := 1 to 25 do
        if odd(x) then
            ans := ans and (c[x-1] = c[x])
        else
            ans := ans and ((c[x-1] = 0) or (c[x-1] > c[x]));

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
