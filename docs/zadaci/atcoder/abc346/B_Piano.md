# Задатак: B_Piano.pas

```pascal
program B_Piano;
{$H+}
const
    maxn = 240;
var
    i, l, r, w, b: int16;
    ans: boolean;
    s, t: string;
    nw, nb: array [0 .. maxn] of int8;

begin
    t := 'wbwbwwbwbwbw';
    setlength(s, maxn);
    for i := 1 to maxn do
        s[i] := t[(i-1) mod 12 + 1];

    readln(w, b);
    l := 1;
    r := 1;
    nw[0] := 0;
    nb[0] := 0;
    ans := false;

    while (r <= maxn) and not ans do begin

        nw[r] := nw[r-1];
        nb[r] := nb[r-1];

        if s[r] = 'w' then
            inc(nw[r])
        else
            inc(nb[r]);

        while (nw[r] - nw[l-1] > w) or (nb[r] - nb[l-1] > b) do inc(l);
        ans := (nw[r] - nw[l-1] = w) and (nb[r] - nb[l-1] = b);

        inc(r);
    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
