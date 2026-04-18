# Задатак: G_Flip_Row_or_Col.pas

```pascal
program G_Flip_Row_or_Col;
{$MODE DELPHI}
uses
    math;
const
    hh = 200 * 1000;
    ww = 18;
    cc = 511 * 513;

var
    h, i, x, y, xx, t, mn: int32;
    w, e: int8;
    found: boolean;
    s: string;
    a: array [1 .. hh] of int32;
    c: array [0 .. cc] of int8;

procedure p();
var
    i, t, x: int32;
    e: int8;
begin
    for i := 1 to h do
        if (2 * c[a[i]] > w) or (2 * c[a[i]] = w) and odd(a[i]) then begin
            a[i] := xx - a[i];
            found := true;
        end;

    x := 0;
    for e := 0 to 29 do begin
        t := 0;
        for i := 1 to h do
            inc(t, (a[i] shr e) and 1);
        if (t > h-t) or (t = h-t) and odd(a[1] shr e) then
            inc(x, (int32(1) shl e));
    end;

    if x > 0 then begin
        for i := 1 to h do
            a[i] := a[i] xor x;
        found := true;
    end;
end;

begin
    readln(h, w);

    c[0] := 0;
    xx := (int32(1) shl w) - 1;
    for x := 1 to xx do
        c[x] := c[x div 2] + x mod 2;
    {for x := 1 to xx do
        c[x] := min(c[x], w-c[x]);}

    for i := 1 to h do begin
        readln(s);
        a[i] := 0;
        for e := 0 to w-1 do
            if s[w-e] = '1' then
                inc(a[i], int32(1) shl e);
    end;

    found := false;
    while not found do p();
    p();
    p();
    p();
    p();
    p();
    p();

    if h < 15 then begin

        mn := h*w;
        for x := 0 to xx do begin
            t := 0;
            for i := 1 to h do begin
                y := a[i] xor x;
                inc(t, min(c[y], w - c[y]));
            end;
            mn := min(mn, t);
        end;

    end else begin

        mn := 0;
        for i := 1 to h do
            inc(mn, min(c[a[i]], w - c[a[i]]));

    end;

    writeln(mn);
end.

```
