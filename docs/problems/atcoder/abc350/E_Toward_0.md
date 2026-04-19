# Problem: E_Toward_0.pas

```pascal
program E_Toward_0;
{$mode delphi}
uses
    math;
const
    inf = 1024 * 1024 * 1024 * 1024 * 1024 * 1024;
    hte = -1.0;
var
    n, hash0: int64;
    a: int8;
    x, htn: int32;
    y: extended;
    ht: array of record
        x: int64;
        y: extended;
        l, r: int32;
    end;

procedure ht_init();
begin
    randomize;
    hash0 := random(inf);
    htn := 0;
    setlength(ht, 1);
end;

function ht_append(x: int64; y: extended): int32;
begin
    inc(htn);
    {if htn mod (1000 * 1000) = 0 then writeln(' htn=',htn);}
    flush(output);
    if length(ht) = htn then setlength(ht, htn*2);
    ht[htn].x := x xor hash0;
    ht[htn].y := y;
    ht[htn].l := 0;
    ht[htn].r := 0;
    ht_append := htn;
end;

procedure ht_update(x: int64; y: extended);
var
    u, v: int32;
    h: int64;
begin
    h := x xor hash0;
    v := 1;

    while (v > 0) and (h <> ht[v].x) do begin
        u := v;
        if h < ht[u].x then
            v := ht[v].l
        else
            v := ht[v].r;
    end;

    if v > 0 then
        ht[v].y := y
    else if h < ht[u].x then
        ht[u].l := ht_append(x, y)
    else
        ht[u].r := ht_append(x, y);
end;

function ht_query(x: int64): extended;
var
    v: int32;
    h: int64;
begin
    h := x xor hash0;
    v := 1;

    while (v > 0) and (h <> ht[v].x) do
        if h < ht[v].x then
            v := ht[v].l
        else
            v := ht[v].r;

    if v > 0 then
        ht_query := ht[v].y
    else
        ht_query := hte;
end;

function ans(n: int64): extended;
var
    b: int8;
begin
    result := ht_query(n);
    if result = hte then begin
        result := y * 6.0;
        for b := 2 to 6 do result := result + ans(n div b);
        result := min(result * 0.2, ans(n div a) + x);
        ht_update(n, result);
    end;
end;

begin
    readln(n, a, x, y);

    ht_init();
    htn := ht_append(0, 0.0);

    writeln(ans(n):30:15);
end.

```
