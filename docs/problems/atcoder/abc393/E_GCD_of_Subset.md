# Problem: E_GCD_of_Subset.pas

```pascal
program E_GCD_of_Subset;
{$MODE DELPHI}
uses
    math;
const
    nn = 1200 * 1000;
    aa = 1000 * 1000;
    hte = 0;
type
    tarr = array of int32;
var
    n, i, k, ai, d, hash0, ioi: int32;
    istr, ostr: string;
    g: array [1 .. nn] of int32;
    ht: array [1 .. aa] of array of record
        x, y, l, r: int32;
    end;
    htn: array [1 .. aa] of int32;

procedure ht_init();
var
    ai: int32;
begin
    randomize;
    hash0 := random(2048 * 1024);
    for ai := 1 to aa do begin
        setlength(ht[ai], 1);
        htn[ai] := 0;
    end;
end;

function ht_append(ai, x, y: int32): int32;
begin
    result := htn[ai];
    if htn[ai] = length(ht[ai]) then setlength(ht[ai], 2 * htn[ai]);
    ht[ai][htn[ai]].x := x xor hash0;
    ht[ai][htn[ai]].y := y;
    ht[ai][htn[ai]].l := -1;
    ht[ai][htn[ai]].r := -1;
    inc(htn[ai]);
end;

procedure ht_update(ai, x, y: int32);
var
    u, v, h: int32;
begin
    h := x xor hash0;
    v := 0;

    while (v > -1) and (h <> ht[ai][v].x) do begin
        u := v;
        if h < ht[ai][u].x then
            v := ht[ai][v].l
        else
            v := ht[ai][v].r;
    end;

    if v > -1 then
        {ht[ai][v].y := max(ht[ai][v].y, y)}
    else if h < ht[ai][u].x then
        ht[ai][u].l := ht_append(ai, x, y)
    else
        ht[ai][u].r := ht_append(ai, x, y);
end;

function ht_query(ai, x: int32): int32;
var
    v, h: int32;
begin
    h := x xor hash0;
    v := 0;

    while (v > -1) and (h <> ht[ai][v].x) do
        if h < ht[ai][v].x then
            v := ht[ai][v].l
        else
            v := ht[ai][v].r;

    if v > -1 then
        ht_query := ht[ai][v].y
    else
        ht_query := hte;
end;

procedure dfs1(m, d, v: int32);
var
    x: int32;
begin
    if v > -1 then begin
        x := ht[m][v].x xor hash0;
        if htn[d] = 0 then
            htn[d] := ht_append(d, x, 1) + 1
        else
            ht_update(d, x, 1);
        dfs1(m, d, ht[m][v].l);
        dfs1(m, d, ht[m][v].r);
    end;
end;

procedure dfs2(d, v: int32);
var
    i: int32;
begin
    if v > -1 then begin
        i := ht[d][v].x xor hash0;
        if g[i] = 1 then g[i] := d;
        dfs2(d, ht[d][v].l);
        dfs2(d, ht[d][v].r);
    end;
end;

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

procedure writedword(x: dword);
begin
    if x >= 10 then writedword(x div 10);
    inc(ioi);
    ostr[ioi] := chr(x mod 10 + ord('0'));
end;

begin
    readln(n, k);
    ht_init();

    readln(istr);
    istr := istr + ' ';
    ioi := 1;

    for i := 1 to n do begin
        ai := readdword();
        if htn[ai] = 0 then
            htn[ai] := ht_append(ai, i, 1) + 1
        else
            ht_update(ai, i, 1);
        g[i] := 1;
    end;

    for d := 2 to aa do begin
        ai := 2*d;
        while ai <= aa do begin
            if htn[ai] > 0 then dfs1(ai, d, 0);
            inc(ai, d);
        end;
    end;

    for d := aa downto 2 do
        if htn[d] >= k then dfs2(d, 0);

    setlength(ostr, n*8);
    ioi := 0;

    for i := 1 to n do begin
        writedword(g[i]);
        inc(ioi);
        ostr[ioi] := LineEnding;
    end;

    setlength(ostr, ioi-1);
    writeln(ostr);
end.

```
