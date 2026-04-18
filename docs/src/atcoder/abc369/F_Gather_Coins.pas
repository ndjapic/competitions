program F_Gather_Coins;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math;
const
    nn = 200 * 1000 + 2;
    tt = 512 * 1024;

type
    generic tcomparer<_t> = class
    public
        function LessOrEqual(constref lhs, rhs: _t): boolean; inline;
    end;
    generic tlist<_t, _c> = class
    private
        i0: sizeint;
        fitems, items2: array of _t;
        count: sizeint;
        function getItem(index: SizeInt): _t; inline;
        procedure setItem(index: SizeInt; item: _t); inline;
    public
        procedure clear();
        constructor create(l, r: sizeint);
        destructor destroy(); override;
        function nonIncreasingTo(lend, rend: sizeint; cmp: _c): sizeint; inline;
        function nonDecreasingTo(lend, rend: sizeint; cmp: _c): sizeint; inline;
        procedure MergeSort(lend, rend: sizeint; cmp: _c; stable: boolean);
        procedure sort(cmp: _c; stable: boolean);
        function bisectr(x: _t; cmp: _c): sizeint;
        property items[index: sizeint]: _t read getItem write setItem; default;
    end;
    icomparer = specialize tcomparer<int64>;
    ilist = specialize tlist<int64, icomparer>;

var
    h, w, n, i, r, c, u, v: int32;
    s: string;
    cmp: icomparer;
    coins: ilist;
    mx, par: array [1 .. nn] of int32;
    st: array [1 .. tt] of int32;

function tcomparer.LessOrEqual(constref lhs, rhs: _t): boolean; inline;
begin
    result := lhs <= rhs;
end;

function tlist.getItem(index: SizeInt): _t; inline;
begin
    result := fitems[index - i0];
end;

procedure tlist.setItem(index: SizeInt; item: _t); inline;
begin
    dec(index, i0);
    if length(fitems) <= index then setlength(fitems, 2 * index);
    fitems[index] := item;
    count := max(count, index + 1);
end;

procedure tlist.clear();
begin
    count := 0;
end;

constructor tlist.create(l, r: sizeint);
begin
    setlength(fitems, max(1, r-l));
    clear();
    i0 := l;
end;

destructor tlist.destroy();
begin
    setlength(fitems, 0);
    setlength(items2, 0);
    inherited;
end;

function tlist.nonIncreasingTo(lend, rend: sizeint; cmp: _c): sizeint; inline;
begin
    result := lend + 1;
    while (result < rend) and
        cmp.LessOrEqual(items[result], items[result - 1]) do inc(result);
end;

function tlist.nonDecreasingTo(lend, rend: sizeint; cmp: _c): sizeint; inline;
begin
    result := lend + 1;
    while (result < rend) and
        cmp.LessOrEqual(items[result - 1], items[result]) do inc(result);
end;

procedure tlist.MergeSort(lend, rend: sizeint; cmp: _c; stable: boolean);
var
    i, l, r, m: sizeint;

begin
    i := nonDecreasingTo(lend, rend, cmp);
    if i >= rend then
    else if not stable and (nonIncreasingTo(lend, rend, cmp) >= rend) then begin

        l := lend;
        r := rend - 1;
        while l < r do begin
            items2[l] := items[l];
            items[l] := items[r];
            items[r] := items2[l];
            inc(l);
            dec(r);
        end;

    end else begin

        m := (lend + rend) div 2;
        if i < m then MergeSort(lend, m, cmp, stable);
        MergeSort(m, rend, cmp, stable);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and cmp.LessOrEqual(items[l], items[r]) then begin
                items2[i] := items[l];
                inc(l);
            end else begin
                items2[i] := items[r];
                inc(r);
            end;

        for i := lend to rend - 1 do items[i] := items2[i];
    end;
end;

procedure tlist.sort(cmp: _c; stable: boolean);
begin
    if length(items2) < length(fitems) then setlength(items2, length(fitems));
    MergeSort(0, count, cmp, stable);
end;

function tlist.bisectr(x: _t; cmp: _c): sizeint;
var
    l, r, m: sizeint;
begin
    l := -1;
    r := count;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if cmp.LessOrEqual(items[m], x) then
            l := m
        else
            r := m;
    end;
    bisectr := r;
end;

procedure setcoin(i, r, c: int32); inline;
begin
    coins[i] := (int64(r) shl 32) + c;
end;

function getr(i: int32): int32; inline;
begin
    result := hi(coins[i]);
end;

function getc(i: int32): int32; inline;
begin
    result := lo(coins[i]);
end;

procedure combine(v: int32); inline;
begin
    if (st[2*v+1] = -1) or (st[2*v] <> -1) and (
        mx[getc(st[2*v])] >= mx[getc(st[2*v+1])]
    ) then
        st[v] := st[2*v]
    else
        st[v] := st[2*v+1];
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
    end;
    st[v] := -1;
end;

procedure update(v, vl, vr, l, r, i: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
    else if (l <= vl) and (vr <= r) then begin
        st[v] := i;
    end else {if vl < vr then} begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, l, r, i);
        update(2*v+1, m+1, vr, l, r, i);
        combine(v);
    end;
end;

function query(v, vl, vr, l, r: int32): int32;
var
    m, lq, rq: int32;
begin
    if (r < vl) or (vr < l) then
        query := 0
    else if (l <= vl) and (vr <= r) then
        query := st[v]
    else {if vl < vr then} begin

        m := (vl+vr) div 2;
        lq := query(2*v, vl, m, l, r);
        rq := query(2*v+1, m+1, vr, l, r);

        if (rq = -1) or (lq <> -1) and (
            mx[getc(lq)] >= mx[getc(rq)]
        ) then
            query := lq
        else
            query := rq;

    end;
end;

begin
    readln(h, w, n);
    cmp := icomparer.create();
    coins := ilist.create(0, 0);

    setcoin(0, 1, 1);
    for i := 1 to n do begin
        readln(r, c);
        setcoin(i, r, c);
    end;
    setcoin(n+1, h, w);

    coins.sort(cmp, false);
    for c := 1 to w do mx[c] := 0;
    mx[1] := 1;
    build(1, 1, w);

    update(1, 1, w, 1, 1, 0);
    for v := 1 to n+1 do begin
        r := getr(v);
        c := getc(v);
        u := query(1, 1, w, 1, c);
        par[v] := u;
        mx[c] := mx[getc(u)] + 1;
        update(1, 1, w, c, c, v);
    end;

    writeln(mx[w] - 2);

    setlength(s, h+w-2);
    i := h+w-2;
    v := n+1;
    r := getr(v);
    c := getc(v);

    while v > 0 do begin
        u := par[v];

        while c > getc(u) do begin
            s[i] := 'R';
            dec(i);
            dec(c);
        end;

        while r > getr(u) do begin
            s[i] := 'D';
            dec(i);
            dec(r);
        end;

        v := u;
    end;

    writeln(s);
end.
