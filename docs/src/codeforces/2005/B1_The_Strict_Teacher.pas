program B1_The_Strict_Teacher;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math;

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

type
    icomparer = specialize tcomparer<int32>;
    ilist = specialize tlist<int32, icomparer>;

var
    ntc, tci, n, m, q, i, j, k, a, x, ans: int32;
    cmp: icomparer;
    b: ilist;


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
        cmp.LessOrEqual(fitems[result], fitems[result - 1]) do inc(result);
end;

function tlist.nonDecreasingTo(lend, rend: sizeint; cmp: _c): sizeint; inline;
begin
    result := lend + 1;
    while (result < rend) and
        cmp.LessOrEqual(fitems[result - 1], fitems[result]) do inc(result);
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
            items2[l] := fitems[l];
            fitems[l] := fitems[r];
            fitems[r] := items2[l];
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
            if (r = rend) or (l < m) and cmp.LessOrEqual(fitems[l], fitems[r]) then begin
                items2[i] := fitems[l];
                inc(l);
            end else begin
                items2[i] := fitems[r];
                inc(r);
            end;

        for i := lend to rend - 1 do fitems[i] := items2[i];
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
        if cmp.LessOrEqual(fitems[m], x) then
            l := m
        else
            r := m;
    end;
    bisectr := i0+r;
end;

begin
    cmp := icomparer.create();
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, q);

        b := iList.Create(1, m);
        for i := 1 to m do begin
            read(x);
            b[i] := x;
        end;
        readln;
        b.Sort(cmp, false);

        for j := 1 to q do begin

            read(a);
            k := b.bisectr(a, cmp);

            if k = 1 then
                ans := b[1] - 1
            else if k = m+1 then
                ans := n - b[m]
            else
                ans := (b[k] - b[k-1]) div 2;

            writeln(ans);

        end;
        readln;

        b.free();
    end;
    cmp.free();
end.
