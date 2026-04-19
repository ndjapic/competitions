# Problem: C_Perform_Operations_to_Maximize_Score.pas

```pascal
program C_Perform_Operations_to_Maximize_Score;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;

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
        property items[index: sizeint]: _t read getItem write setItem;
    end;

type
    tdata = record
        a: int32;
        b: int8;
    end;
    comparer = specialize tcomparer<tdata>;
    datalist = specialize tlist<tdata, comparer>;

var
    ntc, tci: int32;
    n, k, i, m, c, ans1, ans2: int32;
    s: int64;
    d: tdata;
    cmp: comparer;
    list: datalist;

function tcomparer.LessOrEqual(constref lhs, rhs: _t): boolean; inline;
begin
    result := lhs.a <= rhs.a;
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


begin
    list := datalist.create(1, nn+1);
    cmp := comparer.create();

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        list.clear();

        for i := 1 to n do begin
            read(d.a);
            list.items[i] := d;
        end;
        readln;

        for i := 1 to n do begin
            d.a := list.items[i].a;
            read(d.b);
            list.items[i] := d;
        end;
        readln;

        list.sort(cmp, false);

        m := n div 2;
        ans1 := list.items[n].a + list.items[m].a;

        i := n;
        while (i > m) and (list.items[i].b = 0) do dec(i);
        if i > m then
            ans1 := max(ans1, list.items[i].a + k + list.items[m].a);

        i := m;
        s := k;
        while (i < n) and (list.items[i].b = 1) do begin
            inc(s, list.items[i].a);
            inc(i);
            if s div (i-m) <= list.items[i].a then
                ans1 := max(ans1, list.items[n].a + s div (i-m));
        end;

        m := (n+2) div 2;
        ans2 := list.items[m-1].a + list.items[m].a;

        s := k;
        c := 0;
        for i := m-1 downto 1 do
            if list.items[i].b = 1 then begin
                inc(c);
                inc(s, list.items[i].a);
                if s div c <= list.items[m].a then
                    ans2 := max(ans2, (s+c-1) div c + list.items[m].a);
            end;

        writeln(max(ans1, ans2));

    end;
end.

```
